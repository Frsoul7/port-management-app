import axios, { AxiosInstance, AxiosError } from 'axios';
import { VvnReference } from '@shared/types';
import { logger } from '@shared/utils/logger';
import { ICoreBackendClient } from '@domain/ports';

/**
 * HTTP Client for Core Backend integration
 * Fetches Vessel Visit Notifications (VVNs) from the Core Backend
 * Automatically handles authentication using admin credentials
 * 
 * Implements ICoreBackendClient interface for Clean Architecture compliance
 */
export class CoreBackendClient implements ICoreBackendClient {
  private readonly client: AxiosInstance;
  private readonly maxRetries: number;
  private readonly retryDelay: number;
  private adminToken: string | null = null;
  private tokenExpiry: Date | null = null;

  constructor(
    baseURL: string = 'http://localhost:5001',
    maxRetries: number = 3,
    retryDelay: number = 1000
  ) {
    logger.info(`🔗 CoreBackendClient initialized with baseURL: ${baseURL}`);
    
    this.client = axios.create({
      baseURL,
      timeout: 10000,
      headers: {
        'Content-Type': 'application/json',
      },
    });

    this.maxRetries = maxRetries;
    this.retryDelay = retryDelay;

    // Add response interceptor for logging
    this.client.interceptors.response.use(
      (response) => {
        logger.info(
          `CoreBackendClient: ${response.config.method?.toUpperCase()} ${response.config.url} - ${response.status}`
        );
        return response;
      },
      (error) => {
        if (error.response) {
          logger.error(
            `CoreBackendClient: ${error.config.method?.toUpperCase()} ${error.config.url} - ${error.response.status} ${error.response.statusText}`
          );
        } else if (error.request) {
          logger.error(
            `CoreBackendClient: No response received for ${error.config.method?.toUpperCase()} ${error.config.url}`
          );
        } else {
          logger.error(`CoreBackendClient: Request setup error - ${error.message}`);
        }
        return Promise.reject(error);
      }
    );
  }

  /**
   * Retry logic for failed requests
   */
  private async retry<T>(fn: () => Promise<T>, retries: number = this.maxRetries): Promise<T> {
    try {
      return await fn();
    } catch (error) {
      const axiosError = error as AxiosError;

      // Convert axios errors to meaningful error messages when giving up
      if (retries === 0) {
        throw this.handleError(axiosError);
      }

      // Only retry on network errors or 5xx server errors
      const shouldRetry =
        !axiosError.response ||
        (axiosError.response.status >= 500 && axiosError.response.status < 600);

      if (!shouldRetry) {
        throw this.handleError(axiosError);
      }

      logger.warn(`Retrying request... (${this.maxRetries - retries + 1}/${this.maxRetries})`);
      await this.sleep(this.retryDelay);
      return this.retry(fn, retries - 1);
    }
  }

  /**
   * Sleep utility for retry delays
   */
  private sleep(ms: number): Promise<void> {
    return new Promise((resolve) => setTimeout(resolve, ms));
  }

  /**
   * Authenticate with Core Backend using admin credentials
   * Caches token for reuse until expiry
   */
  private async authenticateWithCoreBackend(): Promise<string> {
    // Return cached token if still valid
    if (this.adminToken && this.tokenExpiry && this.tokenExpiry > new Date()) {
      return this.adminToken;
    }

    try {
      const adminEmail = process.env.CORE_BACKEND_ADMIN_EMAIL;
      const adminPassword = process.env.CORE_BACKEND_ADMIN_PASSWORD;

      if (!adminEmail || !adminPassword) {
        throw new Error(
          'Core Backend admin credentials not configured. Set CORE_BACKEND_ADMIN_EMAIL and CORE_BACKEND_ADMIN_PASSWORD in .env'
        );
      }

      logger.info('Authenticating OEM Backend with Core Backend...');

      const response = await this.client.post<{
        accessToken: string;
        expiresIn: number;
      }>('/api/authentication/admin/login', {
        email: adminEmail,
        password: adminPassword,
      });

      if (!response.data.accessToken) {
        throw new Error('Failed to authenticate with Core Backend - no token received');
      }

      this.adminToken = response.data.accessToken;
      // Set expiry to 1 minute before actual expiry for safety
      const expiryMs = (response.data.expiresIn || 28800) * 1000 - 60000;
      this.tokenExpiry = new Date(Date.now() + expiryMs);

      logger.info('✅ OEM Backend authenticated with Core Backend');
      return this.adminToken;
    } catch (error) {
      logger.error('Failed to authenticate with Core Backend:', error);
      throw new Error('Core Backend authentication failed. Check admin credentials.');
    }
  }

  /**
   * Get all approved VVNs from Core Backend
   * Uses cached admin token for authentication
   * @returns Array of approved VVN references
   */
  async getApprovedVvns(): Promise<VvnReference[]> {
    logger.info('🔍 getApprovedVvns called - authenticating...');
    const authToken = await this.authenticateWithCoreBackend();
    logger.info(`🔑 Got admin token (${authToken.substring(0, 20)}...) - fetching VVNs`);
    return this.retry(async () => {
      const response = await this.client.get<{ success: boolean; data: VvnReference[] }>(
        '/api/vvns',
        {
          headers: {
            Authorization: `Bearer ${authToken}`,
          },
          params: {
            state: 'APPROVED',
          },
        }
      );

      if (!response.data.success) {
        throw new Error('Failed to fetch VVNs from Core Backend');
      }

      logger.info(`Fetched ${response.data.data.length} approved VVNs from Core Backend`);
      return response.data.data;
    });
  }

  /**
   * Get a specific VVN by ID
   * Uses cached admin token for authentication
   * @param vvnId - VVN identifier
   * @returns VVN reference or null if not found
   */
  async getVvnById(vvnId: string): Promise<VvnReference | null> {
    const authToken = await this.authenticateWithCoreBackend();
    return this.retry(async () => {
      try {
        const response = await this.client.get<{ success: boolean; data: VvnReference }>(
          `/api/vvns/${vvnId}`,
          {
            headers: {
              Authorization: `Bearer ${authToken}`,
            },
          }
        );

        if (!response.data.success) {
          return null;
        }

        logger.info(`Fetched VVN ${vvnId} from Core Backend`);
        return response.data.data;
      } catch (error) {
        const axiosError = error as AxiosError;

        // Return null for 404 (not found)
        if (axiosError.response?.status === 404) {
          logger.warn(`VVN ${vvnId} not found in Core Backend`);
          return null;
        }

        // Re-throw to let retry logic handle it
        throw error;
      }
    });
  }

  /**
   * Get VVNs for a specific date range
   * Uses cached admin token for authentication
   * @param fromDate - Start date (inclusive)
   * @param toDate - End date (inclusive)
   * @returns Array of VVN references
   */
  async getVvnsByDateRange(
    fromDate: Date,
    toDate: Date
  ): Promise<VvnReference[]> {
    logger.info(`🔍 getVvnsByDateRange called - authenticating...`);
    const authToken = await this.authenticateWithCoreBackend();
    logger.info(`🔑 Got admin token - fetching VVNs for date range`);
    return this.retry(async () => {
      const response = await this.client.get<{ success: boolean; data: VvnReference[] }>(
        '/api/vvns',
        {
          headers: {
            Authorization: `Bearer ${authToken}`,
          },
          params: {
            state: 'APPROVED',
            fromDate: fromDate.toISOString(),
            toDate: toDate.toISOString(),
          },
        }
      );

      if (!response.data.success) {
        throw new Error('Failed to fetch VVNs from Core Backend');
      }

      logger.info(
        `Fetched ${response.data.data.length} VVNs for date range ${fromDate.toISOString()} - ${toDate.toISOString()}`
      );
      return response.data.data;
    });
  }

  /**
   * Check health of Core Backend
   * @returns true if Core Backend is healthy, false otherwise
   */
  async checkHealth(): Promise<boolean> {
    try {
      const response = await this.client.get('/health', { timeout: 5000 });
      return response.status === 200;
    } catch (error) {
      logger.error('Core Backend health check failed:', error);
      return false;
    }
  }

  /**
   * Handle axios errors and convert to meaningful error messages
   */
  private handleError(error: AxiosError): Error {
    if (error.response) {
      // Server responded with error status
      const status = error.response.status;
      const message = (error.response.data as { message?: string })?.message || error.message;

      switch (status) {
        case 400:
          return new Error(`Bad Request: ${message}`);
        case 401:
          return new Error('Unauthorized: Invalid or expired authentication token');
        case 403:
          return new Error('Forbidden: Insufficient permissions');
        case 404:
          return new Error('Not Found: Resource not found');
        case 500:
        case 502:
        case 503:
        case 504:
          return new Error(`Core Backend error: ${message}`);
        default:
          return new Error(`HTTP ${status}: ${message}`);
      }
    } else if (error.request) {
      // Request was made but no response received
      return new Error('Core Backend is unreachable. Please check if the service is running.');
    } else {
      // Error during request setup
      return new Error(`Request error: ${error.message}`);
    }
  }
}

/**
 * Singleton instance of CoreBackendClient
 * Lazy initialization to ensure .env is loaded first
 */
let _coreBackendClient: CoreBackendClient | null = null;

export function getCoreBackendClient(): CoreBackendClient {
  if (!_coreBackendClient) {
    const baseURL = process.env.CORE_BACKEND_URL || 'http://localhost:5001';
    _coreBackendClient = new CoreBackendClient(baseURL);
  }
  return _coreBackendClient;
}
