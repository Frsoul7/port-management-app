// Mock axios BEFORE importing modules
jest.mock('axios', () => ({
  create: jest.fn(() => ({
    get: jest.fn(),
    post: jest.fn(),
    interceptors: {
      request: { use: jest.fn(), eject: jest.fn() },
      response: { use: jest.fn(), eject: jest.fn() },
    },
  })),
}));

import axios from 'axios';
import { CoreBackendClient } from '../CoreBackendClient';
import { VvnReference, OperationType } from '@shared/types';

const mockedAxios = axios as jest.Mocked<typeof axios>;

describe('CoreBackendClient', () => {
  let client: CoreBackendClient;
  let mockAxiosInstance: any;
  const mockAuthToken = 'mock-jwt-token';
  const mockBaseURL = 'http://localhost:5001';

  const mockVvn: VvnReference = {
    vvnId: 'VVN-001',
    vesselImo: 'IMO1234567',
    eta: new Date('2025-12-10T08:00:00Z'),
    etd: new Date('2025-12-10T16:00:00Z'),
    loadingCount: 50,
    unloadingCount: 30,
    purpose: OperationType.BOTH,
    state: 'APPROVED',
  };

  beforeEach(() => {
    jest.clearAllMocks();

    // Mock environment variables for admin authentication
    process.env.CORE_BACKEND_ADMIN_EMAIL = 'admin@test.com';
    process.env.CORE_BACKEND_ADMIN_PASSWORD = 'admin123';

    // Mock axios.create to return a mocked axios instance
    mockAxiosInstance = {
      get: jest.fn(),
      post: jest.fn(),
      interceptors: {
        request: { use: jest.fn(), eject: jest.fn() },
        response: { use: jest.fn(), eject: jest.fn() },
      },
    };

    mockedAxios.create.mockReturnValue(mockAxiosInstance as any);

    client = new CoreBackendClient(mockBaseURL, 2, 100); // 2 retries, 100ms delay for faster tests

    // Mock authentication POST request to return a token
    (mockAxiosInstance.post as jest.Mock).mockResolvedValue({
      data: {
        accessToken: mockAuthToken,
        expiresIn: 28800,
      },
      status: 200,
      statusText: 'OK',
      headers: {},
      config: {} as any,
    });
  });

  describe('getApprovedVvns', () => {
    it('should fetch all approved VVNs successfully', async () => {
      const mockResponse = {
        data: {
          success: true,
          data: [mockVvn],
        },
        status: 200,
        statusText: 'OK',
        headers: {},
        config: {} as any,
      };

      (mockAxiosInstance.get as jest.Mock).mockResolvedValue(mockResponse);

      const result = await client.getApprovedVvns();

      expect(result).toEqual([mockVvn]);
      expect(mockAxiosInstance.get).toHaveBeenCalledWith(
        '/api/vvns',
        expect.objectContaining({
          headers: {
            Authorization: `Bearer ${mockAuthToken}`,
          },
          params: {
            state: 'APPROVED',
          },
        })
      );
    });

    it('should throw error when API returns success: false', async () => {
      const mockResponse = {
        data: {
          success: false,
          data: [],
        },
        status: 200,
        statusText: 'OK',
        headers: {},
        config: {} as any,
      };

      (mockAxiosInstance.get as jest.Mock).mockResolvedValue(mockResponse);

      await expect(client.getApprovedVvns()).rejects.toThrow(
        'Failed to fetch VVNs from Core Backend'
      );
    });

    it('should handle 401 Unauthorized error', async () => {
      const mockError = {
        response: {
          status: 401,
          statusText: 'Unauthorized',
          data: { message: 'Invalid token' },
        },
        config: {},
        request: {},
      };

      (mockAxiosInstance.get as jest.Mock).mockRejectedValue(mockError);

      await expect(client.getApprovedVvns()).rejects.toThrow(
        'Unauthorized: Invalid or expired authentication token'
      );
    });

    it('should retry on 500 server error', async () => {
      const mockError = {
        response: {
          status: 500,
          statusText: 'Internal Server Error',
          data: { message: 'Server error' },
        },
        config: {},
        request: {},
      };

      const mockSuccess = {
        data: {
          success: true,
          data: [mockVvn],
        },
        status: 200,
        statusText: 'OK',
        headers: {},
        config: {} as any,
      };

      (mockAxiosInstance.get as jest.Mock)
        .mockRejectedValueOnce(mockError)
        .mockResolvedValueOnce(mockSuccess);

      const result = await client.getApprovedVvns();

      expect(result).toEqual([mockVvn]);
      expect(mockAxiosInstance.get).toHaveBeenCalledTimes(2);
    });

    it('should not retry on 400 client error', async () => {
      const mockError = {
        response: {
          status: 400,
          statusText: 'Bad Request',
          data: { message: 'Invalid parameters' },
        },
        config: {},
        request: {},
      };

      (mockAxiosInstance.get as jest.Mock).mockRejectedValue(mockError);

      await expect(client.getApprovedVvns()).rejects.toThrow('Bad Request');
      expect(mockAxiosInstance.get).toHaveBeenCalledTimes(1);
    });
  });

  describe('getVvnById', () => {
    it('should fetch specific VVN by ID', async () => {
      const mockResponse = {
        data: {
          success: true,
          data: mockVvn,
        },
        status: 200,
        statusText: 'OK',
        headers: {},
        config: {} as any,
      };

      (mockAxiosInstance.get as jest.Mock).mockResolvedValue(mockResponse);

      const result = await client.getVvnById('VVN-001');

      expect(result).toEqual(mockVvn);
      expect(mockAxiosInstance.get).toHaveBeenCalledWith(
        '/api/vvns/VVN-001',
        expect.objectContaining({
          headers: {
            Authorization: `Bearer ${mockAuthToken}`,
          },
        })
      );
    });

    it('should return null when VVN not found (404)', async () => {
      const mockError = {
        response: {
          status: 404,
          statusText: 'Not Found',
          data: { message: 'VVN not found' },
        },
        config: {},
        request: {},
      };

      (mockAxiosInstance.get as jest.Mock).mockRejectedValue(mockError);

      const result = await client.getVvnById('VVN-999');

      expect(result).toBeNull();
    });

    it('should return null when API returns success: false', async () => {
      const mockResponse = {
        data: {
          success: false,
        },
        status: 200,
        statusText: 'OK',
        headers: {},
        config: {} as any,
      };

      (mockAxiosInstance.get as jest.Mock).mockResolvedValue(mockResponse);

      const result = await client.getVvnById('VVN-001');

      expect(result).toBeNull();
    });
  });

  describe('getVvnsByDateRange', () => {
    it('should fetch VVNs within date range', async () => {
      const fromDate = new Date('2025-12-10T00:00:00Z');
      const toDate = new Date('2025-12-15T23:59:59Z');

      const mockResponse = {
        data: {
          success: true,
          data: [mockVvn],
        },
        status: 200,
        statusText: 'OK',
        headers: {},
        config: {} as any,
      };

      (mockAxiosInstance.get as jest.Mock).mockResolvedValue(mockResponse);

      const result = await client.getVvnsByDateRange(fromDate, toDate);

      expect(result).toEqual([mockVvn]);
      expect(mockAxiosInstance.get).toHaveBeenCalledWith(
        '/api/vvns',
        expect.objectContaining({
          headers: {
            Authorization: `Bearer ${mockAuthToken}`,
          },
          params: {
            state: 'APPROVED',
            fromDate: fromDate.toISOString(),
            toDate: toDate.toISOString(),
          },
        })
      );
    });

    it('should handle empty results', async () => {
      const mockResponse = {
        data: {
          success: true,
          data: [],
        },
        status: 200,
        statusText: 'OK',
        headers: {},
        config: {} as any,
      };

      (mockAxiosInstance.get as jest.Mock).mockResolvedValue(mockResponse);

      const result = await client.getVvnsByDateRange(
        new Date('2025-12-10'),
        new Date('2025-12-15')
      );

      expect(result).toEqual([]);
    });
  });

  describe('checkHealth', () => {
    it('should return true when Core Backend is healthy', async () => {
      const mockResponse = {
        status: 200,
        statusText: 'OK',
        data: { status: 'healthy' },
        headers: {},
        config: {} as any,
      };

      (mockAxiosInstance.get as jest.Mock).mockResolvedValue(mockResponse);

      const result = await client.checkHealth();

      expect(result).toBe(true);
      expect(mockAxiosInstance.get).toHaveBeenCalledWith('/health', { timeout: 5000 });
    });

    it('should return false when Core Backend is unreachable', async () => {
      const mockError = new Error('Network error');

      (mockAxiosInstance.get as jest.Mock).mockRejectedValue(mockError);

      const result = await client.checkHealth();

      expect(result).toBe(false);
    });
  });

  describe('Error Handling', () => {
    it('should handle network errors with retry', async () => {
      const mockError = {
        request: {},
        config: {},
        message: 'Network Error',
      };

      (mockAxiosInstance.get as jest.Mock).mockRejectedValue(mockError);

      await expect(client.getApprovedVvns()).rejects.toThrow(
        'Core Backend is unreachable'
      );

      // Should retry twice (initial + 2 retries = 3 total attempts)
      expect(mockAxiosInstance.get).toHaveBeenCalledTimes(3);
    });

    it('should handle 503 Service Unavailable with retry', async () => {
      const mockError = {
        response: {
          status: 503,
          statusText: 'Service Unavailable',
          data: { message: 'Service temporarily unavailable' },
        },
        config: {},
        request: {},
      };

      (mockAxiosInstance.get as jest.Mock).mockRejectedValue(mockError);

      await expect(client.getApprovedVvns()).rejects.toThrow('Core Backend error');

      // Should retry twice
      expect(mockAxiosInstance.get).toHaveBeenCalledTimes(3);
    });
  });
});
