import { VvnReference } from '@shared/types';

/**
 * Port interface for Core Backend integration
 * Defines the contract for fetching VVN data from the Core Backend
 * 
 * This interface follows the Dependency Inversion Principle (DIP):
 * - Domain/Application layers depend on this abstraction
 * - Infrastructure layer provides the concrete implementation
 */
export interface ICoreBackendClient {
  /**
   * Get all approved VVNs from Core Backend
   * @returns Array of approved VVN references
   */
  getApprovedVvns(): Promise<VvnReference[]>;

  /**
   * Get a specific VVN by ID
   * @param vvnId - VVN identifier
   * @returns VVN reference or null if not found
   */
  getVvnById(vvnId: string): Promise<VvnReference | null>;

  /**
   * Get VVNs for a specific date range
   * @param fromDate - Start date (inclusive)
   * @param toDate - End date (inclusive)
   * @returns Array of VVN references
   */
  getVvnsByDateRange(fromDate: Date, toDate: Date): Promise<VvnReference[]>;

  /**
   * Check health of Core Backend
   * @returns true if Core Backend is healthy, false otherwise
   */
  checkHealth(): Promise<boolean>;
}
