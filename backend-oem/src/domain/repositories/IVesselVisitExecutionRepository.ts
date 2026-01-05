import { VesselVisitExecution } from '@domain/entities/VesselVisitExecution';
import { VesselVisitExecutionStatus, QueryOptions } from '@shared/types';

/**
 * Repository interface for VesselVisitExecution aggregate
 * Follows Dependency Inversion Principle (DIP)
 */
export interface IVesselVisitExecutionRepository {
  /**
   * Find vessel visit execution by ID
   * @param vveId - VVE ID
   * @returns VesselVisitExecution or null if not found
   */
  findById(vveId: string): Promise<VesselVisitExecution | null>;

  /**
   * Find vessel visit execution by VVN ID
   * @param vvnId - VVN ID (from Core Backend)
   * @returns VesselVisitExecution or null if not found
   */
  findByVvnId(vvnId: string): Promise<VesselVisitExecution | null>;

  /**
   * Find all vessel visit executions with optional filtering
   * @param options - Query options
   * @returns Array of vessel visit executions
   */
  findAll(options?: QueryOptions): Promise<VesselVisitExecution[]>;

  /**
   * Find vessel visit executions by status
   * @param status - VVE status to filter by
   * @param options - Query options
   * @returns Array of VVEs with given status
   */
  findByStatus(
    status: VesselVisitExecutionStatus,
    options?: QueryOptions
  ): Promise<VesselVisitExecution[]>;

  /**
   * Find vessel visit executions linked to operation plan
   * @param operationPlanId - Operation plan ID
   * @param options - Query options
   * @returns Array of VVEs linked to this plan
   */
  findByOperationPlanId(
    operationPlanId: string,
    options?: QueryOptions
  ): Promise<VesselVisitExecution[]>;

  /**
   * Find vessel visit executions within a date range
   * Based on actual port arrival time
   * @param fromDate - Start date (inclusive)
   * @param toDate - End date (inclusive)
   * @param options - Query options
   * @returns Array of VVEs within date range
   */
  findByDateRange(
    fromDate: Date,
    toDate: Date,
    options?: QueryOptions
  ): Promise<VesselVisitExecution[]>;

  /**
   * Find VVEs assigned to specific dock
   * @param dockCode - Dock code
   * @param options - Query options
   * @returns Array of VVEs at this dock
   */
  findByDock(dockCode: string, options?: QueryOptions): Promise<VesselVisitExecution[]>;

  /**
   * Find VVEs with linked incidents
   * @param options - Query options
   * @returns Array of VVEs that have incidents
   */
  findWithIncidents(options?: QueryOptions): Promise<VesselVisitExecution[]>;

  /**
   * Find VVEs that are currently in progress
   * Status = IN_PROGRESS
   * @param options - Query options
   * @returns Array of active VVEs
   */
  findInProgress(options?: QueryOptions): Promise<VesselVisitExecution[]>;

  /**
   * Save new vessel visit execution
   * @param vve - VVE to save
   * @returns Saved VVE
   */
  save(vve: VesselVisitExecution): Promise<VesselVisitExecution>;

  /**
   * Update existing vessel visit execution
   * @param vve - VVE to update
   * @returns Updated VVE
   */
  update(vve: VesselVisitExecution): Promise<VesselVisitExecution>;

  /**
   * Delete vessel visit execution by ID
   * @param vveId - VVE ID to delete
   */
  delete(vveId: string): Promise<void>;

  /**
   * Count total vessel visit executions
   * @param status - Optional status filter
   * @returns Total count
   */
  count(status?: VesselVisitExecutionStatus): Promise<number>;

  /**
   * Check if VVE exists for a given VVN
   * @param vvnId - VVN ID to check
   * @returns True if VVE exists for this VVN
   */
  existsForVvn(vvnId: string): Promise<boolean>;

  /**
   * US 4.1.7: Get next sequential number for VVE ID generation
   * Finds the highest VVE ID and returns the next sequential number
   * @returns Next sequential number (starts at 1 if no VVEs exist)
   */
  getNextSequentialNumber(): Promise<number>;

  /**
   * Get average turnaround time for completed VVEs
   * @param fromDate - Optional start date for filtering
   * @param toDate - Optional end date for filtering
   * @returns Average turnaround time in hours
   */
  getAverageTurnaroundTime(fromDate?: Date, toDate?: Date): Promise<number | null>;

  /**
   * Get average berth occupancy time for completed VVEs
   * @param fromDate - Optional start date for filtering
   * @param toDate - Optional end date for filtering
   * @returns Average berth occupancy in hours
   */
  getAverageBerthOccupancy(fromDate?: Date, toDate?: Date): Promise<number | null>;
}
