import { OperationPlan } from '@domain/entities/OperationPlan';
import { OperationPlanStatus, PlanningAlgorithm, QueryOptions } from '@shared/types';

/**
 * Repository interface for OperationPlan aggregate
 * Follows Dependency Inversion Principle (DIP)
 * - Interface defined in Domain layer
 * - Implementation in Infrastructure layer
 * - Application services depend on this interface
 */
export interface IOperationPlanRepository {
  /**
   * Find operation plan by ID
   * @param operationPlanId - Plan ID
   * @returns OperationPlan or null if not found
   */
  findById(operationPlanId: string): Promise<OperationPlan | null>;

  /**
   * Find all operation plans with optional filtering, sorting, and pagination
   * @param options - Query options (page, limit, sortBy, sortOrder)
   * @returns Array of operation plans
   */
  findAll(options?: QueryOptions): Promise<OperationPlan[]>;

  /**
   * Find operation plans by status
   * @param status - Plan status to filter by
   * @param options - Query options
   * @returns Array of operation plans with given status
   */
  findByStatus(status: OperationPlanStatus, options?: QueryOptions): Promise<OperationPlan[]>;

  /**
   * Find operation plan for a specific target date
   * @param targetDate - Target date to search
   * @returns OperationPlan or null if not found
   */
  findByTargetDate(targetDate: Date): Promise<OperationPlan | null>;

  /**
   * Find operation plans within a date range
   * @param fromDate - Start date (inclusive)
   * @param toDate - End date (inclusive)
   * @param options - Query options
   * @returns Array of operation plans within date range
   */
  findByDateRange(fromDate: Date, toDate: Date, options?: QueryOptions): Promise<OperationPlan[]>;

  /**
   * Find operation plans by algorithm used
   * @param algorithm - Planning algorithm
   * @param options - Query options
   * @returns Array of operation plans generated with given algorithm
   */
  findByAlgorithm(algorithm: PlanningAlgorithm, options?: QueryOptions): Promise<OperationPlan[]>;

  /**
   * Find operation plan that includes a specific VVN
   * @param vvnId - VVN ID to search for
   * @returns OperationPlan or null if not found
   */
  findByVvnId(vvnId: string): Promise<OperationPlan | null>;

  /**
   * Save new operation plan
   * @param operationPlan - Plan to save
   * @returns Saved operation plan
   */
  save(operationPlan: OperationPlan): Promise<OperationPlan>;

  /**
   * Update existing operation plan
   * @param operationPlan - Plan to update
   * @returns Updated operation plan
   */
  update(operationPlan: OperationPlan): Promise<OperationPlan>;

  /**
   * Delete operation plan by ID
   * @param operationPlanId - Plan ID to delete
   */
  delete(operationPlanId: string): Promise<void>;

  /**
   * Count total operation plans
   * @param status - Optional status filter
   * @returns Total count
   */
  count(status?: OperationPlanStatus): Promise<number>;

  /**
   * Check if operation plan exists for target date
   * @param targetDate - Date to check
   * @returns True if plan exists for this date
   */
  existsForDate(targetDate: Date): Promise<boolean>;

  /**
   * Mark outdated plans as OUTDATED
   * Plans are outdated when a new plan is created for the same date
   * @param targetDate - Date to mark outdated plans for
   */
  markOutdatedPlansForDate(targetDate: Date): Promise<void>;
}
