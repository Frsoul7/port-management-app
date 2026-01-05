import { ComplementaryTask } from '@domain/entities/ComplementaryTask';
import { ComplementaryTaskStatus, QueryOptions } from '@shared/types';

/**
 * Repository interface for ComplementaryTask aggregate
 * Follows Dependency Inversion Principle (DIP)
 */
export interface IComplementaryTaskRepository {
  /**
   * Find complementary task by ID
   * @param taskId - Task ID
   * @returns ComplementaryTask or null if not found
   */
  findById(taskId: string): Promise<ComplementaryTask | null>;

  /**
   * Find all complementary tasks with optional filtering
   * @param options - Query options
   * @returns Array of complementary tasks
   */
  findAll(options?: QueryOptions): Promise<ComplementaryTask[]>;

  /**
   * Find tasks by status
   * @param status - Task status to filter by
   * @param options - Query options
   * @returns Array of tasks with given status
   */
  findByStatus(
    status: ComplementaryTaskStatus,
    options?: QueryOptions
  ): Promise<ComplementaryTask[]>;

  /**
   * Find tasks by category
   * @param taskCategoryId - Task category ID
   * @param options - Query options
   * @returns Array of tasks in this category
   */
  findByCategory(taskCategoryId: string, options?: QueryOptions): Promise<ComplementaryTask[]>;

  /**
   * Find tasks linked to a vessel visit execution
   * @param vveId - VVE ID
   * @param options - Query options
   * @returns Array of tasks for this VVE
   */
  findByVveId(vveId: string, options?: QueryOptions): Promise<ComplementaryTask[]>;

  /**
   * Find tasks assigned to a specific user or team
   * @param assignedTo - User ID or team name
   * @param options - Query options
   * @returns Array of tasks assigned to this user/team
   */
  findByAssignee(assignedTo: string, options?: QueryOptions): Promise<ComplementaryTask[]>;

  /**
   * Find tasks created by a specific user
   * @param createdBy - Creator name/ID
   * @param options - Query options
   * @returns Array of tasks created by this user
   */
  findByCreator(createdBy: string, options?: QueryOptions): Promise<ComplementaryTask[]>;

  /**
   * Find overdue tasks
   * Tasks with due date in the past and status not COMPLETED or CANCELLED
   * @param options - Query options
   * @returns Array of overdue tasks
   */
  findOverdue(options?: QueryOptions): Promise<ComplementaryTask[]>;

  /**
   * Find tasks due within a specific date range
   * @param fromDate - Start date (inclusive)
   * @param toDate - End date (inclusive)
   * @param options - Query options
   * @returns Array of tasks due in this range
   */
  findByDueDateRange(
    fromDate: Date,
    toDate: Date,
    options?: QueryOptions
  ): Promise<ComplementaryTask[]>;

  /**
   * Find tasks created within a date range
   * @param fromDate - Start date (inclusive)
   * @param toDate - End date (inclusive)
   * @param options - Query options
   * @returns Array of tasks created in this range
   */
  findByCreatedDateRange(
    fromDate: Date,
    toDate: Date,
    options?: QueryOptions
  ): Promise<ComplementaryTask[]>;

  /**
   * Find unassigned tasks
   * Tasks with no assignee
   * @param options - Query options
   * @returns Array of unassigned tasks
   */
  findUnassigned(options?: QueryOptions): Promise<ComplementaryTask[]>;

  /**
   * Find active tasks (PLANNED or IN_PROGRESS)
   * @param options - Query options
   * @returns Array of active tasks
   */
  findActive(options?: QueryOptions): Promise<ComplementaryTask[]>;

  /**
   * Save new complementary task
   * @param task - Task to save
   * @returns Saved task
   */
  save(task: ComplementaryTask): Promise<ComplementaryTask>;

  /**
   * Update existing complementary task
   * @param task - Task to update
   * @returns Updated task
   */
  update(task: ComplementaryTask): Promise<ComplementaryTask>;

  /**
   * Delete complementary task by ID
   * @param taskId - Task ID to delete
   */
  delete(taskId: string): Promise<void>;

  /**
   * Count total complementary tasks
   * @param status - Optional status filter
   * @returns Total count
   */
  count(status?: ComplementaryTaskStatus): Promise<number>;

  /**
   * Count overdue tasks
   * @returns Number of overdue tasks
   */
  countOverdue(): Promise<number>;

  /**
   * Get task completion statistics
   * @param fromDate - Start date for stats
   * @param toDate - End date for stats
   * @returns Task statistics
   */
  getStatistics(
    fromDate: Date,
    toDate: Date
  ): Promise<{
    total: number;
    byStatus: Record<ComplementaryTaskStatus, number>;
    averageCompletionTimeHours: number | null;
    onTimeCompletionRate: number; // Percentage of tasks completed before due date
  }>;

  /**
   * Get average task duration by category
   * @param taskCategoryId - Task category ID
   * @returns Average duration in hours for completed tasks in this category
   */
  getAverageDurationByCategory(taskCategoryId: string): Promise<number | null>;
}
