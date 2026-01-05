import { TaskCategory } from '@domain/entities/TaskCategory';
import { QueryOptions } from '@shared/types';

/**
 * Repository interface for TaskCategory aggregate
 * US 4.1.14: Enhanced with code-based queries
 * Follows Dependency Inversion Principle (DIP)
 */
export interface ITaskCategoryRepository {
  /**
   * Find task category by ID
   * @param taskCategoryId - Task category ID
   * @returns TaskCategory or null if not found
   */
  findById(taskCategoryId: string): Promise<TaskCategory | null>;

  /**
   * US 4.1.14: Find task category by code
   * Category codes are unique (e.g., CTC001, CTC002)
   * @param categoryCode - Category code to search
   * @returns TaskCategory or null if not found
   */
  findByCode(categoryCode: string): Promise<TaskCategory | null>;

  /**
   * Find task category by name
   * Category names should be unique
   * @param categoryName - Category name to search
   * @returns TaskCategory or null if not found
   */
  findByName(categoryName: string): Promise<TaskCategory | null>;

  /**
   * Find all task categories with optional filtering
   * @param options - Query options
   * @returns Array of task categories
   */
  findAll(options?: QueryOptions): Promise<TaskCategory[]>;

  /**
   * Find all active task categories
   * Used for dropdown lists when creating tasks
   * @param options - Query options
   * @returns Array of active task categories
   */
  findActive(options?: QueryOptions): Promise<TaskCategory[]>;

  /**
   * Save new task category
   * @param taskCategory - Task category to save
   * @returns Saved task category
   */
  save(taskCategory: TaskCategory): Promise<TaskCategory>;

  /**
   * Update existing task category
   * @param taskCategory - Task category to update
   * @returns Updated task category
   */
  update(taskCategory: TaskCategory): Promise<TaskCategory>;

  /**
   * Delete task category by ID
   * Note: Should check if category is used in existing tasks before deleting
   * @param taskCategoryId - Task category ID to delete
   */
  delete(taskCategoryId: string): Promise<void>;

  /**
   * Count total task categories
   * @param activeOnly - If true, count only active categories
   * @returns Total count
   */
  count(activeOnly?: boolean): Promise<number>;

  /**
   * US 4.1.14: Check if task category code already exists
   * Used for uniqueness validation
   * @param categoryCode - Category code to check (e.g., CTC001)
   * @param excludeId - Optional ID to exclude from check (for updates)
   * @returns True if code exists
   */
  existsByCode(categoryCode: string, excludeId?: string): Promise<boolean>;

  /**
   * Check if task category name already exists
   * Used for uniqueness validation
   * @param categoryName - Category name to check
   * @param excludeId - Optional ID to exclude from check (for updates)
   * @returns True if name exists
   */
  existsByName(categoryName: string, excludeId?: string): Promise<boolean>;

  /**
   * US 4.1.14: Get next sequential number for code generation
   * Used to generate CTC001, CTC002, etc.
   * @returns Next sequential number
   */
  getNextSequentialNumber(): Promise<number>;

  /**
   * Check if task category is used in any tasks
   * Used before deleting a category
   * @param taskCategoryId - Task category ID to check
   * @returns True if category is used in tasks
   */
  isUsedInTasks(taskCategoryId: string): Promise<boolean>;
}
