/**
 * US 4.1.14: Task Category Model
 * Represents complementary task categories for non-cargo activities
 */

/**
 * Task Category interface
 */
export interface TaskCategory {
  taskCategoryId: string;
  categoryCode: string; // Unique code (e.g., CTC001, CTC002)
  categoryName: string;
  description: string;
  defaultDurationHours?: number; // Optional default duration
  expectedImpact?: string; // Optional impact description
  isActive: boolean;
  createdAt: string; // ISO 8601 datetime string
  updatedAt: string; // ISO 8601 datetime string
}

/**
 * Create Task Category Request
 */
export interface CreateTaskCategoryRequest {
  categoryName: string;
  description: string;
  defaultDurationHours?: number;
  expectedImpact?: string;
}

/**
 * Update Task Category Request
 */
export interface UpdateTaskCategoryRequest {
  categoryName?: string;
  description?: string;
  defaultDurationHours?: number;
  expectedImpact?: string;
}

/**
 * Create Task Category Response
 */
export interface CreateTaskCategoryResponse {
  success: boolean;
  message: string;
  data: TaskCategory;
}

/**
 * Get Task Category Response
 */
export interface GetTaskCategoryResponse {
  success: boolean;
  data: TaskCategory;
}

/**
 * List Task Categories Response
 */
export interface ListTaskCategoriesResponse {
  success: boolean;
  data: TaskCategory[];
  count: number;
}

/**
 * Update Task Category Response
 */
export interface UpdateTaskCategoryResponse {
  success: boolean;
  message: string;
  data: TaskCategory;
}

/**
 * Delete Task Category Response
 */
export interface DeleteTaskCategoryResponse {
  success: boolean;
  message: string;
}

/**
 * Deactivate/Reactivate Task Category Response
 */
export interface ToggleActivationResponse {
  success: boolean;
  message: string;
  data: TaskCategory;
}
