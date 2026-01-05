/**
 * US 4.1.14: DTOs for TaskCategory management
 * Complementary Task Category CRUD operations
 */

/**
 * DTO for creating a new TaskCategory
 */
export interface CreateTaskCategoryDto {
  categoryName: string; // Required, max 100 chars, unique
  description: string; // Required, max 500 chars
  defaultDurationHours?: number; // Optional, must be > 0 if provided
  expectedImpact?: string; // Optional, max 200 chars
}

/**
 * DTO for updating an existing TaskCategory
 */
export interface UpdateTaskCategoryDto {
  categoryName?: string; // Optional, max 100 chars, unique
  description?: string; // Optional, max 500 chars
  defaultDurationHours?: number; // Optional, must be > 0 if provided
  expectedImpact?: string; // Optional, max 200 chars
}

/**
 * DTO for TaskCategory response
 */
export interface TaskCategoryDto {
  taskCategoryId: string; // UUID
  categoryCode: string; // Unique code (e.g., CTC001, CTC002)
  categoryName: string; // Category name
  description: string; // Category description
  defaultDurationHours?: number; // Optional default task duration
  expectedImpact?: string; // Optional impact description
  isActive: boolean; // Active/Inactive status
  createdAt: string; // ISO 8601 date-time string
  updatedAt: string; // ISO 8601 date-time string
}

/**
 * DTO for TaskCategory list response with pagination
 */
export interface TaskCategoryListDto {
  categories: TaskCategoryDto[];
  total: number;
  page: number;
  limit: number;
}
