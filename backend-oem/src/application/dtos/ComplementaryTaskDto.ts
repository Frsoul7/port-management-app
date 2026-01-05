import { ComplementaryTaskStatus } from '@shared/types';

/**
 * DTO for ComplementaryTask note
 */
export interface TaskNoteDto {
  timestamp: string; // ISO 8601 date-time string
  author: string;
  content: string;
}

/**
 * DTO for creating a new ComplementaryTask
 */
export interface CreateComplementaryTaskDto {
  taskCategoryId: string;
  vveId: string;
  title: string;
  description: string;
  dueDate: string; // ISO 8601 date-time string
  estimatedDurationHours?: number;
  assignedTo?: string;
  createdBy: string;
}

/**
 * DTO for ComplementaryTask response
 */
export interface ComplementaryTaskDto {
  taskId: string;
  taskCategoryId: string;
  vveId: string;
  title: string;
  description: string;
  status: ComplementaryTaskStatus;
  dueDate: string; // ISO 8601 date-time string
  assignedTo?: string;
  estimatedDurationHours?: number;
  startedAt?: string;
  completedAt?: string;
  completedBy?: string;
  cancelledAt?: string;
  cancelledBy?: string;
  cancellationReason?: string;
  notes: TaskNoteDto[];
  createdAt: string;
  createdBy: string;
}

/**
 * DTO for updating task assignment
 */
export interface AssignTaskDto {
  assignedTo: string;
}

/**
 * DTO for starting a task
 */
export interface StartTaskDto {
  startedAt?: string; // Optional, defaults to now
}

/**
 * DTO for completing a task
 */
export interface CompleteTaskDto {
  completedBy: string;
  completedAt?: string; // Optional, defaults to now
}

/**
 * DTO for cancelling a task
 */
export interface CancelTaskDto {
  cancelledBy: string;
  cancellationReason: string;
  cancelledAt?: string; // Optional, defaults to now
}

/**
 * DTO for adding a note to a task
 */
export interface AddTaskNoteDto {
  content: string;
  author: string;
}

/**
 * DTO for task statistics
 */
export interface TaskStatisticsDto {
  total: number;
  byStatus: Record<ComplementaryTaskStatus, number>;
  averageCompletionTimeHours: number | null;
  onTimeCompletionRate: number;
}

/**
 * Query parameters for listing complementary tasks with pagination
 */
export interface ListComplementaryTasksQuery {
  // Filter parameters
  status?: string;
  vveId?: string;
  taskCategoryId?: string;
  assignedTo?: string;
  fromDate?: string;
  toDate?: string;
  // Pagination parameters
  page?: number;
  limit?: number;
  sortBy?: string;
  sortOrder?: 'asc' | 'desc';
}

/**
 * NOTE: TaskCategory DTOs have been moved to TaskCategoryDto.ts (US 4.1.14)
 * Import TaskCategoryDto, CreateTaskCategoryDto, UpdateTaskCategoryDto from '@application/dtos'
 */
