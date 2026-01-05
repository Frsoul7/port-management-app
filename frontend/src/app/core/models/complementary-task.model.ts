/**
 * US 4.1.15: Complementary Task Model
 * Represents non-cargo activities performed during vessel visits
 */

/**
 * Task Status Enum
 */
export enum ComplementaryTaskStatus {
  PLANNED = 'PLANNED',
  IN_PROGRESS = 'IN_PROGRESS',
  COMPLETED = 'COMPLETED',
  CANCELLED = 'CANCELLED'
}

/**
 * Task Note interface
 */
export interface TaskNote {
  timestamp: string; // ISO 8601 datetime string
  author: string;
  content: string;
}

/**
 * Complementary Task interface
 */
export interface ComplementaryTask {
  taskId: string;
  taskCategoryId: string;
  vveId: string;
  title: string;
  description: string;
  status: ComplementaryTaskStatus;
  dueDate: string; // ISO 8601 datetime string
  assignedTo?: string;
  estimatedDurationHours?: number;
  startedAt?: string;
  completedAt?: string;
  completedBy?: string;
  cancelledAt?: string;
  cancelledBy?: string;
  cancellationReason?: string;
  notes: TaskNote[];
  createdAt: string;
  createdBy: string;

  // Computed properties (from TaskCategory)
  categoryName?: string;
  categoryCode?: string;
  expectedImpact?: string;
}

/**
 * Create Complementary Task Request
 */
export interface CreateComplementaryTaskRequest {
  taskCategoryId: string;
  vveId: string;
  title: string;
  description: string;
  dueDate: string;
  estimatedDurationHours?: number;
  assignedTo?: string;
  createdBy: string;
}

/**
 * Update Complementary Task Request
 */
export interface UpdateComplementaryTaskRequest {
  title?: string;
  description?: string;
  dueDate?: string;
  estimatedDurationHours?: number;
}

/**
 * Assign Task Request
 */
export interface AssignTaskRequest {
  assignedTo: string;
}

/**
 * Start Task Request
 */
export interface StartTaskRequest {
  startedAt?: string;
}

/**
 * Complete Task Request
 */
export interface CompleteTaskRequest {
  completedBy: string;
  completedAt?: string;
}

/**
 * Cancel Task Request
 */
export interface CancelTaskRequest {
  cancelledBy: string;
  cancellationReason: string;
  cancelledAt?: string;
}

/**
 * Add Task Note Request
 */
export interface AddTaskNoteRequest {
  content: string;
  author: string;
}

/**
 * Task Filters
 */
export interface TaskFilters {
  status?: ComplementaryTaskStatus;
  vveId?: string;
  taskCategoryId?: string;
  assignedTo?: string;
  fromDate?: string;
  toDate?: string;
  sortBy?: string;
  sortOrder?: 'asc' | 'desc';
  page?: number;
  limit?: number;
}

/**
 * Task Statistics
 */
export interface TaskStatistics {
  total: number;
  byStatus: Record<ComplementaryTaskStatus, number>;
  averageCompletionTimeHours: number | null;
  onTimeCompletionRate: number;
}

/**
 * API Responses
 */
export interface CreateComplementaryTaskResponse {
  success: boolean;
  message: string;
  data: ComplementaryTask;
}

export interface GetComplementaryTaskResponse {
  success: boolean;
  data: ComplementaryTask;
}

export interface ListComplementaryTasksResponse {
  success: boolean;
  data: ComplementaryTask[];
  count: number;
}

export interface UpdateComplementaryTaskResponse {
  success: boolean;
  message: string;
  data: ComplementaryTask;
}

export interface DeleteComplementaryTaskResponse {
  success: boolean;
  message: string;
}

export interface TaskActionResponse {
  success: boolean;
  message: string;
  data: ComplementaryTask;
}

export interface TaskStatisticsResponse {
  success: boolean;
  data: TaskStatistics;
}

export interface TaskCountResponse {
  success: boolean;
  data: { count: number };
}
