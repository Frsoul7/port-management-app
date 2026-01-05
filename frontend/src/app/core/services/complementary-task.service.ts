import { Injectable, inject } from '@angular/core';
import { HttpClient, HttpParams } from '@angular/common/http';
import { Observable } from 'rxjs';
import {
  ComplementaryTask,
  ComplementaryTaskStatus,
  CreateComplementaryTaskRequest,
  UpdateComplementaryTaskRequest,
  AssignTaskRequest,
  StartTaskRequest,
  CompleteTaskRequest,
  CancelTaskRequest,
  AddTaskNoteRequest,
  TaskFilters,
  CreateComplementaryTaskResponse,
  GetComplementaryTaskResponse,
  ListComplementaryTasksResponse,
  UpdateComplementaryTaskResponse,
  DeleteComplementaryTaskResponse,
  TaskActionResponse,
  TaskStatisticsResponse,
  TaskCountResponse,
} from '../models/complementary-task.model';

/**
 * US 4.1.15: Complementary Task Service
 * Manages complementary tasks (non-cargo activities) during vessel visits
 */
@Injectable({
  providedIn: 'root'
})
export class ComplementaryTaskService {
  private http = inject(HttpClient);
  private apiUrl = 'http://localhost:3000/api/v1/complementary-tasks';

  /**
   * Create a new complementary task
   * US 4.1.15: Record non-cargo activities
   */
  createTask(request: CreateComplementaryTaskRequest): Observable<CreateComplementaryTaskResponse> {
    return this.http.post<CreateComplementaryTaskResponse>(this.apiUrl, request);
  }

  /**
   * Get complementary task by ID
   * US 4.1.15: Retrieve specific task details
   */
  getTaskById(taskId: string): Observable<GetComplementaryTaskResponse> {
    return this.http.get<GetComplementaryTaskResponse>(`${this.apiUrl}/${taskId}`);
  }

  /**
   * Update complementary task
   * US 4.1.15: Modify task details (only PLANNED tasks)
   */
  updateTask(taskId: string, request: UpdateComplementaryTaskRequest): Observable<UpdateComplementaryTaskResponse> {
    return this.http.patch<UpdateComplementaryTaskResponse>(`${this.apiUrl}/${taskId}`, request);
  }

  /**
   * Delete complementary task
   * US 4.1.15: Remove task (only PLANNED tasks)
   */
  deleteTask(taskId: string): Observable<DeleteComplementaryTaskResponse> {
    return this.http.delete<DeleteComplementaryTaskResponse>(`${this.apiUrl}/${taskId}`);
  }

  /**
   * List complementary tasks with filters
   * US 4.1.15: Search and filter tasks by status, VVE, date range, etc.
   */
  listTasks(filters?: TaskFilters): Observable<ListComplementaryTasksResponse> {
    let params = new HttpParams();

    if (filters?.status) {
      params = params.set('status', filters.status);
    }
    if (filters?.vveId) {
      params = params.set('vveId', filters.vveId);
    }
    if (filters?.taskCategoryId) {
      params = params.set('taskCategoryId', filters.taskCategoryId);
    }
    if (filters?.assignedTo) {
      params = params.set('assignedTo', filters.assignedTo);
    }
    if (filters?.fromDate) {
      params = params.set('fromDate', filters.fromDate);
    }
    if (filters?.toDate) {
      params = params.set('toDate', filters.toDate);
    }
    if (filters?.sortBy) {
      params = params.set('sortBy', filters.sortBy);
    }
    if (filters?.sortOrder) {
      params = params.set('sortOrder', filters.sortOrder);
    }
    if (filters?.page) {
      params = params.set('page', filters.page.toString());
    }
    if (filters?.limit) {
      params = params.set('limit', filters.limit.toString());
    }

    return this.http.get<ListComplementaryTasksResponse>(this.apiUrl, { params });
  }

  /**
   * List tasks by VVE
   * US 4.1.15: Get all tasks for a specific vessel visit execution
   */
  listTasksByVve(vveId: string): Observable<ListComplementaryTasksResponse> {
    return this.http.get<ListComplementaryTasksResponse>(`${this.apiUrl}/vve/${vveId}`);
  }

  /**
   * List tasks by status
   * US 4.1.15: Filter tasks by their current status
   */
  listTasksByStatus(status: ComplementaryTaskStatus): Observable<ListComplementaryTasksResponse> {
    return this.http.get<ListComplementaryTasksResponse>(`${this.apiUrl}/status/${status}`);
  }

  /**
   * Get active tasks impacting operations
   * US 4.1.15: Identify tasks that suspend cargo operations
   */
  getActiveImpactingTasks(): Observable<ListComplementaryTasksResponse> {
    return this.http.get<ListComplementaryTasksResponse>(`${this.apiUrl}/active-impacting`);
  }

  /**
   * Assign task to team or user
   * US 4.1.15: Set responsible team/service
   */
  assignTask(taskId: string, request: AssignTaskRequest): Observable<TaskActionResponse> {
    return this.http.patch<TaskActionResponse>(`${this.apiUrl}/${taskId}/assign`, request);
  }

  /**
   * Start task
   * US 4.1.15: Begin task execution (PLANNED → IN_PROGRESS)
   */
  startTask(taskId: string, request?: StartTaskRequest): Observable<TaskActionResponse> {
    return this.http.patch<TaskActionResponse>(`${this.apiUrl}/${taskId}/start`, request || {});
  }

  /**
   * Complete task
   * US 4.1.15: Mark task as completed (IN_PROGRESS → COMPLETED)
   */
  completeTask(taskId: string, request: CompleteTaskRequest): Observable<TaskActionResponse> {
    return this.http.patch<TaskActionResponse>(`${this.apiUrl}/${taskId}/complete`, request);
  }

  /**
   * Cancel task
   * US 4.1.15: Cancel task with reason
   */
  cancelTask(taskId: string, request: CancelTaskRequest): Observable<TaskActionResponse> {
    return this.http.patch<TaskActionResponse>(`${this.apiUrl}/${taskId}/cancel`, request);
  }

  /**
   * Add note to task
   * US 4.1.15: Record observations during task execution
   */
  addNote(taskId: string, request: AddTaskNoteRequest): Observable<TaskActionResponse> {
    return this.http.post<TaskActionResponse>(`${this.apiUrl}/${taskId}/notes`, request);
  }

  /**
   * Get task statistics
   * US 4.1.15: Analyze task performance over time period
   */
  getStatistics(fromDate: string, toDate: string): Observable<TaskStatisticsResponse> {
    const params = new HttpParams()
      .set('fromDate', fromDate)
      .set('toDate', toDate);

    return this.http.get<TaskStatisticsResponse>(`${this.apiUrl}/statistics`, { params });
  }

  /**
   * Get task count
   * US 4.1.15: Count tasks by status
   */
  getTaskCount(status?: ComplementaryTaskStatus): Observable<TaskCountResponse> {
    let params = new HttpParams();
    if (status) {
      params = params.set('status', status);
    }
    return this.http.get<TaskCountResponse>(`${this.apiUrl}/count`, { params });
  }

  /**
   * Get overdue task count
   * US 4.1.15: Count tasks past their due date
   */
  getOverdueCount(): Observable<TaskCountResponse> {
    return this.http.get<TaskCountResponse>(`${this.apiUrl}/overdue-count`);
  }
}
