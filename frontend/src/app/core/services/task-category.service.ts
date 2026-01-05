import { Injectable, inject } from '@angular/core';
import { HttpClient, HttpParams } from '@angular/common/http';
import { Observable } from 'rxjs';
import {
  TaskCategory,
  CreateTaskCategoryRequest,
  UpdateTaskCategoryRequest,
  CreateTaskCategoryResponse,
  GetTaskCategoryResponse,
  ListTaskCategoriesResponse,
  UpdateTaskCategoryResponse,
  DeleteTaskCategoryResponse,
  ToggleActivationResponse,
} from '../models/task-category.model';

/**
 * US 4.1.14: Task Category Service
 * Manages complementary task category catalog
 */
@Injectable({
  providedIn: 'root'
})
export class TaskCategoryService {
  private http = inject(HttpClient);
  private apiUrl = 'http://localhost:3000/api/v1/task-categories';

  /**
   * Create a new task category
   * US 4.1.14: Add new category to catalog (code is auto-generated)
   */
  createTaskCategory(request: CreateTaskCategoryRequest): Observable<CreateTaskCategoryResponse> {
    return this.http.post<CreateTaskCategoryResponse>(this.apiUrl, request);
  }

  /**
   * Get task category by ID
   * US 4.1.14: Retrieve specific category details
   */
  getTaskCategoryById(id: string): Observable<GetTaskCategoryResponse> {
    return this.http.get<GetTaskCategoryResponse>(`${this.apiUrl}/${id}`);
  }

  /**
   * Get task category by code
   * US 4.1.14: Retrieve category by unique code (e.g., CTC001)
   */
  getTaskCategoryByCode(code: string): Observable<GetTaskCategoryResponse> {
    return this.http.get<GetTaskCategoryResponse>(`${this.apiUrl}/code/${code}`);
  }

  /**
   * List all task categories
   * US 4.1.14: Display catalog with optional filters
   */
  listTaskCategories(options?: {
    activeOnly?: boolean;
    sortBy?: string;
    sortOrder?: 'asc' | 'desc';
  }): Observable<ListTaskCategoriesResponse> {
    let params = new HttpParams();

    if (options?.activeOnly !== undefined) {
      params = params.set('activeOnly', options.activeOnly.toString());
    }
    if (options?.sortBy) {
      params = params.set('sortBy', options.sortBy);
    }
    if (options?.sortOrder) {
      params = params.set('sortOrder', options.sortOrder);
    }

    return this.http.get<ListTaskCategoriesResponse>(this.apiUrl, { params });
  }

  /**
   * List only active task categories
   * US 4.1.14: Display categories available for new tasks
   */
  listActiveTaskCategories(): Observable<ListTaskCategoriesResponse> {
    return this.listTaskCategories({ activeOnly: true });
  }

  /**
   * Update task category
   * US 4.1.14: Modify category properties (code cannot be changed)
   */
  updateTaskCategory(id: string, request: UpdateTaskCategoryRequest): Observable<UpdateTaskCategoryResponse> {
    return this.http.put<UpdateTaskCategoryResponse>(`${this.apiUrl}/${id}`, request);
  }

  /**
   * Deactivate task category
   * US 4.1.14: Soft delete category
   */
  deactivateTaskCategory(id: string): Observable<ToggleActivationResponse> {
    return this.http.post<ToggleActivationResponse>(`${this.apiUrl}/${id}/deactivate`, {});
  }

  /**
   * Reactivate task category
   * US 4.1.14: Restore deactivated category
   */
  reactivateTaskCategory(id: string): Observable<ToggleActivationResponse> {
    return this.http.post<ToggleActivationResponse>(`${this.apiUrl}/${id}/reactivate`, {});
  }

  /**
   * Delete task category permanently
   * US 4.1.14: Remove category from catalog
   * Note: Only possible if category is not used in any complementary tasks
   */
  deleteTaskCategory(id: string): Observable<DeleteTaskCategoryResponse> {
    return this.http.delete<DeleteTaskCategoryResponse>(`${this.apiUrl}/${id}`);
  }

  /**
   * Get count of task categories
   * US 4.1.14: Count total or active categories
   */
  getTaskCategoryCount(activeOnly: boolean = false): Observable<{ success: boolean; data: { count: number } }> {
    let params = new HttpParams();
    if (activeOnly) {
      params = params.set('activeOnly', 'true');
    }
    return this.http.get<{ success: boolean; data: { count: number } }>(`${this.apiUrl}/count`, { params });
  }
}
