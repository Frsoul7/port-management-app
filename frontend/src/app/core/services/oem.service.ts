import { Injectable, inject } from '@angular/core';
import { HttpClient, HttpParams } from '@angular/common/http';
import { Observable } from 'rxjs';
import {
  GenerateOperationPlanRequest,
  GenerateOperationPlanResponse,
  OperationPlan,
  OperationPlanListResponse,
  UpdateOperationRequest,
  UpdateOperationResponse,
  ConflictDetectionRequest,
  ConflictDetectionResponse
} from '../models/operation-plan.model';

@Injectable({
  providedIn: 'root'
})
export class OemService {
  private http = inject(HttpClient);
  private apiUrl = 'http://localhost:3000/api/v1';

  /**
   * US 4.1.2: Generate operation plan for a target date
   */
  generateOperationPlan(request: GenerateOperationPlanRequest): Observable<GenerateOperationPlanResponse> {
    return this.http.post<GenerateOperationPlanResponse>(
      `${this.apiUrl}/operation-plans/generate`,
      request
    );
  }

  /**
   * US 4.1.3: Get all operation plans with optional filters
   */
  getOperationPlans(params?: {
    targetDate?: string;
    status?: string;
    page?: number;
    limit?: number;
  }): Observable<OperationPlanListResponse> {
    let httpParams = new HttpParams();
    
    if (params?.targetDate) {
      httpParams = httpParams.set('targetDate', params.targetDate);
    }
    if (params?.status) {
      httpParams = httpParams.set('status', params.status);
    }
    if (params?.page) {
      httpParams = httpParams.set('page', params.page.toString());
    }
    if (params?.limit) {
      httpParams = httpParams.set('limit', params.limit.toString());
    }

    return this.http.get<OperationPlanListResponse>(
      `${this.apiUrl}/operation-plans`,
      { params: httpParams }
    );
  }

  /**
   * US 4.1.3: Get operation plan by ID
   */
  getOperationPlanById(id: string): Observable<{ success: boolean; data: OperationPlan }> {
    return this.http.get<{ success: boolean; data: OperationPlan }>(
      `${this.apiUrl}/operation-plans/${id}`
    );
  }

  /**
   * US 4.1.3: Get operation plan by date
   */
  getOperationPlanByDate(date: string): Observable<{ success: boolean; data: OperationPlan }> {
    return this.http.get<{ success: boolean; data: OperationPlan }>(
      `${this.apiUrl}/operation-plans/by-date/${date}`
    );
  }

  /**
   * US 4.1.4: Approve operation plan
   */
  approveOperationPlan(id: string): Observable<{ success: boolean; data: OperationPlan }> {
    return this.http.patch<{ success: boolean; data: OperationPlan }>(
      `${this.apiUrl}/operation-plans/${id}/approve`,
      {}
    );
  }

  /**
   * US 4.1.4: Update dock assignment
   */
  updateDockAssignment(id: string, operationId: string, dockId: string): Observable<{ success: boolean; data: OperationPlan }> {
    return this.http.patch<{ success: boolean; data: OperationPlan }>(
      `${this.apiUrl}/operation-plans/${id}/dock`,
      { operationId, dockId }
    );
  }

  /**
   * US 4.1.4: Update staff assignment
   */
  updateStaffAssignment(id: string, operationId: string, staffIds: string[]): Observable<{ success: boolean; data: OperationPlan }> {
    return this.http.patch<{ success: boolean; data: OperationPlan }>(
      `${this.apiUrl}/operation-plans/${id}/staff`,
      { operationId, staffIds }
    );
  }

  /**
   * US 4.1.5: Get VVNs without operation plans for a date
   */
  getMissingPlans(date: string): Observable<{ success: boolean; data: any[] }> {
    return this.http.get<{ success: boolean; data: any[] }>(
      `${this.apiUrl}/operation-plans/missing`,
      { params: { date } }
    );
  }

  /**
   * US 4.1.6: Query resource allocation time
   */
  getResourceAllocation(params: {
    resourceType: 'crane' | 'dock' | 'staff';
    resourceId: string;
    startDate: string;
    endDate: string;
  }): Observable<{ success: boolean; data: any }> {
    let httpParams = new HttpParams()
      .set('resourceType', params.resourceType)
      .set('resourceId', params.resourceId)
      .set('startDate', params.startDate)
      .set('endDate', params.endDate);

    return this.http.get<{ success: boolean; data: any }>(
      `${this.apiUrl}/operation-plans/resource-allocation`,
      { params: httpParams }
    );
  }

  /**
   * Delete operation plan (only if status is GENERATED)
   */
  deleteOperationPlan(id: string): Observable<{ success: boolean; message: string }> {
    return this.http.delete<{ success: boolean; message: string }>(
      `${this.apiUrl}/operation-plans/${id}`
    );
  }

  /**
   * US 4.1.3: List operation plans with advanced filtering, sorting, and pagination
   * Supports: vvnId, vesselImo, status, fromDate, toDate, sortBy, sortOrder, page, limit
   */
  async listOperationPlans(params?: {
    vvnId?: string;
    vesselImo?: string;
    status?: string;
    targetDate?: string;
    fromDate?: string;
    toDate?: string;
    algorithm?: string;
    page?: number;
    limit?: number;
    sortBy?: string;
    sortOrder?: 'asc' | 'desc';
  }): Promise<OperationPlanListResponse> {
    let httpParams = new HttpParams();

    // Add all filter parameters if present
    if (params?.vvnId) httpParams = httpParams.set('vvnId', params.vvnId);
    if (params?.vesselImo) httpParams = httpParams.set('vesselImo', params.vesselImo);
    if (params?.status) httpParams = httpParams.set('status', params.status);
    if (params?.targetDate) httpParams = httpParams.set('targetDate', params.targetDate);
    if (params?.fromDate) httpParams = httpParams.set('fromDate', params.fromDate);
    if (params?.toDate) httpParams = httpParams.set('toDate', params.toDate);
    if (params?.algorithm) httpParams = httpParams.set('algorithm', params.algorithm);
    if (params?.page) httpParams = httpParams.set('page', params.page.toString());
    if (params?.limit) httpParams = httpParams.set('limit', params.limit.toString());
    if (params?.sortBy) httpParams = httpParams.set('sortBy', params.sortBy);
    if (params?.sortOrder) httpParams = httpParams.set('sortOrder', params.sortOrder);

    return this.http
      .get<OperationPlanListResponse>(`${this.apiUrl}/operation-plans`, { params: httpParams })
      .toPromise()
      .then(response => response || {
        success: false,
        data: [],
        pagination: { page: 1, limit: 10, total: 0, totalPages: 0 }
      });
  }

  /**
   * US 4.1.4: Update an operation in a plan (general update endpoint)
   * Allows updating time windows, crane assignments, dock, and staff
   */
  updateOperation(
    planId: string,
    vvnId: string,
    updates: {
      plannedStart?: Date;
      plannedEnd?: Date;
      assignedCranes?: number;
      assignedDock?: string;
      assignedStaff?: string[];
    },
    reason: string
  ): Observable<UpdateOperationResponse> {
    const request: UpdateOperationRequest = {
      vvnId,
      updates: {
        plannedStart: updates.plannedStart?.toISOString(),
        plannedEnd: updates.plannedEnd?.toISOString(),
        assignedCranes: updates.assignedCranes,
        assignedDock: updates.assignedDock,
        assignedStaff: updates.assignedStaff,
      },
      reason,
    };

    return this.http.patch<UpdateOperationResponse>(
      `${this.apiUrl}/operation-plans/${planId}/operations`,
      request
    );
  }

  /**
   * US 4.1.4: Detect conflicts for a proposed operation update
   * Returns conflict information without actually updating the plan
   */
  detectConflicts(
    planId: string,
    vvnId: string,
    updates: {
      plannedStart?: Date;
      plannedEnd?: Date;
      assignedCranes?: number;
      assignedDock?: string;
      assignedStaff?: string[];
    }
  ): Observable<ConflictDetectionResponse> {
    const request: ConflictDetectionRequest = {
      vvnId,
      updates: {
        plannedStart: updates.plannedStart?.toISOString(),
        plannedEnd: updates.plannedEnd?.toISOString(),
        assignedCranes: updates.assignedCranes,
        assignedDock: updates.assignedDock,
        assignedStaff: updates.assignedStaff,
      },
    };

    return this.http.post<ConflictDetectionResponse>(
      `${this.apiUrl}/operation-plans/${planId}/conflicts`,
      request
    );
  }
}
