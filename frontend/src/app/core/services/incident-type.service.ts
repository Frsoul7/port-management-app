import { Injectable, inject } from '@angular/core';
import { HttpClient, HttpParams } from '@angular/common/http';
import { Observable } from 'rxjs';

/**
 * Incident Type interfaces
 * US 4.1.12: Hierarchical incident type management
 */
export interface IncidentType {
  incidentTypeId: string;
  code: string;
  typeName: string;
  description: string;
  defaultSeverity: 'LOW' | 'MEDIUM' | 'HIGH' | 'CRITICAL';
  categoryCode: 'ENV' | 'OPS' | 'SAF';
  categoryName: string;
  parentTypeId: string | null;
  requiresExternalEntities: boolean;
  estimatedResolutionTimeHours: number;
  isActive: boolean;
  isRootType: boolean;
  createdAt: string;
  updatedAt: string;
}

export interface CreateIncidentTypeRequest {
  code: string;
  typeName: string;
  description: string;
  defaultSeverity: 'LOW' | 'MEDIUM' | 'HIGH' | 'CRITICAL';
  categoryCode: 'ENV' | 'OPS' | 'SAF';
  parentTypeId?: string;
  requiresExternalEntities?: boolean;
  estimatedResolutionTimeHours: number;
}

export interface UpdateIncidentTypeRequest {
  typeName?: string;
  description?: string;
  defaultSeverity?: 'LOW' | 'MEDIUM' | 'HIGH' | 'CRITICAL';
  categoryCode?: 'ENV' | 'OPS' | 'SAF';
  parentTypeId?: string;
  requiresExternalEntities?: boolean;
  estimatedResolutionTimeHours?: number;
}

export interface IncidentTypeResponse {
  success: boolean;
  message?: string;
  data: IncidentType;
}

export interface IncidentTypeListResponse {
  success: boolean;
  data: IncidentType[];
  count: number;
}

/**
 * US 4.1.12: Incident Type Service
 * Manages hierarchical incident type catalog
 */
@Injectable({
  providedIn: 'root'
})
export class IncidentTypeService {
  private http = inject(HttpClient);
  private apiUrl = 'http://localhost:3000/api/v1/incident-types';

  /**
   * Create a new incident type
   * US 4.1.12: Add new type to catalog
   */
  createIncidentType(request: CreateIncidentTypeRequest): Observable<IncidentTypeResponse> {
    return this.http.post<IncidentTypeResponse>(this.apiUrl, request);
  }

  /**
   * Get incident type by ID
   * US 4.1.12: Retrieve specific type details
   */
  getIncidentTypeById(id: string): Observable<IncidentTypeResponse> {
    return this.http.get<IncidentTypeResponse>(`${this.apiUrl}/${id}`);
  }

  /**
   * Get incident type by code
   * US 4.1.12: Retrieve type by unique code
   */
  getIncidentTypeByCode(code: string): Observable<IncidentTypeResponse> {
    return this.http.get<IncidentTypeResponse>(`${this.apiUrl}/code/${code}`);
  }

  /**
   * List all incident types
   * US 4.1.12: Display catalog with optional filters
   */
  listIncidentTypes(options?: {
    activeOnly?: boolean;
    category?: 'ENV' | 'OPS' | 'SAF';
    sortBy?: string;
    sortOrder?: 'asc' | 'desc';
  }): Observable<IncidentTypeListResponse> {
    let params = new HttpParams();
    
    if (options?.activeOnly !== undefined) {
      params = params.set('activeOnly', options.activeOnly.toString());
    }
    if (options?.category) {
      params = params.set('category', options.category);
    }
    if (options?.sortBy) {
      params = params.set('sortBy', options.sortBy);
    }
    if (options?.sortOrder) {
      params = params.set('sortOrder', options.sortOrder);
    }

    return this.http.get<IncidentTypeListResponse>(this.apiUrl, { params });
  }

  /**
   * Get root incident types (no parents)
   * US 4.1.12: Get top-level categories for tree view
   */
  getRootIncidentTypes(): Observable<IncidentTypeListResponse> {
    return this.http.get<IncidentTypeListResponse>(`${this.apiUrl}/root`);
  }

  /**
   * Get children of a specific incident type
   * US 4.1.12: Navigate hierarchical structure
   */
  getChildIncidentTypes(parentId: string): Observable<IncidentTypeListResponse> {
    return this.http.get<IncidentTypeListResponse>(`${this.apiUrl}/${parentId}/children`);
  }

  /**
   * Update incident type
   * US 4.1.12: Modify type properties
   */
  updateIncidentType(id: string, request: UpdateIncidentTypeRequest): Observable<IncidentTypeResponse> {
    return this.http.put<IncidentTypeResponse>(`${this.apiUrl}/${id}`, request);
  }

  /**
   * Deactivate incident type
   * US 4.1.12: Soft delete type
   */
  deactivateIncidentType(id: string): Observable<IncidentTypeResponse> {
    return this.http.post<IncidentTypeResponse>(`${this.apiUrl}/${id}/deactivate`, {});
  }

  /**
   * Reactivate incident type
   * US 4.1.12: Restore deactivated type
   */
  reactivateIncidentType(id: string): Observable<IncidentTypeResponse> {
    return this.http.post<IncidentTypeResponse>(`${this.apiUrl}/${id}/reactivate`, {});
  }

  /**
   * Delete incident type permanently
   * US 4.1.12: Remove type from catalog
   */
  deleteIncidentType(id: string): Observable<{ success: boolean; message: string }> {
    return this.http.delete<{ success: boolean; message: string }>(`${this.apiUrl}/${id}`);
  }
}
