import { Injectable, inject } from '@angular/core';
import { HttpClient, HttpParams } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment } from '../../../environments/environment';

/**
 * Incident interfaces
 * US 4.1.13: Incident recording and management
 */
export type IncidentStatus = 'REPORTED' | 'UNDER_INVESTIGATION' | 'RESOLVED' | 'CLOSED';
export type IncidentSeverity = 'LOW' | 'MEDIUM' | 'HIGH' | 'CRITICAL';

export interface Incident {
  incidentId: string;
  incidentTypeId: string;
  vveId: string | null;
  title: string;
  description: string;
  severity: IncidentSeverity;
  status: IncidentStatus;
  reportedAt: string;
  reportedBy: string;
  investigatedAt: string | null;
  investigatedBy: string | null;
  resolvedAt: string | null;
  resolvedBy: string | null;
  closedAt: string | null;
  closedBy: string | null;
  externalEntitiesInvolved: string[];
  notes: IncidentNote[];
  resolutionSummary: string | null;
  impactDescription: string | null;
  durationMinutes: number | null;
}

export interface IncidentNote {
  timestamp: string;
  author: string;
  content: string;
}

export interface CreateIncidentRequest {
  incidentTypeId: string;
  vveId?: string;
  title: string;
  description: string;
  severity: IncidentSeverity;
  reportedBy: string;
  impactDescription?: string;
  externalEntitiesInvolved?: string[];
}

export interface UpdateIncidentRequest {
  title?: string;
  description?: string;
  severity?: IncidentSeverity;
  impactDescription?: string;
}

export interface StartInvestigationRequest {
  investigatedBy: string;
}

export interface ResolveIncidentRequest {
  resolvedBy: string;
  resolutionSummary: string;
}

export interface CloseIncidentRequest {
  closedBy: string;
}

export interface AddNoteRequest {
  author: string;
  content: string;
}

export interface InvolveExternalEntityRequest {
  entityName: string;
}

export interface IncidentFilters {
  status?: IncidentStatus;
  severity?: IncidentSeverity;
  incidentTypeId?: string;
  vveId?: string;
  reportedBy?: string;
  fromDate?: string;
  toDate?: string;
  externalEntity?: string;
  limit?: number;
  offset?: number;
}

export interface IncidentResponse {
  success: boolean;
  message?: string;
  data: Incident;
}

export interface IncidentListResponse {
  success: boolean;
  data: Incident[];
  count: number;
}

/**
 * Incident Service
 * Handles incident management operations
 * US 4.1.13: Record and manage operational incidents
 */
@Injectable({
  providedIn: 'root'
})
export class IncidentService {
  private http = inject(HttpClient);
  private apiUrl = `${environment.backendOemUrl}/incidents`;

  /**
   * Create a new incident
   */
  createIncident(request: CreateIncidentRequest): Observable<IncidentResponse> {
    return this.http.post<IncidentResponse>(this.apiUrl, request);
  }

  /**
   * Get incident by ID
   */
  getIncidentById(id: string): Observable<IncidentResponse> {
    return this.http.get<IncidentResponse>(`${this.apiUrl}/${id}`);
  }

  /**
   * List incidents with optional filtering
   */
  listIncidents(filters?: IncidentFilters): Observable<IncidentListResponse> {
    let params = new HttpParams();
    
    if (filters) {
      if (filters.status) params = params.set('status', filters.status);
      if (filters.severity) params = params.set('severity', filters.severity);
      if (filters.incidentTypeId) params = params.set('incidentTypeId', filters.incidentTypeId);
      if (filters.vveId) params = params.set('vveId', filters.vveId);
      if (filters.reportedBy) params = params.set('reportedBy', filters.reportedBy);
      if (filters.fromDate) params = params.set('fromDate', filters.fromDate);
      if (filters.toDate) params = params.set('toDate', filters.toDate);
      if (filters.externalEntity) params = params.set('externalEntity', filters.externalEntity);
      if (filters.limit) params = params.set('limit', filters.limit.toString());
      if (filters.offset) params = params.set('offset', filters.offset.toString());
    }

    return this.http.get<IncidentListResponse>(this.apiUrl, { params });
  }

  /**
   * Get active incidents (REPORTED or UNDER_INVESTIGATION)
   */
  getActiveIncidents(): Observable<IncidentListResponse> {
    return this.http.get<IncidentListResponse>(`${this.apiUrl}/active`);
  }

  /**
   * Get critical incidents
   */
  getCriticalIncidents(): Observable<IncidentListResponse> {
    return this.http.get<IncidentListResponse>(`${this.apiUrl}/critical`);
  }

  /**
   * Update incident details
   */
  updateIncident(id: string, request: UpdateIncidentRequest): Observable<IncidentResponse> {
    return this.http.put<IncidentResponse>(`${this.apiUrl}/${id}`, request);
  }

  /**
   * Delete incident
   */
  deleteIncident(id: string): Observable<{ success: boolean; message: string }> {
    return this.http.delete<{ success: boolean; message: string }>(`${this.apiUrl}/${id}`);
  }

  /**
   * Start investigation
   */
  startInvestigation(id: string, request: StartInvestigationRequest): Observable<IncidentResponse> {
    return this.http.patch<IncidentResponse>(`${this.apiUrl}/${id}/investigate`, request);
  }

  /**
   * Resolve incident
   */
  resolveIncident(id: string, request: ResolveIncidentRequest): Observable<IncidentResponse> {
    return this.http.patch<IncidentResponse>(`${this.apiUrl}/${id}/resolve`, request);
  }

  /**
   * Close incident
   */
  closeIncident(id: string, request: CloseIncidentRequest): Observable<IncidentResponse> {
    return this.http.patch<IncidentResponse>(`${this.apiUrl}/${id}/close`, request);
  }

  /**
   * Add note to incident
   */
  addNote(id: string, request: AddNoteRequest): Observable<IncidentResponse> {
    return this.http.post<IncidentResponse>(`${this.apiUrl}/${id}/notes`, request);
  }

  /**
   * Involve external entity
   */
  involveExternalEntity(id: string, request: InvolveExternalEntityRequest): Observable<IncidentResponse> {
    return this.http.post<IncidentResponse>(`${this.apiUrl}/${id}/external-entities`, request);
  }
}
