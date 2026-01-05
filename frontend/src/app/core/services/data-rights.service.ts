import { Injectable, inject } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Observable } from 'rxjs';
import { ConfigService } from './config.service';

/**
 * User data export structure
 */
export interface UserDataExport {
  exportedAt: string;
  version: string;
  profile: UserProfileData;
  organization?: OrganizationData;
  createdVvns: VvnSummary[];
  staffAssignments: StaffAssignment[];
  policyAcknowledgments: PolicyAcknowledgment[];
  dataRequests: DataRequestSummary[];
  oemData?: OemData;
  dataSourceNotes?: string;
}

export interface UserProfileData {
  id: string;
  name: string;
  email: string;
  profilePictureUrl?: string;
  role?: string;
  isActive: boolean;
  emailVerified: boolean;
  createdAt: string;
}

export interface OrganizationData {
  id: string;
  name: string;
  type: string;
}

export interface VvnSummary {
  id: string;
  vvnNumber: string;
  status: string;
  createdAt: string;
  vesselName?: string;
}

export interface StaffAssignment {
  staffMemberId: string;
  name: string;
  qualifications: string[];
}

export interface PolicyAcknowledgment {
  policyId: string;
  policyVersion: string;
  acknowledgedAt: string;
}

export interface DataRequestSummary {
  referenceNumber: string;
  requestType: string;
  status: string;
  submittedAt: string;
  completedAt?: string;
}

export interface OemData {
  operationPlans: any[];
  vesselVisitExecutions: any[];
  incidents: any[];
  complementaryTasks: any[];
}

/**
 * Rectification request DTO
 */
export interface RectificationRequest {
  dataCategory: string;
  currentValue: string;
  requestedCorrection: string;
  reason: string;
}

/**
 * Deletion request DTO
 */
export interface DeletionRequest {
  confirmUnderstanding: boolean;
  reason?: string;
}

/**
 * Response for user data requests
 */
export interface UserDataRequestResponse {
  requestId: string;
  referenceNumber: string;
  requestType: string;
  status: string;
  submittedAt: string;
  estimatedResponseDays: number;
  message: string;
}

/**
 * User's own data request (for history view)
 */
export interface MyDataRequest {
  requestId: string;
  referenceNumber: string;
  requestType: string;
  status: string;
  description: string;
  submittedAt: string;
  updatedAt?: string;
  completedAt?: string;
}

/**
 * Service for User Data Rights operations (GDPR Subject Access Requests)
 * US 4.5.3: User Data Rights (SAR)
 */
@Injectable({
  providedIn: 'root'
})
export class DataRightsService {
  private readonly http = inject(HttpClient);
  private readonly configService = inject(ConfigService);

  private get baseUrl(): string {
    return `${this.configService.apiUrl}/api/v1/DataRights`;
  }

  /**
   * Export user's data as JSON (GDPR Article 15)
   */
  exportDataJson(): Observable<UserDataExport> {
    return this.http.get<UserDataExport>(`${this.baseUrl}/export`);
  }

  /**
   * Export user's data as PDF (GDPR Article 15)
   */
  exportDataPdf(): Observable<Blob> {
    return this.http.get(`${this.baseUrl}/export/pdf`, {
      responseType: 'blob'
    });
  }

  /**
   * Submit a rectification request (GDPR Article 16)
   */
  requestRectification(request: RectificationRequest): Observable<UserDataRequestResponse> {
    return this.http.post<UserDataRequestResponse>(`${this.baseUrl}/rectification`, request);
  }

  /**
   * Submit a deletion request (GDPR Article 17)
   */
  requestDeletion(request: DeletionRequest): Observable<UserDataRequestResponse> {
    return this.http.post<UserDataRequestResponse>(`${this.baseUrl}/deletion`, request);
  }

  /**
   * Get user's data request history
   */
  getMyRequests(): Observable<MyDataRequest[]> {
    return this.http.get<MyDataRequest[]>(`${this.baseUrl}/requests`);
  }

  /**
   * Download a blob as a file
   */
  downloadBlob(blob: Blob, filename: string): void {
    const url = window.URL.createObjectURL(blob);
    const link = document.createElement('a');
    link.href = url;
    link.download = filename;
    link.click();
    window.URL.revokeObjectURL(url);
  }

  /**
   * Download JSON data as a file
   */
  downloadJson(data: any, filename: string): void {
    const json = JSON.stringify(data, null, 2);
    const blob = new Blob([json], { type: 'application/json' });
    this.downloadBlob(blob, filename);
  }
}
