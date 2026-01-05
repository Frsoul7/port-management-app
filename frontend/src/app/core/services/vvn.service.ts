import { Injectable } from '@angular/core';
import { HttpClient, HttpParams } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment } from '../../../environments/environment';
import {
  VvnStatusResponse,
  VvnSummaryResponse,
  CreateVvnRequest,
  UpdateVvnRequest,
  UpsertManifestsRequest,
  SetCrewRequest,
  VvnApprovalRequest,
  VvnApprovalResponse,
  VvnRejectionRequest,
  VvnRejectionResponse,
  VvnEntryResponse,
  AddManifestEntryRequest,
  ManifestType,
  VvnSearchParams
} from '../models/vvn.model';
import { AuthService } from './auth.service';

@Injectable({
  providedIn: 'root'
})
export class VvnService {
  private apiUrl = `${environment.apiUrl}/vvns`;

  constructor(
    private http: HttpClient,
    private authService: AuthService
  ) {}

  /**
   * Get all VVNs (for Logistics Operators/Port Authority)
   */
  getAllVvns(): Observable<VvnSummaryResponse[]> {
    return this.http.get<VvnSummaryResponse[]>(this.apiUrl);
  }

  /**
   * Get VVNs for the authenticated user's organization (Shipping Agent)
   * with optional filters
   */
  getMyOrganizationVvns(params?: VvnSearchParams): Observable<VvnStatusResponse[]> {
    let httpParams = new HttpParams();
    
    if (params?.vesselImo) {
      httpParams = httpParams.set('vesselImo', params.vesselImo);
    }
    if (params?.status) {
      httpParams = httpParams.set('status', params.status);
    }
    if (params?.submittedById) {
      httpParams = httpParams.set('submittedById', params.submittedById);
    }
    if (params?.fromDate) {
      httpParams = httpParams.set('fromDate', params.fromDate);
    }
    if (params?.toDate) {
      httpParams = httpParams.set('toDate', params.toDate);
    }

    return this.http.get<VvnStatusResponse[]>(`${this.apiUrl}/my-organization`, { params: httpParams });
  }

  /**
   * Get a specific VVN by ID
   */
  getVvnById(id: string): Observable<VvnSummaryResponse> {
    return this.http.get<VvnSummaryResponse>(`${this.apiUrl}/${id}`);
  }

  /**
   * Get manifest entries for a VVN
   */
  getEntries(id: string, type?: ManifestType): Observable<VvnEntryResponse[]> {
    let httpParams = new HttpParams();
    if (type) {
      httpParams = httpParams.set('type', type);
    }
    return this.http.get<VvnEntryResponse[]>(`${this.apiUrl}/${id}/entries`, { params: httpParams });
  }

  /**
   * Get decision logs for a VVN
   */
  getDecisions(id: string): Observable<any[]> {
    return this.http.get<any[]>(`${this.apiUrl}/${id}/decisions`);
  }

  /**
   * Create a new VVN (Shipping Agent only)
   */
  createVvn(request: CreateVvnRequest): Observable<VvnSummaryResponse> {
    return this.http.post<VvnSummaryResponse>(this.apiUrl, request);
  }

  /**
   * Update VVN header information (Shipping Agent only, while IN_PROGRESS or REJECTED)
   */
  updateVvn(id: string, request: UpdateVvnRequest): Observable<VvnSummaryResponse> {
    return this.http.put<VvnSummaryResponse>(`${this.apiUrl}/${id}`, request);
  }

  /**
   * Replace manifests (loading and/or unloading)
   */
  upsertManifests(id: string, request: UpsertManifestsRequest): Observable<any> {
    return this.http.put<any>(`${this.apiUrl}/${id}/manifests`, request);
  }

  /**
   * Add a single manifest entry
   */
  addManifestEntry(id: string, request: AddManifestEntryRequest): Observable<any> {
    return this.http.post<any>(`${this.apiUrl}/${id}/entries`, request);
  }

  /**
   * Remove a single manifest entry
   */
  removeManifestEntry(id: string, entryId: string, type: ManifestType): Observable<void> {
    let httpParams = new HttpParams().set('type', type);
    return this.http.delete<void>(`${this.apiUrl}/${id}/entries/${entryId}`, { params: httpParams });
  }

  /**
   * Set crew information (hazardous cargo handlers)
   */
  setCrew(id: string, request: SetCrewRequest): Observable<any> {
    return this.http.post<any>(`${this.apiUrl}/${id}/crew`, request);
  }

  /**
   * Submit VVN for approval (Shipping Agent only)
   */
  submitVvn(id: string): Observable<VvnSummaryResponse> {
    return this.http.post<VvnSummaryResponse>(`${this.apiUrl}/${id}:submit`, {});
  }

  /**
   * Reopen a rejected VVN to IN_PROGRESS status (Shipping Agent only)
   */
  reopenVvn(id: string): Observable<any> {
    return this.http.post<any>(`${this.apiUrl}/${id}:reopen`, {});
  }

  /**
   * Approve a VVN (Port Authority only)
   */
  approveVvn(id: string, request: VvnApprovalRequest): Observable<VvnApprovalResponse> {
    return this.http.post<VvnApprovalResponse>(`${this.apiUrl}/${id}/approve`, request);
  }

  /**
   * Reject a VVN (Port Authority only)
   */
  rejectVvn(id: string, request: VvnRejectionRequest): Observable<VvnRejectionResponse> {
    return this.http.post<VvnRejectionResponse>(`${this.apiUrl}/${id}/reject`, request);
  }

  /**
   * Helper method to check if user can edit a VVN
   */
  canEdit(vvn: VvnStatusResponse | VvnSummaryResponse): boolean {
    const isShippingAgent = this.authService.isShippingAgent();
    const status = vvn.status;
    return isShippingAgent && (status === 'IN_PROGRESS' || status === 'REJECTED');
  }

  /**
   * Helper method to check if user can submit a VVN
   */
  canSubmit(vvn: VvnStatusResponse | VvnSummaryResponse): boolean {
    const isShippingAgent = this.authService.isShippingAgent();
    return isShippingAgent && vvn.status === 'IN_PROGRESS';
  }

  /**
   * Helper method to check if user can reopen a VVN
   */
  canReopen(vvn: VvnStatusResponse | VvnSummaryResponse): boolean {
    const isShippingAgent = this.authService.isShippingAgent();
    return isShippingAgent && vvn.status === 'REJECTED';
  }

  /**
   * Helper method to check if user can approve/reject a VVN
   */
  canApproveReject(vvn: VvnStatusResponse | VvnSummaryResponse): boolean {
    const isPortAuthority = this.authService.hasRole('PORT_AUTHORITY_OFFICER' as any);
    return isPortAuthority && vvn.status === 'SUBMITTED';
  }
}
