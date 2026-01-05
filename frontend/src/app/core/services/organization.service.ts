import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment } from '../../../environments/environment';
import {
  Organization,
  CreateOrganizationRequest,
  AddRepresentativesRequest,
  UpdateRepresentativeRequest,
  PatchRepresentativeStatusRequest,
  Representative,
  OrganizationListItem
} from '../models/organization.model';

@Injectable({
  providedIn: 'root'
})
export class OrganizationService {
  private apiUrl = `${environment.apiUrl}/organizations`;

  constructor(private http: HttpClient) {}

  /**
   * Get all organizations (public endpoint for registration)
   */
  getAllOrganizations(): Observable<OrganizationListItem[]> {
    return this.http.get<OrganizationListItem[]>(this.apiUrl);
  }

  /**
   * Get organization by ID
   */
  getOrganizationById(id: string): Observable<Organization> {
    return this.http.get<Organization>(`${this.apiUrl}/${id}`);
  }

  /**
   * Create a new organization (US 2.2.5)
   */
  createOrganization(request: CreateOrganizationRequest): Observable<Organization> {
    return this.http.post<Organization>(this.apiUrl, request);
  }

  /**
   * Add representatives to an organization by GUID (US 2.2.6)
   */
  addRepresentativesById(organizationId: string, request: AddRepresentativesRequest): Observable<Representative[]> {
    return this.http.post<Representative[]>(`${this.apiUrl}/${organizationId}/representatives`, request);
  }

  /**
   * Add representatives to an organization by identifier (US 2.2.6)
   */
  addRepresentativesByIdentifier(identifier: string, request: AddRepresentativesRequest): Observable<Representative[]> {
    return this.http.post<Representative[]>(`${this.apiUrl}/by-identifier/${identifier}/representatives`, request);
  }

  /**
   * Update representative information (US 2.2.6)
   */
  updateRepresentative(
    organizationId: string,
    representativeId: string,
    request: UpdateRepresentativeRequest
  ): Observable<Representative> {
    return this.http.put<Representative>(
      `${this.apiUrl}/${organizationId}/representatives/${representativeId}`,
      request
    );
  }

  /**
   * Change representative status (activate/deactivate) (US 2.2.6)
   */
  patchRepresentativeStatus(
    organizationId: string,
    representativeId: string,
    request: PatchRepresentativeStatusRequest
  ): Observable<Representative> {
    return this.http.patch<Representative>(
      `${this.apiUrl}/${organizationId}/representatives/${representativeId}`,
      request
    );
  }
}
