import { Injectable } from '@angular/core';
import { HttpClient, HttpParams } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment } from '../../../environments/environment';
import { Vessel, CreateVesselRequest, UpdateVesselRequest, VesselType, Organization, VesselSearchParams } from '../models/vessel.model';

@Injectable({
  providedIn: 'root'
})
export class VesselService {
  private apiUrl = `${environment.apiUrl}/vessels`;
  private vesselTypesUrl = `${environment.apiUrl}/vesseltypes`;
  private organizationsUrl = `${environment.apiUrl}/organizations`;

  constructor(private http: HttpClient) {}

  /**
   * Search vessels by IMO, name, or organization
   */
  searchVessels(params: VesselSearchParams): Observable<Vessel[]> {
    let httpParams = new HttpParams();
    
    if (params.imo) {
      httpParams = httpParams.set('imo', params.imo);
    }
    if (params.name) {
      httpParams = httpParams.set('name', params.name);
    }
    if (params.organizationName) {
      httpParams = httpParams.set('organizationName', params.organizationName);
    }

    return this.http.get<Vessel[]>(this.apiUrl, { params: httpParams });
  }

  /**
   * Create a new vessel
   */
  createVessel(vessel: CreateVesselRequest): Observable<Vessel> {
    return this.http.post<Vessel>(this.apiUrl, vessel);
  }

  /**
   * Update an existing vessel
   */
  updateVessel(imo: string, vessel: UpdateVesselRequest): Observable<Vessel> {
    return this.http.put<Vessel>(`${this.apiUrl}/${imo}`, vessel);
  }

  /**
   * Get all vessel types (for dropdown)
   */
  getVesselTypes(name?: string): Observable<{ total: number; page: number; pageSize: number; items: VesselType[] }> {
    let httpParams = new HttpParams()
      .set('page', '1')
      .set('pageSize', '100');
    
    if (name) {
      httpParams = httpParams.set('name', name);
    }

    return this.http.get<{ total: number; page: number; pageSize: number; items: VesselType[] }>(
      this.vesselTypesUrl, 
      { params: httpParams }
    );
  }

  /**
   * Get all organizations (for dropdown)
   */
  getOrganizations(): Observable<Organization[]> {
    return this.http.get<Organization[]>(this.organizationsUrl);
  }
}
