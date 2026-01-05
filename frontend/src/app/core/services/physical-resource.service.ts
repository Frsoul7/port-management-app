import { Injectable } from '@angular/core';
import { HttpClient, HttpParams } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment } from '../../../environments/environment';
import { 
  PhysicalResource, 
  CreatePhysicalResourceRequest, 
  UpdatePhysicalResourceRequest,
  UpdateAvailabilityRequest,
  PhysicalResourceSearchParams,
  DockInfo
} from '../models/physical-resource.model';

@Injectable({
  providedIn: 'root'
})
export class PhysicalResourceService {
  private apiUrl = `${environment.apiUrl}/resources`;

  constructor(private http: HttpClient) {}

  searchPhysicalResources(params: PhysicalResourceSearchParams = {}): Observable<PhysicalResource[]> {
    let httpParams = new HttpParams();
    
    if (params.code) {
      httpParams = httpParams.set('code', params.code);
    }
    if (params.description) {
      httpParams = httpParams.set('description', params.description);
    }
    if (params.availability) {
      httpParams = httpParams.set('availability', params.availability);
    }

    return this.http.get<PhysicalResource[]>(this.apiUrl, { params: httpParams });
  }

  getPhysicalResourceByCode(code: string): Observable<PhysicalResource> {
    return this.http.get<PhysicalResource>(`${this.apiUrl}/${code}`);
  }

  createPhysicalResource(request: CreatePhysicalResourceRequest): Observable<PhysicalResource> {
    return this.http.post<PhysicalResource>(this.apiUrl, request);
  }

  updatePhysicalResource(code: string, request: UpdatePhysicalResourceRequest): Observable<PhysicalResource> {
    return this.http.put<PhysicalResource>(`${this.apiUrl}/${code}`, request);
  }

  updateAvailability(code: string, request: UpdateAvailabilityRequest): Observable<PhysicalResource> {
    return this.http.patch<PhysicalResource>(`${this.apiUrl}/${code}`, request);
  }

  deletePhysicalResource(code: string): Observable<void> {
    return this.http.delete<void>(`${this.apiUrl}/${code}`);
  }

  // Get docks for the dropdown
  getDocks(): Observable<DockInfo[]> {
    return this.http.get<DockInfo[]>(`${environment.apiUrl}/Docks`);
  }
}
