import { Injectable } from '@angular/core';
import { HttpClient, HttpParams } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment } from '../../../environments/environment';
import { Dock, CreateDockRequest, UpdateDockRequest, DockSearchParams, VesselTypeInfo } from '../models/dock.model';

@Injectable({
  providedIn: 'root'
})
export class DockService {
  private apiUrl = `${environment.apiUrl}/Docks`;

  constructor(private http: HttpClient) {}

  searchDocks(params: DockSearchParams = {}): Observable<Dock[]> {
    let httpParams = new HttpParams();
    
    if (params.name) {
      httpParams = httpParams.set('name', params.name);
    }
    if (params.location) {
      httpParams = httpParams.set('location', params.location);
    }
    if (params.vesselTypeId) {
      httpParams = httpParams.set('vesselTypeId', params.vesselTypeId);
    }

    return this.http.get<Dock[]>(this.apiUrl, { params: httpParams });
  }

  getDockByCode(code: string): Observable<Dock> {
    return this.http.get<Dock>(`${this.apiUrl}/${code}`);
  }

  createDock(request: CreateDockRequest): Observable<Dock> {
    return this.http.post<Dock>(this.apiUrl, request);
  }

  updateDock(code: string, request: UpdateDockRequest): Observable<Dock> {
    return this.http.put<Dock>(`${this.apiUrl}/${code}`, request);
  }

  deleteDock(code: string): Observable<void> {
    return this.http.delete<void>(`${this.apiUrl}/${code}`);
  }

  // Get vessel types for the dropdown (reusing vessel service or direct call)
  getVesselTypes(): Observable<{ items: VesselTypeInfo[] }> {
    return this.http.get<{ items: VesselTypeInfo[] }>(`${environment.apiUrl}/VesselTypes`);
  }
}
