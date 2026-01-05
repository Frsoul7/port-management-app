import { Injectable } from '@angular/core';
import { HttpClient, HttpParams } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment } from '../../../environments/environment';
import { StorageArea, CreateStorageAreaRequest, UpdateStorageAreaRequest, UpdateOccupancyRequest, StorageAreaSearchParams, DockInfo } from '../models/storage-area.model';

@Injectable({
  providedIn: 'root'
})
export class StorageAreaService {
  private apiUrl = `${environment.apiUrl}/StorageAreas`;

  constructor(private http: HttpClient) {}

  searchStorageAreas(params: StorageAreaSearchParams = {}): Observable<StorageArea[]> {
    let httpParams = new HttpParams();
    
    if (params.name) {
      httpParams = httpParams.set('name', params.name);
    }
    if (params.location) {
      httpParams = httpParams.set('location', params.location);
    }
    if (params.type) {
      httpParams = httpParams.set('type', params.type);
    }
    if (params.servesAllDocks !== undefined) {
      httpParams = httpParams.set('servesAllDocks', params.servesAllDocks.toString());
    }

    return this.http.get<StorageArea[]>(this.apiUrl, { params: httpParams });
  }

  getStorageAreaById(id: string): Observable<StorageArea> {
    return this.http.get<StorageArea>(`${this.apiUrl}/${id}`);
  }

  createStorageArea(request: CreateStorageAreaRequest): Observable<StorageArea> {
    return this.http.post<StorageArea>(this.apiUrl, request);
  }

  updateStorageArea(id: string, request: UpdateStorageAreaRequest): Observable<StorageArea> {
    return this.http.put<StorageArea>(`${this.apiUrl}/${id}`, request);
  }

  updateOccupancy(id: string, request: UpdateOccupancyRequest): Observable<StorageArea> {
    return this.http.patch<StorageArea>(`${this.apiUrl}/${id}/occupancy`, request);
  }

  deleteStorageArea(id: string): Observable<void> {
    return this.http.delete<void>(`${this.apiUrl}/${id}`);
  }

  // Get docks for the dropdown
  getDocks(): Observable<DockInfo[]> {
    return this.http.get<DockInfo[]>(`${environment.apiUrl}/Docks`);
  }
}
