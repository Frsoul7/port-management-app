import { Injectable, inject } from '@angular/core';
import { HttpClient, HttpParams } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment } from '../../../environments/environment';
import {
  VesselType,
  CreateVesselTypeRequest,
  UpdateVesselTypeRequest,
  VesselTypeSearchParams,
  VesselTypeSearchResponse
} from '../models/vessel-type.model';

@Injectable({
  providedIn: 'root'
})
export class VesselTypeService {
  private http = inject(HttpClient);
  private apiUrl = `${environment.apiUrl}/VesselTypes`;

  /**
   * Search/filter vessel types with pagination
   */
  searchVesselTypes(params?: VesselTypeSearchParams): Observable<VesselTypeSearchResponse> {
    let httpParams = new HttpParams();

    if (params?.name) {
      httpParams = httpParams.set('name', params.name);
    }
    if (params?.description) {
      httpParams = httpParams.set('description', params.description);
    }
    if (params?.page) {
      httpParams = httpParams.set('page', params.page.toString());
    }
    if (params?.pageSize) {
      httpParams = httpParams.set('pageSize', params.pageSize.toString());
    }

    return this.http.get<VesselTypeSearchResponse>(this.apiUrl, { params: httpParams });
  }

  /**
   * Create a new vessel type
   */
  createVesselType(request: CreateVesselTypeRequest): Observable<VesselType> {
    return this.http.post<VesselType>(this.apiUrl, request);
  }

  /**
   * Update an existing vessel type
   */
  updateVesselType(id: string, request: UpdateVesselTypeRequest): Observable<VesselType> {
    return this.http.put<VesselType>(`${this.apiUrl}/${id}`, request);
  }
}
