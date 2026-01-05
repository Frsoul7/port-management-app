import { Injectable, inject } from '@angular/core';
import { Observable } from 'rxjs';
import { ApiService } from './api.service';
import {
  NonUserDataRequestDto,
  NonUserDataRequestResponse
} from '../models/data-request.model';

/**
 * Service for Non-User Data Requests
 * US 4.5.4: Non-User Data Rights
 * 
 * Allows non-users (crew members, captains, representatives)
 * to submit GDPR data requests without being registered
 */
@Injectable({
  providedIn: 'root'
})
export class DataRequestService {
  private api = inject(ApiService);
  private readonly basePath = 'DataRequests';

  /**
   * Submit a data request from a non-user
   * Public endpoint - no authentication required
   */
  submitNonUserRequest(request: NonUserDataRequestDto): Observable<NonUserDataRequestResponse> {
    return this.api.post<NonUserDataRequestResponse>(`${this.basePath}/non-user`, request);
  }

  /**
   * Check the status of a non-user data request
   * Public endpoint - requires reference number and email
   */
  checkRequestStatus(referenceNumber: string, email: string): Observable<NonUserDataRequestResponse> {
    return this.api.get<NonUserDataRequestResponse>(`${this.basePath}/non-user/status`, {
      referenceNumber,
      email
    });
  }
}
