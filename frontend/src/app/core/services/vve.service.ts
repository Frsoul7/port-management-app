import { Injectable, inject } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import {
  InitializeVVERequest,
  InitializeVVEResponse,
  RecordBerthingRequest,
  RecordBerthingResponse,
  VesselVisitExecution,
  UpdateExecutedOperationRequest,
  UpdateExecutedOperationResponse,
  ExecutedOperationStatus
} from '../models/vve.model';

@Injectable({
  providedIn: 'root'
})
export class VveService {
  private http = inject(HttpClient);
  private apiUrl = 'http://localhost:3000/api/v1/vessel-visit-executions';

  /**
   * US 4.1.7: Initialize VVE from operation plan
   * US 4.1.9: Added vvnId to derive operations from planned operations
   * @param operationPlanId - The operation plan ID to initialize VVE from
   * @param vvnId - The VVN ID to filter planned operations for this vessel
   * @param arrivalTime - Actual arrival time at port (ISO 8601 format)
   * @returns Observable with VVE creation response
   */
  initializeVVE(operationPlanId: string, vvnId: string, arrivalTime: string): Observable<InitializeVVEResponse> {
    const request: InitializeVVERequest = {
      operationPlanId,
      vvnId,
      arrivalTime
    };

    return this.http.post<InitializeVVEResponse>(
      `${this.apiUrl}/initialize`,
      request
    );
  }

  /**
   * US 4.1.10: Get VVE by ID (for future use)
   * @param id - The VVE ID to retrieve
   * @returns Observable with VVE data
   */
  getVVEById(id: string): Observable<{ success: boolean; data: VesselVisitExecution }> {
    return this.http.get<{ success: boolean; data: VesselVisitExecution }>(
      `${this.apiUrl}/${id}`
    );
  }

  /**
   * US 4.1.8: Record berthing time and dock assignment
   * @param vveId - The VVE ID to update
   * @param berthTime - Actual berth time (ISO 8601 format)
   * @param dockId - Assigned dock ID
   * @returns Observable with VVE update response and possible warnings
   */
  recordBerthing(
    vveId: string,
    berthTime: string,
    dockId: string
  ): Observable<RecordBerthingResponse> {
    const request: RecordBerthingRequest = {
      berthTime,
      dockId
    };

    return this.http.post<RecordBerthingResponse>(
      `${this.apiUrl}/${vveId}/berth`,
      request
    );
  }

  /**
   * US 4.1.9: Update executed operation
   * @param vveId - The VVE ID to update
   * @param operationIndex - Index of the operation to update
   * @param updates - Updates to apply to the operation
   * @returns Observable with VVE update response
   */
  updateExecutedOperation(
    vveId: string,
    operationIndex: number,
    updates: UpdateExecutedOperationRequest
  ): Observable<UpdateExecutedOperationResponse> {
    return this.http.patch<UpdateExecutedOperationResponse>(
      `${this.apiUrl}/${vveId}/operations/${operationIndex}`,
      updates
    );
  }

  /**
   * US 4.1.9: Complete an operation
   * @param vveId - The VVE ID
   * @param operationIndex - Index of the operation to complete
   * @param endTime - End time of the operation (ISO 8601 format)
   * @param containersProcessed - Number of containers processed
   * @returns Observable with VVE update response
   */
  completeOperation(
    vveId: string,
    operationIndex: number,
    endTime: string,
    containersProcessed: number
  ): Observable<UpdateExecutedOperationResponse> {
    return this.updateExecutedOperation(vveId, operationIndex, {
      endTime,
      containersProcessed,
      status: ExecutedOperationStatus.COMPLETED
    });
  }

  /**
   * US 4.1.9: Mark operation as delayed
   * @param vveId - The VVE ID
   * @param operationIndex - Index of the operation to mark as delayed
   * @returns Observable with VVE update response
   */
  markOperationAsDelayed(
    vveId: string,
    operationIndex: number
  ): Observable<UpdateExecutedOperationResponse> {
    return this.updateExecutedOperation(vveId, operationIndex, {
      status: ExecutedOperationStatus.DELAYED
    });
  }

  /**
   * US 4.1.10: List all VVEs (for list component)
   * @returns Observable with list of VVEs
   */
  getAllVVEs(): Observable<{ success: boolean; data: VesselVisitExecution[] }> {
    return this.http.get<{ success: boolean; data: VesselVisitExecution[] }>(
      this.apiUrl
    );
  }

  /**
   * US 4.1.11: Complete VVE by recording port departure
   * @param vveId - The VVE ID to complete
   * @param departureTime - Port departure time (ISO 8601 format)
   * @returns Observable with VVE completion response
   */
  completeVVE(
    vveId: string,
    departureTime: string
  ): Observable<{ success: boolean; data: VesselVisitExecution; message?: string }> {
    return this.http.post<{ success: boolean; data: VesselVisitExecution; message?: string }>(
      `${this.apiUrl}/${vveId}/complete`,
      { departureTime }
    );
  }
}
