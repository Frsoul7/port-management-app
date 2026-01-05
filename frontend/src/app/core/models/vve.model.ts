import { OperationType } from './operation-plan.model';

/**
 * US 4.1.7: VVE Status Enum (matches backend)
 */
export enum VesselVisitExecutionStatus {
  PLANNED = 'PLANNED',
  IN_PROGRESS = 'IN_PROGRESS',
  COMPLETED = 'COMPLETED',
  DISRUPTED = 'DISRUPTED'
}

/**
 * US 4.1.9: Executed Operation Status (matches backend)
 */
export enum ExecutedOperationStatus {
  STARTED = 'STARTED',
  COMPLETED = 'COMPLETED',
  DELAYED = 'DELAYED'
}

/**
 * US 4.1.9: Executed Operation interface
 * ExecutedOperations derive from PlannedOperations in Operation Plan
 */
export interface ExecutedOperation {
  operationType: OperationType;
  startTime: string;
  endTime?: string;
  containersProcessed: number;
  cranesUsed: number;
  staffAssigned: string[];
  status: ExecutedOperationStatus;
  plannedOperationId?: string; // US 4.1.9: Link to PlannedOperation
}

/**
 * US 4.1.7: Main Vessel Visit Execution interface
 */
export interface VesselVisitExecution {
  vveId: string;
  vvnId: string;
  operationPlanId?: string;
  status: VesselVisitExecutionStatus;
  actualPortArrivalTime?: string;
  actualBerthTime?: string;
  actualUnberthTime?: string;
  unberthTime?: string; // Alias for actualUnberthTime for backward compatibility
  actualPortDepartureTime?: string;
  assignedDock?: string;
  operations: ExecutedOperation[];
  incidents: string[];
  completedAt?: string;
  completedBy?: string;
  createdAt: string;
  createdBy?: string;
  // US 4.1.10: Execution metrics
  turnaroundTimeHours?: number | null;
  berthOccupancyHours?: number | null;
  waitingTimeHours?: number | null;
}

/**
 * US 4.1.7: Initialize VVE Request
 * US 4.1.9: Added vvnId to identify which vessel's operations to derive
 */
export interface InitializeVVERequest {
  operationPlanId: string;
  vvnId: string; // US 4.1.9: VVN ID to filter planned operations
  arrivalTime: string; // ISO 8601 datetime
}

/**
 * US 4.1.7: Initialize VVE Response
 */
export interface InitializeVVEResponse {
  success: boolean;
  message: string;
  data: VesselVisitExecution;
}

/**
 * US 4.1.8: Record Berthing Request
 */
export interface RecordBerthingRequest {
  berthTime: string; // ISO 8601 datetime
  dockId: string;
}

/**
 * US 4.1.8: Record Berthing Response
 */
export interface RecordBerthingResponse {
  success: boolean;
  message: string;
  data: VesselVisitExecution;
  warnings?: string[]; // Dock mismatch warnings
}

/**
 * US 4.1.9: Update Executed Operation Request
 */
export interface UpdateExecutedOperationRequest {
  startTime?: string; // ISO 8601 datetime
  endTime?: string; // ISO 8601 datetime
  containersProcessed?: number;
  cranesUsed?: number;
  staffAssigned?: string[];
  status?: ExecutedOperationStatus;
}

/**
 * US 4.1.9: Update Executed Operation Response
 */
export interface UpdateExecutedOperationResponse {
  success: boolean;
  message: string;
  data: VesselVisitExecution;
}
