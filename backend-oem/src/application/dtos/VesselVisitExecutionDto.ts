import { VesselVisitExecutionStatus, OperationType, ExecutedOperationStatus } from '@shared/types';

/**
 * DTO for listing/filtering VesselVisitExecutions with pagination
 */
export interface ListVesselVisitExecutionsQuery {
  // Filter parameters
  vvnId?: string;
  status?: VesselVisitExecutionStatus;
  fromDate?: string;
  toDate?: string;
  // Pagination parameters
  page?: number;
  limit?: number;
  sortBy?: string;
  sortOrder?: 'asc' | 'desc';
}

/**
 * DTO for ExecutedOperation within a VesselVisitExecution
 * US 4.1.9: Added plannedOperationId to link to PlannedOperation
 */
export interface ExecutedOperationDto {
  operationType: OperationType;
  startTime: string; // ISO 8601 date-time string
  endTime?: string; // ISO 8601 date-time string
  containersProcessed: number;
  cranesUsed: number;
  staffAssigned: string[];
  status: ExecutedOperationStatus;
  plannedOperationId?: string; // US 4.1.9: Link to PlannedOperation
}

/**
 * DTO for creating a new VesselVisitExecution
 */
export interface CreateVesselVisitExecutionDto {
  vvnId: string;
}

/**
 * US 4.1.7: DTO for initializing VVE from approved operation plan
 * US 4.1.9: Added vvnId to identify which vessel's operations to derive
 */
export interface InitializeVveFromPlanDto {
  operationPlanId: string;
  vvnId: string; // US 4.1.9: VVN ID to filter planned operations for this specific vessel
  arrivalTime: string; // ISO 8601 date-time string
  createdBy: string; // User ID from JWT token
}

/**
 * DTO for VesselVisitExecution response
 */
export interface VesselVisitExecutionDto {
  vveId: string;
  vvnId: string;
  status: VesselVisitExecutionStatus;
  actualPortArrivalTime?: string; // ISO 8601 date-time string
  actualBerthTime?: string;
  actualUnberthTime?: string;
  actualPortDepartureTime?: string;
  assignedDock?: string;
  operations: ExecutedOperationDto[];
  incidents: string[]; // Array of incident IDs
  completedAt?: string;
  completedBy?: string;
  createdAt: string;
}

/**
 * DTO for recording port arrival
 */
export interface RecordPortArrivalDto {
  arrivalTime: string; // ISO 8601 date-time string
}

/**
 * DTO for recording berthing
 */
export interface RecordBerthingDto {
  berthTime: string; // ISO 8601 date-time string
  dockId: string;
}

/**
 * DTO for recording unberthing
 */
export interface RecordUnberthingDto {
  unberthTime: string; // ISO 8601 date-time string
}

/**
 * DTO for adding an executed operation
 */
export interface AddExecutedOperationDto {
  operationType: OperationType;
  startTime: string; // ISO 8601 date-time string
  cranesUsed: number;
  staffAssigned: string[];
}

/**
 * DTO for completing an operation
 */
export interface CompleteOperationDto {
  operationIndex: number; // Index of the operation in the operations array
  endTime: string; // ISO 8601 date-time string
  containersProcessed: number;
}

/**
 * US 4.1.9: DTO for updating executed operation
 */
export interface UpdateExecutedOperationDto {
  operationIndex: number;
  startTime?: string; // ISO 8601 date-time string
  endTime?: string; // ISO 8601 date-time string
  containersProcessed?: number;
  cranesUsed?: number;
  staffAssigned?: string[];
  status?: ExecutedOperationStatus; // STARTED, COMPLETED, DELAYED
  updatedBy: string; // User ID from JWT token
}

/**
 * DTO for completing VVE
 */
export interface CompleteVveDto {
  departureTime: string; // ISO 8601 date-time string
  completedBy: string;
}

/**
 * DTO for linking/unlinking incident
 */
export interface LinkIncidentDto {
  incidentId: string;
}

/**
 * DTO for VVE metrics response
 */
export interface VveMetricsDto {
  vveId: string;
  totalTurnaroundTime: number | null; // Hours from port arrival to departure
  berthOccupancyTime: number | null; // Hours from berth to unberth
  waitingTimeForBerthing: number | null; // Hours from arrival to berth
  totalOperationsTime: number | null; // Hours spent on operations
  operationsCount: number;
  incidentsCount: number;
  totalContainersProcessed: number;
  status: VesselVisitExecutionStatus;
}
