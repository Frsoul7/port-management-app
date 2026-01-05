import { OperationPlanStatus, PlanningAlgorithm, OperationType } from '@shared/types';

/**
 * US 4.1.4: DTO for audit log entry
 */
export interface OperationPlanAuditEntryDto {
  timestamp: string; // ISO 8601
  userId: string;
  userName: string;
  action: 'CREATED' | 'UPDATED' | 'APPROVED' | 'IN_EXECUTION' | 'COMPLETED' | 'DELETED';
  vvnId?: string;
  changes: {
    field: string;
    oldValue: any;
    newValue: any;
  }[];
  reason?: string;
}

/**
 * DTO for PlannedOperation within an OperationPlan
 */
export interface PlannedOperationDto {
  vvnId: string;
  vesselImo: string;
  plannedStart: string; // ISO 8601 date string
  plannedEnd: string; // ISO 8601 date string
  assignedCranes: number;
  operationType: OperationType;
  assignedDock?: string;
  assignedStaff?: string[];
}

/**
 * DTO for creating a new OperationPlan
 */
export interface CreateOperationPlanDto {
  targetDate: string; // ISO 8601 date string (YYYY-MM-DD)
  algorithm: PlanningAlgorithm;
  vvnIds?: string[]; // Optional: specific VVNs to include, otherwise fetches all approved
}

/**
 * DTO for OperationPlan response
 */
export interface OperationPlanDto {
  operationPlanId: string;
  targetDate: string; // ISO 8601 date string
  algorithm: PlanningAlgorithm;
  status: OperationPlanStatus;
  totalDelay: number;
  operations: PlannedOperationDto[];
  createdAt: string; // ISO 8601 date-time string
  createdBy: string;
  approvedAt?: string;
  approvedBy?: string;
  startedAt?: string;
  completedAt?: string;
  auditLog?: OperationPlanAuditEntryDto[]; // US 4.1.4: Change tracking
}

/**
 * DTO for updating dock assignment
 */
export interface UpdateDockAssignmentDto {
  vvnId: string;
  dockId: string;
}

/**
 * DTO for updating staff assignment
 */
export interface UpdateStaffAssignmentDto {
  vvnId: string;
  staffIds: string[];
}

/**
 * DTO for listing operation plans with filters and pagination
 */
export interface ListOperationPlansQuery {
  // Filter parameters
  targetDate?: string; // ISO 8601 date string
  algorithm?: PlanningAlgorithm;
  status?: OperationPlanStatus;
  vvnId?: string;
  vesselImo?: string;
  fromDate?: string; // ISO 8601 date string
  toDate?: string; // ISO 8601 date string
  
  // Pagination parameters
  page?: number;
  limit?: number;
  sortBy?: string;
  sortOrder?: 'asc' | 'desc';
}
