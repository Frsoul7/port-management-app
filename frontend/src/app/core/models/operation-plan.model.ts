export interface PlannedOperation {
  vvnId: string;
  vesselImo: string;
  plannedStart: string;
  plannedEnd: string;
  assignedCranes: number;
  operationType: OperationType;
  assignedDock?: string;
  assignedStaff?: string[];
}

/**
 * US 4.1.4: Audit log entry for operation plan changes
 */
export interface OperationPlanAuditEntry {
  timestamp: string;
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

export interface OperationPlan {
  id?: string; // Optional - backend uses operationPlanId as primary identifier
  operationPlanId: string;
  targetDate: string;
  algorithm: PlanningAlgorithm;
  status: OperationPlanStatus;
  totalDelay: number;
  operations: PlannedOperation[];
  createdAt: string;
  createdBy: string;
  updatedAt?: string;
  updatedBy?: string;
  approvedAt?: string;
  approvedBy?: string;
  startedAt?: string;
  completedAt?: string;
  auditLog?: OperationPlanAuditEntry[];
}

export interface GenerateOperationPlanRequest {
  targetDate: string;
  algorithm: PlanningAlgorithm;
}

export interface GenerateOperationPlanResponse {
  success: boolean;
  message: string;
  data: OperationPlan;
}

export interface OperationPlanListResponse {
  success: boolean;
  data: OperationPlan[];
  pagination: {
    page: number;
    limit: number;
    total: number;
    totalPages: number;
  };
}

/**
 * US 4.1.4: Update operation request
 */
export interface UpdateOperationRequest {
  vvnId: string;
  updates: {
    plannedStart?: string;
    plannedEnd?: string;
    assignedCranes?: number;
    assignedDock?: string;
    assignedStaff?: string[];
  };
  reason: string;
}

/**
 * US 4.1.4: Conflict detection request
 */
export interface ConflictDetectionRequest {
  vvnId: string;
  updates: {
    plannedStart?: string;
    plannedEnd?: string;
    assignedCranes?: number;
    assignedDock?: string;
    assignedStaff?: string[];
  };
}

/**
 * US 4.1.4: Resource conflict details
 */
export interface ResourceConflict {
  type: 'CRANE' | 'DOCK' | 'STAFF';
  severity: 'WARNING' | 'ERROR';
  message: string;
  conflictingVvnId: string;
  conflictingVesselImo: string;
  overlap: {
    start: string;
    end: string;
    durationMinutes: number;
  };
  affectedResource?: string;
}

/**
 * US 4.1.4: Conflict detection response
 */
export interface ConflictDetectionResponse {
  success: boolean;
  data: {
    hasConflicts: boolean;
    hasErrors: boolean;
    hasWarnings: boolean;
    conflicts: ResourceConflict[];
    summary: string;
  };
}

/**
 * US 4.1.4: Update operation response
 */
export interface UpdateOperationResponse {
  success: boolean;
  message: string;
  data: OperationPlan;
}

export enum PlanningAlgorithm {
  OPTIMAL = 'optimal',
  WEIGHTED_PRIORITY = 'weighted-priority',
  MULTI_CRANES = 'multi_cranes',
  ARRIVAL_TIME = 'arrival-time',
  DEPARTURE_TIME = 'departure-time',
  SPT = 'spt',
  MST = 'mst'
}

export enum OperationPlanStatus {
  GENERATED = 'GENERATED',
  APPROVED = 'APPROVED',
  REJECTED = 'REJECTED',
  IN_EXECUTION = 'IN_EXECUTION',
  COMPLETED = 'COMPLETED'
}

export enum OperationType {
  LOAD = 'LOAD',
  UNLOAD = 'UNLOAD',
  BOTH = 'BOTH',
  MAINTENANCE = 'MAINTENANCE'
}
