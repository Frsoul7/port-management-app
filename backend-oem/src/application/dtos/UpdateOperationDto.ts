/**
 * US 4.1.4: DTO for updating an operation in a plan
 */
export interface UpdateOperationDto {
  vvnId: string;
  updates: {
    plannedStart?: Date;
    plannedEnd?: Date;
    assignedCranes?: number;
    assignedDock?: string;
    assignedStaff?: string[];
  };
  reason: string;
}

/**
 * US 4.1.4: DTO for requesting conflict detection
 */
export interface ConflictDetectionRequestDto {
  vvnId: string;
  updates: {
    plannedStart?: Date;
    plannedEnd?: Date;
    assignedCranes?: number;
    assignedDock?: string;
    assignedStaff?: string[];
  };
}
