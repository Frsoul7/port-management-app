import { ConflictType, ConflictSeverity } from '@domain/services/ConflictDetectionService';

/**
 * US 4.1.4: DTO for resource conflict information
 */
export interface ResourceConflictDto {
  type: ConflictType;
  resourceId: string;
  resourceName?: string;
  conflictingPlanId: string;
  conflictingVvnId: string;
  conflictingVesselName?: string;
  timeOverlap: {
    start: string; // ISO 8601
    end: string; // ISO 8601
    durationMinutes: number;
  };
  severity: ConflictSeverity;
  message: string;
}

/**
 * US 4.1.4: DTO for conflict detection result
 */
export interface ConflictDetectionResultDto {
  hasConflicts: boolean;
  hasErrors: boolean;
  hasWarnings: boolean;
  conflicts: ResourceConflictDto[];
  summary: string;
}
