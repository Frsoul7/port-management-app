import { OperationPlan } from '@domain/entities/OperationPlan';
import { PlannedOperation } from '@domain/value-objects/PlannedOperation';

/**
 * US 4.1.4: Conflict Detection Service
 * Domain service for detecting resource conflicts between operation plans
 *
 * Detects conflicts in:
 * - Crane assignments (same crane at overlapping times)
 * - Dock assignments (same dock at overlapping times)
 * - Staff assignments (same staff member at overlapping times)
 */

export enum ConflictType {
  CRANE = 'CRANE',
  DOCK = 'DOCK',
  STAFF = 'STAFF',
}

export enum ConflictSeverity {
  WARNING = 'WARNING', // Overlapping but might be acceptable (e.g., staff can multitask)
  ERROR = 'ERROR', // Clear conflict that should be resolved (e.g., same dock for two vessels)
}

export interface ResourceConflict {
  type: ConflictType;
  resourceId: string;
  resourceName?: string;
  conflictingPlanId: string;
  conflictingVvnId: string;
  conflictingVesselName?: string;
  timeOverlap: {
    start: Date;
    end: Date;
    durationMinutes: number;
  };
  severity: ConflictSeverity;
  message: string;
}

export interface ConflictDetectionResult {
  hasConflicts: boolean;
  hasErrors: boolean;
  hasWarnings: boolean;
  conflicts: ResourceConflict[];
  summary: string;
}

export class ConflictDetectionService {
  /**
   * Detect conflicts for a specific operation update
   * @param targetPlan The plan being updated
   * @param targetVvnId The VVN being updated within the plan
   * @param updatedOperation The proposed updated operation
   * @param allPlansForDate All operation plans for the same date
   * @returns Conflict detection result with list of conflicts
   */
  detectConflicts(
    targetPlan: OperationPlan,
    targetVvnId: string,
    updatedOperation: PlannedOperation,
    allPlansForDate: OperationPlan[]
  ): ConflictDetectionResult {
    const conflicts: ResourceConflict[] = [];

    // Check each other plan for the same date
    for (const plan of allPlansForDate) {
      // Skip the target plan itself (we're checking against other plans)
      if (plan.operationPlanId === targetPlan.operationPlanId) {
        // But check OTHER operations within the same plan
        for (const otherOp of plan.operations) {
          if (otherOp.vvnId === targetVvnId) {
            continue; // Skip the operation being updated
          }

          this.checkOperationConflict(
            updatedOperation,
            otherOp,
            plan.operationPlanId,
            conflicts
          );
        }
      } else {
        // Check all operations in other plans
        for (const otherOp of plan.operations) {
          this.checkOperationConflict(
            updatedOperation,
            otherOp,
            plan.operationPlanId,
            conflicts
          );
        }
      }
    }

    const hasErrors = conflicts.some((c) => c.severity === ConflictSeverity.ERROR);
    const hasWarnings = conflicts.some((c) => c.severity === ConflictSeverity.WARNING);

    return {
      hasConflicts: conflicts.length > 0,
      hasErrors,
      hasWarnings,
      conflicts,
      summary: this.generateSummary(conflicts),
    };
  }

  /**
   * Check for conflicts between two operations
   */
  private checkOperationConflict(
    operation: PlannedOperation,
    otherOp: PlannedOperation,
    otherPlanId: string,
    conflicts: ResourceConflict[]
  ): void {
    // Check for time overlap first
    const overlap = this.calculateTimeOverlap(
      operation.plannedStart,
      operation.plannedEnd,
      otherOp.plannedStart,
      otherOp.plannedEnd
    );

    if (!overlap) {
      return; // No time overlap, no conflict possible
    }

    // Check crane conflicts
    // Note: In real system, would check actual crane IDs from a registry
    // For now, we use crane count as a simplified identifier
    if (operation.assignedCranes === otherOp.assignedCranes) {
      conflicts.push({
        type: ConflictType.CRANE,
        resourceId: `CRANE-${operation.assignedCranes}`,
        resourceName: `Crane #${operation.assignedCranes}`,
        conflictingPlanId: otherPlanId,
        conflictingVvnId: otherOp.vvnId,
        conflictingVesselName: otherOp.vesselImo,
        timeOverlap: overlap,
        severity: ConflictSeverity.ERROR,
        message: `Crane #${operation.assignedCranes} is already assigned to ${otherOp.vvnId} (${otherOp.vesselImo}) during this time window`,
      });
    }

    // Check dock conflicts
    if (
      operation.assignedDock &&
      otherOp.assignedDock &&
      operation.assignedDock === otherOp.assignedDock
    ) {
      conflicts.push({
        type: ConflictType.DOCK,
        resourceId: operation.assignedDock,
        resourceName: `Dock ${operation.assignedDock}`,
        conflictingPlanId: otherPlanId,
        conflictingVvnId: otherOp.vvnId,
        conflictingVesselName: otherOp.vesselImo,
        timeOverlap: overlap,
        severity: ConflictSeverity.ERROR,
        message: `Dock ${operation.assignedDock} is already assigned to ${otherOp.vvnId} (${otherOp.vesselImo}) during this time window`,
      });
    }

    // Check staff conflicts
    if (
      operation.assignedStaff &&
      otherOp.assignedStaff &&
      this.hasStaffOverlap(operation.assignedStaff, otherOp.assignedStaff)
    ) {
      const overlappingStaff = this.getOverlappingStaff(
        operation.assignedStaff,
        otherOp.assignedStaff
      );
      conflicts.push({
        type: ConflictType.STAFF,
        resourceId: overlappingStaff.join(', '),
        resourceName: `Staff: ${overlappingStaff.join(', ')}`,
        conflictingPlanId: otherPlanId,
        conflictingVvnId: otherOp.vvnId,
        conflictingVesselName: otherOp.vesselImo,
        timeOverlap: overlap,
        severity: ConflictSeverity.WARNING, // Staff might be able to handle overlapping tasks
        message: `Staff members ${overlappingStaff.join(', ')} are already assigned to ${otherOp.vvnId} (${otherOp.vesselImo}) during this time window`,
      });
    }
  }

  /**
   * Calculate time overlap between two time windows
   * Returns overlap details or null if no overlap
   */
  private calculateTimeOverlap(
    start1: Date,
    end1: Date,
    start2: Date,
    end2: Date
  ): { start: Date; end: Date; durationMinutes: number } | null {
    // Convert to timestamps for comparison
    const start1Time = start1.getTime();
    const end1Time = end1.getTime();
    const start2Time = start2.getTime();
    const end2Time = end2.getTime();

    // Calculate overlap start and end
    const overlapStartTime = Math.max(start1Time, start2Time);
    const overlapEndTime = Math.min(end1Time, end2Time);

    // No overlap if end is before or equal to start
    if (overlapStartTime >= overlapEndTime) {
      return null;
    }

    const durationMs = overlapEndTime - overlapStartTime;
    const durationMinutes = Math.floor(durationMs / (1000 * 60));

    return {
      start: new Date(overlapStartTime),
      end: new Date(overlapEndTime),
      durationMinutes,
    };
  }

  /**
   * Check if two staff arrays have any overlap
   */
  private hasStaffOverlap(staff1: string[], staff2: string[]): boolean {
    return staff1.some((s) => staff2.includes(s));
  }

  /**
   * Get overlapping staff members
   */
  private getOverlappingStaff(staff1: string[], staff2: string[]): string[] {
    return staff1.filter((s) => staff2.includes(s));
  }

  /**
   * Generate human-readable summary of conflicts
   */
  private generateSummary(conflicts: ResourceConflict[]): string {
    if (conflicts.length === 0) {
      return 'No conflicts detected';
    }

    const errorCount = conflicts.filter((c) => c.severity === ConflictSeverity.ERROR).length;
    const warningCount = conflicts.filter((c) => c.severity === ConflictSeverity.WARNING).length;

    let summary = `Found ${conflicts.length} conflict(s)`;

    const parts: string[] = [];
    if (errorCount > 0) {
      parts.push(`${errorCount} error(s)`);
    }
    if (warningCount > 0) {
      parts.push(`${warningCount} warning(s)`);
    }

    if (parts.length > 0) {
      summary += ` (${parts.join(', ')})`;
    }

    return summary;
  }
}
