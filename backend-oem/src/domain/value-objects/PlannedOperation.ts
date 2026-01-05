import { OperationType } from '@shared/types';

/**
 * PlannedOperation Value Object
 * Represents a planned operation within an OperationPlan
 *
 * Value object characteristics:
 * - Immutable (no setters)
 * - Compared by value, not identity
 * - No separate lifecycle from parent aggregate
 */
export class PlannedOperation {
  readonly vvnId: string;
  readonly vesselImo: string;
  readonly plannedStart: Date;
  readonly plannedEnd: Date;
  readonly assignedCranes: number;
  readonly assignedDock?: string;
  readonly assignedStaff?: string[];
  readonly operationType: OperationType;

  constructor(props: {
    vvnId: string;
    vesselImo: string;
    plannedStart: Date;
    plannedEnd: Date;
    assignedCranes: number;
    assignedDock?: string;
    assignedStaff?: string[];
    operationType: OperationType;
  }) {
    // Validation
    if (!props.vvnId || props.vvnId.trim().length === 0) {
      throw new Error('VVN ID is required');
    }

    if (!props.vesselImo || props.vesselImo.trim().length === 0) {
      throw new Error('Vessel IMO is required');
    }

    if (!props.plannedStart) {
      throw new Error('Planned start time is required');
    }

    if (!props.plannedEnd) {
      throw new Error('Planned end time is required');
    }

    if (props.plannedEnd <= props.plannedStart) {
      throw new Error('Planned end time must be after planned start time');
    }

    if (props.assignedCranes < 1) {
      throw new Error('At least one crane must be assigned');
    }

    if (!props.operationType) {
      throw new Error('Operation type is required');
    }

    // Assign properties (immutable)
    this.vvnId = props.vvnId;
    this.vesselImo = props.vesselImo;
    this.plannedStart = props.plannedStart;
    this.plannedEnd = props.plannedEnd;
    this.assignedCranes = props.assignedCranes;
    this.assignedDock = props.assignedDock;
    this.assignedStaff = props.assignedStaff ? [...props.assignedStaff] : undefined;
    this.operationType = props.operationType;
  }

  /**
   * Calculate operation duration in hours
   */
  getDurationHours(): number {
    const diffMs = this.plannedEnd.getTime() - this.plannedStart.getTime();
    return diffMs / (1000 * 60 * 60);
  }

  /**
   * Check if dock is assigned
   */
  hasDockAssigned(): boolean {
    return this.assignedDock !== undefined && this.assignedDock.length > 0;
  }

  /**
   * Check if staff is assigned
   */
  hasStaffAssigned(): boolean {
    return this.assignedStaff !== undefined && this.assignedStaff.length > 0;
  }

  /**
   * Create a copy with updated dock assignment
   * (Value objects are immutable, so we return a new instance)
   */
  withDock(dockCode: string): PlannedOperation {
    return new PlannedOperation({
      vvnId: this.vvnId,
      vesselImo: this.vesselImo,
      plannedStart: this.plannedStart,
      plannedEnd: this.plannedEnd,
      assignedCranes: this.assignedCranes,
      assignedDock: dockCode,
      assignedStaff: this.assignedStaff,
      operationType: this.operationType,
    });
  }

  /**
   * Create a copy with updated staff assignment
   */
  withStaff(staffIds: string[]): PlannedOperation {
    return new PlannedOperation({
      vvnId: this.vvnId,
      vesselImo: this.vesselImo,
      plannedStart: this.plannedStart,
      plannedEnd: this.plannedEnd,
      assignedCranes: this.assignedCranes,
      assignedDock: this.assignedDock,
      assignedStaff: staffIds,
      operationType: this.operationType,
    });
  }

  /**
   * Create a copy with updated time window
   * US 4.1.4: Support for updating operation timing
   */
  withTimeWindow(plannedStart: Date, plannedEnd: Date): PlannedOperation {
    if (!plannedStart) {
      throw new Error('Planned start time is required');
    }

    if (!plannedEnd) {
      throw new Error('Planned end time is required');
    }

    if (plannedEnd <= plannedStart) {
      throw new Error('Planned end time must be after planned start time');
    }

    return new PlannedOperation({
      vvnId: this.vvnId,
      vesselImo: this.vesselImo,
      plannedStart,
      plannedEnd,
      assignedCranes: this.assignedCranes,
      assignedDock: this.assignedDock,
      assignedStaff: this.assignedStaff,
      operationType: this.operationType,
    });
  }

  /**
   * Create a copy with updated crane assignment
   * US 4.1.4: Support for updating crane count
   */
  withCranes(craneCount: number): PlannedOperation {
    if (craneCount < 1) {
      throw new Error('At least one crane must be assigned');
    }

    return new PlannedOperation({
      vvnId: this.vvnId,
      vesselImo: this.vesselImo,
      plannedStart: this.plannedStart,
      plannedEnd: this.plannedEnd,
      assignedCranes: craneCount,
      assignedDock: this.assignedDock,
      assignedStaff: this.assignedStaff,
      operationType: this.operationType,
    });
  }

  /**
   * Create a copy with multiple fields updated
   * US 4.1.4: General update method for any combination of fields
   */
  withUpdates(updates: {
    plannedStart?: Date;
    plannedEnd?: Date;
    assignedCranes?: number;
    assignedDock?: string;
    assignedStaff?: string[];
  }): PlannedOperation {
    const newStart = updates.plannedStart ?? this.plannedStart;
    const newEnd = updates.plannedEnd ?? this.plannedEnd;
    const newCranes = updates.assignedCranes ?? this.assignedCranes;

    // Validate time window if either start or end is being updated
    if (updates.plannedStart || updates.plannedEnd) {
      if (newEnd <= newStart) {
        throw new Error('Planned end time must be after planned start time');
      }
    }

    // Validate crane count if being updated
    if (updates.assignedCranes !== undefined && updates.assignedCranes < 1) {
      throw new Error('At least one crane must be assigned');
    }

    return new PlannedOperation({
      vvnId: this.vvnId,
      vesselImo: this.vesselImo,
      plannedStart: newStart,
      plannedEnd: newEnd,
      assignedCranes: newCranes,
      assignedDock: updates.assignedDock ?? this.assignedDock,
      assignedStaff: updates.assignedStaff ?? this.assignedStaff,
      operationType: this.operationType,
    });
  }

  /**
   * Equality comparison (value-based)
   */
  equals(other: PlannedOperation): boolean {
    return (
      this.vvnId === other.vvnId &&
      this.vesselImo === other.vesselImo &&
      this.plannedStart.getTime() === other.plannedStart.getTime() &&
      this.plannedEnd.getTime() === other.plannedEnd.getTime() &&
      this.assignedCranes === other.assignedCranes &&
      this.operationType === other.operationType
    );
  }

  /**
   * Convert to plain object for serialization
   */
  toJSON() {
    return {
      vvnId: this.vvnId,
      vesselImo: this.vesselImo,
      plannedStart: this.plannedStart.toISOString(),
      plannedEnd: this.plannedEnd.toISOString(),
      assignedCranes: this.assignedCranes,
      assignedDock: this.assignedDock,
      assignedStaff: this.assignedStaff,
      operationType: this.operationType,
    };
  }
}
