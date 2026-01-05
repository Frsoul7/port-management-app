import { ExecutedOperationStatus, OperationType } from '@shared/types';

/**
 * ExecutedOperation Value Object
 * Represents an executed operation within a VesselVisitExecution
 *
 * Value object characteristics:
 * - Immutable (no setters)
 * - Compared by value, not identity
 * - No separate lifecycle from parent aggregate (VVE)
 */
export class ExecutedOperation {
  readonly operationType: OperationType;
  readonly startTime: Date;
  readonly endTime?: Date;
  readonly containersProcessed: number;
  readonly cranesUsed: number;
  readonly staffAssigned: string[];
  readonly status: ExecutedOperationStatus;
  readonly plannedOperationId?: string; // US 4.1.9: Link to planned operation

  constructor(props: {
    operationType: OperationType;
    startTime: Date;
    endTime?: Date;
    containersProcessed: number;
    cranesUsed: number;
    staffAssigned: string[];
    status: ExecutedOperationStatus;
    plannedOperationId?: string; // US 4.1.9: Link to planned operation
  }) {
    // Validation
    if (!props.operationType) {
      throw new Error('Operation type is required');
    }

    if (!props.startTime) {
      throw new Error('Start time is required');
    }

    if (props.endTime && props.endTime <= props.startTime) {
      throw new Error('End time must be after start time');
    }

    if (props.containersProcessed < 0) {
      throw new Error('Containers processed cannot be negative');
    }

    if (props.cranesUsed < 1) {
      throw new Error('At least one crane must be used');
    }

    if (!props.status) {
      throw new Error('Status is required');
    }

    // Business rule: COMPLETED status requires end time
    if (props.status === ExecutedOperationStatus.COMPLETED && !props.endTime) {
      throw new Error('Completed operations must have an end time');
    }

    // Business rule: Containers processed should be > 0 for COMPLETED operations
    if (props.status === ExecutedOperationStatus.COMPLETED && props.containersProcessed === 0) {
      throw new Error('Completed operations must have processed at least one container');
    }

    // Assign properties (immutable)
    this.operationType = props.operationType;
    this.startTime = props.startTime;
    this.endTime = props.endTime;
    this.containersProcessed = props.containersProcessed;
    this.cranesUsed = props.cranesUsed;
    this.staffAssigned = [...props.staffAssigned]; // Copy array for immutability
    this.status = props.status;
    this.plannedOperationId = props.plannedOperationId; // US 4.1.9
  }

  /**
   * Calculate actual operation duration in hours
   * Returns null if operation not completed yet
   */
  getActualDurationHours(): number | null {
    if (!this.endTime) {
      return null;
    }
    const diffMs = this.endTime.getTime() - this.startTime.getTime();
    return diffMs / (1000 * 60 * 60);
  }

  /**
   * Calculate elapsed time (hours) since operation started
   * Useful for ongoing operations
   */
  getElapsedHours(): number {
    const now = new Date();
    const diffMs = now.getTime() - this.startTime.getTime();
    return diffMs / (1000 * 60 * 60);
  }

  /**
   * Check if operation is ongoing
   */
  isOngoing(): boolean {
    return this.status === ExecutedOperationStatus.STARTED && !this.endTime;
  }

  /**
   * Check if operation is completed
   */
  isCompleted(): boolean {
    return this.status === ExecutedOperationStatus.COMPLETED && !!this.endTime;
  }

  /**
   * Check if operation is delayed
   */
  isDelayed(): boolean {
    return this.status === ExecutedOperationStatus.DELAYED;
  }

  /**
   * Create a copy marking operation as completed
   * (Value objects are immutable, so we return a new instance)
   */
  complete(endTime: Date, containersProcessed: number): ExecutedOperation {
    if (this.isCompleted()) {
      throw new Error('Operation is already completed');
    }

    return new ExecutedOperation({
      operationType: this.operationType,
      startTime: this.startTime,
      endTime,
      containersProcessed,
      cranesUsed: this.cranesUsed,
      staffAssigned: this.staffAssigned,
      status: ExecutedOperationStatus.COMPLETED,
      plannedOperationId: this.plannedOperationId, // US 4.1.9: Preserve link
    });
  }

  /**
   * Create a copy marking operation as delayed
   */
  markAsDelayed(): ExecutedOperation {
    return new ExecutedOperation({
      operationType: this.operationType,
      startTime: this.startTime,
      endTime: this.endTime,
      containersProcessed: this.containersProcessed,
      cranesUsed: this.cranesUsed,
      staffAssigned: this.staffAssigned,
      status: ExecutedOperationStatus.DELAYED,
      plannedOperationId: this.plannedOperationId, // US 4.1.9: Preserve link
    });
  }

  /**
   * Calculate containers processed per hour (productivity metric)
   */
  getContainersPerHour(): number | null {
    const duration = this.getActualDurationHours();
    if (!duration || duration === 0) {
      return null;
    }
    return this.containersProcessed / duration;
  }

  /**
   * Equality comparison (value-based)
   */
  equals(other: ExecutedOperation): boolean {
    return (
      this.operationType === other.operationType &&
      this.startTime.getTime() === other.startTime.getTime() &&
      this.endTime?.getTime() === other.endTime?.getTime() &&
      this.containersProcessed === other.containersProcessed &&
      this.status === other.status
    );
  }

  /**
   * Convert to plain object for serialization
   */
  toJSON() {
    return {
      operationType: this.operationType,
      startTime: this.startTime.toISOString(),
      endTime: this.endTime?.toISOString(),
      containersProcessed: this.containersProcessed,
      cranesUsed: this.cranesUsed,
      staffAssigned: this.staffAssigned,
      status: this.status,
      plannedOperationId: this.plannedOperationId, // US 4.1.9
    };
  }
}
