import { VesselVisitExecutionStatus, VveMetrics } from '@shared/types';
import { ExecutedOperation } from '@domain/value-objects/ExecutedOperation';
import { generateUUID } from '@shared/utils';

/**
 * VesselVisitExecution (VVE) Aggregate Root
 * Tracks actual execution of vessel visits
 * Records what really happened vs. what was planned
 *
 * Lifecycle:
 * 1. PLANNED → Created when vessel is scheduled
 * 2. IN_PROGRESS → Vessel arrives at port
 * 3. COMPLETED → Vessel departs from port
 * 4. DISRUPTED → Incident affects execution
 */
export class VesselVisitExecution {
  readonly vveId: string;
  readonly vvnId: string; // Reference to VVN in Core Backend
  readonly operationPlanId?: string; // Optional reference to OperationPlan
  private _actualPortArrivalTime?: Date;
  private _actualBerthTime?: Date;
  private _actualUnberthTime?: Date;
  private _actualPortDepartureTime?: Date;
  private _assignedDock?: string;
  private _operations: ExecutedOperation[];
  private _status: VesselVisitExecutionStatus;
  private _completedAt?: Date;
  private _completedBy?: string;
  private _incidents: string[]; // Array of Incident IDs

  constructor(props: {
    vveId?: string;
    vvnId: string;
    operationPlanId?: string;
    actualPortArrivalTime?: Date;
    actualBerthTime?: Date;
    actualUnberthTime?: Date;
    actualPortDepartureTime?: Date;
    assignedDock?: string;
    operations?: ExecutedOperation[];
    status?: VesselVisitExecutionStatus;
    completedAt?: Date;
    completedBy?: string;
    incidents?: string[];
  }) {
    // Validation
    if (!props.vvnId || props.vvnId.trim().length === 0) {
      throw new Error('VVN ID is required');
    }

    // Business rule: Validate timestamp sequence
    if (props.actualBerthTime && props.actualPortArrivalTime) {
      if (props.actualBerthTime <= props.actualPortArrivalTime) {
        throw new Error('Berth time must be after port arrival time');
      }
    }

    if (props.actualUnberthTime && props.actualBerthTime) {
      if (props.actualUnberthTime <= props.actualBerthTime) {
        throw new Error('Unberth time must be after berth time');
      }
    }

    if (props.actualPortDepartureTime && props.actualUnberthTime) {
      if (props.actualPortDepartureTime <= props.actualUnberthTime) {
        throw new Error('Port departure time must be after unberth time');
      }
    }

    // Assign properties
    this.vveId = props.vveId || generateUUID();
    this.vvnId = props.vvnId;
    this.operationPlanId = props.operationPlanId;
    this._actualPortArrivalTime = props.actualPortArrivalTime;
    this._actualBerthTime = props.actualBerthTime;
    this._actualUnberthTime = props.actualUnberthTime;
    this._actualPortDepartureTime = props.actualPortDepartureTime;
    this._assignedDock = props.assignedDock;
    this._operations = props.operations ? [...props.operations] : [];
    this._status = props.status || VesselVisitExecutionStatus.PLANNED;
    this._completedAt = props.completedAt;
    this._completedBy = props.completedBy;
    this._incidents = props.incidents ? [...props.incidents] : [];
  }

  // Getters
  get actualPortArrivalTime(): Date | undefined {
    return this._actualPortArrivalTime;
  }

  get actualBerthTime(): Date | undefined {
    return this._actualBerthTime;
  }

  get actualUnberthTime(): Date | undefined {
    return this._actualUnberthTime;
  }

  get actualPortDepartureTime(): Date | undefined {
    return this._actualPortDepartureTime;
  }

  get assignedDock(): string | undefined {
    return this._assignedDock;
  }

  get operations(): readonly ExecutedOperation[] {
    return [...this._operations]; // Return copy
  }

  get status(): VesselVisitExecutionStatus {
    return this._status;
  }

  get completedAt(): Date | undefined {
    return this._completedAt;
  }

  get completedBy(): string | undefined {
    return this._completedBy;
  }

  get incidents(): readonly string[] {
    return [...this._incidents]; // Return copy
  }

  /**
   * Record vessel arrival at port
   * Business rule: Only PLANNED or IN_PROGRESS VVEs can record arrival
   */
  recordPortArrival(arrivalTime: Date): void {
    if (
      this._status !== VesselVisitExecutionStatus.PLANNED &&
      this._status !== VesselVisitExecutionStatus.IN_PROGRESS
    ) {
      throw new Error(`Cannot record port arrival for VVE with status ${this._status}`);
    }

    this._actualPortArrivalTime = arrivalTime;
    this._status = VesselVisitExecutionStatus.IN_PROGRESS;
  }

  /**
   * Record vessel berthing (docking)
   * Business rule: Must have port arrival time first
   */
  recordBerthing(berthTime: Date, dockCode: string): void {
    if (!this._actualPortArrivalTime) {
      throw new Error('Cannot record berthing without port arrival time');
    }

    if (berthTime <= this._actualPortArrivalTime) {
      throw new Error('Berth time must be after port arrival time');
    }

    if (this._status !== VesselVisitExecutionStatus.IN_PROGRESS) {
      throw new Error(`Cannot record berthing for VVE with status ${this._status}`);
    }

    this._actualBerthTime = berthTime;
    this._assignedDock = dockCode;
  }

  /**
   * Add executed operation
   * Business rule: Can only add operations when IN_PROGRESS
   */
  addExecutedOperation(operation: ExecutedOperation): void {
    if (this._status !== VesselVisitExecutionStatus.IN_PROGRESS) {
      throw new Error(`Cannot add operation for VVE with status ${this._status}`);
    }

    this._operations.push(operation);
  }

  /**
   * Update executed operation (replace existing)
   * Used when operation is completed or status changes
   */
  updateExecutedOperation(index: number, operation: ExecutedOperation): void {
    if (this._status !== VesselVisitExecutionStatus.IN_PROGRESS) {
      throw new Error(`Cannot update operation for VVE with status ${this._status}`);
    }

    if (index < 0 || index >= this._operations.length) {
      throw new Error(`Invalid operation index ${index}`);
    }

    this._operations[index] = operation;
  }

  /**
   * Record vessel unberthing (leaving dock)
   * Business rule: Must have berth time first
   */
  recordUnberthing(unberthTime: Date): void {
    if (!this._actualBerthTime) {
      throw new Error('Cannot record unberthing without berth time');
    }

    if (unberthTime <= this._actualBerthTime) {
      throw new Error('Unberth time must be after berth time');
    }

    if (this._status !== VesselVisitExecutionStatus.IN_PROGRESS) {
      throw new Error(`Cannot record unberthing for VVE with status ${this._status}`);
    }

    this._actualUnberthTime = unberthTime;
  }

  /**
   * Mark VVE as completed
   * Business rule: All operations must be completed, and unberth time required
   */
  markAsCompleted(departureTime: Date, completedBy: string): void {
    if (this._status !== VesselVisitExecutionStatus.IN_PROGRESS) {
      throw new Error(`Cannot complete VVE with status ${this._status}`);
    }

    if (!this._actualUnberthTime) {
      throw new Error('Cannot complete VVE without unberth time');
    }

    if (departureTime <= this._actualUnberthTime) {
      throw new Error('Port departure time must be after unberth time');
    }

    // Check all operations are completed
    const hasIncompleteOperations = this._operations.some((op) => !op.isCompleted());
    if (hasIncompleteOperations) {
      throw new Error('Cannot complete VVE with incomplete operations');
    }

    this._actualPortDepartureTime = departureTime;
    this._status = VesselVisitExecutionStatus.COMPLETED;
    this._completedAt = new Date();
    this._completedBy = completedBy;
  }

  /**
   * Mark VVE as disrupted
   * Business rule: Can mark as disrupted when incidents affect execution
   */
  markAsDisrupted(): void {
    if (this._status === VesselVisitExecutionStatus.COMPLETED) {
      throw new Error('Cannot mark completed VVE as disrupted');
    }

    this._status = VesselVisitExecutionStatus.DISRUPTED;
  }

  /**
   * Resume VVE after disruption
   */
  resumeAfterDisruption(): void {
    if (this._status !== VesselVisitExecutionStatus.DISRUPTED) {
      throw new Error('Can only resume disrupted VVEs');
    }

    this._status = VesselVisitExecutionStatus.IN_PROGRESS;
  }

  /**
   * Link incident to this VVE
   */
  linkIncident(incidentId: string): void {
    if (this._incidents.includes(incidentId)) {
      throw new Error(`Incident ${incidentId} already linked to this VVE`);
    }

    this._incidents.push(incidentId);
  }

  /**
   * Remove incident link
   */
  unlinkIncident(incidentId: string): void {
    const index = this._incidents.indexOf(incidentId);
    if (index === -1) {
      throw new Error(`Incident ${incidentId} not linked to this VVE`);
    }

    this._incidents.splice(index, 1);
  }

  /**
   * Calculate total turnaround time (port arrival to departure)
   * Returns null if not completed
   */
  getTurnaroundTimeHours(): number | null {
    if (!this._actualPortArrivalTime || !this._actualPortDepartureTime) {
      return null;
    }

    const diffMs = this._actualPortDepartureTime.getTime() - this._actualPortArrivalTime.getTime();
    return diffMs / (1000 * 60 * 60);
  }

  /**
   * Calculate berth occupancy time (berth to unberth)
   * Returns null if not available
   */
  getBerthOccupancyHours(): number | null {
    if (!this._actualBerthTime || !this._actualUnberthTime) {
      return null;
    }

    const diffMs = this._actualUnberthTime.getTime() - this._actualBerthTime.getTime();
    return diffMs / (1000 * 60 * 60);
  }

  /**
   * Calculate waiting time for berthing (arrival to berth)
   * Returns null if not available
   */
  getWaitingTimeHours(): number | null {
    if (!this._actualPortArrivalTime || !this._actualBerthTime) {
      return null;
    }

    const diffMs = this._actualBerthTime.getTime() - this._actualPortArrivalTime.getTime();
    return diffMs / (1000 * 60 * 60);
  }

  /**
   * Get total containers processed
   */
  getTotalContainersProcessed(): number {
    return this._operations.reduce((sum, op) => sum + op.containersProcessed, 0);
  }

  /**
   * Check if VVE is editable
   */
  isEditable(): boolean {
    return this._status !== VesselVisitExecutionStatus.COMPLETED;
  }

  /**
   * Check if VVE has incidents
   */
  hasIncidents(): boolean {
    return this._incidents.length > 0;
  }

  /**
   * Calculate execution metrics for this VVE
   * Business logic: Aggregates all operational metrics in one place
   */
  calculateMetrics(): VveMetrics {
    return {
      vveId: this.vveId,
      totalTurnaroundTime: this.getTurnaroundTimeHours(),
      berthOccupancyTime: this.getBerthOccupancyHours(),
      waitingTimeForBerthing: this.getWaitingTimeHours(),
      totalOperationsTime: this.calculateTotalOperationsTime(),
      operationsCount: this._operations.length,
      incidentsCount: this._incidents.length,
      totalContainersProcessed: this.getTotalContainersProcessed(),
      status: this._status,
    };
  }

  /**
   * Calculate total time spent on operations (sum of all operation durations)
   * Returns null if no operations have duration data
   */
  private calculateTotalOperationsTime(): number | null {
    if (this._operations.length === 0) {
      return null;
    }

    const totalHours = this._operations.reduce((sum, op) => {
      const duration = op.getActualDurationHours();
      return duration !== null ? sum + duration : sum;
    }, 0);

    return totalHours > 0 ? totalHours : null;
  }

  /**
   * Convert to plain object for serialization
   */
  toJSON() {
    return {
      vveId: this.vveId,
      vvnId: this.vvnId,
      operationPlanId: this.operationPlanId,
      actualPortArrivalTime: this._actualPortArrivalTime?.toISOString(),
      actualBerthTime: this._actualBerthTime?.toISOString(),
      actualUnberthTime: this._actualUnberthTime?.toISOString(),
      actualPortDepartureTime: this._actualPortDepartureTime?.toISOString(),
      assignedDock: this._assignedDock,
      operations: this._operations.map((op) => op.toJSON()),
      status: this._status,
      completedAt: this._completedAt?.toISOString(),
      completedBy: this._completedBy,
      incidents: this._incidents,
    };
  }
}
