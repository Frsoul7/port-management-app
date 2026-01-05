import { OperationPlanStatus, PlanningAlgorithm } from '@shared/types';
import { PlannedOperation } from '@domain/value-objects/PlannedOperation';
import { generateUUID } from '@shared/utils';

/**
 * Audit log entry for tracking changes to operation plans
 * US 4.1.4: Change tracking and accountability
 */
export interface OperationPlanAuditEntry {
  timestamp: Date;
  userId: string;
  userName: string;
  action: 'CREATED' | 'UPDATED' | 'APPROVED' | 'IN_EXECUTION' | 'COMPLETED' | 'DELETED';
  vvnId?: string; // For operation-specific changes
  changes: {
    field: string;
    oldValue: any;
    newValue: any;
  }[];
  reason?: string;
}

/**
 * OperationPlan Aggregate Root
 * Represents a generated operation plan for vessel visits on a specific date
 *
 * Aggregate root characteristics:
 * - Has unique identity (operationPlanId)
 * - Enforces invariants
 * - Controls access to internal entities/value objects
 * - Entry point for all operations on the aggregate
 */
export class OperationPlan {
  readonly operationPlanId: string;
  readonly targetDate: Date;
  readonly algorithm: PlanningAlgorithm;
  readonly createdBy: string;
  readonly createdAt: Date;
  private _status: OperationPlanStatus;
  readonly totalDelay: number;
  private _operations: PlannedOperation[];
  private _auditLog: OperationPlanAuditEntry[]; // US 4.1.4: Change tracking

  constructor(props: {
    operationPlanId?: string;
    targetDate: Date;
    algorithm: PlanningAlgorithm;
    createdBy: string;
    createdAt?: Date;
    status?: OperationPlanStatus;
    totalDelay: number;
    operations: PlannedOperation[];
    auditLog?: OperationPlanAuditEntry[]; // US 4.1.4
  }) {
    // Validation
    if (!props.targetDate) {
      throw new Error('Target date is required');
    }

    if (!props.algorithm) {
      throw new Error('Algorithm is required');
    }

    if (!props.createdBy || props.createdBy.trim().length === 0) {
      throw new Error('CreatedBy (user ID) is required');
    }

    if (props.totalDelay < 0) {
      throw new Error('Total delay cannot be negative');
    }

    if (!props.operations || props.operations.length === 0) {
      throw new Error('Operation plan must have at least one operation');
    }

    // Business rule: Cannot create plan for past dates (only for new plans, not when loading from DB)
    if (!props.operationPlanId) {
      const today = new Date();
      today.setHours(0, 0, 0, 0);
      const targetDateOnly = new Date(props.targetDate);
      targetDateOnly.setHours(0, 0, 0, 0);

      if (targetDateOnly < today) {
        throw new Error('Cannot create operation plan for past dates');
      }
    }

    // Assign properties
    this.operationPlanId = props.operationPlanId || generateUUID();
    this.targetDate = props.targetDate;
    this.algorithm = props.algorithm;
    this.createdBy = props.createdBy;
    this.createdAt = props.createdAt || new Date();
    this._status = props.status || OperationPlanStatus.GENERATED;
    this.totalDelay = props.totalDelay;
    this._operations = [...props.operations]; // Copy array

    // US 4.1.4: Initialize audit log
    this._auditLog = props.auditLog ? [...props.auditLog] : [];

    // Add creation audit entry if new plan (not loaded from DB)
    if (!props.operationPlanId && this._auditLog.length === 0) {
      this._auditLog.push({
        timestamp: this.createdAt,
        userId: this.createdBy,
        userName: 'System', // Will be updated when user context is available
        action: 'CREATED',
        changes: [],
      });
    }
  }

  // Getters
  get status(): OperationPlanStatus {
    return this._status;
  }

  get operations(): readonly PlannedOperation[] {
    return [...this._operations]; // Return copy to prevent external modification
  }

  get auditLog(): readonly OperationPlanAuditEntry[] {
    return [...this._auditLog]; // US 4.1.4: Return copy to prevent external modification
  }

  /**
   * Approve the operation plan
   * Business rule: Only GENERATED plans can be approved
   */
  approve(): void {
    if (this._status !== OperationPlanStatus.GENERATED) {
      throw new Error(`Cannot approve plan with status ${this._status}`);
    }
    this._status = OperationPlanStatus.APPROVED;
  }

  /**
   * Mark plan as in execution
   * Business rule: Only APPROVED plans can be put in execution
   */
  startExecution(): void {
    if (this._status !== OperationPlanStatus.APPROVED) {
      throw new Error(`Cannot start execution of plan with status ${this._status}`);
    }
    this._status = OperationPlanStatus.IN_EXECUTION;
  }

  /**
   * Mark plan as completed
   * Business rule: Only IN_EXECUTION plans can be completed
   */
  complete(): void {
    if (this._status !== OperationPlanStatus.IN_EXECUTION) {
      throw new Error(`Cannot complete plan with status ${this._status}`);
    }
    this._status = OperationPlanStatus.COMPLETED;
  }

  /**
   * Mark plan as outdated
   * Business rule: Plans can be marked outdated if new plans are generated
   */
  markAsOutdated(): void {
    if (
      this._status === OperationPlanStatus.COMPLETED ||
      this._status === OperationPlanStatus.IN_EXECUTION
    ) {
      throw new Error(`Cannot mark ${this._status} plan as outdated`);
    }
    this._status = OperationPlanStatus.OUTDATED;
  }

  /**
   * Update dock assignment for a specific operation (by VVN ID)
   * Business rule: Can only update GENERATED or APPROVED plans
   */
  updateDockAssignment(vvnId: string, dockCode: string): void {
    if (
      this._status !== OperationPlanStatus.GENERATED &&
      this._status !== OperationPlanStatus.APPROVED
    ) {
      throw new Error(`Cannot update plan with status ${this._status}`);
    }

    const operationIndex = this._operations.findIndex((op) => op.vvnId === vvnId);
    if (operationIndex === -1) {
      throw new Error(`Operation with VVN ID ${vvnId} not found in plan`);
    }

    // Value objects are immutable, so create new one with updated dock
    this._operations[operationIndex] = this._operations[operationIndex]!.withDock(dockCode);
  }

  /**
   * Update staff assignment for a specific operation (by VVN ID)
   * Business rule: Can only update GENERATED or APPROVED plans
   */
  updateStaffAssignment(vvnId: string, staffIds: string[]): void {
    if (
      this._status !== OperationPlanStatus.GENERATED &&
      this._status !== OperationPlanStatus.APPROVED
    ) {
      throw new Error(`Cannot update plan with status ${this._status}`);
    }

    const operationIndex = this._operations.findIndex((op) => op.vvnId === vvnId);
    if (operationIndex === -1) {
      throw new Error(`Operation with VVN ID ${vvnId} not found in plan`);
    }

    // Value objects are immutable, so create new one with updated staff
    this._operations[operationIndex] = this._operations[operationIndex]!.withStaff(staffIds);
  }

  /**
   * US 4.1.4: Update operation with audit logging
   * General update method supporting any combination of fields
   * Business rule: Can only update GENERATED or APPROVED plans
   */
  updateOperation(
    vvnId: string,
    updates: {
      plannedStart?: Date;
      plannedEnd?: Date;
      assignedCranes?: number;
      assignedDock?: string;
      assignedStaff?: string[];
    },
    userId: string,
    userName: string,
    reason: string
  ): void {
    // Check if plan is editable
    if (!this.isEditable()) {
      throw new Error(`Cannot update plan with status ${this._status}`);
    }

    // Validate reason is provided
    if (!reason || reason.trim().length === 0) {
      throw new Error('Reason for change is required');
    }

    // Find operation
    const operationIndex = this._operations.findIndex((op) => op.vvnId === vvnId);
    if (operationIndex === -1) {
      throw new Error(`Operation with VVN ID ${vvnId} not found in plan`);
    }

    const oldOperation = this._operations[operationIndex]!;

    // Validate updates (time window validation is done in withUpdates)
    if (updates.assignedCranes !== undefined && updates.assignedCranes < 1) {
      throw new Error('At least one crane must be assigned');
    }

    // Apply updates using value object's withUpdates method
    const newOperation = oldOperation.withUpdates(updates);
    this._operations[operationIndex] = newOperation;

    // Track changes for audit log
    const changes: { field: string; oldValue: any; newValue: any }[] = [];

    if (updates.plannedStart && updates.plannedStart.getTime() !== oldOperation.plannedStart.getTime()) {
      changes.push({
        field: 'plannedStart',
        oldValue: oldOperation.plannedStart.toISOString(),
        newValue: updates.plannedStart.toISOString(),
      });
    }

    if (updates.plannedEnd && updates.plannedEnd.getTime() !== oldOperation.plannedEnd.getTime()) {
      changes.push({
        field: 'plannedEnd',
        oldValue: oldOperation.plannedEnd.toISOString(),
        newValue: updates.plannedEnd.toISOString(),
      });
    }

    if (updates.assignedCranes !== undefined && updates.assignedCranes !== oldOperation.assignedCranes) {
      changes.push({
        field: 'assignedCranes',
        oldValue: oldOperation.assignedCranes,
        newValue: updates.assignedCranes,
      });
    }

    if (updates.assignedDock && updates.assignedDock !== oldOperation.assignedDock) {
      changes.push({
        field: 'assignedDock',
        oldValue: oldOperation.assignedDock || 'None',
        newValue: updates.assignedDock,
      });
    }

    if (updates.assignedStaff && JSON.stringify(updates.assignedStaff) !== JSON.stringify(oldOperation.assignedStaff)) {
      changes.push({
        field: 'assignedStaff',
        oldValue: oldOperation.assignedStaff || [],
        newValue: updates.assignedStaff,
      });
    }

    // Add audit log entry if there were actual changes
    if (changes.length > 0) {
      this._auditLog.push({
        timestamp: new Date(),
        userId,
        userName,
        action: 'UPDATED',
        vvnId,
        changes,
        reason: reason.trim(),
      });

      // Limit audit log size (keep last 100 entries)
      if (this._auditLog.length > 100) {
        this._auditLog = this._auditLog.slice(-100);
      }
    }
  }

  /**
   * Get operation by VVN ID
   */
  getOperationByVvnId(vvnId: string): PlannedOperation | undefined {
    return this._operations.find((op) => op.vvnId === vvnId);
  }

  /**
   * Get total number of operations
   */
  getOperationCount(): number {
    return this._operations.length;
  }

  /**
   * Get total planned duration (earliest start to latest end)
   */
  getTotalPlannedDuration(): number {
    if (this._operations.length === 0) {
      return 0;
    }

    const startTimes = this._operations.map((op) => op.plannedStart.getTime());
    const endTimes = this._operations.map((op) => op.plannedEnd.getTime());

    const earliestStart = Math.min(...startTimes);
    const latestEnd = Math.max(...endTimes);

    return (latestEnd - earliestStart) / (1000 * 60 * 60); // Convert to hours
  }

  /**
   * Get operations sorted by planned start time
   */
  getOperationsSortedByStartTime(): readonly PlannedOperation[] {
    return [...this._operations].sort(
      (a, b) => a.plannedStart.getTime() - b.plannedStart.getTime()
    );
  }

  /**
   * Check if plan is editable
   */
  isEditable(): boolean {
    return (
      this._status === OperationPlanStatus.GENERATED ||
      this._status === OperationPlanStatus.APPROVED
    );
  }

  /**
   * Check if plan is read-only
   */
  isReadOnly(): boolean {
    return (
      this._status === OperationPlanStatus.COMPLETED ||
      this._status === OperationPlanStatus.OUTDATED
    );
  }

  /**
   * Check if all operations have dock assignments
   */
  allOperationsHaveDocks(): boolean {
    return this._operations.every((op) => op.hasDockAssigned());
  }

  /**
   * Check if all operations have staff assignments
   */
  allOperationsHaveStaff(): boolean {
    return this._operations.every((op) => op.hasStaffAssigned());
  }

  /**
   * Convert to plain object for serialization
   */
  toJSON() {
    return {
      operationPlanId: this.operationPlanId,
      targetDate: this.targetDate.toISOString(),
      algorithm: this.algorithm,
      createdBy: this.createdBy,
      createdAt: this.createdAt.toISOString(),
      status: this._status,
      totalDelay: this.totalDelay,
      operations: this._operations.map((op) => op.toJSON()),
    };
  }
}
