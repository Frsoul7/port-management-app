import { Component, OnInit, signal } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { ActivatedRoute, Router } from '@angular/router';
import { OemService } from '../../core/services/oem.service';
import {
  OperationPlan,
  PlannedOperation,
  ResourceConflict,
  OperationPlanAuditEntry
} from '../../core/models/operation-plan.model';

/**
 * US 4.1.4: Edit Operation Plan Component
 *
 * Features:
 * - View and edit individual operations in a plan
 * - Update time windows, crane assignments, dock, and staff
 * - Real-time conflict detection
 * - Audit log display
 * - Validation before submission
 */
@Component({
  selector: 'app-edit-operation-plan',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './edit-operation-plan.component.html',
  styleUrls: ['./edit-operation-plan.component.scss']
})
export class EditOperationPlanComponent implements OnInit {
  // Data signals
  operationPlan = signal<OperationPlan | null>(null);
  loading = signal(false);
  saving = signal(false);
  error = signal<string | null>(null);
  successMessage = signal<string | null>(null);

  // Editing state
  selectedOperation = signal<PlannedOperation | null>(null);
  editedOperation: Partial<PlannedOperation> | null = null;
  updateReason = '';

  // Conflict detection
  conflicts = signal<ResourceConflict[]>([]);
  showConflictModal = signal(false);
  checkingConflicts = signal(false);

  // Audit log
  showAuditLog = signal(false);

  constructor(
    private oemService: OemService,
    private route: ActivatedRoute,
    private router: Router
  ) {}

  ngOnInit(): void {
    const planId = this.route.snapshot.paramMap.get('id');
    if (planId) {
      this.loadOperationPlan(planId);
    } else {
      this.error.set('No operation plan ID provided');
    }
  }

  /**
   * Load operation plan by ID
   */
  async loadOperationPlan(planId: string): Promise<void> {
    this.loading.set(true);
    this.error.set(null);

    try {
      const response = await this.oemService.getOperationPlanById(planId).toPromise();

      if (response?.success && response.data) {
        this.operationPlan.set(response.data);
      } else {
        this.error.set('Failed to load operation plan');
      }
    } catch (err: any) {
      console.error('Error loading operation plan:', err);
      this.error.set(err.message || 'An error occurred while loading the operation plan');
    } finally {
      this.loading.set(false);
    }
  }

  /**
   * Select an operation to edit
   */
  selectOperation(operation: PlannedOperation): void {
    this.selectedOperation.set(operation);
    this.editedOperation = {
      vvnId: operation.vvnId,
      vesselImo: operation.vesselImo,
      plannedStart: this.toDateTimeLocalFormat(operation.plannedStart),
      plannedEnd: this.toDateTimeLocalFormat(operation.plannedEnd),
      assignedCranes: operation.assignedCranes,
      assignedDock: operation.assignedDock,
      assignedStaff: operation.assignedStaff || [],
      operationType: operation.operationType
    };
    this.updateReason = '';
    this.conflicts.set([]);
    this.successMessage.set(null);
  }

  /**
   * Convert ISO 8601 date string to datetime-local input format (yyyy-MM-ddThh:mm)
   */
  private toDateTimeLocalFormat(isoDate: string): string {
    // Remove milliseconds and timezone (Z) to match datetime-local format
    // Input: "2025-12-27T21:55:00.000Z"
    // Output: "2025-12-27T21:55"
    return isoDate.slice(0, 16);
  }

  /**
   * Cancel editing
   */
  cancelEdit(): void {
    this.selectedOperation.set(null);
    this.editedOperation = null;
    this.updateReason = '';
    this.conflicts.set([]);
  }

  /**
   * Check for conflicts before saving
   */
  async checkConflicts(): Promise<void> {
    const plan = this.operationPlan();
    if (!plan || !this.editedOperation) {
      console.log('Cannot check conflicts: missing plan or editedOperation');
      return;
    }

    console.log('Checking conflicts for plan:', plan.operationPlanId);
    this.checkingConflicts.set(true);
    this.conflicts.set([]);
    this.error.set(null);
    this.successMessage.set(null);

    try {
      const updates: any = {};

      if (this.editedOperation.plannedStart) {
        updates.plannedStart = new Date(this.editedOperation.plannedStart);
      }
      if (this.editedOperation.plannedEnd) {
        updates.plannedEnd = new Date(this.editedOperation.plannedEnd);
      }
      if (this.editedOperation.assignedCranes !== undefined) {
        updates.assignedCranes = this.editedOperation.assignedCranes;
      }
      if (this.editedOperation.assignedDock) {
        updates.assignedDock = this.editedOperation.assignedDock;
      }
      if (this.editedOperation.assignedStaff) {
        updates.assignedStaff = this.editedOperation.assignedStaff;
      }

      const response = await this.oemService.detectConflicts(
        plan.operationPlanId,
        this.editedOperation.vvnId!,
        updates
      ).toPromise();

      console.log('Conflict check response:', response);

      if (response?.success && response.data) {
        this.conflicts.set(response.data.conflicts || []);

        console.log('Conflicts found:', response.data.conflicts.length);
        console.log('Has conflicts:', response.data.hasConflicts);

        if (response.data.hasConflicts) {
          this.showConflictModal.set(true);
        } else {
          // No conflicts detected - show success message
          this.successMessage.set('No conflicts detected. Changes can be saved safely.');
          this.error.set(null);
        }
      }
    } catch (err: any) {
      console.error('Error checking conflicts:', err);
      const errorMessage = err?.error?.message || err?.message || 'Failed to check for conflicts';
      this.error.set(`Conflict check failed: ${errorMessage}`);
    } finally {
      this.checkingConflicts.set(false);
    }
  }

  /**
   * Save the edited operation
   */
  async saveOperation(proceedDespiteConflicts: boolean = false): Promise<void> {
    const plan = this.operationPlan();
    if (!plan || !this.editedOperation) return;

    // Validate reason
    if (!this.updateReason.trim()) {
      this.error.set('Please provide a reason for this update');
      return;
    }

    // Check for conflicts first if not already checked
    if (!proceedDespiteConflicts && this.conflicts().length === 0) {
      await this.checkConflicts();

      // If conflicts found, modal will handle the decision
      if (this.conflicts().length > 0) {
        return;
      }
    }

    // If there are ERROR-level conflicts, prevent save
    const hasErrors = this.conflicts().some(c => c.severity === 'ERROR');
    if (hasErrors && !proceedDespiteConflicts) {
      this.error.set('Cannot save: critical conflicts detected. Please resolve them first.');
      return;
    }

    this.saving.set(true);
    this.error.set(null);

    try {
      const updates: any = {};

      if (this.editedOperation.plannedStart) {
        updates.plannedStart = new Date(this.editedOperation.plannedStart);
      }
      if (this.editedOperation.plannedEnd) {
        updates.plannedEnd = new Date(this.editedOperation.plannedEnd);
      }
      if (this.editedOperation.assignedCranes !== undefined) {
        updates.assignedCranes = this.editedOperation.assignedCranes;
      }
      if (this.editedOperation.assignedDock) {
        updates.assignedDock = this.editedOperation.assignedDock;
      }
      if (this.editedOperation.assignedStaff) {
        updates.assignedStaff = this.editedOperation.assignedStaff;
      }

      const response = await this.oemService.updateOperation(
        plan.operationPlanId,
        this.editedOperation.vvnId!,
        updates,
        this.updateReason
      ).toPromise();

      if (response?.success && response.data) {
        this.operationPlan.set(response.data);
        this.successMessage.set('Operation updated successfully');
        this.cancelEdit();
        this.showConflictModal.set(false);
      } else {
        this.error.set(response?.message || 'Failed to update operation');
      }
    } catch (err: any) {
      console.error('Error saving operation:', err);
      this.error.set(err.message || 'An error occurred while saving the operation');
    } finally {
      this.saving.set(false);
    }
  }

  /**
   * Close conflict modal without saving
   */
  closeConflictModal(): void {
    this.showConflictModal.set(false);
  }

  /**
   * Proceed with save despite warnings
   */
  proceedDespiteWarnings(): void {
    this.saveOperation(true);
  }

  /**
   * Toggle audit log display
   */
  toggleAuditLog(): void {
    this.showAuditLog.set(!this.showAuditLog());
  }

  /**
   * Check if plan is editable
   */
  isEditable(): boolean {
    const plan = this.operationPlan();
    return plan?.status === 'GENERATED' || plan?.status === 'APPROVED';
  }

  /**
   * Format date for display
   */
  formatDate(date: string | Date): string {
    return new Date(date).toLocaleString('en-US', {
      year: 'numeric',
      month: 'short',
      day: 'numeric',
      hour: '2-digit',
      minute: '2-digit'
    });
  }

  /**
   * Format date for input field (ISO string without timezone)
   */
  formatDateForInput(date: string | Date): string {
    const d = new Date(date);
    return d.toISOString().slice(0, 16);
  }

  /**
   * Get conflict severity class
   */
  getConflictSeverityClass(severity: 'WARNING' | 'ERROR'): string {
    return severity === 'ERROR' ? 'conflict-error' : 'conflict-warning';
  }

  /**
   * Check if conflicts contain any errors
   */
  hasErrors(): boolean {
    return this.conflicts().some(c => c.severity === 'ERROR');
  }

  /**
   * Check if conflicts contain only warnings
   */
  hasOnlyWarnings(): boolean {
    const currentConflicts = this.conflicts();
    return currentConflicts.length > 0 && currentConflicts.every(c => c.severity === 'WARNING');
  }

  /**
   * Get audit action class
   */
  getAuditActionClass(action: string): string {
    const actionClasses: Record<string, string> = {
      CREATED: 'audit-created',
      UPDATED: 'audit-updated',
      APPROVED: 'audit-approved',
      IN_EXECUTION: 'audit-in-execution',
      COMPLETED: 'audit-completed',
      DELETED: 'audit-deleted'
    };
    return actionClasses[action] || 'audit-default';
  }

  /**
   * Go back to list
   */
  goBack(): void {
    this.router.navigate(['/operation-plans']);
  }
}
