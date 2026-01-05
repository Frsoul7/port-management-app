import { Component, OnInit, signal, inject } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { Router, ActivatedRoute } from '@angular/router';
import { VveService } from '../../core/services/vve.service';
import {
  VesselVisitExecution,
  VesselVisitExecutionStatus,
  ExecutedOperation,
  ExecutedOperationStatus
} from '../../core/models/vve.model';

@Component({
  selector: 'app-manage-vve-operations',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './manage-vve-operations.component.html',
  styleUrls: ['./manage-vve-operations.component.scss']
})
export class ManageVveOperationsComponent implements OnInit {
  private vveService = inject(VveService);
  private router = inject(Router);
  private route = inject(ActivatedRoute);

  // VVE data
  vve = signal<VesselVisitExecution | null>(null);
  operations = signal<ExecutedOperation[]>([]);
  vveId = '';

  // Loading & error states
  loading = signal(false);
  error = signal<string | null>(null);

  // Selected operation for actions
  selectedOperation = signal<{ operation: ExecutedOperation; index: number } | null>(null);

  // Complete operation modal state
  showCompleteModal = signal(false);
  completeForm = {
    endTime: '',
    containersProcessed: 0
  };
  completingOperation = signal(false);
  completeSuccess = signal<string | null>(null);
  completeError = signal<string | null>(null);

  // Complete VVE modal state (US 4.1.11)
  showCompleteVVEModal = signal(false);
  completeVVEForm = {
    departureTime: ''
  };
  completingVVE = signal(false);
  completeVVESuccess = signal<string | null>(null);
  completeVVEError = signal<string | null>(null);

  // Delay modal state
  showDelayModal = signal(false);
  markingDelayed = signal(false);
  delaySuccess = signal<string | null>(null);
  delayError = signal<string | null>(null);

  // Status enums for template
  VesselVisitExecutionStatus = VesselVisitExecutionStatus;
  ExecutedOperationStatus = ExecutedOperationStatus;

  ngOnInit(): void {
    this.vveId = this.route.snapshot.paramMap.get('id') || '';
    if (this.vveId) {
      this.loadVVE();
    } else {
      this.error.set('VVE ID is required');
    }
  }

  /**
   * Load VVE by ID
   */
  async loadVVE(): Promise<void> {
    this.loading.set(true);
    this.error.set(null);

    try {
      const response = await this.vveService.getVVEById(this.vveId).toPromise();

      if (response?.success && response.data) {
        this.vve.set(response.data);
        this.operations.set(response.data.operations);
      } else {
        this.error.set('Failed to load vessel visit execution');
      }
    } catch (err: any) {
      console.error('Error loading VVE:', err);
      this.error.set(err.error?.message || err.message || 'Failed to load vessel visit execution');
    } finally {
      this.loading.set(false);
    }
  }

  /**
   * US 4.1.11: Check if all operations are completed
   */
  areAllOperationsCompleted(): boolean {
    const vve = this.vve();
    if (!vve || !vve.operations || vve.operations.length === 0) {
      return false;
    }
    return vve.operations.every(op => op.status === 'COMPLETED');
  }

  /**
   * Get count of completed operations for display
   */
  getCompletedOperationsCount(): number {
    const vve = this.vve();
    if (!vve || !vve.operations) {
      return 0;
    }
    return vve.operations.filter(op => op.status === 'COMPLETED').length;
  }

  /**
   * US 4.1.11: Check if VVE can be completed
   */
  canCompleteVVE(): boolean {
    const vve = this.vve();
    return (
      vve !== null &&
      vve.status !== 'COMPLETED' &&
      vve.actualUnberthTime !== undefined &&
      vve.actualUnberthTime !== null &&
      this.areAllOperationsCompleted()
    );
  }

  /**
   * Open complete operation modal
   */
  openCompleteModal(operation: ExecutedOperation, index: number): void {
    this.selectedOperation.set({ operation, index });
    // Pre-populate end time with current time
    this.completeForm.endTime = this.toDateTimeLocal(new Date().toISOString());
    this.completeForm.containersProcessed = operation.containersProcessed || 0;
    this.completeSuccess.set(null);
    this.completeError.set(null);
    this.showCompleteModal.set(true);
  }

  /**
   * Close complete operation modal
   */
  closeCompleteModal(): void {
    this.showCompleteModal.set(false);
    this.selectedOperation.set(null);
    this.completeForm = {
      endTime: '',
      containersProcessed: 0
    };
    this.completeSuccess.set(null);
    this.completeError.set(null);
  }

  /**
   * Validate complete operation form
   */
  isCompleteFormValid(): boolean {
    const selected = this.selectedOperation();
    if (!selected || !this.completeForm.endTime || this.completeForm.containersProcessed < 0) {
      return false;
    }

    // End time must be after start time
    const startTime = new Date(selected.operation.startTime);
    const endTime = new Date(this.completeForm.endTime);
    if (endTime <= startTime) {
      return false;
    }

    return true;
  }

  /**
   * Submit complete operation
   */
  async submitComplete(): Promise<void> {
    const selected = this.selectedOperation();
    if (!selected || !this.isCompleteFormValid()) {
      this.completeError.set('Please fill all required fields correctly');
      return;
    }

    this.completingOperation.set(true);
    this.completeError.set(null);
    this.completeSuccess.set(null);

    try {
      // Convert datetime-local to ISO 8601
      const endTimeISO = new Date(this.completeForm.endTime).toISOString();

      const response = await this.vveService.completeOperation(
        this.vveId,
        selected.index,
        endTimeISO,
        this.completeForm.containersProcessed
      ).toPromise();

      if (response?.success) {
        this.completeSuccess.set('Operation completed successfully');

        // Auto-close after 1.5 seconds and refresh VVE
        setTimeout(() => {
          this.closeCompleteModal();
          this.loadVVE();
        }, 1500);
      } else {
        this.completeError.set(response?.message || 'Failed to complete operation');
      }
    } catch (err: any) {
      console.error('Error completing operation:', err);
      this.completeError.set(err.error?.message || err.message || 'Failed to complete operation');
    } finally {
      this.completingOperation.set(false);
    }
  }

  /**
   * US 4.1.11: Open complete VVE modal
   */
  openCompleteVVEModal(): void {
    if (!this.canCompleteVVE()) {
      this.completeVVEError.set('Cannot complete VVE: Ensure all operations are completed and vessel has unberthed');
      return;
    }
    this.completeVVEForm.departureTime = this.toDateTimeLocal(new Date().toISOString());
    this.completeVVESuccess.set(null);
    this.completeVVEError.set(null);
    this.showCompleteVVEModal.set(true);
  }

  /**
   * US 4.1.11: Close complete VVE modal
   */
  closeCompleteVVEModal(): void {
    this.showCompleteVVEModal.set(false);
    this.completeVVEForm = {
      departureTime: '',
    };
    this.completeVVESuccess.set(null);
    this.completeVVEError.set(null);
  }

  /**
   * US 4.1.11: Validate complete VVE form
   */
  isCompleteVVEFormValid(): boolean {
    const vve = this.vve();
    if (!vve || !this.completeVVEForm.departureTime) {
      return false;
    }

    // Departure time must be after unberth time
    if (vve.actualUnberthTime) {
      const unberthTime = new Date(vve.actualUnberthTime);
      const departureTime = new Date(this.completeVVEForm.departureTime);
      if (departureTime <= unberthTime) {
        return false;
      }
    }

    return true;
  }

  /**
   * US 4.1.11: Submit complete VVE
   */
  async submitCompleteVVE(): Promise<void> {
    if (!this.isCompleteVVEFormValid()) {
      this.completeVVEError.set('Please provide a valid departure time after unberth time');
      return;
    }

    this.completingVVE.set(true);
    this.completeVVEError.set(null);
    this.completeVVESuccess.set(null);

    try {
      // Convert datetime-local to ISO 8601
      const departureTimeISO = new Date(this.completeVVEForm.departureTime).toISOString();

      const response = await this.vveService.completeVVE(
        this.vveId,
        departureTimeISO
      ).toPromise();

      if (response?.success) {
        this.completeVVESuccess.set('VVE completed successfully! Redirecting...');

        // Auto-close after 2 seconds and navigate back to list
        setTimeout(() => {
          this.router.navigate(['/vessel-visit-executions']);
        }, 2000);
      } else {
        this.completeVVEError.set(response?.message || 'Failed to complete VVE');
      }
    } catch (err: any) {
      console.error('Error completing VVE:', err);
      this.completeVVEError.set(err.error?.message || err.message || 'Failed to complete VVE');
    } finally {
      this.completingVVE.set(false);
    }
  }

  /**
   * Open delay modal
   */
  openDelayModal(operation: ExecutedOperation, index: number): void {
    this.selectedOperation.set({ operation, index });
    this.delaySuccess.set(null);
    this.delayError.set(null);
    this.showDelayModal.set(true);
  }

  /**
   * Close delay modal
   */
  closeDelayModal(): void {
    this.showDelayModal.set(false);
    this.selectedOperation.set(null);
    this.delaySuccess.set(null);
    this.delayError.set(null);
  }

  /**
   * Mark operation as delayed
   */
  async markAsDelayed(): Promise<void> {
    const selected = this.selectedOperation();
    if (!selected) {
      return;
    }

    this.markingDelayed.set(true);
    this.delayError.set(null);
    this.delaySuccess.set(null);

    try {
      const response = await this.vveService.markOperationAsDelayed(
        this.vveId,
        selected.index
      ).toPromise();

      if (response?.success) {
        this.delaySuccess.set('Operation marked as delayed');

        // Auto-close after 1.5 seconds and refresh VVE
        setTimeout(() => {
          this.closeDelayModal();
          this.loadVVE();
        }, 1500);
      } else {
        this.delayError.set(response?.message || 'Failed to mark operation as delayed');
      }
    } catch (err: any) {
      console.error('Error marking operation as delayed:', err);
      this.delayError.set(err.error?.message || err.message || 'Failed to mark operation as delayed');
    } finally {
      this.markingDelayed.set(false);
    }
  }

  /**
   * Get status badge class
   */
  getStatusClass(status: string): string {
    switch (status) {
      case ExecutedOperationStatus.STARTED:
        return 'status-started';
      case ExecutedOperationStatus.COMPLETED:
        return 'status-completed';
      case ExecutedOperationStatus.DELAYED:
        return 'status-delayed';
      case VesselVisitExecutionStatus.IN_PROGRESS:
        return 'status-in-progress';
      case VesselVisitExecutionStatus.COMPLETED:
        return 'status-completed';
      case VesselVisitExecutionStatus.DISRUPTED:
        return 'status-disrupted';
      default:
        return '';
    }
  }

  /**
   * Check if operation can be completed/delayed
   */
  canModifyOperation(operation: ExecutedOperation): boolean {
    return operation.status === ExecutedOperationStatus.STARTED;
  }

  /**
   * Format date for display
   */
  formatDate(dateStr: string | undefined): string {
    if (!dateStr) return 'N/A';
    const date = new Date(dateStr);
    return date.toLocaleString('en-US', {
      year: 'numeric',
      month: 'short',
      day: 'numeric',
      hour: '2-digit',
      minute: '2-digit'
    });
  }

  /**
   * Convert ISO 8601 to datetime-local format
   */
  toDateTimeLocal(isoDate: string): string {
    return isoDate.slice(0, 16);
  }

  /**
   * Navigate back to VVE list
   */
  navigateBack(): void {
    this.router.navigate(['/vessel-visit-executions']);
  }
}
