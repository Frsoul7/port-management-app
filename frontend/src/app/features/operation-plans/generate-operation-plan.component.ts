import { Component, inject, signal } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { Router } from '@angular/router';
import { OemService } from '../../core/services/oem.service';
import { LoadingService } from '../../core/services/loading.service';
import { MessageModalService } from '../../core/services/message-modal.service';
import { 
  GenerateOperationPlanRequest, 
  OperationPlan,
  PlanningAlgorithm,
  OperationType
} from '../../core/models/operation-plan.model';

@Component({
  selector: 'app-generate-operation-plan',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './generate-operation-plan.component.html',
  styleUrls: ['./generate-operation-plan.component.scss']
})
export class GenerateOperationPlanComponent {
  private router = inject(Router);
  private oemService = inject(OemService);
  private loadingService = inject(LoadingService);
  private messageModal = inject(MessageModalService);

  // Form data
  targetDate = signal<string>('');
  selectedAlgorithm = signal<PlanningAlgorithm>(PlanningAlgorithm.OPTIMAL);
  
  // Result data
  generatedPlan = signal<OperationPlan | null>(null);
  showResults = signal<boolean>(false);
  isPlanSaved = signal<boolean>(false);
  
  // UI state
  isGenerating = signal<boolean>(false);

  // Enum references for template
  readonly PlanningAlgorithm = PlanningAlgorithm;
  readonly OperationType = OperationType;

  // Algorithm options for dropdown
  readonly algorithmOptions = [
    { value: PlanningAlgorithm.OPTIMAL, label: 'Optimal (SPT)' },
    { value: PlanningAlgorithm.WEIGHTED_PRIORITY, label: 'Weighted Priority' },
    { value: PlanningAlgorithm.MULTI_CRANES, label: 'Multi Cranes' }
  ];

  constructor() {
    // Set default date to today
    const today = new Date().toISOString().split('T')[0];
    this.targetDate.set(today);
  }

  /**
   * Navigate back to dashboard
   */
  goBack(): void {
    this.router.navigate(['/dashboard']);
  }

  /**
   * Generate operation plan for the selected date and algorithm
   */
  async generatePlan(): Promise<void> {
    if (!this.targetDate()) {
      this.messageModal.showError('Validation Error', 'Please select a target date');
      return;
    }

    this.isGenerating.set(true);
    this.loadingService.show();

    const request: GenerateOperationPlanRequest = {
      targetDate: this.targetDate(),
      algorithm: this.selectedAlgorithm()
    };

    try {
      const response = await this.oemService.generateOperationPlan(request).toPromise();
      
      if (response && response.success) {
        this.generatedPlan.set(response.data);
        this.showResults.set(true);
        this.isPlanSaved.set(false); // Plan generated but not yet saved
        // Don't show success message yet - wait for explicit save
      } else {
        this.messageModal.showError('Error', 'Failed to generate operation plan');
      }
    } catch (error: any) {
      console.error('Error generating operation plan:', error);
      const errorMessage = error.error?.message || 'Failed to generate operation plan';
      this.messageModal.showError('Error', errorMessage);
    } finally {
      this.isGenerating.set(false);
      this.loadingService.hide();
    }
  }

  /**
   * Save the generated plan to the OEM module
   */
  async savePlan(): Promise<void> {
    if (!this.generatedPlan()) {
      return;
    }

    // Plan is already saved in backend from generation
    // This is just a confirmation step for the user
    this.isPlanSaved.set(true);
    this.messageModal.showSuccess(
      'Success', 
      `Operation plan ${this.generatedPlan()!.operationPlanId} saved successfully!`
    );
    
    // Navigate back after brief delay
    setTimeout(() => {
      this.router.navigate(['/dashboard']);
    }, 2000);
  }

  /**
   * Discard the generated plan without saving
   */
  discardPlan(): void {
    this.generatedPlan.set(null);
    this.showResults.set(false);
    this.isPlanSaved.set(false);
  }

  /**
   * Reset form and results
   */
  reset(): void {
    this.generatedPlan.set(null);
    this.showResults.set(false);
    this.isPlanSaved.set(false);
    const today = new Date().toISOString().split('T')[0];
    this.targetDate.set(today);
    this.selectedAlgorithm.set(PlanningAlgorithm.OPTIMAL);
  }

  /**
   * Format date for display
   */
  formatDate(dateString: string): string {
    return new Date(dateString).toLocaleString();
  }

  /**
   * Format duration in hours
   */
  formatDuration(start: string, end: string): string {
    const startDate = new Date(start);
    const endDate = new Date(end);
    const durationMs = endDate.getTime() - startDate.getTime();
    const hours = (durationMs / (1000 * 60 * 60)).toFixed(2);
    return `${hours}h`;
  }

  /**
   * Get operation type label
   */
  getOperationTypeLabel(type: OperationType): string {
    switch (type) {
      case OperationType.LOAD:
        return 'Load';
      case OperationType.UNLOAD:
        return 'Unload';
      case OperationType.BOTH:
        return 'Load & Unload';
      case OperationType.MAINTENANCE:
        return 'Maintenance';
      default:
        return type;
    }
  }

  /**
   * Get operation type badge class
   */
  getOperationTypeBadgeClass(type: OperationType): string {
    switch (type) {
      case OperationType.LOAD:
        return 'badge-success';
      case OperationType.UNLOAD:
        return 'badge-warning';
      case OperationType.BOTH:
        return 'badge-info';
      case OperationType.MAINTENANCE:
        return 'badge-secondary';
      default:
        return 'badge-light';
    }
  }
}
