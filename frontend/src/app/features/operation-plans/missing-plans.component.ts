import { Component, OnInit, signal } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { Router } from '@angular/router';
import { OemService } from '../../core/services/oem.service';
import { GenerateOperationPlanRequest, PlanningAlgorithm } from '../../core/models/operation-plan.model';

/**
 * US 4.1.5: Identify Missing Plans Component
 *
 * Features:
 * - Date picker to select target date
 * - Display list of VVNs without operation plans
 * - Trigger regeneration for entire day
 * - Warning dialog before regeneration
 */
@Component({
  selector: 'app-missing-plans',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './missing-plans.component.html',
  styleUrls: ['./missing-plans.component.scss']
})
export class MissingPlansComponent implements OnInit {
  // Data signals
  missingVvns = signal<any[]>([]);
  loading = signal(false);
  error = signal<string | null>(null);
  regenerating = signal(false);

  // Form state
  selectedDate = '';
  selectedAlgorithm: PlanningAlgorithm = PlanningAlgorithm.WEIGHTED_PRIORITY;

  // Confirmation dialog
  showConfirmDialog = false;

  // Algorithm options
  algorithmOptions = [
    { value: PlanningAlgorithm.WEIGHTED_PRIORITY, label: 'Weighted Priority' },
    { value: PlanningAlgorithm.ARRIVAL_TIME, label: 'Arrival Time (FCFS)' },
    { value: PlanningAlgorithm.DEPARTURE_TIME, label: 'Departure Time' },
    { value: PlanningAlgorithm.SPT, label: 'Shortest Processing Time' },
    { value: PlanningAlgorithm.MST, label: 'Most Slack Time' }
  ];

  constructor(
    private oemService: OemService,
    private router: Router
  ) {}

  ngOnInit(): void {
    // Set default date to today
    this.selectedDate = this.getTodayDateString();
  }

  /**
   * Get today's date in YYYY-MM-DD format
   */
  getTodayDateString(): string {
    const today = new Date();
    return today.toISOString().split('T')[0];
  }

  /**
   * Load VVNs without operation plans for the selected date
   */
  async loadMissingPlans(): Promise<void> {
    if (!this.selectedDate) {
      this.error.set('Please select a date');
      return;
    }

    this.loading.set(true);
    this.error.set(null);

    try {
      const response = await this.oemService.getMissingPlans(this.selectedDate).toPromise();
      
      if (response && response.success) {
        this.missingVvns.set(response.data);
        
        if (response.data.length === 0) {
          this.error.set('All VVNs for this date have operation plans.');
        }
      } else {
        this.error.set('Failed to load missing plans');
      }
    } catch (err: any) {
      console.error('Error loading missing plans:', err);
      this.error.set(err.error?.message || 'Failed to load missing plans. Please try again.');
    } finally {
      this.loading.set(false);
    }
  }

  /**
   * Show confirmation dialog before regeneration
   */
  showRegenerateConfirmation(): void {
    if (this.missingVvns().length === 0) {
      this.error.set('No missing plans to regenerate');
      return;
    }

    this.showConfirmDialog = true;
  }

  /**
   * Cancel regeneration
   */
  cancelRegeneration(): void {
    this.showConfirmDialog = false;
  }

  /**
   * Regenerate all operation plans for the selected date
   */
  async regeneratePlans(): Promise<void> {
    this.showConfirmDialog = false;
    this.regenerating.set(true);
    this.error.set(null);

    try {
      const request: GenerateOperationPlanRequest = {
        targetDate: this.selectedDate,
        algorithm: this.selectedAlgorithm
      };

      const response = await this.oemService.generateOperationPlan(request).toPromise();

      if (response && response.success) {
        // Success - reload missing plans to see updated list
        await this.loadMissingPlans();
        
        // Show success message
        alert(`✅ Operation plans regenerated successfully for ${this.selectedDate} using ${this.getAlgorithmLabel()} algorithm.`);
      } else {
        this.error.set('Failed to regenerate plans');
      }
    } catch (err: any) {
      console.error('Error regenerating plans:', err);
      this.error.set(err.error?.message || 'Failed to regenerate plans. Please try again.');
    } finally {
      this.regenerating.set(false);
    }
  }

  /**
   * Get algorithm label for display
   */
  getAlgorithmLabel(): string {
    const option = this.algorithmOptions.find(opt => opt.value === this.selectedAlgorithm);
    return option?.label || this.selectedAlgorithm;
  }

  /**
   * Format date for display
   */
  formatDate(dateString: string): string {
    if (!dateString) return 'N/A';
    
    try {
      const date = new Date(dateString);
      return date.toLocaleString('en-US', {
        year: 'numeric',
        month: 'short',
        day: 'numeric',
        hour: '2-digit',
        minute: '2-digit'
      });
    } catch {
      return dateString;
    }
  }

  /**
   * Navigate back to operation plans list
   */
  goBack(): void {
    this.router.navigate(['/operation-plans']);
  }
}
