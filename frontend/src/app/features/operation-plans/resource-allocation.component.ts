import { Component, inject, signal } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { Router } from '@angular/router';
import { OemService } from '../../core/services/oem.service';

/**
 * US 4.1.6: Query Resource Allocation Component
 *
 * Features:
 * - Query resource allocation by type, ID, and date range
 * - Display total allocated time and operation count
 * - Show resource utilization summary
 */

interface ResourceAllocationData {
  resourceType: string;
  resourceId: string;
  totalAllocatedTime: number; // in minutes
  operationCount: number;
  utilizationRate?: number;
  operations: {
    operationPlanId: string;
    vvnId: string;
    vesselImo: string;
    plannedStart: string;
    plannedEnd: string;
    allocatedTime: number;
  }[];
}

@Component({
  selector: 'app-resource-allocation',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './resource-allocation.component.html',
  styleUrls: ['./resource-allocation.component.scss']
})
export class ResourceAllocationComponent {
  private oemService = inject(OemService);
  private router = inject(Router);

  // Form data
  resourceType = signal<'crane' | 'dock' | 'staff'>('crane');
  resourceId = signal<string>('');
  startDate = signal<string>('');
  endDate = signal<string>('');

  // Results
  loading = signal(false);
  error = signal<string | null>(null);
  results = signal<ResourceAllocationData | null>(null);
  hasSearched = signal(false);

  // Resource type options
  resourceTypes = [
    { value: 'crane', label: 'Crane' },
    { value: 'dock', label: 'Dock' },
    { value: 'staff', label: 'Staff' }
  ] as const;

  /**
   * Query resource allocation
   */
  async queryAllocation(): Promise<void> {
    // Validate inputs
    if (!this.resourceId() || !this.startDate() || !this.endDate()) {
      this.error.set('Please fill in all required fields');
      return;
    }

    // Validate date range
    if (new Date(this.startDate()) > new Date(this.endDate())) {
      this.error.set('Start date must be before or equal to end date');
      return;
    }

    this.loading.set(true);
    this.error.set(null);
    this.results.set(null);

    try {
      const response = await this.oemService.getResourceAllocation({
        resourceType: this.resourceType(),
        resourceId: this.resourceId(),
        startDate: this.startDate(),
        endDate: this.endDate()
      }).toPromise();

      if (response?.success && response.data) {
        this.results.set(response.data);
        this.hasSearched.set(true);
      } else {
        this.error.set('Failed to retrieve resource allocation data');
      }
    } catch (err: any) {
      console.error('Error querying resource allocation:', err);
      this.error.set(err.message || 'An error occurred while querying resource allocation');
    } finally {
      this.loading.set(false);
    }
  }

  /**
   * Reset form and results
   */
  reset(): void {
    this.resourceId.set('');
    this.startDate.set('');
    this.endDate.set('');
    this.results.set(null);
    this.error.set(null);
    this.hasSearched.set(false);
  }

  /**
   * Format duration in minutes to hours and minutes
   */
  formatDuration(minutes: number): string {
    const hours = Math.floor(minutes / 60);
    const mins = minutes % 60;
    if (hours === 0) {
      return `${mins}m`;
    }
    return mins > 0 ? `${hours}h ${mins}m` : `${hours}h`;
  }

  /**
   * Format date for display
   */
  formatDate(date: string): string {
    return new Date(date).toLocaleString('en-US', {
      year: 'numeric',
      month: 'short',
      day: 'numeric',
      hour: '2-digit',
      minute: '2-digit'
    });
  }

  /**
   * Calculate utilization percentage (assuming 24/7 availability)
   */
  calculateUtilization(): number {
    const results = this.results();
    if (!results) return 0;

    const start = new Date(this.startDate());
    const end = new Date(this.endDate());
    const totalPeriodMinutes = (end.getTime() - start.getTime()) / (1000 * 60);
    
    if (totalPeriodMinutes <= 0) return 0;

    return Math.round((results.totalAllocatedTime / totalPeriodMinutes) * 100);
  }

  /**
   * Set date range to current week
   */
  setCurrentWeek(): void {
    const now = new Date();
    const dayOfWeek = now.getDay();
    const monday = new Date(now);
    monday.setDate(now.getDate() - dayOfWeek + (dayOfWeek === 0 ? -6 : 1));
    monday.setHours(0, 0, 0, 0);

    const sunday = new Date(monday);
    sunday.setDate(monday.getDate() + 6);
    sunday.setHours(23, 59, 59, 999);

    this.startDate.set(monday.toISOString().split('T')[0]);
    this.endDate.set(sunday.toISOString().split('T')[0]);
  }

  /**
   * Set date range to current month
   */
  setCurrentMonth(): void {
    const now = new Date();
    const firstDay = new Date(now.getFullYear(), now.getMonth(), 1);
    const lastDay = new Date(now.getFullYear(), now.getMonth() + 1, 0);

    this.startDate.set(firstDay.toISOString().split('T')[0]);
    this.endDate.set(lastDay.toISOString().split('T')[0]);
  }

  /**
   * Navigate back to operation plans list
   */
  goBack(): void {
    this.router.navigate(['/operation-plans']);
  }
}
