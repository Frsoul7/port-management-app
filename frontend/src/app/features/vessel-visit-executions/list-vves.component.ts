import { Component, OnInit, signal, inject } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { Router } from '@angular/router';
import { VveService } from '../../core/services/vve.service';
import { DockService } from '../../core/services/dock.service';
import { Dock } from '../../core/models/dock.model';
import {
  VesselVisitExecution,
  VesselVisitExecutionStatus,
  RecordBerthingResponse
} from '../../core/models/vve.model';
import { TranslatePipe } from '../../core/pipes/translate.pipe';

@Component({
  selector: 'app-list-vves',
  standalone: true,
  imports: [CommonModule, FormsModule, TranslatePipe],
  templateUrl: './list-vves.component.html',
  styleUrls: ['./list-vves.component.scss']
})
export class ListVvesComponent implements OnInit {
  private vveService = inject(VveService);
  private dockService = inject(DockService);
  private router = inject(Router);

  // VVE data
  vves = signal<VesselVisitExecution[]>([]);
  filteredVves = signal<VesselVisitExecution[]>([]);

  // Dock data for dropdown
  availableDocks = signal<Dock[]>([]);

  // Loading & error states
  loading = signal(false);
  error = signal<string | null>(null);

  // Filter state
  filters = {
    vvnId: '',
    vesselImo: '', // US 4.1.10: Add vessel filter
    status: '',
    fromDate: '',
    toDate: ''
  };

  // Pagination
  currentPage = signal(1);
  itemsPerPage = 10;
  totalPages = signal(1);

  // Berthing modal state
  showBerthingModal = signal(false);
  selectedVVE = signal<VesselVisitExecution | null>(null);
  berthTime = ''; // Regular property for ngModel binding
  dockId = ''; // Regular property for ngModel binding
  updatingBerthing = signal(false);
  berthingSuccess = signal<string | null>(null);
  berthingError = signal<string | null>(null);
  berthingWarnings = signal<string[]>([]);

  // Status enum for template
  VesselVisitExecutionStatus = VesselVisitExecutionStatus;

  ngOnInit(): void {
    this.loadVVEs();
    this.loadDocks();
  }

  /**
   * Load all VVEs from API
   */
  async loadVVEs(): Promise<void> {
    this.loading.set(true);
    this.error.set(null);

    try {
      const response = await this.vveService.getAllVVEs().toPromise();

      if (response?.success && response.data) {
        this.vves.set(response.data);
        this.applyFilters();
      } else {
        this.error.set('Failed to load vessel visit executions');
      }
    } catch (err: any) {
      console.error('Error loading VVEs:', err);
      this.error.set(err.error?.message || err.message || 'Failed to load vessel visit executions');
    } finally {
      this.loading.set(false);
    }
  }

  /**
   * Load all docks for dropdown
   */
  async loadDocks(): Promise<void> {
    try {
      const docks = await this.dockService.searchDocks({}).toPromise();
      if (docks) {
        this.availableDocks.set(docks);
      }
    } catch (err: any) {
      console.error('Error loading docks:', err);
      // Don't show error to user - docks are optional for viewing VVEs
    }
  }

  /**
   * Apply filters to VVE list
   */
  applyFilters(): void {
    let filtered = [...this.vves()];

    // Filter by VVN ID
    if (this.filters.vvnId) {
      filtered = filtered.filter(vve =>
        vve.vvnId.toLowerCase().includes(this.filters.vvnId.toLowerCase())
      );
    }

    // US 4.1.10: Filter by vessel IMO
    if (this.filters.vesselImo) {
      filtered = filtered.filter(vve =>
        vve.vvnId.toLowerCase().includes(this.filters.vesselImo.toLowerCase())
      );
    }

    // Filter by status
    if (this.filters.status) {
      filtered = filtered.filter(vve => vve.status === this.filters.status);
    }

    // Filter by date range
    if (this.filters.fromDate) {
      const fromDate = new Date(this.filters.fromDate);
      filtered = filtered.filter(vve => {
        if (!vve.actualPortArrivalTime) return false;
        return new Date(vve.actualPortArrivalTime) >= fromDate;
      });
    }

    if (this.filters.toDate) {
      const toDate = new Date(this.filters.toDate);
      filtered = filtered.filter(vve => {
        if (!vve.actualPortArrivalTime) return false;
        return new Date(vve.actualPortArrivalTime) <= toDate;
      });
    }

    this.filteredVves.set(filtered);
    this.totalPages.set(Math.ceil(filtered.length / this.itemsPerPage));
    this.currentPage.set(1);
  }

  /**
   * Clear all filters
   */
  clearFilters(): void {
    this.filters = {
      vvnId: '',
      vesselImo: '', // US 4.1.10
      status: '',
      fromDate: '',
      toDate: ''
    };
    this.applyFilters();
  }

  /**
   * Get VVEs for current page
   */
  getPaginatedVVEs(): VesselVisitExecution[] {
    const start = (this.currentPage() - 1) * this.itemsPerPage;
    const end = start + this.itemsPerPage;
    return this.filteredVves().slice(start, end);
  }

  /**
   * Navigate to specific page
   */
  goToPage(page: number): void {
    if (page >= 1 && page <= this.totalPages()) {
      this.currentPage.set(page);
    }
  }

  /**
   * Get status badge class
   */
  getStatusClass(status: string): string {
    switch (status) {
      case VesselVisitExecutionStatus.IN_PROGRESS:
        return 'status-in-progress';
      case VesselVisitExecutionStatus.COMPLETED:
        return 'status-completed';
      case VesselVisitExecutionStatus.DISRUPTED:
        return 'status-disrupted';
      case VesselVisitExecutionStatus.PLANNED:
        return 'status-planned';
      default:
        return '';
    }
  }

  /**
   * Check if "Update Berthing" button should be shown
   * Allow updating berth time multiple times while IN_PROGRESS (for corrections)
   */
  shouldShowBerthingButton(vve: VesselVisitExecution): boolean {
    return vve.status === VesselVisitExecutionStatus.IN_PROGRESS;
  }

  /**
   * Open berthing modal for a VVE
   * Pre-populate with existing values if updating
   */
  openBerthingModal(vve: VesselVisitExecution): void {
    this.selectedVVE.set(vve);
    // Pre-populate with existing values if updating
    this.berthTime = vve.actualBerthTime ? this.toDateTimeLocal(vve.actualBerthTime) : '';
    this.dockId = vve.assignedDock || '';
    this.berthingSuccess.set(null);
    this.berthingError.set(null);
    this.berthingWarnings.set([]);
    this.showBerthingModal.set(true);
  }

  /**
   * Close berthing modal
   */
  closeBerthingModal(): void {
    this.showBerthingModal.set(false);
    this.selectedVVE.set(null);
    this.berthTime = '';
    this.dockId = '';
    this.berthingSuccess.set(null);
    this.berthingError.set(null);
    this.berthingWarnings.set([]);
  }

  /**
   * Validate berthing form
   */
  isBerthingFormValid(): boolean {
    const vve = this.selectedVVE();
    if (!vve || !this.berthTime || !this.dockId) {
      return false;
    }

    // Berth time must be after arrival time
    if (vve.actualPortArrivalTime) {
      const arrivalTime = new Date(vve.actualPortArrivalTime);
      const berthTime = new Date(this.berthTime);
      if (berthTime <= arrivalTime) {
        return false;
      }
    }

    return true;
  }

  /**
   * Submit berthing update
   */
  async submitBerthing(): Promise<void> {
    const vve = this.selectedVVE();
    if (!vve || !this.isBerthingFormValid()) {
      this.berthingError.set('Please fill all required fields correctly');
      return;
    }

    this.updatingBerthing.set(true);
    this.berthingError.set(null);
    this.berthingSuccess.set(null);
    this.berthingWarnings.set([]);

    try {
      // Convert datetime-local to ISO 8601
      const berthTimeISO = new Date(this.berthTime).toISOString();

      const response: RecordBerthingResponse | undefined = await this.vveService.recordBerthing(
        vve.vveId,
        berthTimeISO,
        this.dockId
      ).toPromise();

      if (response?.success) {
        this.berthingSuccess.set('Berthing recorded successfully');

        // Display warnings if any
        if (response.warnings && response.warnings.length > 0) {
          this.berthingWarnings.set(response.warnings);
        }

        // Auto-close after 2 seconds and refresh list
        setTimeout(() => {
          this.closeBerthingModal();
          this.loadVVEs();
        }, 2000);
      } else {
        this.berthingError.set(response?.message || 'Failed to record berthing');
      }
    } catch (err: any) {
      console.error('Error recording berthing:', err);
      this.berthingError.set(err.error?.message || err.message || 'Failed to record berthing');
    } finally {
      this.updatingBerthing.set(false);
    }
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
   * Get page numbers for pagination
   */
  getPageNumbers(): number[] {
    const total = this.totalPages();
    const current = this.currentPage();
    const pages: number[] = [];

    if (total <= 7) {
      for (let i = 1; i <= total; i++) {
        pages.push(i);
      }
    } else {
      if (current <= 4) {
        for (let i = 1; i <= 5; i++) pages.push(i);
        pages.push(-1);
        pages.push(total);
      } else if (current >= total - 3) {
        pages.push(1);
        pages.push(-1);
        for (let i = total - 4; i <= total; i++) pages.push(i);
      } else {
        pages.push(1);
        pages.push(-1);
        for (let i = current - 1; i <= current + 1; i++) pages.push(i);
        pages.push(-1);
        pages.push(total);
      }
    }

    return pages;
  }

  /**
   * US 4.1.10: Format metrics hours for display
   */
  formatMetric(hours: number | null | undefined): string {
    if (hours === null || hours === undefined) {
      return 'N/A';
    }
    return `${hours.toFixed(1)} hrs`;
  }

  /**
   * US 4.1.10: Check if metrics are available
   */
  hasMetrics(vve: VesselVisitExecution): boolean {
    return vve.turnaroundTimeHours !== null ||
           vve.berthOccupancyHours !== null ||
           vve.waitingTimeHours !== null;
  }

  /**
   * US 4.1.9: Navigate to manage operations page
   */
  navigateToOperations(vveId: string): void {
    this.router.navigate(['/vessel-visit-executions', vveId, 'operations']);
  }

  /**
   * Navigate back to dashboard
   */
  goBack(): void {
    this.router.navigate(['/dashboard']);
  }
}
