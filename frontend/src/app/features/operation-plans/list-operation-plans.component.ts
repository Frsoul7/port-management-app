import { Component, OnInit, signal } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { Router, RouterLink } from '@angular/router';
import { OemService } from '../../core/services/oem.service';
import { VveService } from '../../core/services/vve.service';
import { OperationPlan, OperationPlanStatus } from '../../core/models/operation-plan.model';
import { VesselVisitExecution, VesselVisitExecutionStatus } from '../../core/models/vve.model';
import { TranslatePipe } from '../../core/pipes/translate.pipe';

/**
 * US 4.1.3: Search and List Operation Plans Component
 *
 * Features:
 * - Searchable and filterable table showing plan summaries
 * - Date range filtering (fromDate/toDate)
 * - VVN ID filtering (vessel identifier)
 * - Status filtering
 * - Sortable columns (targetDate, status, operations count)
 * - Pagination
 */
@Component({
  selector: 'app-list-operation-plans',
  standalone: true,
  imports: [CommonModule, FormsModule, RouterLink, TranslatePipe],
  templateUrl: './list-operation-plans.component.html',
  styleUrls: ['./list-operation-plans.component.scss']
})
export class ListOperationPlansComponent implements OnInit {
  // Data signals
  operationPlans = signal<OperationPlan[]>([]);
  loading = signal(false);
  error = signal<string | null>(null);

  // US 4.1.7: VVE Creation modal state
  showVVEModal = signal(false);
  selectedPlanForVVE = signal<OperationPlan | null>(null);
  vveArrivalTime = signal<string>('');
  creatingVVE = signal(false);
  vveError = signal<string | null>(null);
  vveSuccess = signal<string | null>(null);

  // Filter state
  filters = {
    vvnId: '',
    status: '' as OperationPlanStatus | '',
    fromDate: '',
    toDate: '',
    searchTerm: ''
  };

  // Sorting state
  sortBy = 'targetDate';
  sortOrder: 'asc' | 'desc' = 'desc';

  // Pagination state
  currentPage = 1;
  pageSize = 10;
  totalItems = 0;
  totalPages = 0;

  // Status options for dropdown
  statusOptions: (OperationPlanStatus | '')[] = [
    '',
    OperationPlanStatus.GENERATED,
    OperationPlanStatus.APPROVED,
    OperationPlanStatus.REJECTED,
    OperationPlanStatus.IN_EXECUTION,
    OperationPlanStatus.COMPLETED
  ];

  // Sortable columns
  sortableColumns = [
    { key: 'targetDate', label: 'Target Date' },
    { key: 'status', label: 'Status' },
    { key: 'algorithm', label: 'Algorithm' },
    { key: 'createdAt', label: 'Created At' }
  ];

  constructor(
    private oemService: OemService,
    private router: Router,
    private vveService: VveService
  ) {}

  ngOnInit(): void {
    this.loadOperationPlans();
  }

  /**
   * Load operation plans with current filters, sorting, and pagination
   */
  async loadOperationPlans(): Promise<void> {
    this.loading.set(true);
    this.error.set(null);

    try {
      const params: any = {
        page: this.currentPage,
        limit: this.pageSize,
        sortBy: this.sortBy,
        sortOrder: this.sortOrder
      };

      // Add filters if present
      if (this.filters.vvnId) params.vvnId = this.filters.vvnId;
      if (this.filters.status) params.status = this.filters.status;
      if (this.filters.fromDate) params.fromDate = this.filters.fromDate;
      if (this.filters.toDate) params.toDate = this.filters.toDate;

      const response = await this.oemService.listOperationPlans(params);

      if (response.success) {
        this.operationPlans.set(response.data || []);
        if (response.pagination) {
          this.totalItems = response.pagination.total;
          this.totalPages = response.pagination.totalPages;
        }
      } else {
        this.error.set('Failed to load operation plans');
      }
    } catch (err: any) {
      console.error('Error loading operation plans:', err);
      this.error.set(err.message || 'An error occurred while loading operation plans');
    } finally {
      this.loading.set(false);
    }
  }

  /**
   * Apply filters and reset to page 1
   */
  applyFilters(): void {
    this.currentPage = 1;
    this.loadOperationPlans();
  }

  /**
   * Clear all filters
   */
  clearFilters(): void {
    this.filters = {
      vvnId: '',
      status: '',
      fromDate: '',
      toDate: '',
      searchTerm: ''
    };
    this.currentPage = 1;
    this.loadOperationPlans();
  }

  /**
   * Sort by column
   */
  sortByColumn(column: string): void {
    if (this.sortBy === column) {
      // Toggle sort order if same column
      this.sortOrder = this.sortOrder === 'asc' ? 'desc' : 'asc';
    } else {
      this.sortBy = column;
      this.sortOrder = 'desc';
    }
    this.loadOperationPlans();
  }

  /**
   * Get sort icon for column
   */
  getSortIcon(column: string): string {
    if (this.sortBy !== column) return '↕️';
    return this.sortOrder === 'asc' ? '↑' : '↓';
  }

  /**
   * Go to specific page
   */
  goToPage(page: number): void {
    if (page < 1 || page > this.totalPages) return;
    this.currentPage = page;
    this.loadOperationPlans();
  }

  /**
   * Get page numbers for pagination
   */
  getPageNumbers(): number[] {
    const pages: number[] = [];
    const maxVisible = 5;
    let startPage = Math.max(1, this.currentPage - Math.floor(maxVisible / 2));
    let endPage = Math.min(this.totalPages, startPage + maxVisible - 1);

    if (endPage - startPage < maxVisible - 1) {
      startPage = Math.max(1, endPage - maxVisible + 1);
    }

    for (let i = startPage; i <= endPage; i++) {
      pages.push(i);
    }

    return pages;
  }

  /**
   * View operation plan details (navigates to edit page for now)
   */
  viewPlan(plan: OperationPlan): void {
    const planId = plan.id || plan.operationPlanId;
    if (!planId) {
      console.error('Plan has no valid ID', plan);
      this.error.set('Unable to view plan: missing ID');
      return;
    }
    // Navigate to edit page since there's no dedicated view page
    this.router.navigate(['/operation-plans', planId, 'edit']);
  }

  /**
   * US 4.1.4: Edit operation plan
   */
  editPlan(plan: OperationPlan): void {
    const planId = plan.id || plan.operationPlanId;
    if (!planId) {
      console.error('Plan has no valid ID', plan);
      this.error.set('Unable to edit plan: missing ID');
      return;
    }
    this.router.navigate(['/operation-plans', planId, 'edit']);
  }

  /**
   * US 4.1.4: Check if plan can be edited
   */
  canEdit(plan: OperationPlan): boolean {
    return plan.status === 'GENERATED' || plan.status === 'APPROVED';
  }

  /**
   * Get status badge class
   */
  getStatusClass(status: OperationPlanStatus): string {
    const statusClasses: Record<OperationPlanStatus, string> = {
      [OperationPlanStatus.GENERATED]: 'status-generated',
      [OperationPlanStatus.APPROVED]: 'status-approved',
      [OperationPlanStatus.REJECTED]: 'status-rejected',
      [OperationPlanStatus.IN_EXECUTION]: 'status-in-execution',
      [OperationPlanStatus.COMPLETED]: 'status-completed'
    };
    return statusClasses[status] || 'status-default';
  }

  /**
   * Format date for display
   */
  formatDate(date: string | Date): string {
    return new Date(date).toLocaleDateString('en-US', {
      year: 'numeric',
      month: 'short',
      day: 'numeric'
    });
  }

  /**
   * Get operations count
   */
  getOperationsCount(plan: OperationPlan): number {
    return plan.operations?.length || 0;
  }

  /**
   * Get total processing time
   */
  getTotalProcessingTime(plan: OperationPlan): string {
    const total = plan.operations?.reduce((sum: number, op: any) => sum + (op.processingTime || 0), 0) || 0;
    const hours = Math.floor(total / 60);
    const minutes = total % 60;
    return `${hours}h ${minutes}m`;
  }

  /**
   * US 4.1.7: Check if plan can initialize VVE
   */
  canInitializeVVE(plan: OperationPlan): boolean {
    // Only APPROVED plans can initialize VVE
    return plan.status === OperationPlanStatus.APPROVED;
  }

  /**
   * US 4.1.7: Open VVE creation modal
   */
  openVVEModal(plan: OperationPlan): void {
    this.selectedPlanForVVE.set(plan);
    // Default to current time
    const now = new Date();
    const localDateTime = now.toISOString().slice(0, 16); // Format for datetime-local input
    this.vveArrivalTime.set(localDateTime);
    this.vveError.set(null);
    this.vveSuccess.set(null);
    this.showVVEModal.set(true);
  }

  /**
   * US 4.1.7: Close VVE modal
   */
  closeVVEModal(): void {
    this.showVVEModal.set(false);
    this.selectedPlanForVVE.set(null);
    this.vveArrivalTime.set('');
    this.vveError.set(null);
    this.vveSuccess.set(null);
  }

  /**
   * US 4.1.7: Initialize VVE from operation plan
   * US 4.1.9: Now requires vvnId to derive operations from planned operations
   */
  async initializeVVE(): Promise<void> {
    const plan = this.selectedPlanForVVE();
    const arrivalTime = this.vveArrivalTime();

    if (!plan || !arrivalTime) {
      this.vveError.set('Operation plan and arrival time are required');
      return;
    }

    // US 4.1.9: Get vvnId from first operation in the plan
    // TODO: In future, allow user to select which vessel if plan has multiple VVNs
    if (!plan.operations || plan.operations.length === 0) {
      this.vveError.set('Operation plan has no operations');
      return;
    }

    const vvnId = plan.operations[0]!.vvnId;

    this.creatingVVE.set(true);
    this.vveError.set(null);

    try {
      // Convert local datetime to ISO 8601
      const arrivalDate = new Date(arrivalTime);
      const isoArrivalTime = arrivalDate.toISOString();

      const response = await this.vveService.initializeVVE(
        plan.operationPlanId,
        vvnId, // US 4.1.9: Include vvnId to filter planned operations
        isoArrivalTime
      ).toPromise();

      if (response?.success) {
        this.vveSuccess.set(
          `VVE ${response.data.vveId} created successfully for VVN ${vvnId} with ${response.data.operations.length} operations derived from plan`
        );

        // Close modal after brief delay
        setTimeout(() => {
          this.closeVVEModal();
          // Refresh the list to update statuses
          this.loadOperationPlans();
        }, 2000);
      } else {
        this.vveError.set('Failed to initialize VVE');
      }
    } catch (err: any) {
      console.error('Error initializing VVE:', err);
      const errorMessage = err.error?.message || err.message || 'An error occurred while initializing VVE';
      this.vveError.set(errorMessage);
    } finally {
      this.creatingVVE.set(false);
    }
  }

  /**
   * Navigate back to dashboard
   */
  goBack(): void {
    this.router.navigate(['/dashboard']);
  }
}
