import { Component, OnInit, signal, inject } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { Router, RouterLink } from '@angular/router';
import { ComplementaryTaskService } from '../../../../core/services/complementary-task.service';
import { TaskCategoryService } from '../../../../core/services/task-category.service';
import { ComplementaryTask, ComplementaryTaskStatus } from '../../../../core/models/complementary-task.model';
import { TaskCategory } from '../../../../core/models/task-category.model';
import { TranslatePipe } from '../../../../core/pipes/translate.pipe';

/**
 * US 4.1.15: List and Manage Complementary Tasks
 * Allows Logistics Operators to view and manage complementary tasks during vessel visits
 */
@Component({
  selector: 'app-list-complementary-tasks',
  standalone: true,
  imports: [CommonModule, FormsModule, RouterLink, TranslatePipe],
  templateUrl: './list-complementary-tasks.component.html',
  styleUrls: ['./list-complementary-tasks.component.scss']
})
export class ListComplementaryTasksComponent implements OnInit {
  private taskService = inject(ComplementaryTaskService);
  private categoryService = inject(TaskCategoryService);
  private router = inject(Router);

  tasks = signal<ComplementaryTask[]>([]);
  filteredTasks = signal<ComplementaryTask[]>([]);
  categories = signal<TaskCategory[]>([]);
  loading = signal(false);
  errorMessage = signal<string | null>(null);
  successMessage = signal<string | null>(null);

  // Filter values
  filters = {
    search: '',
    status: '' as ComplementaryTaskStatus | '',
    vveId: '',
    categoryId: '',
    assignedTo: '',
    fromDate: '',
    toDate: ''
  };

  // Status enum for template
  TaskStatus = ComplementaryTaskStatus;

  // Status options for dropdown
  statusOptions: (ComplementaryTaskStatus | '')[] = [
    '',
    ComplementaryTaskStatus.PLANNED,
    ComplementaryTaskStatus.IN_PROGRESS,
    ComplementaryTaskStatus.COMPLETED,
    ComplementaryTaskStatus.CANCELLED
  ];

  // Pagination
  currentPage = 1;
  pageSize = 10;
  totalItems = 0;

  ngOnInit(): void {
    this.loadTaskCategories();
    this.loadTasks();
  }

  /**
   * Load task categories for dropdown
   */
  loadTaskCategories(): void {
    this.categoryService.listActiveTaskCategories().subscribe({
      next: (response) => {
        this.categories.set(response.data);
      },
      error: (error) => {
        console.error('Error loading task categories', error);
      }
    });
  }

  /**
   * Load complementary tasks with filters
   */
  loadTasks(): void {
    this.loading.set(true);
    this.errorMessage.set(null);

    const filters: any = {
      page: this.currentPage,
      limit: this.pageSize
    };

    if (this.filters.status) filters.status = this.filters.status;
    if (this.filters.vveId) filters.vveId = this.filters.vveId;
    if (this.filters.categoryId) filters.taskCategoryId = this.filters.categoryId;
    if (this.filters.assignedTo) filters.assignedTo = this.filters.assignedTo;
    if (this.filters.fromDate) filters.fromDate = this.filters.fromDate;
    if (this.filters.toDate) filters.toDate = this.filters.toDate;

    this.taskService.listTasks(filters).subscribe({
      next: (response) => {
        // Enrich tasks with category names
        const enrichedTasks = response.data.map(task => {
          const category = this.categories().find(c => c.taskCategoryId === task.taskCategoryId);
          return {
            ...task,
            categoryName: category?.categoryName,
            categoryCode: category?.categoryCode,
            expectedImpact: category?.expectedImpact
          };
        });
        this.tasks.set(enrichedTasks);
        this.applyFilters();
        this.totalItems = response.count;
        this.loading.set(false);
      },
      error: (error) => {
        console.error('Error loading tasks', error);
        this.errorMessage.set('Failed to load complementary tasks');
        this.loading.set(false);
      }
    });
  }

  /**
   * Apply client-side filters
   */
  applyFilters(): void {
    let filtered = this.tasks();

    // Search filter (title, description, VVE ID)
    if (this.filters.search) {
      const search = this.filters.search.toLowerCase();
      filtered = filtered.filter(task =>
        task.title.toLowerCase().includes(search) ||
        task.description.toLowerCase().includes(search) ||
        task.vveId.toLowerCase().includes(search) ||
        (task.assignedTo && task.assignedTo.toLowerCase().includes(search))
      );
    }

    this.filteredTasks.set(filtered);
  }

  /**
   * Clear all filters
   */
  clearFilters(): void {
    this.filters = {
      search: '',
      status: '',
      vveId: '',
      categoryId: '',
      assignedTo: '',
      fromDate: '',
      toDate: ''
    };
    this.currentPage = 1;
    this.loadTasks();
  }

  /**
   * Navigate to create task page
   */
  createTask(): void {
    this.router.navigate(['/complementary-tasks/create']);
  }

  /**
   * Navigate to edit task page
   */
  editTask(task: ComplementaryTask): void {
    this.router.navigate(['/complementary-tasks', task.taskId, 'edit']);
  }

  /**
   * Delete task
   */
  deleteTask(task: ComplementaryTask): void {
    if (!confirm(`Are you sure you want to delete task "${task.title}"?`)) {
      return;
    }

    this.taskService.deleteTask(task.taskId).subscribe({
      next: () => {
        this.successMessage.set('Task deleted successfully');
        this.loadTasks();
        setTimeout(() => this.successMessage.set(null), 3000);
      },
      error: (error) => {
        console.error('Error deleting task', error);
        this.errorMessage.set(error.error?.message || 'Failed to delete task');
      }
    });
  }

  /**
   * Start task
   */
  startTask(task: ComplementaryTask): void {
    this.taskService.startTask(task.taskId).subscribe({
      next: () => {
        this.successMessage.set('Task started successfully');
        this.loadTasks();
        setTimeout(() => this.successMessage.set(null), 3000);
      },
      error: (error) => {
        console.error('Error starting task', error);
        this.errorMessage.set(error.error?.message || 'Failed to start task');
      }
    });
  }

  /**
   * Complete task
   */
  completeTask(task: ComplementaryTask): void {
    const completedBy = prompt('Enter your name:');
    if (!completedBy) return;

    this.taskService.completeTask(task.taskId, { completedBy }).subscribe({
      next: () => {
        this.successMessage.set('Task completed successfully');
        this.loadTasks();
        setTimeout(() => this.successMessage.set(null), 3000);
      },
      error: (error) => {
        console.error('Error completing task', error);
        this.errorMessage.set(error.error?.message || 'Failed to complete task');
      }
    });
  }

  /**
   * Cancel task
   */
  cancelTask(task: ComplementaryTask): void {
    const cancelledBy = prompt('Enter your name:');
    if (!cancelledBy) return;

    const cancellationReason = prompt('Enter cancellation reason:');
    if (!cancellationReason) return;

    this.taskService.cancelTask(task.taskId, { cancelledBy, cancellationReason }).subscribe({
      next: () => {
        this.successMessage.set('Task cancelled successfully');
        this.loadTasks();
        setTimeout(() => this.successMessage.set(null), 3000);
      },
      error: (error) => {
        console.error('Error cancelling task', error);
        this.errorMessage.set(error.error?.message || 'Failed to cancel task');
      }
    });
  }

  /**
   * Check if task is impacting operations
   */
  isImpactingOperations(task: ComplementaryTask): boolean {
    return task.status === ComplementaryTaskStatus.IN_PROGRESS &&
           !!task.expectedImpact?.toLowerCase().includes('suspend');
  }

  /**
   * Get status badge class
   */
  getStatusClass(status: ComplementaryTaskStatus): string {
    switch (status) {
      case ComplementaryTaskStatus.PLANNED:
        return 'badge-planned';
      case ComplementaryTaskStatus.IN_PROGRESS:
        return 'badge-in-progress';
      case ComplementaryTaskStatus.COMPLETED:
        return 'badge-completed';
      case ComplementaryTaskStatus.CANCELLED:
        return 'badge-cancelled';
      default:
        return '';
    }
  }

  /**
   * Format date
   */
  formatDate(dateString: string): string {
    if (!dateString) return '-';
    const date = new Date(dateString);
    return date.toLocaleDateString() + ' ' + date.toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' });
  }

  /**
   * Check if task is overdue
   */
  isOverdue(task: ComplementaryTask): boolean {
    if (!task.dueDate || task.status === ComplementaryTaskStatus.COMPLETED || task.status === ComplementaryTaskStatus.CANCELLED) {
      return false;
    }
    return new Date(task.dueDate) < new Date();
  }

  /**
   * Go back to dashboard
   */
  goBack(): void {
    this.router.navigate(['/dashboard']);
  }

  /**
   * Pagination
   */
  get totalPages(): number {
    return Math.ceil(this.totalItems / this.pageSize);
  }

  previousPage(): void {
    if (this.currentPage > 1) {
      this.currentPage--;
      this.loadTasks();
    }
  }

  nextPage(): void {
    if (this.currentPage < this.totalPages) {
      this.currentPage++;
      this.loadTasks();
    }
  }
}
