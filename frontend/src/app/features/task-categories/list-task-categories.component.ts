import { Component, OnInit, signal, inject } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { Router } from '@angular/router';
import { TaskCategoryService } from '../../core/services/task-category.service';
import { TaskCategory } from '../../core/models/task-category.model';
import { TaskCategoryFormComponent } from './task-category-form.component';
import { TranslatePipe } from '../../core/pipes/translate.pipe';

/**
 * US 4.1.14: List and Manage Task Categories
 * Allows Port Operations Supervisors to manage complementary task categories
 */
@Component({
  selector: 'app-list-task-categories',
  standalone: true,
  imports: [CommonModule, FormsModule, TaskCategoryFormComponent, TranslatePipe],
  templateUrl: './list-task-categories.component.html',
  styleUrls: ['./list-task-categories.component.scss']
})
export class ListTaskCategoriesComponent implements OnInit {
  private taskCategoryService = inject(TaskCategoryService);
  private router = inject(Router);

  taskCategories = signal<TaskCategory[]>([]);
  filteredCategories = signal<TaskCategory[]>([]);
  loading = signal(false);
  errorMessage = signal<string | null>(null);
  successMessage = signal<string | null>(null);

  // Modal state
  showModal = signal(false);
  editingCategory = signal<TaskCategory | null>(null);

  // Filter values
  filters = {
    search: '',
    showActiveOnly: true
  };

  ngOnInit(): void {
    this.loadTaskCategories();
  }

  /**
   * Load all task categories
   */
  loadTaskCategories(): void {
    this.loading.set(true);
    this.errorMessage.set(null);

    const options = {
      activeOnly: this.filters.showActiveOnly
    };

    this.taskCategoryService.listTaskCategories(options).subscribe({
      next: (response) => {
        this.taskCategories.set(response.data);
        this.applyFilters();
        this.loading.set(false);
      },
      error: (error) => {
        console.error('Error loading task categories', error);
        this.errorMessage.set('Failed to load task categories');
        this.loading.set(false);
      }
    });
  }

  /**
   * Apply client-side filters
   */
  applyFilters(): void {
    let filtered = this.taskCategories();

    // Search filter
    if (this.filters.search) {
      const search = this.filters.search.toLowerCase();
      filtered = filtered.filter(cat =>
        cat.categoryCode.toLowerCase().includes(search) ||
        cat.categoryName.toLowerCase().includes(search) ||
        cat.description.toLowerCase().includes(search)
      );
    }

    this.filteredCategories.set(filtered);
  }

  /**
   * Clear all filters
   */
  clearFilters(): void {
    this.filters.search = '';
    this.filters.showActiveOnly = true;
    this.loadTaskCategories();
  }

  /**
   * Open modal to create new category
   */
  openCreateModal(): void {
    this.editingCategory.set(null);
    this.showModal.set(true);
  }

  /**
   * Open modal to edit existing category
   */
  openEditModal(category: TaskCategory): void {
    this.editingCategory.set(category);
    this.showModal.set(true);
  }

  /**
   * Close modal
   */
  closeModal(): void {
    this.showModal.set(false);
    this.editingCategory.set(null);
  }

  /**
   * Handle category saved
   */
  onCategorySaved(): void {
    this.closeModal();
    this.loadTaskCategories();
    this.showSuccessMessage('Task category saved successfully');
  }

  /**
   * Deactivate category
   */
  deactivateCategory(category: TaskCategory): void {
    if (!confirm(`Are you sure you want to deactivate "${category.categoryName}"?`)) {
      return;
    }

    this.taskCategoryService.deactivateTaskCategory(category.taskCategoryId).subscribe({
      next: () => {
        this.loadTaskCategories();
        this.showSuccessMessage('Task category deactivated successfully');
      },
      error: (error) => {
        console.error('Error deactivating category', error);
        this.errorMessage.set(error.error?.message || 'Failed to deactivate category');
      }
    });
  }

  /**
   * Reactivate category
   */
  reactivateCategory(category: TaskCategory): void {
    this.taskCategoryService.reactivateTaskCategory(category.taskCategoryId).subscribe({
      next: () => {
        this.loadTaskCategories();
        this.showSuccessMessage('Task category reactivated successfully');
      },
      error: (error) => {
        console.error('Error reactivating category', error);
        this.errorMessage.set(error.error?.message || 'Failed to reactivate category');
      }
    });
  }

  /**
   * Delete category permanently
   */
  deleteCategory(category: TaskCategory): void {
    if (!confirm(`Are you sure you want to permanently delete "${category.categoryName}"? This cannot be undone.`)) {
      return;
    }

    this.taskCategoryService.deleteTaskCategory(category.taskCategoryId).subscribe({
      next: () => {
        this.loadTaskCategories();
        this.showSuccessMessage('Task category deleted successfully');
      },
      error: (error) => {
        console.error('Error deleting category', error);
        this.errorMessage.set(error.error?.message || 'Failed to delete category');
      }
    });
  }

  /**
   * Show success message with auto-dismiss
   */
  private showSuccessMessage(message: string): void {
    this.successMessage.set(message);
    setTimeout(() => {
      this.successMessage.set(null);
    }, 3000);
  }

  /**
   * Format duration for display
   */
  formatDuration(hours?: number): string {
    if (!hours) return 'Not specified';
    if (hours < 1) return `${Math.round(hours * 60)} min`;
    return `${hours} ${hours === 1 ? 'hour' : 'hours'}`;
  }

  /**
   * Navigate back to dashboard
   */
  goBack(): void {
    this.router.navigate(['/dashboard']);
  }
}
