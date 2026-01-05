import { Component, Input, Output, EventEmitter, OnInit, signal, inject } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { TaskCategoryService } from '../../core/services/task-category.service';
import { TaskCategory } from '../../core/models/task-category.model';

/**
 * US 4.1.14: Task Category Form Component
 * Modal form for creating and editing task categories
 */
@Component({
  selector: 'app-task-category-form',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './task-category-form.component.html',
  styleUrls: ['./task-category-form.component.scss']
})
export class TaskCategoryFormComponent implements OnInit {
  private taskCategoryService = inject(TaskCategoryService);

  @Input() category: TaskCategory | null = null;
  @Output() saved = new EventEmitter<void>();
  @Output() cancelled = new EventEmitter<void>();

  loading = signal(false);
  errorMessage = signal<string | null>(null);

  // Form fields
  formData = {
    categoryName: '',
    description: '',
    defaultDurationHours: undefined as number | undefined,
    expectedImpact: ''
  };

  // Validation errors
  validationErrors = {
    categoryName: '',
    description: '',
    defaultDurationHours: '',
    expectedImpact: ''
  };

  ngOnInit(): void {
    if (this.category) {
      // Editing existing category
      this.formData.categoryName = this.category.categoryName;
      this.formData.description = this.category.description;
      this.formData.defaultDurationHours = this.category.defaultDurationHours;
      this.formData.expectedImpact = this.category.expectedImpact || '';
    }
  }

  /**
   * Validate form
   */
  private validateForm(): boolean {
    let isValid = true;
    this.validationErrors = {
      categoryName: '',
      description: '',
      defaultDurationHours: '',
      expectedImpact: ''
    };

    // Category Name
    if (!this.formData.categoryName.trim()) {
      this.validationErrors.categoryName = 'Category name is required';
      isValid = false;
    } else if (this.formData.categoryName.length > 100) {
      this.validationErrors.categoryName = 'Category name must not exceed 100 characters';
      isValid = false;
    }

    // Description
    if (!this.formData.description.trim()) {
      this.validationErrors.description = 'Description is required';
      isValid = false;
    } else if (this.formData.description.length > 500) {
      this.validationErrors.description = 'Description must not exceed 500 characters';
      isValid = false;
    }

    // Default Duration
    if (this.formData.defaultDurationHours !== undefined) {
      if (this.formData.defaultDurationHours <= 0) {
        this.validationErrors.defaultDurationHours = 'Duration must be greater than 0';
        isValid = false;
      } else if (this.formData.defaultDurationHours > 168) {
        this.validationErrors.defaultDurationHours = 'Duration must not exceed 168 hours (1 week)';
        isValid = false;
      }
    }

    // Expected Impact
    if (this.formData.expectedImpact && this.formData.expectedImpact.length > 200) {
      this.validationErrors.expectedImpact = 'Expected impact must not exceed 200 characters';
      isValid = false;
    }

    return isValid;
  }

  /**
   * Submit form
   */
  onSubmit(): void {
    if (!this.validateForm()) {
      return;
    }

    this.loading.set(true);
    this.errorMessage.set(null);

    const request = {
      categoryName: this.formData.categoryName.trim(),
      description: this.formData.description.trim(),
      defaultDurationHours: this.formData.defaultDurationHours,
      expectedImpact: this.formData.expectedImpact.trim() || undefined
    };

    if (this.category) {
      // Update existing category
      this.taskCategoryService.updateTaskCategory(this.category.taskCategoryId, request).subscribe({
        next: () => {
          this.loading.set(false);
          this.saved.emit();
        },
        error: (error) => {
          console.error('Error updating category', error);
          this.errorMessage.set(error.error?.message || 'Failed to update category');
          this.loading.set(false);
        }
      });
    } else {
      // Create new category
      this.taskCategoryService.createTaskCategory(request).subscribe({
        next: () => {
          this.loading.set(false);
          this.saved.emit();
        },
        error: (error) => {
          console.error('Error creating category', error);
          this.errorMessage.set(error.error?.message || 'Failed to create category');
          this.loading.set(false);
        }
      });
    }
  }

  /**
   * Cancel form
   */
  onCancel(): void {
    this.cancelled.emit();
  }

  /**
   * Get modal title
   */
  get modalTitle(): string {
    return this.category ? 'Edit Task Category' : 'Create New Task Category';
  }

  /**
   * Get submit button text
   */
  get submitButtonText(): string {
    return this.category ? 'Update Category' : 'Create Category';
  }
}
