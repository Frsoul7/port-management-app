import { Component, OnInit, signal, inject } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { Router } from '@angular/router';
import { ComplementaryTaskService } from '../../../../core/services/complementary-task.service';
import { TaskCategoryService } from '../../../../core/services/task-category.service';
import { CreateComplementaryTaskRequest } from '../../../../core/models/complementary-task.model';
import { TaskCategory } from '../../../../core/models/task-category.model';
import { TranslatePipe } from '../../../../core/pipes/translate.pipe';

/**
 * US 4.1.15: Create Complementary Task
 * Form to create a new complementary task for a vessel visit
 */
@Component({
  selector: 'app-create-complementary-task',
  standalone: true,
  imports: [CommonModule, FormsModule, TranslatePipe],
  templateUrl: './create-complementary-task.component.html',
  styleUrls: ['./create-complementary-task.component.scss']
})
export class CreateComplementaryTaskComponent implements OnInit {
  private taskService = inject(ComplementaryTaskService);
  private categoryService = inject(TaskCategoryService);
  private router = inject(Router);

  categories = signal<TaskCategory[]>([]);
  loading = signal(false);
  errorMessage = signal<string | null>(null);

  // Form data
  formData: CreateComplementaryTaskRequest = {
    vveId: '',
    taskCategoryId: '',
    title: '',
    description: '',
    dueDate: '',
    estimatedDurationHours: 0,
    assignedTo: '',
    createdBy: 'current-user' // TODO: Get from auth service
  };

  // Validation errors
  validationErrors = {
    vveId: '',
    taskCategoryId: '',
    title: '',
    description: '',
    dueDate: '',
    estimatedDuration: ''
  };

  ngOnInit(): void {
    this.loadTaskCategories();
  }

  /**
   * Load active task categories
   */
  loadTaskCategories(): void {
    this.categoryService.listActiveTaskCategories().subscribe({
      next: (response) => {
        this.categories.set(response.data);
      },
      error: (error) => {
        console.error('Error loading task categories', error);
        this.errorMessage.set('Failed to load task categories');
      }
    });
  }

  /**
   * Validate form
   */
  validateForm(): boolean {
    let isValid = true;
    this.validationErrors = {
      vveId: '',
      taskCategoryId: '',
      title: '',
      description: '',
      dueDate: '',
      estimatedDuration: ''
    };

    // VVE ID validation
    if (!this.formData.vveId || this.formData.vveId.trim() === '') {
      this.validationErrors.vveId = 'VVE ID is required';
      isValid = false;
    } else if (this.formData.vveId.length < 3 || this.formData.vveId.length > 50) {
      this.validationErrors.vveId = 'VVE ID must be between 3 and 50 characters';
      isValid = false;
    }

    // Task Category validation
    if (!this.formData.taskCategoryId || this.formData.taskCategoryId.trim() === '') {
      this.validationErrors.taskCategoryId = 'Task Category is required';
      isValid = false;
    }

    // Title validation
    if (!this.formData.title || this.formData.title.trim() === '') {
      this.validationErrors.title = 'Title is required';
      isValid = false;
    } else if (this.formData.title.length < 5 || this.formData.title.length > 200) {
      this.validationErrors.title = 'Title must be between 5 and 200 characters';
      isValid = false;
    }

    // Description validation
    if (!this.formData.description || this.formData.description.trim() === '') {
      this.validationErrors.description = 'Description is required';
      isValid = false;
    } else if (this.formData.description.length < 10 || this.formData.description.length > 1000) {
      this.validationErrors.description = 'Description must be between 10 and 1000 characters';
      isValid = false;
    }

    // Due Date validation
    if (!this.formData.dueDate) {
      this.validationErrors.dueDate = 'Due Date is required';
      isValid = false;
    } else {
      const dueDate = new Date(this.formData.dueDate);
      const now = new Date();
      if (dueDate < now) {
        this.validationErrors.dueDate = 'Due Date must be in the future';
        isValid = false;
      }
    }

    // Estimated Duration validation
    if (!this.formData.estimatedDurationHours || this.formData.estimatedDurationHours <= 0) {
      this.validationErrors.estimatedDuration = 'Estimated Duration must be greater than 0';
      isValid = false;
    } else if (this.formData.estimatedDurationHours > 24) {
      this.validationErrors.estimatedDuration = 'Estimated Duration cannot exceed 24 hours';
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

    this.taskService.createTask(this.formData).subscribe({
      next: (response) => {
        this.loading.set(false);
        // Navigate to list page with success message
        this.router.navigate(['/complementary-tasks'], {
          state: { successMessage: 'Task created successfully' }
        });
      },
      error: (error) => {
        console.error('Error creating task', error);
        this.errorMessage.set(error.error?.message || 'Failed to create task');
        this.loading.set(false);
      }
    });
  }

  /**
   * Cancel and go back
   */
  cancel(): void {
    if (confirm('Are you sure you want to cancel? All changes will be lost.')) {
      this.router.navigate(['/complementary-tasks']);
    }
  }

  /**
   * Get category info
   */
  getSelectedCategoryInfo(): TaskCategory | undefined {
    return this.categories().find(c => c.taskCategoryId === this.formData.taskCategoryId);
  }
}
