import { Component, OnInit, signal, inject } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { Router, ActivatedRoute } from '@angular/router';
import { ComplementaryTaskService } from '../../../../core/services/complementary-task.service';
import { TaskCategoryService } from '../../../../core/services/task-category.service';
import { ComplementaryTask, UpdateComplementaryTaskRequest, ComplementaryTaskStatus, TaskNote } from '../../../../core/models/complementary-task.model';
import { TaskCategory } from '../../../../core/models/task-category.model';
import { TranslatePipe } from '../../../../core/pipes/translate.pipe';

/**
 * US 4.1.15: Edit Complementary Task
 * Form to edit an existing complementary task and manage its lifecycle
 */
@Component({
  selector: 'app-edit-complementary-task',
  standalone: true,
  imports: [CommonModule, FormsModule, TranslatePipe],
  templateUrl: './edit-complementary-task.component.html',
  styleUrls: ['./edit-complementary-task.component.scss']
})
export class EditComplementaryTaskComponent implements OnInit {
  private taskService = inject(ComplementaryTaskService);
  private categoryService = inject(TaskCategoryService);
  private router = inject(Router);
  private route = inject(ActivatedRoute);

  task = signal<ComplementaryTask | null>(null);
  categories = signal<TaskCategory[]>([]);
  loading = signal(false);
  errorMessage = signal<string | null>(null);
  successMessage = signal<string | null>(null);

  taskId: string = '';

  // Form data
  formData: UpdateComplementaryTaskRequest = {
    title: '',
    description: '',
    dueDate: '',
    estimatedDurationHours: 0
  };

  // New note
  newNote = {
    content: '',
    author: 'current-user'
  };

  // Status enum for template
  TaskStatus = ComplementaryTaskStatus;

  // Validation errors
  validationErrors = {
    title: '',
    description: '',
    dueDate: '',
    estimatedDuration: '',
    noteContent: '',
    noteAddedBy: ''
  };

  ngOnInit(): void {
    this.taskId = this.route.snapshot.paramMap.get('id') || '';
    if (!this.taskId) {
      this.errorMessage.set('Invalid task ID');
      return;
    }
    this.loadTaskCategories();
    this.loadTask();
  }

  /**
   * Load task categories
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
   * Load task data
   */
  loadTask(): void {
    this.loading.set(true);
    this.taskService.getTaskById(this.taskId).subscribe({
      next: (response) => {
        const task = response.data;
        this.task.set(task);
        
        // Populate form
        this.formData = {
          title: task.title,
          description: task.description,
          dueDate: task.dueDate,
          estimatedDurationHours: task.estimatedDurationHours
        };

        this.loading.set(false);
      },
      error: (error) => {
        console.error('Error loading task', error);
        this.errorMessage.set('Failed to load task');
        this.loading.set(false);
      }
    });
  }

  /**
   * Validate form
   */
  validateForm(): boolean {
    let isValid = true;
    this.validationErrors = {
      title: '',
      description: '',
      dueDate: '',
      estimatedDuration: '',
      noteContent: '',
      noteAddedBy: ''
    };

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
   * Update task
   */
  onSubmit(): void {
    if (!this.validateForm()) {
      return;
    }

    this.loading.set(true);
    this.errorMessage.set(null);

    this.taskService.updateTask(this.taskId, this.formData).subscribe({
      next: () => {
        this.successMessage.set('Task updated successfully');
        this.loadTask();
        setTimeout(() => this.successMessage.set(null), 3000);
      },
      error: (error) => {
        console.error('Error updating task', error);
        this.errorMessage.set(error.error?.message || 'Failed to update task');
        this.loading.set(false);
      }
    });
  }

  /**
   * Assign task
   */
  assignTask(): void {
    const assignedTo = prompt('Assign task to (enter name):');
    if (!assignedTo) return;

    this.taskService.assignTask(this.taskId, { assignedTo }).subscribe({
      next: () => {
        this.successMessage.set('Task assigned successfully');
        this.loadTask();
        setTimeout(() => this.successMessage.set(null), 3000);
      },
      error: (error) => {
        console.error('Error assigning task', error);
        this.errorMessage.set(error.error?.message || 'Failed to assign task');
      }
    });
  }

  /**
   * Start task
   */
  startTask(): void {
    if (!confirm('Start this task?')) return;

    this.taskService.startTask(this.taskId).subscribe({
      next: () => {
        this.successMessage.set('Task started successfully');
        this.loadTask();
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
  completeTask(): void {
    const completedBy = prompt('Enter your name:');
    if (!completedBy) return;

    this.taskService.completeTask(this.taskId, { completedBy }).subscribe({
      next: () => {
        this.successMessage.set('Task completed successfully');
        this.loadTask();
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
  cancelTask(): void {
    const cancelledBy = prompt('Enter your name:');
    if (!cancelledBy) return;

    const cancellationReason = prompt('Enter cancellation reason:');
    if (!cancellationReason) return;

    this.taskService.cancelTask(this.taskId, { cancelledBy, cancellationReason }).subscribe({
      next: () => {
        this.successMessage.set('Task cancelled successfully');
        this.loadTask();
        setTimeout(() => this.successMessage.set(null), 3000);
      },
      error: (error) => {
        console.error('Error cancelling task', error);
        this.errorMessage.set(error.error?.message || 'Failed to cancel task');
      }
    });
  }

  /**
   * Add note
   */
  addNote(): void {
    // Validate note
    if (!this.newNote.content || this.newNote.content.trim() === '') {
      this.validationErrors.noteContent = 'Note content is required';
      return;
    }
    if (this.newNote.content.length < 5 || this.newNote.content.length > 500) {
      this.validationErrors.noteContent = 'Note must be between 5 and 500 characters';
      return;
    }
    if (!this.newNote.author || this.newNote.author.trim() === '') {
      this.validationErrors.noteAddedBy = 'Author name is required';
      return;
    }

    this.taskService.addNote(this.taskId, this.newNote).subscribe({
      next: () => {
        this.successMessage.set('Note added successfully');
        this.newNote = { content: '', author: 'current-user' };
        this.validationErrors.noteContent = '';
        this.validationErrors.noteAddedBy = '';
        this.loadTask();
        setTimeout(() => this.successMessage.set(null), 3000);
      },
      error: (error) => {
        console.error('Error adding note', error);
        this.errorMessage.set(error.error?.message || 'Failed to add note');
      }
    });
  }

  /**
   * Cancel and go back
   */
  cancel(): void {
    this.router.navigate(['/complementary-tasks']);
  }

  /**
   * Get category name
   */
  getCategoryName(): string {
    const task = this.task();
    if (!task) return '-';
    const category = this.categories().find(c => c.taskCategoryId === task.taskCategoryId);
    return category ? `${category.categoryCode} - ${category.categoryName}` : task.taskCategoryId;
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
   * Can edit task (only PLANNED tasks)
   */
  canEdit(): boolean {
    return this.task()?.status === ComplementaryTaskStatus.PLANNED;
  }

  /**
   * Can start task (only PLANNED tasks)
   */
  canStart(): boolean {
    return this.task()?.status === ComplementaryTaskStatus.PLANNED;
  }

  /**
   * Can complete task (only IN_PROGRESS tasks)
   */
  canComplete(): boolean {
    return this.task()?.status === ComplementaryTaskStatus.IN_PROGRESS;
  }

  /**
   * Can cancel task (PLANNED or IN_PROGRESS tasks)
   */
  canCancel(): boolean {
    const status = this.task()?.status;
    return status === ComplementaryTaskStatus.PLANNED || status === ComplementaryTaskStatus.IN_PROGRESS;
  }
}
