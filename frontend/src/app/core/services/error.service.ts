import { Injectable, inject } from '@angular/core';
import { MatSnackBar } from '@angular/material/snack-bar';

/**
 * Error handling service with user-friendly notifications
 * Implements US 3.1.4 requirements
 */
@Injectable({
  providedIn: 'root'
})
export class ErrorService {
  private snackBar = inject(MatSnackBar);

  /**
   * Show error message to user (US 3.1.4)
   */
  showError(message: string, duration: number = 5000): void {
    this.snackBar.open(message, 'Close', {
      duration,
      horizontalPosition: 'end',
      verticalPosition: 'top',
      panelClass: ['error-snackbar']
    });
  }

  /**
   * Show success message to user
   */
  showSuccess(message: string, duration: number = 3000): void {
    this.snackBar.open(message, 'Close', {
      duration,
      horizontalPosition: 'end',
      verticalPosition: 'top',
      panelClass: ['success-snackbar']
    });
  }

  /**
   * Show info message to user
   */
  showInfo(message: string, duration: number = 4000): void {
    this.snackBar.open(message, 'Close', {
      duration,
      horizontalPosition: 'end',
      verticalPosition: 'top',
      panelClass: ['info-snackbar']
    });
  }

  /**
   * Handle HTTP error with user-friendly message
   */
  handleHttpError(error: any): string {
    let message = 'An unexpected error occurred';

    if (error.status === 0) {
      message = 'Cannot connect to server. Please check your connection.';
    } else if (error.status === 400) {
      message = error.error?.message || 'Invalid request data';
    } else if (error.status === 401) {
      message = 'Authentication required';
    } else if (error.status === 403) {
      message = 'You do not have permission to perform this action';
    } else if (error.status === 404) {
      message = 'Resource not found';
    } else if (error.status === 409) {
      message = error.error?.message || 'Conflict occurred';
    } else if (error.status >= 500) {
      message = 'Server error. Please try again later.';
    } else if (error.error?.message) {
      message = error.error.message;
    }

    this.showError(message);
    return message;
  }
}
