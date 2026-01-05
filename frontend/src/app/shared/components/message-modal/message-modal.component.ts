import { Component, EventEmitter, Input, Output } from '@angular/core';
import { CommonModule } from '@angular/common';

export type MessageType = 'success' | 'error' | 'warning' | 'info';

@Component({
  selector: 'app-message-modal',
  standalone: true,
  imports: [CommonModule],
  template: `
    <div class="modal-overlay" *ngIf="isOpen" (click)="onBackdropClick($event)">
      <div class="modal-container" [class]="'modal-' + type" (click)="$event.stopPropagation()">
        <div class="modal-header">
          <div class="modal-icon">
            <span *ngIf="type === 'success'">✅</span>
            <span *ngIf="type === 'error'">❌</span>
            <span *ngIf="type === 'warning'">⚠️</span>
            <span *ngIf="type === 'info'">ℹ️</span>
          </div>
          <h2>{{ title }}</h2>
        </div>
        <div class="modal-body">
          <p>{{ message }}</p>
        </div>
        <div class="modal-footer">
          <button class="btn-primary" (click)="handleClose()">{{ confirmText }}</button>
          <button *ngIf="showCancel" class="btn-secondary" (click)="handleCancel()">{{ cancelText }}</button>
        </div>
      </div>
    </div>
  `,
  styles: [`
    .modal-overlay {
      position: fixed;
      top: 0;
      left: 0;
      right: 0;
      bottom: 0;
      background: rgba(0, 0, 0, 0.5);
      display: flex;
      align-items: center;
      justify-content: center;
      z-index: 10000;
      animation: fadeIn 0.2s ease-in;
    }

    @keyframes fadeIn {
      from { opacity: 0; }
      to { opacity: 1; }
    }

    .modal-container {
      background: white;
      border-radius: 12px;
      box-shadow: 0 10px 40px rgba(0, 0, 0, 0.2);
      max-width: 500px;
      width: 90%;
      animation: slideUp 0.3s ease-out;
    }

    @keyframes slideUp {
      from {
        transform: translateY(20px);
        opacity: 0;
      }
      to {
        transform: translateY(0);
        opacity: 1;
      }
    }

    .modal-header {
      padding: 24px 24px 16px;
      text-align: center;
      border-bottom: 1px solid #e0e0e0;
    }

    .modal-icon {
      font-size: 48px;
      margin-bottom: 12px;
    }

    .modal-header h2 {
      margin: 0;
      font-size: 24px;
      font-weight: 600;
    }

    .modal-success .modal-header h2 { color: #2e7d32; }
    .modal-error .modal-header h2 { color: #d32f2f; }
    .modal-warning .modal-header h2 { color: #f57c00; }
    .modal-info .modal-header h2 { color: #1976d2; }

    .modal-body {
      padding: 24px;
      text-align: center;
    }

    .modal-body p {
      margin: 0;
      font-size: 16px;
      line-height: 1.5;
      color: #495057;
      white-space: pre-line;
    }

    .modal-footer {
      padding: 16px 24px 24px;
      display: flex;
      gap: 12px;
      justify-content: center;
    }

    button {
      padding: 12px 32px;
      border: none;
      border-radius: 6px;
      font-size: 16px;
      font-weight: 500;
      cursor: pointer;
      transition: all 0.2s;
      min-width: 100px;
    }

    .btn-primary {
      background: #667eea;
      color: white;
    }

    .btn-primary:hover {
      background: #5568d3;
      transform: translateY(-1px);
      box-shadow: 0 4px 8px rgba(102, 126, 234, 0.3);
    }

    .btn-secondary {
      background: #f5f5f5;
      color: #495057;
    }

    .btn-secondary:hover {
      background: #e0e0e0;
    }

    .modal-success .btn-primary {
      background: #2e7d32;
    }

    .modal-success .btn-primary:hover {
      background: #1b5e20;
    }

    .modal-error .btn-primary {
      background: #d32f2f;
    }

    .modal-error .btn-primary:hover {
      background: #b71c1c;
    }

    .modal-warning .btn-primary {
      background: #f57c00;
    }

    .modal-warning .btn-primary:hover {
      background: #ef6c00;
    }

    @media (max-width: 480px) {
      .modal-container {
        width: 95%;
      }

      .modal-header h2 {
        font-size: 20px;
      }

      .modal-body p {
        font-size: 14px;
      }

      button {
        padding: 10px 24px;
        font-size: 14px;
        min-width: 80px;
      }
    }
  `]
})
export class MessageModalComponent {
  @Input() isOpen = false;
  @Input() type: MessageType = 'info';
  @Input() title = '';
  @Input() message = '';
  @Input() confirmText = 'OK';
  @Input() cancelText = 'Cancel';
  @Input() showCancel = false;
  
  @Output() onClose = new EventEmitter<void>();
  @Output() onConfirm = new EventEmitter<void>();
  @Output() onCancel = new EventEmitter<void>();

  handleClose(): void {
    this.isOpen = false;
    this.onConfirm.emit();
    this.onClose.emit();
  }

  handleCancel(): void {
    this.isOpen = false;
    this.onCancel.emit();
    this.onClose.emit();
  }

  onBackdropClick(event: Event): void {
    this.handleClose();
  }
}
