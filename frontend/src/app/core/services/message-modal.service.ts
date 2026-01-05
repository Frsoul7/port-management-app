import { Injectable, signal } from '@angular/core';
import { MessageType } from '../../shared/components/message-modal/message-modal.component';

export interface ModalConfig {
  type: MessageType;
  title: string;
  message: string;
  confirmText?: string;
  cancelText?: string;
  showCancel?: boolean;
  onConfirm?: () => void;
  onCancel?: () => void;
}

@Injectable({
  providedIn: 'root'
})
export class MessageModalService {
  private modalSignal = signal<ModalConfig | null>(null);
  
  get modal() {
    return this.modalSignal.asReadonly();
  }

  showSuccess(title: string, message: string, onConfirm?: () => void): void {
    this.modalSignal.set({
      type: 'success',
      title,
      message,
      confirmText: 'OK',
      showCancel: false,
      onConfirm
    });
  }

  showError(title: string, message: string, onConfirm?: () => void): void {
    this.modalSignal.set({
      type: 'error',
      title,
      message,
      confirmText: 'OK',
      showCancel: false,
      onConfirm
    });
  }

  showWarning(title: string, message: string, onConfirm?: () => void): void {
    this.modalSignal.set({
      type: 'warning',
      title,
      message,
      confirmText: 'OK',
      showCancel: false,
      onConfirm
    });
  }

  showInfo(title: string, message: string, onConfirm?: () => void): void {
    this.modalSignal.set({
      type: 'info',
      title,
      message,
      confirmText: 'OK',
      showCancel: false,
      onConfirm
    });
  }

  showConfirm(title: string, message: string, onConfirm: () => void, onCancel?: () => void): void {
    this.modalSignal.set({
      type: 'warning',
      title,
      message,
      confirmText: 'Confirm',
      cancelText: 'Cancel',
      showCancel: true,
      onConfirm,
      onCancel
    });
  }

  close(): void {
    this.modalSignal.set(null);
  }
}
