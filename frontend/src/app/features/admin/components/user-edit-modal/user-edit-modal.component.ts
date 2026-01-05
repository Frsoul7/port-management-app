import { Component, EventEmitter, Input, OnChanges, Output, SimpleChanges } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { User, UpdateUserRequest, Organization, AVAILABLE_ROLES } from '../../../../core/models/user-management.model';

@Component({
  selector: 'app-user-edit-modal',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './user-edit-modal.component.html',
  styleUrls: ['./user-edit-modal.component.scss']
})
export class UserEditModalComponent implements OnChanges {
  @Input() show = false;
  @Input() user: User | null = null;
  @Input() organizations: Organization[] = [];
  @Input() saving = false;
  @Output() onClose = new EventEmitter<void>();
  @Output() onSave = new EventEmitter<{ userId: string; request: UpdateUserRequest }>();

  availableRoles = AVAILABLE_ROLES;
  
  editForm: UpdateUserRequest = {
    name: '',
    organizationId: '',
    role: ''
  };

  ngOnChanges(changes: SimpleChanges): void {
    if (changes['user'] && this.user) {
      this.editForm = {
        name: this.user.name,
        organizationId: this.user.organizationId,
        role: this.user.role
      };
    }
  }

  close(): void {
    this.onClose.emit();
  }

  save(): void {
    if (!this.user || !this.editForm.name.trim()) {
      return;
    }

    this.onSave.emit({
      userId: this.user.userId,
      request: this.editForm
    });
  }

  handleBackdropClick(event: MouseEvent): void {
    if (event.target === event.currentTarget) {
      this.close();
    }
  }
}
