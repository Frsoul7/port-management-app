import { Component, EventEmitter, Input, Output } from '@angular/core';
import { CommonModule } from '@angular/common';
import { User } from '../../../../core/models/user-management.model';

@Component({
  selector: 'app-user-table',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './user-table.component.html',
  styleUrls: ['./user-table.component.scss']
})
export class UserTableComponent {
  @Input() users: User[] = [];
  @Input() loading = false;
  @Output() onEdit = new EventEmitter<User>();
  @Output() onActivate = new EventEmitter<User>();
  @Output() onDeactivate = new EventEmitter<User>();

  editUser(user: User): void {
    this.onEdit.emit(user);
  }

  activateUser(user: User, event: Event): void {
    event.stopPropagation();
    this.onActivate.emit(user);
  }

  deactivateUser(user: User, event: Event): void {
    event.stopPropagation();
    this.onDeactivate.emit(user);
  }

  formatRole(role: string): string {
    return role.replace(/_/g, ' ');
  }

  getStatusBadge(user: User): string {
    if (!user.emailVerified) return 'pending-email';
    if (!user.isActive) return 'pending-activation';
    return 'active';
  }

  getStatusText(user: User): string {
    if (!user.emailVerified) return 'Email Pending';
    if (!user.isActive) return 'Awaiting Activation';
    return 'Active';
  }

  canActivate(user: User): boolean {
    return user.emailVerified && !user.isActive;
  }

  canDeactivate(user: User): boolean {
    return user.isActive;
  }
}
