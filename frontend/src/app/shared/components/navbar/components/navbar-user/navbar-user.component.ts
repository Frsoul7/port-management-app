import { Component, EventEmitter, Input, Output, signal, HostListener } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterLink } from '@angular/router';
import { User, UserRole } from '../../../../../core/models/user.model';
import { TranslatePipe } from '../../../../../core/pipes/translate.pipe';

/**
 * Navbar user profile component
 * Displays user avatar, name, role, dropdown menu with data rights, and logout button
 */
@Component({
  selector: 'app-navbar-user',
  standalone: true,
  imports: [CommonModule, TranslatePipe, RouterLink],
  templateUrl: './navbar-user.component.html',
  styleUrl: './navbar-user.component.scss'
})
export class NavbarUserComponent {
  @Input() user: User | null = null;
  @Output() logout = new EventEmitter<void>();

  showDropdown = signal(false);

  @HostListener('document:click', ['$event'])
  onDocumentClick(event: Event): void {
    const target = event.target as HTMLElement;
    if (!target.closest('.navbar-user')) {
      this.showDropdown.set(false);
    }
  }

  toggleDropdown(): void {
    this.showDropdown.update(v => !v);
  }

  onLogout(): void {
    this.showDropdown.set(false);
    this.logout.emit();
  }

  formatRole(role?: UserRole): string {
    if (!role) return 'Unknown';
    const roleMap: Record<UserRole, string> = {
      [UserRole.PORT_AUTHORITY_OFFICER]: 'Port Authority',
      [UserRole.SHIPPING_AGENT_REPRESENTATIVE]: 'Shipping Agent',
      [UserRole.LOGISTICS_OPERATOR]: 'Logistics',
      [UserRole.ADMINISTRATOR]: 'Administrator'
    };
    return roleMap[role] || role;
  }
}
