import { Component, EventEmitter, Input, Output } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterLink, RouterLinkActive } from '@angular/router';
import { TranslatePipe } from '../../../../../core/pipes/translate.pipe';

export interface NavItem {
  label: string;
  path: string;
  icon: string;
  special?: boolean; // For special styling (3D Visualization and User Management)
}

/**
 * Navigation menu component
 * Displays list of navigation links with active state highlighting
 * Responsive: horizontal on desktop, vertical dropdown on mobile
 */
@Component({
  selector: 'app-navbar-menu',
  standalone: true,
  imports: [CommonModule, RouterLink, RouterLinkActive, TranslatePipe],
  templateUrl: './navbar-menu.component.html',
  styleUrl: './navbar-menu.component.scss'
})
export class NavbarMenuComponent {
  @Input() navItems: NavItem[] = [];
  @Input() isMobileOpen = false;
  @Output() linkClick = new EventEmitter<void>();

  onLinkClick(): void {
    this.linkClick.emit();
  }
}
