import { Component, OnInit, effect } from '@angular/core';
import { CommonModule } from '@angular/common';
import { Router } from '@angular/router';
import { AuthService } from '../../../core/services/auth.service';
import { User, UserRole } from '../../../core/models/user.model';
import { NavbarBrandComponent } from './components/navbar-brand/navbar-brand.component';
import { MobileMenuToggleComponent } from './components/mobile-menu-toggle/mobile-menu-toggle.component';
import { NavbarMenuComponent, NavItem } from './components/navbar-menu/navbar-menu.component';
import { NavbarUserComponent } from './components/navbar-user/navbar-user.component';
import { LanguageSelectorComponent } from '../../../core/components/language-selector/language-selector.component';

interface NavItemWithRoles extends NavItem {
  roles: UserRole[];
  special?: boolean; // For special styling (3D Visualization and User Management)
}

/**
 * Main navigation bar component
 * Orchestrates child components: brand, menu, user profile, mobile toggle
 * Manages user authentication state and role-based navigation visibility
 */
@Component({
  selector: 'app-navbar',
  standalone: true,
  imports: [
    CommonModule,
    NavbarBrandComponent,
    MobileMenuToggleComponent,
    NavbarMenuComponent,
    NavbarUserComponent,
    LanguageSelectorComponent
  ],
  templateUrl: './navbar.component.html',
  styleUrl: './navbar.component.scss'
})
export class NavbarComponent implements OnInit {
  currentUser: User | null = null;
  visibleNavItems: NavItem[] = [];
  isMobileMenuOpen = false;

  private allNavItems: NavItemWithRoles[] = [
    // Shipping Agents section
    { label: 'NAV.SHIPPING_AGENTS', path: '/shipping-agents', icon: '', roles: [UserRole.PORT_AUTHORITY_OFFICER, UserRole.ADMINISTRATOR] },

    // Vessel Types and Vessels section
    { label: 'NAV.VESSEL_TYPES', path: '/vessel-types', icon: '', roles: [UserRole.PORT_AUTHORITY_OFFICER, UserRole.ADMINISTRATOR] },
    { label: 'NAV.VESSELS', path: '/vessels', icon: '', roles: [UserRole.PORT_AUTHORITY_OFFICER, UserRole.ADMINISTRATOR] },

    // Docks and Storage Areas section
    { label: 'NAV.DOCKS', path: '/docks', icon: '', roles: [UserRole.PORT_AUTHORITY_OFFICER, UserRole.ADMINISTRATOR] },
    { label: 'NAV.STORAGE', path: '/storage-areas', icon: '', roles: [UserRole.PORT_AUTHORITY_OFFICER, UserRole.ADMINISTRATOR] },

    // Physical Resources section
    { label: 'NAV.RESOURCES', path: '/physical-resources', icon: '', roles: [UserRole.LOGISTICS_OPERATOR, UserRole.ADMINISTRATOR] },

    // Operations section (VVN, VVE, Operation Plans, Task Categories, Complementary Tasks)
    { label: 'NAV.VVN', path: '/vvns', icon: '', roles: [UserRole.PORT_AUTHORITY_OFFICER, UserRole.SHIPPING_AGENT_REPRESENTATIVE, UserRole.LOGISTICS_OPERATOR, UserRole.ADMINISTRATOR] },
    { label: 'NAV.VESSEL_VISIT_EXECUTIONS', path: '/vessel-visit-executions', icon: '', roles: [UserRole.LOGISTICS_OPERATOR, UserRole.PORT_AUTHORITY_OFFICER, UserRole.ADMINISTRATOR] },
    { label: 'NAV.OPERATION_PLANS', path: '/operation-plans', icon: '', roles: [UserRole.LOGISTICS_OPERATOR, UserRole.PORT_AUTHORITY_OFFICER, UserRole.ADMINISTRATOR] },
    { label: 'NAV.TASK_CATEGORIES', path: '/task-categories', icon: '', roles: [UserRole.PORT_AUTHORITY_OFFICER, UserRole.LOGISTICS_OPERATOR, UserRole.ADMINISTRATOR] },
    { label: 'NAV.COMPLEMENTARY_TASKS', path: '/complementary-tasks', icon: '', roles: [UserRole.LOGISTICS_OPERATOR, UserRole.PORT_AUTHORITY_OFFICER, UserRole.ADMINISTRATOR] },

    // Special items with different styling
    { label: 'NAV.VISUALIZATION', path: '/visualization', icon: '', roles: [UserRole.PORT_AUTHORITY_OFFICER, UserRole.SHIPPING_AGENT_REPRESENTATIVE, UserRole.LOGISTICS_OPERATOR, UserRole.ADMINISTRATOR], special: true },
    { label: 'NAV.USER_MANAGEMENT', path: '/admin/users', icon: '', roles: [UserRole.ADMINISTRATOR], special: true }
  ];

  constructor(private authService: AuthService, private router: Router) {
    // Simplified effect - just react to user changes
    effect(() => {
      const user = this.authService.currentUser();
      this.currentUser = user;
      this.updateVisibleNavItems();
    });
  }

  ngOnInit(): void {
    // Initial load
    this.loadUserAndNavigation();
  }

  private loadUserAndNavigation(): void {
    this.currentUser = this.authService.getCurrentUser();
    this.updateVisibleNavItems();
  }

  private updateVisibleNavItems(): void {
    if (!this.currentUser?.role) {
      this.visibleNavItems = [];
      return;
    }
    this.visibleNavItems = this.allNavItems.filter(item =>
      item.roles.includes(this.currentUser!.role)
    );
  }

  onToggleMobileMenu(): void {
    this.isMobileMenuOpen = !this.isMobileMenuOpen;
  }

  onCloseMobileMenu(): void {
    this.isMobileMenuOpen = false;
  }

  onLogout(): void {
    this.authService.logout();
  }
}
