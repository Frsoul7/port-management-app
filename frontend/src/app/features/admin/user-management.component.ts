import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { Router } from '@angular/router';
import { UserManagementService } from '../../core/services/user-management.service';
import { MessageModalService } from '../../core/services/message-modal.service';
import { User, UpdateUserRequest, Organization } from '../../core/models/user-management.model';
import { UserSearchBarComponent, UserSearchParams } from './components/user-search-bar/user-search-bar.component';
import { UserTableComponent } from './components/user-table/user-table.component';
import { UserEditModalComponent } from './components/user-edit-modal/user-edit-modal.component';
import { AlertBannerComponent } from '../vessels/components/alert-banner/alert-banner.component';

@Component({
  selector: 'app-user-management',
  standalone: true,
  imports: [
    CommonModule,
    UserSearchBarComponent,
    UserTableComponent,
    UserEditModalComponent,
    AlertBannerComponent
  ],
  templateUrl: './user-management.component.html',
  styleUrls: ['./user-management.component.scss']
})
export class UserManagementComponent implements OnInit {
  users: User[] = [];
  allUsers: User[] = [];
  organizations: Organization[] = [];
  loading = false;
  saving = false;
  errorMessage = '';
  successMessage = '';
  
  showEditModal = false;
  selectedUser: User | null = null;

  constructor(
    private userManagementService: UserManagementService,
    private modalService: MessageModalService,
    private router: Router
  ) {}

  ngOnInit(): void {
    this.loadInitialData();
  }

  async loadInitialData(): Promise<void> {
    this.loading = true;
    try {
      await this.loadUsers();
      await this.loadOrganizations();
    } finally {
      this.loading = false;
    }
  }

  async loadUsers(): Promise<void> {
    return new Promise((resolve) => {
      this.userManagementService.getAllUsers().subscribe({
        next: (users) => {
          this.users = users;
          this.allUsers = users;
          resolve();
        },
        error: (error) => {
          console.error('Error loading users:', error);
          this.errorMessage = 'Failed to load users. Please try again.';
          resolve();
        }
      });
    });
  }

  async loadOrganizations(): Promise<void> {
    return new Promise((resolve) => {
      this.userManagementService.getOrganizations().subscribe({
        next: (orgs) => {
          this.organizations = orgs;
          resolve();
        },
        error: (error) => {
          console.error('Error loading organizations:', error);
          resolve();
        }
      });
    });
  }

  // Search Bar Event Handlers
  handleSearch(params: UserSearchParams): void {
    this.users = this.allUsers.filter(user => {
      const matchesName = !params.name || 
        user.name.toLowerCase().includes(params.name.toLowerCase());
      const matchesEmail = !params.email || 
        user.email.toLowerCase().includes(params.email.toLowerCase());
      const matchesOrg = !params.organizationName || 
        user.organizationName.toLowerCase().includes(params.organizationName.toLowerCase());
      
      return matchesName && matchesEmail && matchesOrg;
    });
  }

  handleClear(): void {
    this.users = [...this.allUsers];
  }

  // Table Event Handlers
  handleEdit(user: User): void {
    this.selectedUser = user;
    this.showEditModal = true;
  }

  handleActivate(user: User): void {
    const roleFormatted = this.formatRole(user.role);
    
    this.modalService.showConfirm(
      'Activate User',
      `Activate user ${user.name}?\n\nThis will:\n• Confirm their role: ${roleFormatted}\n• Activate their account\n• Send them an email notification`,
      () => this.performActivation(user),
      () => console.log('Activation cancelled')
    );
  }

  private performActivation(user: User): void {
    this.loading = true;
    this.errorMessage = '';
    this.successMessage = '';

    this.userManagementService.activateUser(user.userId, user.role, true).subscribe({
      next: (response) => {
        if (response.success && response.user) {
          // Update user in both arrays
          const updateInArray = (arr: User[]) => {
            const index = arr.findIndex(u => u.userId === user.userId);
            if (index !== -1) {
              arr[index] = response.user!;
            }
          };
          
          updateInArray(this.users);
          updateInArray(this.allUsers);
          
          this.modalService.showSuccess(
            'User Activated',
            `${user.name} has been activated successfully and notified via email!`
          );
        }
        this.loading = false;
      },
      error: (error) => {
        console.error('Error activating user:', error);
        const errorMsg = error.error?.error || error.error?.message || 'Failed to activate user. Please try again.';
        this.modalService.showError('Activation Failed', errorMsg);
        this.loading = false;
      }
    });
  }

  handleDeactivate(user: User): void {
    this.modalService.showConfirm(
      'Deactivate User',
      `Deactivate user ${user.name}?\n\nThis will:\n• Revoke their access to the system\n• They will not be able to log in\n• Their data will be preserved`,
      () => this.performDeactivation(user),
      () => console.log('Deactivation cancelled')
    );
  }

  private performDeactivation(user: User): void {
    this.loading = true;
    this.errorMessage = '';
    this.successMessage = '';

    this.userManagementService.activateUser(user.userId, user.role, false).subscribe({
      next: (response) => {
        if (response.success && response.user) {
          // Update user in both arrays
          const updateInArray = (arr: User[]) => {
            const index = arr.findIndex(u => u.userId === user.userId);
            if (index !== -1) {
              arr[index] = response.user!;
            }
          };
          
          updateInArray(this.users);
          updateInArray(this.allUsers);
          
          this.modalService.showSuccess(
            'User Deactivated',
            `${user.name} has been deactivated successfully. They can no longer access the system.`
          );
        }
        this.loading = false;
      },
      error: (error) => {
        console.error('Error deactivating user:', error);
        const errorMsg = error.error?.error || error.error?.message || 'Failed to deactivate user. Please try again.';
        this.modalService.showError('Deactivation Failed', errorMsg);
        this.loading = false;
      }
    });
  }

  formatRole(role: string): string {
    return role.replace(/_/g, ' ');
  }

  // Modal Event Handlers
  handleModalClose(): void {
    this.showEditModal = false;
    this.selectedUser = null;
  }

  handleSave(data: { userId: string; request: UpdateUserRequest }): void {
    this.saving = true;
    this.errorMessage = '';
    this.successMessage = '';

    this.userManagementService.updateUser(data.userId, data.request).subscribe({
      next: (updatedUser) => {
        // Update user in both arrays
        const updateInArray = (arr: User[]) => {
          const index = arr.findIndex(u => u.userId === data.userId);
          if (index !== -1) {
            arr[index] = updatedUser;
          }
        };
        
        updateInArray(this.users);
        updateInArray(this.allUsers);
        
        this.successMessage = 'User updated successfully!';
        this.showEditModal = false;
        this.saving = false;
        this.autoHideMessage();
      },
      error: (error) => {
        console.error('Error updating user:', error);
        this.errorMessage = error.error?.error || 'Failed to update user. Please try again.';
        this.saving = false;
      }
    });
  }

  autoHideMessage(): void {
    setTimeout(() => {
      this.successMessage = '';
      this.errorMessage = '';
    }, 5000);
  }

  goBack(): void {
    this.router.navigate(['/dashboard']);
  }
}
