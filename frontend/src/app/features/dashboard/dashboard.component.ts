import { Component, OnInit, effect } from '@angular/core';
import { CommonModule } from '@angular/common';
import { AuthService } from '../../core/services/auth.service';
import { User, UserRole } from '../../core/models/user.model';

@Component({
  selector: 'app-dashboard',
  standalone: true,
  imports: [CommonModule],
  template: `
    <div class="dashboard">
      <div class="loading-spinner" *ngIf="isLoading">
        <div class="spinner"></div>
        <p>Loading your dashboard...</p>
      </div>
      <div class="info-card" *ngIf="!isLoading && currentUser">
        <h1>Welcome, {{ currentUser.fullName || currentUser.username }}</h1>
        <p>Your role: <strong>{{ formatRole(currentUser.role) }}</strong></p>
        <p *ngIf="currentUser.email">Email: {{ currentUser.email }}</p>
        <p *ngIf="currentUser.organizationName">
          Organization: {{ currentUser.organizationName }}
        </p>
        <p class="note">
          Use the navigation bar above to access the system features available to your role.
        </p>
      </div>
      <div class="info-card" *ngIf="!isLoading && !currentUser">
        <h1>Welcome to Port Management System</h1>
        <p class="note">Please log in to access the dashboard.</p>
      </div>
    </div>
  `,
  styles: [`
    .dashboard {
      display: flex;
      align-items: center;
      justify-content: center;
      min-height: 100vh;
      width: 100%;
      background: linear-gradient(135deg, #f5f7fa, #c3cfe2);
      padding: 20px;
      box-sizing: border-box;
    }

    .loading-spinner {
      text-align: center;
    }

    .spinner {
      border: 4px solid #f3f3f3;
      border-top: 4px solid #667eea;
      border-radius: 50%;
      width: 50px;
      height: 50px;
      animation: spin 1s linear infinite;
      margin: 0 auto 20px;
    }

    @keyframes spin {
      0% { transform: rotate(0deg); }
      100% { transform: rotate(360deg); }
    }

    .loading-spinner p {
      color: #667eea;
      font-size: 16px;
    }

    .info-card {
      background: white;
      border-radius: 16px;
      padding: 40px;
      box-shadow: 0 4px 20px rgba(0, 0, 0, 0.1);
      text-align: center;
      max-width: 500px;
      width: 100%;
    }

    .info-card h1 {
      color: #667eea;
      margin-bottom: 12px;
      font-size: 26px;
      font-weight: 700;
    }

    .info-card p {
      color: #333;
      margin: 8px 0;
      font-size: 15px;
    }

    .info-card .note {
      margin-top: 20px;
      font-size: 14px;
      color: #666;
      line-height: 1.5;
    }

    @media (max-width: 600px) {
      .info-card {
        padding: 24px;
      }
      .info-card h1 {
        font-size: 22px;
      }
    }
  `]
})
export class DashboardComponent implements OnInit {
  currentUser: User | null = null;
  isLoading = true;

  constructor(private authService: AuthService) {
    // Use effect to react to auth state and user changes
    effect(() => {
      const isReady = this.authService.authStateReady();
      if (isReady) {
        this.currentUser = this.authService.getCurrentUser();
        this.isLoading = false;
        console.log('Dashboard: User loaded', this.currentUser);
      }
    });
    
    effect(() => {
      const user = this.authService.currentUser();
      this.currentUser = user;
      if (this.authService.authStateReady()) {
        this.isLoading = false;
      }
      console.log('Dashboard: User updated', user);
    });
  }

  ngOnInit(): void {
    // Initial load handled by effects
  }

  formatRole(role?: UserRole): string {
    const roleMap: Record<UserRole, string> = {
      [UserRole.PORT_AUTHORITY_OFFICER]: 'Port Authority Officer',
      [UserRole.SHIPPING_AGENT_REPRESENTATIVE]: 'Shipping Agent Representative',
      [UserRole.LOGISTICS_OPERATOR]: 'Logistics Operator',
      [UserRole.ADMINISTRATOR]: 'System Administrator'
    };
    return role ? roleMap[role] || role : 'Unknown';
  }
}
