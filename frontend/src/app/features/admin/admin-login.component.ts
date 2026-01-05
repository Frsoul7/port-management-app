import { Component, OnInit, inject } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { Router } from '@angular/router';
import { AuthService } from '../../core/services/auth.service';
import { MessageModalService } from '../../core/services/message-modal.service';

@Component({
  selector: 'app-admin-login',
  standalone: true,
  imports: [CommonModule, FormsModule],
  template: `
    <div class="admin-login-container">
      <div class="login-card">
        <div class="lock-icon">Admin</div>
        <h2>Administrator Login</h2>
        <p class="subtitle">System administrators only</p>

        <form (ngSubmit)="onSubmit()" #loginForm="ngForm">
          <div class="form-group">
            <label for="email">Email</label>
            <input
              type="email"
              id="email"
              name="email"
              [(ngModel)]="email"
              required
              email
              placeholder="email@provider.com"
              [disabled]="loading"
            />
          </div>

          <div class="form-group">
            <label for="password">Password</label>
            <input
              type="password"
              id="password"
              name="password"
              [(ngModel)]="password"
              required
              minlength="8"
              placeholder="Enter your password"
              [disabled]="loading"
            />
          </div>

          <div class="error-message" *ngIf="errorMessage">
            {{ errorMessage }}
          </div>

          <button 
            type="submit" 
            class="login-btn" 
            [disabled]="!loginForm.valid || loading"
          >
            {{ loading ? 'Logging in...' : 'Login' }}
          </button>
        </form>

        <div class="divider">
          <span>or</span>
        </div>

        <button class="google-btn" (click)="goToGoogleLogin()" [disabled]="loading">
          <img src="https://www.google.com/favicon.ico" alt="Google" />
          Back to Google Login
        </button>

        <div class="admin-note">
          <strong>Note:</strong> Only system administrators can login here.
          Regular users must use Google authentication.
        </div>
      </div>
    </div>
  `,
  styles: [`
    .admin-login-container {
      min-height: 100vh;
      display: flex;
      align-items: center;
      justify-content: center;
      background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
      padding: 20px;
    }

    .login-card {
      background: white;
      border-radius: 16px;
      padding: 48px;
      max-width: 420px;
      width: 100%;
      box-shadow: 0 20px 60px rgba(0, 0, 0, 0.3);
    }

    .lock-icon {
      font-size: 64px;
      text-align: center;
      margin-bottom: 24px;
    }

    h2 {
      margin: 0 0 8px 0;
      text-align: center;
      color: #333;
      font-size: 28px;
    }

    .subtitle {
      text-align: center;
      color: #666;
      margin-bottom: 32px;
      font-size: 14px;
    }

    .form-group {
      margin-bottom: 24px;
    }

    label {
      display: block;
      margin-bottom: 8px;
      color: #333;
      font-weight: 600;
      font-size: 14px;
    }

    input {
      width: 100%;
      padding: 12px 16px;
      border: 2px solid #e0e0e0;
      border-radius: 8px;
      font-size: 16px;
      transition: all 0.3s;
      box-sizing: border-box;
    }

    input:focus {
      outline: none;
      border-color: #667eea;
      box-shadow: 0 0 0 3px rgba(102, 126, 234, 0.1);
    }

    input:disabled {
      background: #f5f5f5;
      cursor: not-allowed;
    }

    .error-message {
      background: #ffebee;
      color: #c62828;
      padding: 12px 16px;
      border-radius: 8px;
      margin-bottom: 16px;
      font-size: 14px;
      text-align: center;
    }

    .login-btn {
      width: 100%;
      padding: 14px;
      background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
      color: white;
      border: none;
      border-radius: 8px;
      font-size: 16px;
      font-weight: 600;
      cursor: pointer;
      transition: transform 0.2s, box-shadow 0.2s;
    }

    .login-btn:hover:not(:disabled) {
      transform: translateY(-2px);
      box-shadow: 0 8px 20px rgba(102, 126, 234, 0.4);
    }

    .login-btn:disabled {
      opacity: 0.6;
      cursor: not-allowed;
      transform: none;
    }

    .divider {
      text-align: center;
      margin: 32px 0;
      position: relative;
    }

    .divider::before,
    .divider::after {
      content: '';
      position: absolute;
      top: 50%;
      width: 40%;
      height: 1px;
      background: #e0e0e0;
    }

    .divider::before {
      left: 0;
    }

    .divider::after {
      right: 0;
    }

    .divider span {
      background: white;
      padding: 0 16px;
      color: #999;
      font-size: 14px;
    }

    .google-btn {
      width: 100%;
      padding: 14px;
      background: white;
      color: #333;
      border: 2px solid #e0e0e0;
      border-radius: 8px;
      font-size: 16px;
      font-weight: 600;
      cursor: pointer;
      display: flex;
      align-items: center;
      justify-content: center;
      gap: 12px;
      transition: all 0.3s;
    }

    .google-btn:hover:not(:disabled) {
      border-color: #667eea;
      background: #f8f9ff;
    }

    .google-btn:disabled {
      opacity: 0.6;
      cursor: not-allowed;
    }

    .google-btn img {
      width: 20px;
      height: 20px;
    }

    .admin-note {
      margin-top: 24px;
      padding: 16px;
      background: #fff3e0;
      border-left: 4px solid #ff9800;
      border-radius: 4px;
      font-size: 13px;
      line-height: 1.5;
      color: #666;
    }

    .admin-note strong {
      color: #e65100;
      display: block;
      margin-bottom: 4px;
    }
  `]
})
export class AdminLoginComponent implements OnInit {
  email: string = '';
  password: string = '';
  loading: boolean = false;
  errorMessage: string = '';

  private messageModalService = inject(MessageModalService);

  constructor(
    private authService: AuthService,
    private router: Router
  ) {}

  ngOnInit() {
    // If already logged in, redirect to dashboard
    if (this.authService.isAuthenticated()) {
      this.router.navigate(['/dashboard']);
    }
  }

  async onSubmit() {
    this.loading = true;
    this.errorMessage = '';

    try {
      const success = await this.authService.loginAdmin(this.email, this.password);
      
      if (success) {
        this.router.navigate(['/dashboard']);
      } else {
        this.messageModalService.showError(
          'Login Failed',
          'Invalid credentials or you are not an administrator. Please check your email and password.'
        );
      }
    } catch (error: any) {
      const errorMsg = error?.error?.error || 'Login failed. Please try again.';
      this.messageModalService.showError('Login Failed', errorMsg);
      console.error('Admin login failed:', error);
    } finally {
      this.loading = false;
    }
  }

  goToGoogleLogin() {
    this.router.navigate(['/auth/login']);
  }
}
