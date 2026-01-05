import { Component, OnInit, inject } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { Router } from '@angular/router';
import { AuthService } from '../../../core/services/auth.service';
import { ApiService } from '../../../core/services/api.service';
import { MessageModalService } from '../../../core/services/message-modal.service';

interface GoogleUserInfo {
  email: string;
  name: string;
  googleId?: string;
  picture?: string;
}

interface Organization {
  id: string;
  identifier: string;
  legalName: string;
  alternativeName?: string;
  type: string;
}

@Component({
  selector: 'app-register',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './register.component.html',
  styleUrls: ['./register.component.scss']
})
export class RegisterComponent implements OnInit {
  userInfo: GoogleUserInfo | null = null;
  organizations: Organization[] = [];
  name: string = '';
  selectedOrganizationId: string = '';
  isSubmitting: boolean = false;
  errorMessage: string = '';

  private messageModalService = inject(MessageModalService);

  constructor(
    private router: Router,
    private authService: AuthService,
    private apiService: ApiService
  ) { }

  ngOnInit(): void {
    // Get user info from session storage (set by auth callback)
    const userInfoJson = sessionStorage.getItem('pendingRegistration');
    if (userInfoJson) {
      this.userInfo = JSON.parse(userInfoJson);
      this.name = this.userInfo?.name || '';
    } else {
      // No pending registration, redirect to login
      this.router.navigate(['/login']);
      return;
    }

    // Load available organizations
    this.loadOrganizations();
  }

  async loadOrganizations(): Promise<void> {
    try {
      this.organizations = await this.apiService.get<Organization[]>('Organizations').toPromise() || [];
    } catch (error) {
      console.error('Failed to load organizations:', error);
      this.errorMessage = 'Failed to load organizations. Please try again.';
    }
  }

  async onSubmit(): Promise<void> {
    if (!this.userInfo) return;

    this.isSubmitting = true;
    this.errorMessage = '';

    try {
      const response = await this.apiService.post<any>('Authentication/register', {
        Email: this.userInfo.email,
        Name: this.name,
        OrganizationId: this.selectedOrganizationId,
        GoogleId: this.userInfo.googleId,
        ProfilePictureUrl: this.userInfo.picture
      }).toPromise();

      // User registration successful - email verification required
      console.log('Registration successful - activation email sent');

      // Clear pending registration
      sessionStorage.removeItem('pendingRegistration');

      // Show success message and redirect
      this.messageModalService.showSuccess(
        'Registration Successful! 🎉',
        'Please check your email to verify your address. After verification, an administrator will review and activate your account.'
      );

      // Redirect to login page after user closes modal
      setTimeout(() => {
        this.router.navigate(['/login']);
      }, 500);
    } catch (error: any) {
      console.error('Registration failed:', error);
      this.errorMessage = error?.error?.error || 'Registration failed. Please try again.';
      this.isSubmitting = false;
    }
  }

  cancel(): void {
    sessionStorage.removeItem('pendingRegistration');
    this.router.navigate(['/login']);
  }
}
