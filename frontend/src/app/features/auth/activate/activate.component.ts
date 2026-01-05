import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ActivatedRoute, Router } from '@angular/router';
import { AuthService } from '../../../core/services/auth.service';

@Component({
  selector: 'app-activate',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './activate.component.html',
  styleUrls: ['./activate.component.scss']
})
export class ActivateComponent implements OnInit {
  loading = true;
  success = false;
  message = '';
  token = '';
  email = '';

  constructor(
    private route: ActivatedRoute,
    private router: Router,
    private authService: AuthService
  ) { }

  ngOnInit() {
    // Get token and email from query parameters
    this.route.queryParams.subscribe(params => {
      this.token = params['token'] || '';
      this.email = params['email'] || '';

      console.log('Activation component initialized');
      console.log('Token:', this.token ? `${this.token.substring(0, 20)}...` : 'MISSING');
      console.log('Email:', this.email || 'MISSING');

      if (!this.token || !this.email) {
        this.loading = false;
        this.success = false;
        this.message = 'Invalid activation link. The token or email is missing.';
        return;
      }

      this.activateAccount();
    });
  }

  private activateAccount() {
    console.log('Calling activation API...');
    this.authService.activateUser(this.token, this.email).subscribe({
      next: (response) => {
        console.log('Activation API response:', response);
        this.loading = false;
        this.success = response.success;
        this.message = response.message || 'Account activated successfully!';

        // Don't auto-redirect - let user read the message and click the button
        console.log('Activation complete. Success:', this.success, 'Message:', this.message);
      },
      error: (error) => {
        console.error('Activation API error:', error);
        console.error('Error status:', error.status);
        console.error('Error body:', error.error);
        this.loading = false;
        this.success = false;
        this.message = error.error?.message || 'An error occurred during activation. Please try again.';
      }
    });
  }

  goToLogin() {
    this.router.navigate(['/login']);
  }

  goToRegister() {
    this.router.navigate(['/register']);
  }
}
