import { Component, OnInit } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { CommonModule } from '@angular/common';
import { AuthService } from '../../../core/services/auth.service';
import { MessageModalService } from '../../../core/services/message-modal.service';

@Component({
  selector: 'app-auth-callback',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './auth-callback.component.html',
  styleUrls: ['./auth-callback.component.scss']
})
export class AuthCallbackComponent implements OnInit {
  message = 'Authenticating with Google...';

  constructor(
    private route: ActivatedRoute,
    private router: Router,
    private authService: AuthService,
    private modalService: MessageModalService
  ) { }

  ngOnInit(): void {
    console.log('AuthCallbackComponent initialized');
    console.log('Current URL:', window.location.href);

    this.route.queryParams.subscribe(params => {
      console.log('Query params received:', params);

      const code = params['code'];
      const state = params['state'];
      const error = params['error'];

      if (error) {
        this.message = 'Authentication failed. Redirecting...';
        console.error('OAuth error:', error);
        this.modalService.showError(
          'Authentication Failed',
          'Unable to authenticate with Google. Please try again.',
          () => this.router.navigate(['/login'])
        );
        return;
      }

      if (code && state) {
        console.log('Processing OAuth callback...');
        console.log('Code:', code.substring(0, 20) + '...');
        console.log('State:', state);

        this.authService.handleAuthCallback(code, state).subscribe({
          next: (result) => {
            console.log('Authentication result:', result);

            if (result.status === 'Success') {
              console.log('Authentication successful!');
              this.message = 'Authentication successful! Redirecting...';

              // Get the intended URL or default to dashboard
              const redirectUrl = sessionStorage.getItem('redirectUrl') || '/dashboard';
              console.log('Redirecting to:', redirectUrl);
              sessionStorage.removeItem('redirectUrl');

              // Wait a bit longer to ensure user state is fully propagated
              // This gives time for BehaviorSubject to emit and components to subscribe
              setTimeout(() => {
                console.log('Navigating to:', redirectUrl);
                this.router.navigateByUrl(redirectUrl, { replaceUrl: true }).then(() => {
                  console.log('Navigation complete');
                });
              }, 500);
            } else if (result.status === 'RequiresRegistration') {
              console.log('User requires registration');
              this.message = 'Please complete your registration...';

              setTimeout(() => {
                this.router.navigate(['/register']);
              }, 500);
            } else if (result.status === 'Failed' && result.errorMessage) {
              // Show specific error message from backend
              this.modalService.showError(
                'Authentication Failed',
                result.errorMessage,
                () => this.router.navigate(['/login'])
              );
            } else {
              this.message = 'Authentication failed. Redirecting...';
              this.modalService.showError(
                'Authentication Failed',
                'Unable to sign in. Please try again.',
                () => this.router.navigate(['/login'])
              );
            }
          },
          error: (err) => {
            this.message = 'Authentication failed. Redirecting...';
            console.error('Callback handling error:', err);

            const errorMessage = err.error?.message || err.error?.errorMessage ||
              err.error?.error || 'Unable to sign in. Please try again.';

            this.modalService.showError(
              'Authentication Failed',
              errorMessage,
              () => this.router.navigate(['/login'])
            );
          }
        });
      } else {
        console.error('Missing code or state in callback');
        this.message = 'Invalid authentication response. Redirecting...';
        this.modalService.showError(
          'Authentication Error',
          'Invalid authentication response received. Please try again.',
          () => this.router.navigate(['/login'])
        );
      }
    });
  }
}
