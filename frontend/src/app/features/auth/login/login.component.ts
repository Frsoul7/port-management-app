import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterLink, Router } from '@angular/router';
import { AuthService } from '../../../core/services/auth.service';
import { MessageModalService } from '../../../core/services/message-modal.service';
import { environment } from '../../../../environments/environment';

@Component({
  selector: 'app-login',
  standalone: true,
  imports: [CommonModule, RouterLink],
  templateUrl: './login.component.html',
  styleUrls: ['./login.component.scss']
})
export class LoginComponent implements OnInit {
  showDebug = false;
  clientId = environment.clientId;
  redirectUri = environment.redirectUri || `${window.location.origin}/auth/callback`;
  scopes = environment.scopes;

  get isClientIdValid(): boolean {
    return this.clientId !== 'YOUR_GOOGLE_CLIENT_ID.apps.googleusercontent.com'
      && this.clientId.includes('.apps.googleusercontent.com');
  }

  get clientIdStatus(): string {
    if (this.clientId === 'YOUR_GOOGLE_CLIENT_ID.apps.googleusercontent.com') {
      return 'Not configured';
    }
    if (!this.clientId.includes('.apps.googleusercontent.com')) {
      return 'Invalid format';
    }
    return this.clientId.substring(0, 20) + '...';
  }

  constructor(private authService: AuthService, private modalService: MessageModalService, private router: Router) { }

  ngOnInit() {
    // Redirect to dashboard if already authenticated
    if (this.authService.isAuthenticated()) {
      this.router.navigate(['/dashboard']);
    }
  }

  loginWithGoogle(): void {
    if (!this.isClientIdValid) {
      this.modalService.showError(
        'Configuration Error',
        'Google Client ID is not configured. Please update your environment.ts file with your Google OAuth credentials.',
        () => { this.showDebug = true; }
      );
      return;
    }

    // Pre-flight check
    console.log('=== Pre-flight OAuth Check ===');
    console.log('Redirect URI:', this.redirectUri);
    console.log('Has /auth/callback?', this.redirectUri.includes('/auth/callback'));
    console.log('Ends with /auth/callback?', this.redirectUri.endsWith('/auth/callback'));

    if (!this.redirectUri.includes('/auth/callback')) {
      this.modalService.showError(
        'Configuration Error',
        `Your redirect URI is missing /auth/callback\n\nCurrent: ${this.redirectUri}\nShould be: http://vm.nunoepteixeira.me/auth/callback\n\nPlease update src/environments/environment.ts`
      );
      return;
    }

    console.log('Initiating Google OAuth login...');
    this.authService.login();
  }
}
