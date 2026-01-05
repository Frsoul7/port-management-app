import { Component } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { environment } from '../../../../environments/environment';

@Component({
  selector: 'app-oauth-debug',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './oauth-debug.component.html',
  styleUrls: ['./oauth-debug.component.scss']
})
export class OAuthDebugComponent {
  config = {
    iamUrl: environment.iamUrl,
    clientId: environment.clientId,
    redirectUri: environment.redirectUri || `${window.location.origin}/auth/callback`,
    scopes: environment.scopes
  };

  isValidUrl(url: string): boolean {
    return url.startsWith('https://accounts.google.com/');
  }

  isValidClientId(): boolean {
    return this.config.clientId !== 'YOUR_GOOGLE_CLIENT_ID.apps.googleusercontent.com' &&
      this.config.clientId.includes('.apps.googleusercontent.com') &&
      this.config.clientId.length > 50;
  }

  isValidRedirectUri(): boolean {
    return (this.config.redirectUri.startsWith('http://') ||
      this.config.redirectUri.startsWith('https://')) &&
      this.config.redirectUri.includes('/auth/callback') &&
      this.config.redirectUri.endsWith('/auth/callback'); // Must end with /auth/callback
  }

  allValid(): boolean {
    return this.isValidUrl(this.config.iamUrl) &&
      this.isValidClientId() &&
      this.isValidRedirectUri();
  }

  getClientIdValidation(): string {
    if (this.config.clientId === 'YOUR_GOOGLE_CLIENT_ID.apps.googleusercontent.com') {
      return 'Not configured (placeholder value)';
    }
    if (!this.config.clientId.includes('.apps.googleusercontent.com')) {
      return 'Invalid format (must end with .apps.googleusercontent.com)';
    }
    if (this.config.clientId.length < 50) {
      return 'Too short (Google Client IDs are typically 70+ characters)';
    }
    return 'Valid format';
  }

  getRedirectUriValidation(): string {
    if (!this.config.redirectUri.startsWith('http://') && !this.config.redirectUri.startsWith('https://')) {
      return 'Must start with http:// or https://';
    }
    if (!this.config.redirectUri.includes('/auth/callback')) {
      return 'Must include /auth/callback path';
    }
    if (!this.config.redirectUri.endsWith('/auth/callback')) {
      return 'Must end with /auth/callback (not just the domain)';
    }
    return 'Valid format';
  }

  getOrigin(): string {
    try {
      const url = new URL(this.config.redirectUri);
      return `${url.protocol}//${url.host}`;
    } catch {
      return 'Invalid URL';
    }
  }

  generateOAuthUrl(): string {
    const params = {
      client_id: this.config.clientId,
      RedirectUri: this.config.redirectUri,
      response_type: 'code',
      scope: this.config.scopes,
      state: 'TEST_STATE_12345',
      access_type: 'offline',
      prompt: 'consent'
    };

    const queryString = Object.entries(params)
      .map(([key, value]) => `${encodeURIComponent(key)}=${encodeURIComponent(value)}`)
      .join('&');

    return `${this.config.iamUrl}?${queryString}`;
  }

  copyToClipboard(): void {
    navigator.clipboard.writeText(this.generateOAuthUrl())
      .then(() => alert('OAuth URL copied to clipboard!'))
      .catch(() => alert('Failed to copy to clipboard'));
  }

  testOAuth(): void {
    if (!this.allValid()) {
      alert('Please fix configuration issues before testing');
      return;
    }
    const url = this.generateOAuthUrl();
    console.log('Testing OAuth with URL:', url);
    window.open(url, '_blank');
  }

  resetToDefaults(): void {
    this.config = {
      iamUrl: 'https://accounts.google.com/o/oauth2/v2/auth',
      clientId: 'YOUR_GOOGLE_CLIENT_ID.apps.googleusercontent.com',
      redirectUri: 'http://vm.nunoepteixeira.me/auth/callback', // FIXED
      scopes: 'openid profile email'
    };
  }
}
