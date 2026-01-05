
import { Component } from '@angular/core';
import { CommonModule } from '@angular/common';
import { environment } from '../../../../environments/environment';

@Component({
  selector: 'app-redirect-uri-checker',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './redirect-uri-checker.component.html',
  styleUrls: ['./redirect-uri-checker.component.scss']
})
export class RedirectUriCheckerComponent {
  clientId = environment.clientId;
  redirectUri = environment.redirectUri || `${window.location.origin}/auth/callback`;
  origin = this.getOrigin();
  copied = false;

  getOrigin(): string {
    try {
      const url = new URL(this.redirectUri);
      return `${url.protocol}//${url.host}`;
    } catch {
      return 'http://vm.nunoepteixeira.me';
    }
  }

  copy(text: string): void {
    navigator.clipboard.writeText(text).then(() => {
      this.copied = true;
      setTimeout(() => {
        this.copied = false;
      }, 2000);
    });
  }
}
