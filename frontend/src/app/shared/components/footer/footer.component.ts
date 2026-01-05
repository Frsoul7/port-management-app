import { Component } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterLink } from '@angular/router';
import { TranslatePipe } from '../../../core/pipes/translate.pipe';

/**
 * Footer component with privacy policy link (GDPR compliance - US 4.5.2)
 * Provides easy access to privacy policy from any page
 */
@Component({
  selector: 'app-footer',
  standalone: true,
  imports: [CommonModule, RouterLink, TranslatePipe],
  template: `
    <footer class="app-footer">
      <div class="footer-content">
        <div class="footer-links">
          <a routerLink="/privacy-policy" class="footer-link">
            {{ 'privacy_policy.footer_link' | translate }}
          </a>
          <span class="separator">|</span>
          <span class="copyright">© {{ currentYear }} Port Management System</span>
        </div>
      </div>
    </footer>
  `,
  styles: [`
    .app-footer {
      background: #2c3e50;
      color: #ecf0f1;
      padding: 1rem 2rem;
      position: fixed;
      bottom: 0;
      left: 0;
      right: 0;
      z-index: 100;
    }

    .footer-content {
      max-width: 1200px;
      margin: 0 auto;
      display: flex;
      justify-content: center;
      align-items: center;
    }

    .footer-links {
      display: flex;
      align-items: center;
      gap: 1rem;
      font-size: 0.85rem;
    }

    .footer-link {
      color: #3498db;
      text-decoration: none;
      transition: color 0.2s;
    }

    .footer-link:hover {
      color: #5dade2;
      text-decoration: underline;
    }

    .separator {
      color: #7f8c8d;
    }

    .copyright {
      color: #95a5a6;
    }

    @media (max-width: 600px) {
      .app-footer {
        padding: 0.75rem 1rem;
      }

      .footer-links {
        flex-direction: column;
        gap: 0.5rem;
      }

      .separator {
        display: none;
      }
    }
  `]
})
export class FooterComponent {
  currentYear = new Date().getFullYear();
}
