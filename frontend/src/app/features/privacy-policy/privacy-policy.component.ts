import { Component, OnInit, inject } from '@angular/core';
import { CommonModule } from '@angular/common';
import { Router } from '@angular/router';
import { PrivacyPolicyService } from '../../core/services/privacy-policy.service';
import { PrivacyPolicy } from '../../core/models/privacy-policy.model';
import { TranslationService } from '../../core/services/translation.service';
import { TranslatePipe } from '../../core/pipes/translate.pipe';
import { NonUserRequestFormComponent } from './non-user-request-form/non-user-request-form.component';

/**
 * Privacy Policy Page Component
 * US 4.5.2: As a System User, I want clear and accessible information
 * about personal data processing.
 * 
 * This component displays the current privacy policy to all users.
 * It is publicly accessible (no authentication required).
 */
@Component({
  selector: 'app-privacy-policy',
  standalone: true,
  imports: [CommonModule, TranslatePipe, NonUserRequestFormComponent],
  template: `
    <div class="privacy-policy-container">
      <div class="privacy-policy-card">
        <!-- Header -->
        <div class="header">
          <button class="back-btn" (click)="goBack()" title="{{ 'COMMON.BACK' | translate }}">
            ← {{ 'privacy_policy.back_to_dashboard' | translate }}
          </button>
          <h1>{{ 'privacy_policy.title' | translate }}</h1>
          @if (policy) {
            <div class="meta">
              <span class="version">{{ 'privacy_policy.version' | translate }}: {{ policy.version }}</span>
              <span class="date">{{ 'privacy_policy.effective_date' | translate }}: {{ policy.effectiveDate | date:'dd/MM/yyyy' }}</span>
            </div>
          }
        </div>

        <!-- Loading state -->
        @if (loading) {
          <div class="loading">
            <div class="spinner"></div>
            <p>{{ 'COMMON.LOADING' | translate }}</p>
          </div>
        }

        <!-- Error state -->
        @if (error && !loading) {
          <div class="error">
            <span class="error-icon">⚠️</span>
            <p>{{ error }}</p>
            <button class="retry-btn" (click)="loadPolicy()">
              {{ 'COMMON.RETRY' | translate }}
            </button>
          </div>
        }

        <!-- Policy content -->
        @if (policy && !loading && !error) {
          <div class="policy-content">
            <div class="markdown-content" [innerHTML]="renderedContent"></div>
          </div>

          <!-- Change summary (if available) -->
          @if (policy.changeSummary) {
            <div class="change-summary">
              <h3>{{ 'privacy_policy.recent_changes' | translate }}</h3>
              <p>{{ policy.changeSummary }}</p>
            </div>
          }
        }

        <!-- No policy found -->
        @if (!policy && !loading && !error) {
          <div class="no-policy">
            <p>{{ 'privacy_policy.no_policy' | translate }}</p>
          </div>
        }

        <!-- Footer with language switcher -->
        <div class="footer">
          <div class="language-selector">
            <span>{{ 'privacy_policy.language' | translate }}:</span>
            <button 
              [class.active]="currentLang === 'pt'" 
              (click)="setLanguage('pt')">
              Português
            </button>
            <button 
              [class.active]="currentLang === 'en'" 
              (click)="setLanguage('en')">
              English
            </button>
          </div>
          <p class="last-updated" *ngIf="policy">
            {{ 'privacy_policy.last_updated' | translate }}: {{ policy.createdAt | date:'dd/MM/yyyy HH:mm' }}
          </p>
        </div>
      </div>

      <!-- Non-User Data Request Form (US 4.5.4) -->
      <app-non-user-request-form></app-non-user-request-form>
    </div>
  `,
  styles: [`
    .privacy-policy-container {
      min-height: 100vh;
      padding: 2rem;
      background: linear-gradient(135deg, #f5f7fa 0%, #e4e9f0 100%);
    }

    .privacy-policy-container > * {
      max-width: 900px;
      margin-left: auto;
      margin-right: auto;
    }

    .privacy-policy-card {
      max-width: 900px;
      margin: 0 auto;
      background: white;
      border-radius: 12px;
      box-shadow: 0 4px 20px rgba(0, 0, 0, 0.1);
      overflow: hidden;
    }

    .header {
      background: linear-gradient(135deg, #2c3e50 0%, #34495e 100%);
      color: white;
      padding: 2rem;
      text-align: center;
      position: relative;
    }

    .back-btn {
      position: absolute;
      top: 1rem;
      left: 1rem;
      background: rgba(255, 255, 255, 0.15);
      color: white;
      border: 1px solid rgba(255, 255, 255, 0.3);
      padding: 0.5rem 1rem;
      border-radius: 6px;
      cursor: pointer;
      font-size: 0.9rem;
      transition: all 0.2s ease;
      display: flex;
      align-items: center;
      gap: 0.5rem;
    }

    .back-btn:hover {
      background: rgba(255, 255, 255, 0.25);
      border-color: rgba(255, 255, 255, 0.5);
    }

    .header h1 {
      margin: 0 0 1rem 0;
      font-size: 2rem;
      font-weight: 600;
    }

    .header .meta {
      display: flex;
      justify-content: center;
      gap: 2rem;
      font-size: 0.9rem;
      opacity: 0.9;
    }

    .loading, .error, .no-policy {
      padding: 3rem;
      text-align: center;
    }

    .spinner {
      width: 40px;
      height: 40px;
      border: 3px solid #f3f3f3;
      border-top: 3px solid #2c3e50;
      border-radius: 50%;
      animation: spin 1s linear infinite;
      margin: 0 auto 1rem;
    }

    @keyframes spin {
      0% { transform: rotate(0deg); }
      100% { transform: rotate(360deg); }
    }

    .error {
      color: #e74c3c;
    }

    .error-icon {
      font-size: 2rem;
    }

    .retry-btn {
      margin-top: 1rem;
      padding: 0.5rem 1.5rem;
      background: #2c3e50;
      color: white;
      border: none;
      border-radius: 4px;
      cursor: pointer;
      transition: background 0.2s;
    }

    .retry-btn:hover {
      background: #34495e;
    }

    .policy-content {
      padding: 2rem;
      line-height: 1.8;
    }

    .markdown-content {
      color: #333;
    }

    .markdown-content h2 {
      color: #2c3e50;
      margin-top: 2rem;
      padding-bottom: 0.5rem;
      border-bottom: 2px solid #ecf0f1;
    }

    .markdown-content h3 {
      color: #34495e;
      margin-top: 1.5rem;
    }

    .markdown-content p {
      margin: 1rem 0;
    }

    .markdown-content ul, .markdown-content ol {
      margin: 1rem 0;
      padding-left: 2rem;
    }

    .markdown-content li {
      margin: 0.5rem 0;
    }

    .change-summary {
      background: #f8f9fa;
      border-top: 1px solid #ecf0f1;
      padding: 1.5rem 2rem;
    }

    .change-summary h3 {
      margin: 0 0 0.5rem 0;
      color: #2c3e50;
      font-size: 1rem;
    }

    .change-summary p {
      margin: 0;
      color: #666;
      font-style: italic;
    }

    .footer {
      border-top: 1px solid #ecf0f1;
      padding: 1.5rem 2rem;
      display: flex;
      justify-content: space-between;
      align-items: center;
      background: #fafafa;
    }

    .language-selector {
      display: flex;
      align-items: center;
      gap: 0.5rem;
    }

    .language-selector span {
      color: #666;
      font-size: 0.9rem;
    }

    .language-selector button {
      padding: 0.3rem 0.8rem;
      border: 1px solid #ddd;
      background: white;
      border-radius: 4px;
      cursor: pointer;
      transition: all 0.2s;
    }

    .language-selector button:hover {
      border-color: #2c3e50;
    }

    .language-selector button.active {
      background: #2c3e50;
      color: white;
      border-color: #2c3e50;
    }

    .last-updated {
      color: #888;
      font-size: 0.85rem;
      margin: 0;
    }

    @media (max-width: 768px) {
      .privacy-policy-container {
        padding: 1rem;
      }

      .header {
        padding: 1.5rem;
      }

      .header h1 {
        font-size: 1.5rem;
      }

      .header .meta {
        flex-direction: column;
        gap: 0.5rem;
      }

      .policy-content {
        padding: 1.5rem;
      }

      .footer {
        flex-direction: column;
        gap: 1rem;
        text-align: center;
      }
    }
  `]
})
export class PrivacyPolicyComponent implements OnInit {
  private privacyPolicyService = inject(PrivacyPolicyService);
  private translationService = inject(TranslationService);
  private router = inject(Router);

  policy: PrivacyPolicy | null = null;
  loading = false;
  error: string | null = null;
  currentLang = 'pt';
  renderedContent = '';

  ngOnInit(): void {
    // Use the current language from translation service
    this.currentLang = this.translationService.currentLang();
    this.loadPolicy();
  }

  loadPolicy(): void {
    this.loading = true;
    this.error = null;

    this.privacyPolicyService.getCurrentPolicy(this.currentLang).subscribe({
      next: (policy) => {
        this.policy = policy;
        this.renderedContent = this.renderMarkdown(policy?.content || '');
        this.loading = false;
      },
      error: (err) => {
        console.error('Error loading privacy policy:', err);
        this.error = err.status === 404 
          ? this.translationService.translate('privacy_policy.not_found')
          : this.translationService.translate('privacy_policy.load_error');
        this.loading = false;
      }
    });
  }

  setLanguage(lang: string): void {
    if (lang !== this.currentLang) {
      this.currentLang = lang;
      this.loadPolicy();
    }
  }

  /**
   * Navigate back to dashboard or previous page
   */
  goBack(): void {
    // Navigate to dashboard
    this.router.navigate(['/dashboard']);
  }

  /**
   * Simple markdown to HTML conversion
   * In production, use a proper markdown library like marked.js
   */
  private renderMarkdown(markdown: string): string {
    if (!markdown) return '';
    
    // Basic markdown conversion
    let html = markdown
      // Escape HTML
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      // Headers
      .replace(/^### (.*$)/gm, '<h3>$1</h3>')
      .replace(/^## (.*$)/gm, '<h2>$1</h2>')
      .replace(/^# (.*$)/gm, '<h1>$1</h1>')
      // Bold
      .replace(/\*\*(.*?)\*\*/g, '<strong>$1</strong>')
      // Italic
      .replace(/\*(.*?)\*/g, '<em>$1</em>')
      // Line breaks
      .replace(/\n\n/g, '</p><p>')
      // Lists (simple)
      .replace(/^\- (.*$)/gm, '<li>$1</li>')
      .replace(/^\d+\. (.*$)/gm, '<li>$1</li>');

    // Wrap in paragraph tags
    html = '<p>' + html + '</p>';
    
    // Clean up list items
    html = html.replace(/<\/p><li>/g, '</p><ul><li>');
    html = html.replace(/<\/li><p>/g, '</li></ul><p>');
    
    return html;
  }
}
