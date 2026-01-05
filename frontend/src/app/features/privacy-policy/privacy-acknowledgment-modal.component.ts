import { Component, EventEmitter, Input, Output, inject } from '@angular/core';
import { CommonModule } from '@angular/common';
import { PrivacyPolicyService } from '../../core/services/privacy-policy.service';
import { PrivacyPolicy, AcknowledgmentStatus } from '../../core/models/privacy-policy.model';
import { TranslatePipe } from '../../core/pipes/translate.pipe';

/**
 * Privacy Policy Acknowledgment Modal
 * US 4.5.1: A change in the policy must trigger a system user notification upon next login.
 * 
 * This modal is shown when a user needs to acknowledge a new privacy policy.
 * The user must acknowledge before continuing to use the system.
 */
@Component({
  selector: 'app-privacy-acknowledgment-modal',
  standalone: true,
  imports: [CommonModule, TranslatePipe],
  template: `
    <div class="modal-overlay" *ngIf="isOpen" (click)="onOverlayClick($event)">
      <div class="modal-content" (click)="$event.stopPropagation()">
        <!-- Header -->
        <div class="modal-header">
          <h2>{{ 'privacy_policy.new_policy_title' | translate }}</h2>
          <p class="subtitle">{{ 'privacy_policy.acknowledgment_required' | translate }}</p>
        </div>

        <!-- Loading state -->
        @if (loading) {
          <div class="modal-body loading">
            <div class="spinner"></div>
            <p>{{ 'COMMON.LOADING' | translate }}</p>
          </div>
        }

        <!-- Policy content -->
        @if (policy && !loading) {
          <div class="modal-body">
            <div class="policy-info">
              <span class="version-badge">v{{ policy.version }}</span>
              <span class="date">{{ 'privacy_policy.effective_date' | translate }}: {{ policy.effectiveDate | date:'dd/MM/yyyy' }}</span>
            </div>

            @if (policy.changeSummary) {
              <div class="change-summary">
                <h4>{{ 'privacy_policy.whats_changed' | translate }}</h4>
                <p>{{ policy.changeSummary }}</p>
              </div>
            }

            <div class="policy-content" [innerHTML]="renderedContent"></div>
          </div>

          <div class="modal-footer">
            <p class="legal-notice">
              {{ 'privacy_policy.legal_notice' | translate }}
            </p>
            
            <div class="actions">
              <a class="view-full" href="/privacy-policy" target="_blank">
                {{ 'privacy_policy.view_full' | translate }}
              </a>
              <button 
                class="acknowledge-btn" 
                [disabled]="acknowledging"
                (click)="acknowledge()">
                @if (acknowledging) {
                  <span class="btn-spinner"></span>
                } @else {
                  {{ 'privacy_policy.acknowledge' | translate }}
                }
              </button>
            </div>
          </div>
        }

        <!-- Error state -->
        @if (error && !loading) {
          <div class="modal-body error">
            <span class="error-icon">⚠️</span>
            <p>{{ error }}</p>
            <button class="retry-btn" (click)="loadPolicy()">
              {{ 'COMMON.RETRY' | translate }}
            </button>
          </div>
        }
      </div>
    </div>
  `,
  styles: [`
    .modal-overlay {
      position: fixed;
      top: 0;
      left: 0;
      right: 0;
      bottom: 0;
      background: rgba(0, 0, 0, 0.7);
      display: flex;
      justify-content: center;
      align-items: center;
      z-index: 10000;
      padding: 1rem;
    }

    .modal-content {
      background: white;
      border-radius: 12px;
      max-width: 700px;
      width: 100%;
      max-height: 90vh;
      overflow: hidden;
      display: flex;
      flex-direction: column;
      box-shadow: 0 20px 60px rgba(0, 0, 0, 0.3);
    }

    .modal-header {
      background: linear-gradient(135deg, #2c3e50 0%, #34495e 100%);
      color: white;
      padding: 1.5rem 2rem;
      text-align: center;
    }

    .modal-header h2 {
      margin: 0;
      font-size: 1.5rem;
      font-weight: 600;
    }

    .modal-header .subtitle {
      margin: 0.5rem 0 0 0;
      opacity: 0.9;
      font-size: 0.9rem;
    }

    .modal-body {
      flex: 1;
      overflow-y: auto;
      padding: 1.5rem 2rem;
    }

    .modal-body.loading, .modal-body.error {
      display: flex;
      flex-direction: column;
      justify-content: center;
      align-items: center;
      min-height: 200px;
    }

    .spinner {
      width: 40px;
      height: 40px;
      border: 3px solid #f3f3f3;
      border-top: 3px solid #2c3e50;
      border-radius: 50%;
      animation: spin 1s linear infinite;
      margin-bottom: 1rem;
    }

    .btn-spinner {
      display: inline-block;
      width: 18px;
      height: 18px;
      border: 2px solid rgba(255,255,255,0.3);
      border-top: 2px solid white;
      border-radius: 50%;
      animation: spin 1s linear infinite;
    }

    @keyframes spin {
      0% { transform: rotate(0deg); }
      100% { transform: rotate(360deg); }
    }

    .policy-info {
      display: flex;
      align-items: center;
      gap: 1rem;
      margin-bottom: 1.5rem;
    }

    .version-badge {
      background: #27ae60;
      color: white;
      padding: 0.25rem 0.75rem;
      border-radius: 20px;
      font-size: 0.85rem;
      font-weight: 500;
    }

    .date {
      color: #666;
      font-size: 0.9rem;
    }

    .change-summary {
      background: #f8f9fa;
      border-left: 4px solid #3498db;
      padding: 1rem;
      margin-bottom: 1.5rem;
      border-radius: 0 4px 4px 0;
    }

    .change-summary h4 {
      margin: 0 0 0.5rem 0;
      color: #2c3e50;
      font-size: 0.95rem;
    }

    .change-summary p {
      margin: 0;
      color: #555;
      line-height: 1.5;
    }

    .policy-content {
      line-height: 1.7;
      color: #333;
      max-height: 300px;
      overflow-y: auto;
      padding-right: 0.5rem;
    }

    .policy-content h2 {
      font-size: 1.2rem;
      color: #2c3e50;
      margin-top: 1.5rem;
    }

    .policy-content h3 {
      font-size: 1.1rem;
      color: #34495e;
      margin-top: 1rem;
    }

    .modal-footer {
      border-top: 1px solid #ecf0f1;
      padding: 1.5rem 2rem;
      background: #fafafa;
    }

    .legal-notice {
      font-size: 0.85rem;
      color: #666;
      margin: 0 0 1rem 0;
      text-align: center;
    }

    .actions {
      display: flex;
      justify-content: space-between;
      align-items: center;
    }

    .view-full {
      color: #3498db;
      text-decoration: none;
      font-size: 0.9rem;
    }

    .view-full:hover {
      text-decoration: underline;
    }

    .acknowledge-btn {
      padding: 0.75rem 2rem;
      background: linear-gradient(135deg, #27ae60 0%, #2ecc71 100%);
      color: white;
      border: none;
      border-radius: 6px;
      font-size: 1rem;
      font-weight: 500;
      cursor: pointer;
      transition: transform 0.2s, box-shadow 0.2s;
      min-width: 150px;
    }

    .acknowledge-btn:hover:not(:disabled) {
      transform: translateY(-2px);
      box-shadow: 0 4px 12px rgba(39, 174, 96, 0.4);
    }

    .acknowledge-btn:disabled {
      opacity: 0.7;
      cursor: not-allowed;
    }

    .retry-btn {
      padding: 0.5rem 1.5rem;
      background: #2c3e50;
      color: white;
      border: none;
      border-radius: 4px;
      cursor: pointer;
      margin-top: 1rem;
    }

    .error {
      color: #e74c3c;
    }

    .error-icon {
      font-size: 2rem;
      margin-bottom: 0.5rem;
    }

    @media (max-width: 600px) {
      .modal-header {
        padding: 1rem 1.5rem;
      }

      .modal-body {
        padding: 1rem 1.5rem;
      }

      .modal-footer {
        padding: 1rem 1.5rem;
      }

      .actions {
        flex-direction: column;
        gap: 1rem;
      }

      .acknowledge-btn {
        width: 100%;
      }
    }
  `]
})
export class PrivacyAcknowledgmentModalComponent {
  private privacyPolicyService = inject(PrivacyPolicyService);

  @Input() isOpen = false;
  @Input() acknowledgmentStatus: AcknowledgmentStatus | null = null;
  @Output() acknowledged = new EventEmitter<void>();

  policy: PrivacyPolicy | null = null;
  loading = false;
  acknowledging = false;
  error: string | null = null;
  renderedContent = '';

  ngOnChanges(): void {
    if (this.isOpen && this.acknowledgmentStatus?.currentPolicyId) {
      this.loadPolicy();
    }
  }

  loadPolicy(): void {
    if (!this.acknowledgmentStatus?.currentPolicyId) return;

    this.loading = true;
    this.error = null;

    this.privacyPolicyService.getPolicyById(this.acknowledgmentStatus.currentPolicyId).subscribe({
      next: (policy) => {
        this.policy = policy;
        this.renderedContent = this.renderMarkdown(policy?.content || '');
        this.loading = false;
      },
      error: (err) => {
        console.error('Error loading policy:', err);
        this.error = 'Failed to load privacy policy. Please try again.';
        this.loading = false;
      }
    });
  }

  acknowledge(): void {
    if (!this.policy?.policyId) return;

    this.acknowledging = true;

    this.privacyPolicyService.acknowledgePolicy(this.policy.policyId).subscribe({
      next: () => {
        this.acknowledging = false;
        this.acknowledged.emit();
      },
      error: (err) => {
        console.error('Error acknowledging policy:', err);
        this.error = 'Failed to acknowledge policy. Please try again.';
        this.acknowledging = false;
      }
    });
  }

  onOverlayClick(event: Event): void {
    // Prevent closing by clicking outside - user must acknowledge
    event.stopPropagation();
  }

  private renderMarkdown(markdown: string): string {
    if (!markdown) return '';
    
    let html = markdown
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/^### (.*$)/gm, '<h3>$1</h3>')
      .replace(/^## (.*$)/gm, '<h2>$1</h2>')
      .replace(/^# (.*$)/gm, '<h1>$1</h1>')
      .replace(/\*\*(.*?)\*\*/g, '<strong>$1</strong>')
      .replace(/\*(.*?)\*/g, '<em>$1</em>')
      .replace(/\n\n/g, '</p><p>')
      .replace(/^\- (.*$)/gm, '<li>$1</li>')
      .replace(/^\d+\. (.*$)/gm, '<li>$1</li>');

    html = '<p>' + html + '</p>';
    html = html.replace(/<\/p><li>/g, '</p><ul><li>');
    html = html.replace(/<\/li><p>/g, '</li></ul><p>');
    
    return html;
  }
}
