import { Component, inject, signal, output } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { TranslatePipe } from '../../../core/pipes/translate.pipe';
import { DataRequestService } from '../../../core/services/data-request.service';
import { 
  NonUserDataRequestDto, 
  DataRequestType,
  NonUserDataRequestResponse 
} from '../../../core/models/data-request.model';

/**
 * Non-User Data Request Form Component
 * US 4.5.4: Non-User Data Rights
 * 
 * Contact form for non-users (crew members, captains, representatives)
 * to submit GDPR data requests
 */
@Component({
  selector: 'app-non-user-request-form',
  standalone: true,
  imports: [CommonModule, FormsModule, TranslatePipe],
  template: `
    <div class="request-form-container" id="contact-form">
      <!-- Form Header -->
      <div class="form-header">
        <h3>{{ 'data_request.title' | translate }}</h3>
        <p class="subtitle">{{ 'data_request.subtitle' | translate }}</p>
      </div>

      <!-- Success State -->
      @if (submitted() && submissionResult()) {
        <div class="success-state">
          <div class="success-icon">✓</div>
          <h4>{{ 'data_request.success_title' | translate }}</h4>
          <p class="reference">
            {{ 'data_request.reference_number' | translate }}: 
            <strong>{{ submissionResult()!.referenceNumber }}</strong>
          </p>
          <p class="message">{{ submissionResult()!.message }}</p>
          <p class="timeline">
            {{ 'data_request.response_timeline' | translate }}: 
            <strong>{{ submissionResult()!.estimatedResponseDays }} {{ 'data_request.days' | translate }}</strong>
          </p>
          <button class="btn btn-secondary" (click)="resetForm()">
            {{ 'data_request.submit_another' | translate }}
          </button>
        </div>
      }

      <!-- Error State -->
      @if (error()) {
        <div class="error-banner">
          <span class="error-icon">⚠️</span>
          <p>{{ error() }}</p>
          <button class="btn-close" (click)="error.set(null)">×</button>
        </div>
      }

      <!-- Form -->
      @if (!submitted()) {
        <form (ngSubmit)="onSubmit()" #requestForm="ngForm" class="request-form">
          <!-- Personal Information Section -->
          <div class="form-section">
            <h4 class="section-title">{{ 'data_request.personal_info' | translate }}</h4>
            
            <div class="form-row">
              <div class="form-group">
                <label for="fullName">{{ 'data_request.full_name' | translate }} *</label>
                <input 
                  type="text" 
                  id="fullName"
                  name="fullName"
                  class="form-control"
                  [(ngModel)]="formData.fullName"
                  required
                  minlength="3"
                  #fullName="ngModel"
                  placeholder="John Doe">
                @if (fullName.invalid && fullName.touched) {
                  <span class="error-text">{{ 'data_request.name_required' | translate }}</span>
                }
              </div>

              <div class="form-group">
                <label for="email">{{ 'data_request.email' | translate }} *</label>
                <input 
                  type="email" 
                  id="email"
                  name="email"
                  class="form-control"
                  [(ngModel)]="formData.email"
                  required
                  email
                  #email="ngModel"
                  placeholder="john.doe&#64;example.com">
                @if (email.invalid && email.touched) {
                  <span class="error-text">{{ 'data_request.email_invalid' | translate }}</span>
                }
              </div>
            </div>

            <div class="form-group">
              <label for="phone">{{ 'data_request.phone' | translate }}</label>
              <input 
                type="tel" 
                id="phone"
                name="phone"
                class="form-control"
                [(ngModel)]="formData.phone"
                placeholder="+351 XXX XXX XXX">
              <small class="help-text">{{ 'data_request.phone_help' | translate }}</small>
            </div>
          </div>

          <!-- Request Details Section -->
          <div class="form-section">
            <h4 class="section-title">{{ 'data_request.request_details' | translate }}</h4>
            
            <div class="form-group">
              <label for="requestType">{{ 'data_request.request_type' | translate }} *</label>
              <select 
                id="requestType"
                name="requestType"
                class="form-control"
                [(ngModel)]="formData.requestType"
                required
                #requestType="ngModel">
                <option value="">{{ 'data_request.select_type' | translate }}</option>
                <option value="ACCESS">{{ 'data_request.type_access' | translate }}</option>
                <option value="RECTIFICATION">{{ 'data_request.type_rectification' | translate }}</option>
                <option value="DELETION">{{ 'data_request.type_deletion' | translate }}</option>
                <option value="OBJECTION">{{ 'data_request.type_objection' | translate }}</option>
                <option value="OTHER">{{ 'data_request.type_other' | translate }}</option>
              </select>
              @if (requestType.invalid && requestType.touched) {
                <span class="error-text">{{ 'data_request.type_required' | translate }}</span>
              }
            </div>

            <div class="form-row">
              <div class="form-group">
                <label for="vesselReference">{{ 'data_request.vessel_reference' | translate }}</label>
                <input 
                  type="text" 
                  id="vesselReference"
                  name="vesselReference"
                  class="form-control"
                  [(ngModel)]="formData.vesselReference"
                  placeholder="e.g., MV Ocean Star">
                <small class="help-text">{{ 'data_request.vessel_help' | translate }}</small>
              </div>

              <div class="form-group">
                <label for="vvnReference">{{ 'data_request.vvn_reference' | translate }}</label>
                <input 
                  type="text" 
                  id="vvnReference"
                  name="vvnReference"
                  class="form-control"
                  [(ngModel)]="formData.vvnReference"
                  placeholder="e.g., VVN-2026-001234">
                <small class="help-text">{{ 'data_request.vvn_help' | translate }}</small>
              </div>
            </div>

            <div class="form-group">
              <label for="description">{{ 'data_request.description' | translate }} *</label>
              <textarea 
                id="description"
                name="description"
                class="form-control"
                [(ngModel)]="formData.description"
                required
                minlength="20"
                maxlength="2000"
                rows="5"
                #description="ngModel"
                placeholder="{{ 'data_request.description_placeholder' | translate }}"></textarea>
              <div class="textarea-footer">
                @if (description.invalid && description.touched) {
                  <span class="error-text">{{ 'data_request.description_required' | translate }}</span>
                }
                <span class="char-count">{{ (formData.description || '').length }}/2000</span>
              </div>
            </div>
          </div>

          <!-- Consent Section -->
          <div class="form-section consent-section">
            <div class="consent-checkbox">
              <input 
                type="checkbox" 
                id="consent"
                name="consent"
                [(ngModel)]="formData.consentGiven"
                required
                #consent="ngModel">
              <label for="consent">
                {{ 'data_request.consent_text' | translate }} *
              </label>
            </div>
            @if (consent.invalid && consent.touched) {
              <span class="error-text">{{ 'data_request.consent_required' | translate }}</span>
            }
          </div>

          <!-- Submit Button -->
          <div class="form-actions">
            <button 
              type="submit" 
              class="btn btn-primary"
              [disabled]="requestForm.invalid || submitting()">
              @if (submitting()) {
                <span class="btn-spinner"></span>
              }
              {{ 'data_request.submit' | translate }}
            </button>
          </div>

          <!-- GDPR Notice -->
          <div class="gdpr-notice">
            <p>{{ 'data_request.gdpr_notice' | translate }}</p>
          </div>
        </form>
      }
    </div>
  `,
  styles: [`
    .request-form-container {
      background: white;
      border-radius: 12px;
      box-shadow: 0 2px 10px rgba(0, 0, 0, 0.08);
      padding: 2rem;
      margin-top: 2rem;
    }

    .form-header {
      text-align: center;
      margin-bottom: 2rem;
      padding-bottom: 1.5rem;
      border-bottom: 2px solid #f0f0f0;
    }

    .form-header h3 {
      color: #2c3e50;
      font-size: 1.5rem;
      margin-bottom: 0.5rem;
    }

    .subtitle {
      color: #6c757d;
      font-size: 0.95rem;
      margin: 0;
    }

    .form-section {
      margin-bottom: 2rem;
    }

    .section-title {
      color: #34495e;
      font-size: 1.1rem;
      margin-bottom: 1rem;
      padding-bottom: 0.5rem;
      border-bottom: 1px solid #eee;
    }

    .form-row {
      display: grid;
      grid-template-columns: 1fr 1fr;
      gap: 1rem;
    }

    @media (max-width: 768px) {
      .form-row {
        grid-template-columns: 1fr;
      }
    }

    .form-group {
      margin-bottom: 1rem;
    }

    .form-group label {
      display: block;
      margin-bottom: 0.5rem;
      font-weight: 500;
      color: #495057;
    }

    .form-control {
      width: 100%;
      padding: 0.75rem 1rem;
      border: 1px solid #dee2e6;
      border-radius: 6px;
      font-size: 1rem;
      transition: border-color 0.2s, box-shadow 0.2s;
    }

    .form-control:focus {
      outline: none;
      border-color: #3498db;
      box-shadow: 0 0 0 3px rgba(52, 152, 219, 0.1);
    }

    .form-control.ng-invalid.ng-touched {
      border-color: #e74c3c;
    }

    textarea.form-control {
      resize: vertical;
      min-height: 120px;
    }

    select.form-control {
      cursor: pointer;
      background: white url("data:image/svg+xml;charset=utf-8,%3Csvg xmlns='http://www.w3.org/2000/svg' width='12' height='12' viewBox='0 0 12 12'%3E%3Cpath fill='%23495057' d='M6 8L1 3h10z'/%3E%3C/svg%3E") no-repeat right 1rem center;
      -webkit-appearance: none;
      appearance: none;
      padding-right: 2.5rem;
    }

    .help-text {
      display: block;
      margin-top: 0.25rem;
      font-size: 0.8rem;
      color: #6c757d;
    }

    .error-text {
      display: block;
      margin-top: 0.25rem;
      font-size: 0.85rem;
      color: #e74c3c;
    }

    .textarea-footer {
      display: flex;
      justify-content: space-between;
      align-items: center;
      margin-top: 0.25rem;
    }

    .char-count {
      font-size: 0.8rem;
      color: #6c757d;
    }

    /* Consent Section */
    .consent-section {
      background: #f8f9fa;
      padding: 1rem 1.5rem;
      border-radius: 8px;
      margin-bottom: 1.5rem;
    }

    .consent-checkbox {
      display: flex;
      align-items: flex-start;
      gap: 0.75rem;
    }

    .consent-checkbox input[type="checkbox"] {
      width: 20px;
      height: 20px;
      margin-top: 2px;
      cursor: pointer;
    }

    .consent-checkbox label {
      font-size: 0.9rem;
      line-height: 1.5;
      cursor: pointer;
    }

    /* Form Actions */
    .form-actions {
      text-align: center;
      margin-bottom: 1.5rem;
    }

    .btn {
      padding: 0.75rem 2rem;
      border: none;
      border-radius: 6px;
      font-size: 1rem;
      cursor: pointer;
      transition: all 0.2s;
      display: inline-flex;
      align-items: center;
      justify-content: center;
      gap: 0.5rem;
    }

    .btn-primary {
      background: #3498db;
      color: white;
    }

    .btn-primary:hover:not(:disabled) {
      background: #2980b9;
    }

    .btn-primary:disabled {
      background: #bdc3c7;
      cursor: not-allowed;
    }

    .btn-secondary {
      background: #95a5a6;
      color: white;
    }

    .btn-secondary:hover {
      background: #7f8c8d;
    }

    .btn-spinner {
      width: 16px;
      height: 16px;
      border: 2px solid rgba(255,255,255,0.3);
      border-top-color: white;
      border-radius: 50%;
      animation: spin 0.8s linear infinite;
    }

    @keyframes spin {
      0% { transform: rotate(0deg); }
      100% { transform: rotate(360deg); }
    }

    /* GDPR Notice */
    .gdpr-notice {
      text-align: center;
      padding: 1rem;
      background: #f1f3f5;
      border-radius: 6px;
      font-size: 0.85rem;
      color: #6c757d;
    }

    .gdpr-notice p {
      margin: 0;
    }

    /* Success State */
    .success-state {
      text-align: center;
      padding: 2rem;
    }

    .success-icon {
      width: 60px;
      height: 60px;
      background: #27ae60;
      color: white;
      border-radius: 50%;
      display: flex;
      align-items: center;
      justify-content: center;
      font-size: 2rem;
      margin: 0 auto 1.5rem;
    }

    .success-state h4 {
      color: #27ae60;
      margin-bottom: 1rem;
    }

    .success-state .reference {
      font-size: 1.1rem;
      margin-bottom: 1rem;
    }

    .success-state .message {
      color: #6c757d;
      margin-bottom: 1rem;
    }

    .success-state .timeline {
      color: #495057;
      margin-bottom: 1.5rem;
    }

    /* Error Banner */
    .error-banner {
      display: flex;
      align-items: center;
      gap: 1rem;
      padding: 1rem 1.5rem;
      background: #fdf2f2;
      border: 1px solid #e74c3c;
      border-radius: 8px;
      margin-bottom: 1.5rem;
    }

    .error-banner .error-icon {
      font-size: 1.5rem;
    }

    .error-banner p {
      flex: 1;
      margin: 0;
      color: #c0392b;
    }

    .btn-close {
      background: none;
      border: none;
      font-size: 1.5rem;
      color: #6c757d;
      cursor: pointer;
      padding: 0;
      line-height: 1;
    }

    .btn-close:hover {
      color: #e74c3c;
    }
  `]
})
export class NonUserRequestFormComponent {
  private dataRequestService = inject(DataRequestService);

  // State signals
  submitting = signal(false);
  submitted = signal(false);
  error = signal<string | null>(null);
  submissionResult = signal<NonUserDataRequestResponse | null>(null);

  // Event output
  requestSubmitted = output<NonUserDataRequestResponse>();

  // Form data
  formData: NonUserDataRequestDto = {
    fullName: '',
    email: '',
    phone: '',
    requestType: '' as DataRequestType,
    vesselReference: '',
    vvnReference: '',
    description: '',
    consentGiven: false
  };

  onSubmit(): void {
    if (!this.formData.consentGiven) {
      this.error.set('Please provide consent to process your request.');
      return;
    }

    this.submitting.set(true);
    this.error.set(null);

    this.dataRequestService.submitNonUserRequest(this.formData).subscribe({
      next: (response) => {
        this.submitting.set(false);
        this.submitted.set(true);
        this.submissionResult.set(response);
        this.requestSubmitted.emit(response);
      },
      error: (err) => {
        this.submitting.set(false);
        console.error('Error submitting request:', err);
        this.error.set(
          err.error?.message || 
          'Failed to submit your request. Please try again or contact us directly.'
        );
      }
    });
  }

  resetForm(): void {
    this.formData = {
      fullName: '',
      email: '',
      phone: '',
      requestType: '' as DataRequestType,
      vesselReference: '',
      vvnReference: '',
      description: '',
      consentGiven: false
    };
    this.submitted.set(false);
    this.submissionResult.set(null);
    this.error.set(null);
  }
}
