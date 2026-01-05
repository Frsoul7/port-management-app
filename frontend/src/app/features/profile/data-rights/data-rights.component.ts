import { Component, OnInit, inject, signal, computed } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule, ReactiveFormsModule, FormBuilder, FormGroup, Validators } from '@angular/forms';
import { TranslatePipe } from '../../../core/pipes/translate.pipe';
import { TranslationService } from '../../../core/services/translation.service';
import { DataRightsService, UserDataExport, MyDataRequest, RectificationRequest } from '../../../core/services/data-rights.service';

/**
 * Component for User Data Rights management (GDPR Subject Access Requests)
 * US 4.5.3: User Data Rights (SAR)
 * 
 * Features:
 * - Export data (JSON/PDF) - GDPR Article 15
 * - Request rectification - GDPR Article 16
 * - Request deletion - GDPR Article 17
 * - View request history
 */
@Component({
  selector: 'app-data-rights',
  standalone: true,
  imports: [CommonModule, FormsModule, ReactiveFormsModule, TranslatePipe],
  template: `
    <div class="data-rights-container">
      <h1>{{ 'data_rights.title' | translate }}</h1>
      <p class="subtitle">{{ 'data_rights.subtitle' | translate }}</p>

      <!-- Tab Navigation -->
      <div class="tabs">
        <button 
          [class.active]="activeTab() === 'export'" 
          (click)="activeTab.set('export')">
          {{ 'data_rights.tabs.export' | translate }}
        </button>
        <button 
          [class.active]="activeTab() === 'rectification'" 
          (click)="activeTab.set('rectification')">
          {{ 'data_rights.tabs.rectification' | translate }}
        </button>
        <button 
          [class.active]="activeTab() === 'deletion'" 
          (click)="activeTab.set('deletion')">
          {{ 'data_rights.tabs.deletion' | translate }}
        </button>
        <button 
          [class.active]="activeTab() === 'history'" 
          (click)="activeTab.set('history'); loadRequests()">
          {{ 'data_rights.tabs.history' | translate }}
        </button>
      </div>

      <!-- Export Tab -->
      @if (activeTab() === 'export') {
        <div class="tab-content">
          <div class="info-card">
            <h2>{{ 'data_rights.export.title' | translate }}</h2>
            <p>{{ 'data_rights.export.description' | translate }}</p>
            <p class="legal-ref">{{ 'data_rights.export.legal_reference' | translate }}</p>
            
            <div class="export-buttons">
              <button 
                class="btn btn-primary" 
                (click)="exportJson()" 
                [disabled]="isExporting()">
                @if (isExporting()) {
                  <span class="spinner"></span>
                }
                {{ 'data_rights.export.download_json' | translate }}
              </button>
              
              <button 
                class="btn btn-secondary" 
                (click)="exportPdf()" 
                [disabled]="isExporting()">
                @if (isExporting()) {
                  <span class="spinner"></span>
                }
                {{ 'data_rights.export.download_pdf' | translate }}
              </button>
            </div>

            @if (exportSuccess()) {
              <div class="alert alert-success">
                {{ 'data_rights.export.success' | translate }}
              </div>
            }

            @if (exportError()) {
              <div class="alert alert-error">
                {{ exportError() }}
              </div>
            }
          </div>
        </div>
      }

      <!-- Rectification Tab -->
      @if (activeTab() === 'rectification') {
        <div class="tab-content">
          <div class="info-card">
            <h2>{{ 'data_rights.rectification.title' | translate }}</h2>
            <p>{{ 'data_rights.rectification.description' | translate }}</p>
            <p class="legal-ref">{{ 'data_rights.rectification.legal_reference' | translate }}</p>

            <form [formGroup]="rectificationForm" (ngSubmit)="submitRectification()">
              <div class="form-group">
                <label for="dataCategory">{{ 'data_rights.rectification.data_category' | translate }} *</label>
                <select id="dataCategory" formControlName="dataCategory">
                  <option value="">{{ 'data_rights.rectification.select_category' | translate }}</option>
                  <option value="profile">{{ 'data_rights.rectification.categories.profile' | translate }}</option>
                  <option value="contact">{{ 'data_rights.rectification.categories.contact' | translate }}</option>
                  <option value="organization">{{ 'data_rights.rectification.categories.organization' | translate }}</option>
                  <option value="other">{{ 'data_rights.rectification.categories.other' | translate }}</option>
                </select>
              </div>

              <div class="form-group">
                <label for="currentValue">{{ 'data_rights.rectification.current_value' | translate }} *</label>
                <input type="text" id="currentValue" formControlName="currentValue" 
                       [placeholder]="'data_rights.rectification.current_value_placeholder' | translate">
              </div>

              <div class="form-group">
                <label for="requestedCorrection">{{ 'data_rights.rectification.requested_correction' | translate }} *</label>
                <input type="text" id="requestedCorrection" formControlName="requestedCorrection"
                       [placeholder]="'data_rights.rectification.requested_correction_placeholder' | translate">
              </div>

              <div class="form-group">
                <label for="reason">{{ 'data_rights.rectification.reason' | translate }} *</label>
                <textarea id="reason" formControlName="reason" rows="4"
                          [placeholder]="'data_rights.rectification.reason_placeholder' | translate"></textarea>
              </div>

              <button type="submit" class="btn btn-primary" 
                      [disabled]="!rectificationForm.valid || isSubmitting()">
                @if (isSubmitting()) {
                  <span class="spinner"></span>
                }
                {{ 'data_rights.rectification.submit' | translate }}
              </button>
            </form>

            @if (rectificationSuccess()) {
              <div class="alert alert-success">
                <p>{{ 'data_rights.rectification.success' | translate }}</p>
                <p><strong>{{ 'data_rights.reference' | translate }}:</strong> {{ lastReferenceNumber() }}</p>
              </div>
            }

            @if (rectificationError()) {
              <div class="alert alert-error">
                {{ rectificationError() }}
              </div>
            }
          </div>
        </div>
      }

      <!-- Deletion Tab -->
      @if (activeTab() === 'deletion') {
        <div class="tab-content">
          <div class="info-card warning-card">
            <h2>{{ 'data_rights.deletion.title' | translate }}</h2>
            <p>{{ 'data_rights.deletion.description' | translate }}</p>
            <p class="legal-ref">{{ 'data_rights.deletion.legal_reference' | translate }}</p>

            <div class="warning-box">
              <h3>⚠️ {{ 'data_rights.deletion.warning_title' | translate }}</h3>
              <ul>
                <li>{{ 'data_rights.deletion.warning_1' | translate }}</li>
                <li>{{ 'data_rights.deletion.warning_2' | translate }}</li>
                <li>{{ 'data_rights.deletion.warning_3' | translate }}</li>
              </ul>
            </div>

            <form [formGroup]="deletionForm" (ngSubmit)="submitDeletion()">
              <div class="form-group">
                <label for="deletionReason">{{ 'data_rights.deletion.reason' | translate }}</label>
                <textarea id="deletionReason" formControlName="reason" rows="3"
                          [placeholder]="'data_rights.deletion.reason_placeholder' | translate"></textarea>
              </div>

              <div class="form-group checkbox-group">
                <label class="checkbox-label">
                  <input type="checkbox" formControlName="confirmUnderstanding">
                  {{ 'data_rights.deletion.confirm_understanding' | translate }}
                </label>
              </div>

              <button type="submit" class="btn btn-danger" 
                      [disabled]="!deletionForm.valid || isSubmitting()">
                @if (isSubmitting()) {
                  <span class="spinner"></span>
                }
                {{ 'data_rights.deletion.submit' | translate }}
              </button>
            </form>

            @if (deletionSuccess()) {
              <div class="alert alert-success">
                <p>{{ 'data_rights.deletion.success' | translate }}</p>
                <p><strong>{{ 'data_rights.reference' | translate }}:</strong> {{ lastReferenceNumber() }}</p>
              </div>
            }

            @if (deletionError()) {
              <div class="alert alert-error">
                {{ deletionError() }}
              </div>
            }
          </div>
        </div>
      }

      <!-- History Tab -->
      @if (activeTab() === 'history') {
        <div class="tab-content">
          <div class="info-card">
            <h2>{{ 'data_rights.history.title' | translate }}</h2>
            <p>{{ 'data_rights.history.description' | translate }}</p>

            @if (isLoadingRequests()) {
              <div class="loading">
                <span class="spinner"></span>
                {{ 'common.loading' | translate }}
              </div>
            }

            @if (requests().length === 0 && !isLoadingRequests()) {
              <p class="no-data">{{ 'data_rights.history.no_requests' | translate }}</p>
            }

            @if (requests().length > 0) {
              <table class="requests-table">
                <thead>
                  <tr>
                    <th>{{ 'data_rights.history.reference' | translate }}</th>
                    <th>{{ 'data_rights.history.type' | translate }}</th>
                    <th>{{ 'data_rights.history.status' | translate }}</th>
                    <th>{{ 'data_rights.history.submitted' | translate }}</th>
                    <th>{{ 'data_rights.history.completed' | translate }}</th>
                  </tr>
                </thead>
                <tbody>
                  @for (request of requests(); track request.requestId) {
                    <tr>
                      <td>{{ request.referenceNumber }}</td>
                      <td>
                        <span class="type-badge" [attr.data-type]="request.requestType.toLowerCase()">
                          {{ 'data_rights.types.' + request.requestType.toLowerCase() | translate }}
                        </span>
                      </td>
                      <td>
                        <span class="status-badge" [attr.data-status]="request.status.toLowerCase()">
                          {{ 'data_rights.status.' + request.status.toLowerCase() | translate }}
                        </span>
                      </td>
                      <td>{{ request.submittedAt | date:'medium' }}</td>
                      <td>{{ request.completedAt ? (request.completedAt | date:'medium') : '-' }}</td>
                    </tr>
                  }
                </tbody>
              </table>
            }
          </div>
        </div>
      }
    </div>
  `,
  styles: [`
    .data-rights-container {
      max-width: 900px;
      margin: 0 auto;
      padding: 2rem;
    }

    h1 {
      margin-bottom: 0.5rem;
      color: #1a1a1a;
    }

    .subtitle {
      color: #666;
      margin-bottom: 2rem;
    }

    .tabs {
      display: flex;
      gap: 0.5rem;
      margin-bottom: 1.5rem;
      border-bottom: 2px solid #e0e0e0;
      padding-bottom: 0;
    }

    .tabs button {
      padding: 0.75rem 1.5rem;
      border: none;
      background: none;
      cursor: pointer;
      font-size: 1rem;
      color: #666;
      border-bottom: 2px solid transparent;
      margin-bottom: -2px;
      transition: all 0.2s;
    }

    .tabs button:hover {
      color: #0066cc;
    }

    .tabs button.active {
      color: #0066cc;
      border-bottom-color: #0066cc;
      font-weight: 500;
    }

    .tab-content {
      animation: fadeIn 0.3s ease;
    }

    @keyframes fadeIn {
      from { opacity: 0; transform: translateY(10px); }
      to { opacity: 1; transform: translateY(0); }
    }

    .info-card {
      background: white;
      border-radius: 8px;
      padding: 2rem;
      box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
    }

    .info-card h2 {
      margin-top: 0;
      margin-bottom: 1rem;
      color: #1a1a1a;
    }

    .info-card p {
      color: #555;
      line-height: 1.6;
    }

    .legal-ref {
      font-size: 0.9rem;
      color: #888;
      font-style: italic;
      margin-bottom: 1.5rem;
    }

    .export-buttons {
      display: flex;
      gap: 1rem;
      margin-top: 1.5rem;
    }

    .btn {
      padding: 0.75rem 1.5rem;
      border: none;
      border-radius: 4px;
      cursor: pointer;
      font-size: 1rem;
      display: inline-flex;
      align-items: center;
      gap: 0.5rem;
      transition: all 0.2s;
    }

    .btn:disabled {
      opacity: 0.6;
      cursor: not-allowed;
    }

    .btn-primary {
      background: #0066cc;
      color: white;
    }

    .btn-primary:hover:not(:disabled) {
      background: #0052a3;
    }

    .btn-secondary {
      background: #f0f0f0;
      color: #333;
    }

    .btn-secondary:hover:not(:disabled) {
      background: #e0e0e0;
    }

    .btn-danger {
      background: #dc3545;
      color: white;
    }

    .btn-danger:hover:not(:disabled) {
      background: #c82333;
    }

    .form-group {
      margin-bottom: 1.5rem;
    }

    .form-group label {
      display: block;
      margin-bottom: 0.5rem;
      font-weight: 500;
      color: #333;
    }

    .form-group input,
    .form-group select,
    .form-group textarea {
      width: 100%;
      padding: 0.75rem;
      border: 1px solid #ccc;
      border-radius: 4px;
      font-size: 1rem;
      transition: border-color 0.2s;
    }

    .form-group input:focus,
    .form-group select:focus,
    .form-group textarea:focus {
      outline: none;
      border-color: #0066cc;
    }

    .checkbox-group {
      margin-top: 1rem;
    }

    .checkbox-label {
      display: flex;
      align-items: flex-start;
      gap: 0.5rem;
      cursor: pointer;
      font-weight: normal;
    }

    .checkbox-label input {
      width: auto;
      margin-top: 0.25rem;
    }

    .warning-card {
      border-left: 4px solid #dc3545;
    }

    .warning-box {
      background: #fff3cd;
      border: 1px solid #ffc107;
      border-radius: 4px;
      padding: 1rem;
      margin: 1.5rem 0;
    }

    .warning-box h3 {
      margin: 0 0 0.75rem 0;
      color: #856404;
    }

    .warning-box ul {
      margin: 0;
      padding-left: 1.25rem;
      color: #856404;
    }

    .warning-box li {
      margin-bottom: 0.5rem;
    }

    .alert {
      padding: 1rem;
      border-radius: 4px;
      margin-top: 1.5rem;
    }

    .alert-success {
      background: #d4edda;
      border: 1px solid #c3e6cb;
      color: #155724;
    }

    .alert-error {
      background: #f8d7da;
      border: 1px solid #f5c6cb;
      color: #721c24;
    }

    .spinner {
      display: inline-block;
      width: 16px;
      height: 16px;
      border: 2px solid currentColor;
      border-right-color: transparent;
      border-radius: 50%;
      animation: spin 0.75s linear infinite;
    }

    @keyframes spin {
      to { transform: rotate(360deg); }
    }

    .loading {
      display: flex;
      align-items: center;
      gap: 0.5rem;
      color: #666;
      padding: 2rem 0;
    }

    .no-data {
      text-align: center;
      color: #888;
      padding: 2rem 0;
    }

    .requests-table {
      width: 100%;
      border-collapse: collapse;
      margin-top: 1.5rem;
    }

    .requests-table th,
    .requests-table td {
      padding: 0.75rem;
      text-align: left;
      border-bottom: 1px solid #e0e0e0;
    }

    .requests-table th {
      background: #f8f9fa;
      font-weight: 500;
      color: #333;
    }

    .status-badge,
    .type-badge {
      display: inline-block;
      padding: 0.25rem 0.75rem;
      border-radius: 12px;
      font-size: 0.85rem;
      font-weight: 500;
    }

    .status-badge[data-status="submitted"],
    .status-badge[data-status="pending"] {
      background: #fff3cd;
      color: #856404;
    }

    .status-badge[data-status="in_progress"] {
      background: #cce5ff;
      color: #004085;
    }

    .status-badge[data-status="completed"] {
      background: #d4edda;
      color: #155724;
    }

    .status-badge[data-status="rejected"] {
      background: #f8d7da;
      color: #721c24;
    }

    .type-badge[data-type="access"] {
      background: #e7f3ff;
      color: #0066cc;
    }

    .type-badge[data-type="rectification"] {
      background: #fff3e0;
      color: #e65100;
    }

    .type-badge[data-type="deletion"] {
      background: #ffebee;
      color: #c62828;
    }

    .type-badge[data-type="portability"] {
      background: #e8f5e9;
      color: #2e7d32;
    }

    @media (max-width: 768px) {
      .data-rights-container {
        padding: 1rem;
      }

      .tabs {
        flex-wrap: wrap;
      }

      .tabs button {
        flex: 1;
        min-width: 120px;
        text-align: center;
      }

      .export-buttons {
        flex-direction: column;
      }

      .requests-table {
        font-size: 0.9rem;
      }

      .requests-table th,
      .requests-table td {
        padding: 0.5rem;
      }
    }
  `]
})
export class DataRightsComponent implements OnInit {
  private readonly dataRightsService = inject(DataRightsService);
  private readonly fb = inject(FormBuilder);
  private readonly translationService = inject(TranslationService);

  // State signals
  activeTab = signal<'export' | 'rectification' | 'deletion' | 'history'>('export');
  isExporting = signal(false);
  isSubmitting = signal(false);
  isLoadingRequests = signal(false);
  
  exportSuccess = signal(false);
  exportError = signal<string | null>(null);
  
  rectificationSuccess = signal(false);
  rectificationError = signal<string | null>(null);
  
  deletionSuccess = signal(false);
  deletionError = signal<string | null>(null);
  
  lastReferenceNumber = signal<string | null>(null);
  requests = signal<MyDataRequest[]>([]);

  // Forms
  rectificationForm!: FormGroup;
  deletionForm!: FormGroup;

  ngOnInit(): void {
    this.initForms();
  }

  private initForms(): void {
    this.rectificationForm = this.fb.group({
      dataCategory: ['', Validators.required],
      currentValue: ['', Validators.required],
      requestedCorrection: ['', Validators.required],
      reason: ['', Validators.required]
    });

    this.deletionForm = this.fb.group({
      reason: [''],
      confirmUnderstanding: [false, Validators.requiredTrue]
    });
  }

  async exportJson(): Promise<void> {
    this.isExporting.set(true);
    this.exportSuccess.set(false);
    this.exportError.set(null);

    try {
      const data = await this.dataRightsService.exportDataJson().toPromise();
      if (data) {
        const filename = `gdpr_data_export_${new Date().toISOString().split('T')[0]}.json`;
        this.dataRightsService.downloadJson(data, filename);
        this.exportSuccess.set(true);
      }
    } catch (error: any) {
      this.exportError.set(error.error?.message || this.translationService.translate('data_rights.export.error'));
    } finally {
      this.isExporting.set(false);
    }
  }

  async exportPdf(): Promise<void> {
    this.isExporting.set(true);
    this.exportSuccess.set(false);
    this.exportError.set(null);

    try {
      const blob = await this.dataRightsService.exportDataPdf().toPromise();
      if (blob) {
        const filename = `gdpr_data_export_${new Date().toISOString().split('T')[0]}.pdf`;
        this.dataRightsService.downloadBlob(blob, filename);
        this.exportSuccess.set(true);
      }
    } catch (error: any) {
      this.exportError.set(error.error?.message || this.translationService.translate('data_rights.export.error'));
    } finally {
      this.isExporting.set(false);
    }
  }

  async submitRectification(): Promise<void> {
    if (!this.rectificationForm.valid) return;

    this.isSubmitting.set(true);
    this.rectificationSuccess.set(false);
    this.rectificationError.set(null);

    try {
      const request: RectificationRequest = this.rectificationForm.value;
      const response = await this.dataRightsService.requestRectification(request).toPromise();
      if (response) {
        this.lastReferenceNumber.set(response.referenceNumber);
        this.rectificationSuccess.set(true);
        this.rectificationForm.reset();
      }
    } catch (error: any) {
      this.rectificationError.set(error.error?.message || this.translationService.translate('data_rights.rectification.error'));
    } finally {
      this.isSubmitting.set(false);
    }
  }

  async submitDeletion(): Promise<void> {
    if (!this.deletionForm.valid) return;

    this.isSubmitting.set(true);
    this.deletionSuccess.set(false);
    this.deletionError.set(null);

    try {
      const response = await this.dataRightsService.requestDeletion({
        confirmUnderstanding: true,
        reason: this.deletionForm.value.reason
      }).toPromise();
      
      if (response) {
        this.lastReferenceNumber.set(response.referenceNumber);
        this.deletionSuccess.set(true);
        this.deletionForm.reset();
      }
    } catch (error: any) {
      this.deletionError.set(error.error?.message || this.translationService.translate('data_rights.deletion.error'));
    } finally {
      this.isSubmitting.set(false);
    }
  }

  async loadRequests(): Promise<void> {
    this.isLoadingRequests.set(true);

    try {
      const requests = await this.dataRightsService.getMyRequests().toPromise();
      this.requests.set(requests || []);
    } catch (error) {
      console.error('Failed to load requests:', error);
      this.requests.set([]);
    } finally {
      this.isLoadingRequests.set(false);
    }
  }
}
