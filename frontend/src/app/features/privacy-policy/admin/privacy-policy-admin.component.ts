import { Component, OnInit, inject, signal, computed } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { PrivacyPolicyService } from '../../../core/services/privacy-policy.service';
import { 
  PrivacyPolicy, 
  PrivacyPolicySummary, 
  PublishPrivacyPolicyRequest 
} from '../../../core/models/privacy-policy.model';
import { TranslatePipe } from '../../../core/pipes/translate.pipe';
import { TranslationService } from '../../../core/services/translation.service';
import { MessageModalService } from '../../../core/services/message-modal.service';

/**
 * Privacy Policy Admin Component
 * US 4.5.1: As Administrator, I want to define and update the privacy policy
 * 
 * Features:
 * - Create/Edit privacy policy content (Markdown)
 * - Live preview of policy
 * - Publish new versions
 * - View policy history
 * - View acknowledgment statistics
 */
@Component({
  selector: 'app-privacy-policy-admin',
  standalone: true,
  imports: [CommonModule, FormsModule, TranslatePipe],
  template: `
    <div class="admin-container">
      <header class="admin-header">
        <h1>{{ 'privacy_policy.admin.title' | translate }}</h1>
        <p class="subtitle">{{ 'privacy_policy.admin.subtitle' | translate }}</p>
      </header>

      <div class="admin-content">
        <!-- Tab Navigation -->
        <nav class="tab-nav">
          <button 
            class="tab-btn"
            [class.active]="activeTab() === 'editor'"
            (click)="activeTab.set('editor')">
            <span class="tab-icon">✏️</span>
            {{ 'privacy_policy.admin.tabs.editor' | translate }}
          </button>
          <button 
            class="tab-btn"
            [class.active]="activeTab() === 'preview'"
            (click)="activeTab.set('preview')">
            <span class="tab-icon">👁️</span>
            {{ 'privacy_policy.admin.tabs.preview' | translate }}
          </button>
          <button 
            class="tab-btn"
            [class.active]="activeTab() === 'history'"
            (click)="activeTab.set('history')">
            <span class="tab-icon">📜</span>
            {{ 'privacy_policy.admin.tabs.history' | translate }}
          </button>
        </nav>

        <!-- Loading State -->
        @if (loading()) {
          <div class="loading-state">
            <div class="spinner"></div>
            <p>{{ 'COMMON.LOADING' | translate }}</p>
          </div>
        }

        <!-- Error State -->
        @if (error()) {
          <div class="error-state">
            <span class="error-icon">⚠️</span>
            <p>{{ error() }}</p>
            <button class="btn btn-primary" (click)="loadData()">
              {{ 'COMMON.RETRY' | translate }}
            </button>
          </div>
        }

        <!-- Editor Tab -->
        @if (activeTab() === 'editor' && !loading() && !error()) {
          <div class="editor-tab">
            <!-- Language Selector -->
            <div class="language-selector">
              <label>{{ 'privacy_policy.admin.select_language' | translate }}:</label>
              <div class="lang-buttons">
                <button 
                  class="lang-btn"
                  [class.active]="editingLanguage() === 'pt'"
                  (click)="switchLanguage('pt')">
                  🇵🇹 Português
                </button>
                <button 
                  class="lang-btn"
                  [class.active]="editingLanguage() === 'en'"
                  (click)="switchLanguage('en')">
                  🇬🇧 English
                </button>
              </div>
            </div>

            <!-- Current Policy Info -->
            @if (currentPolicy()) {
              <div class="current-info">
                <span class="info-badge">
                  {{ 'privacy_policy.admin.current_version' | translate }}: {{ currentPolicy()!.version }}
                </span>
                <span class="info-badge">
                  {{ 'privacy_policy.effective_date' | translate }}: {{ currentPolicy()!.effectiveDate | date:'dd/MM/yyyy' }}
                </span>
              </div>
            }

            <!-- Editor Form -->
            <div class="editor-form">
              <div class="form-group">
                <label for="version">{{ 'privacy_policy.admin.new_version' | translate }} *</label>
                <input 
                  type="text" 
                  id="version"
                  class="form-control"
                  [(ngModel)]="newPolicy.version"
                  placeholder="e.g., 2.0.0">
                <small class="help-text">
                  {{ 'privacy_policy.admin.version_help' | translate }}
                </small>
              </div>

              <div class="form-group">
                <label for="changeSummary">{{ 'privacy_policy.admin.change_summary' | translate }}</label>
                <input 
                  type="text" 
                  id="changeSummary"
                  class="form-control"
                  [(ngModel)]="newPolicy.changeSummary"
                  placeholder="Brief description of changes...">
              </div>

              <div class="form-group">
                <label for="effectiveDate">{{ 'privacy_policy.effective_date' | translate }}</label>
                <input 
                  type="date" 
                  id="effectiveDate"
                  class="form-control"
                  [(ngModel)]="newPolicy.effectiveDate">
                <small class="help-text">
                  {{ 'privacy_policy.admin.effective_date_help' | translate }}
                </small>
              </div>

              <div class="form-group">
                <label for="content">{{ 'privacy_policy.admin.content' | translate }} * (Markdown)</label>
                <textarea 
                  id="content"
                  class="form-control content-editor"
                  [(ngModel)]="newPolicy.content"
                  rows="20"
                  placeholder="Enter privacy policy content in Markdown format..."></textarea>
                <div class="editor-toolbar">
                  <button type="button" class="toolbar-btn" (click)="insertMarkdown('## ')" title="Heading">H2</button>
                  <button type="button" class="toolbar-btn" (click)="insertMarkdown('### ')" title="Subheading">H3</button>
                  <button type="button" class="toolbar-btn" (click)="insertMarkdown('**', '**')" title="Bold">B</button>
                  <button type="button" class="toolbar-btn" (click)="insertMarkdown('*', '*')" title="Italic">I</button>
                  <button type="button" class="toolbar-btn" (click)="insertMarkdown('- ')" title="List">•</button>
                  <button type="button" class="toolbar-btn" (click)="insertMarkdown('1. ')" title="Numbered List">1.</button>
                  <button type="button" class="toolbar-btn" (click)="loadCurrentContent()" title="Load current policy">
                    ⬇️ {{ 'privacy_policy.admin.load_current' | translate }}
                  </button>
                </div>
              </div>

              <div class="form-actions">
                <button 
                  class="btn btn-secondary"
                  (click)="activeTab.set('preview')">
                  👁️ {{ 'privacy_policy.admin.preview' | translate }}
                </button>
                <button 
                  class="btn btn-primary"
                  [disabled]="!canPublish() || publishing()"
                  (click)="confirmPublish()">
                  @if (publishing()) {
                    <span class="btn-spinner"></span>
                  }
                  🚀 {{ 'privacy_policy.admin.publish' | translate }}
                </button>
              </div>
            </div>
          </div>
        }

        <!-- Preview Tab -->
        @if (activeTab() === 'preview' && !loading() && !error()) {
          <div class="preview-tab">
            <div class="preview-header">
              <h2>{{ 'privacy_policy.admin.preview_title' | translate }}</h2>
              <div class="preview-meta">
                @if (newPolicy.version) {
                  <span class="meta-badge">Version: {{ newPolicy.version }}</span>
                }
                <span class="meta-badge">Language: {{ editingLanguage() === 'pt' ? 'Português' : 'English' }}</span>
              </div>
            </div>
            <div class="preview-content">
              @if (newPolicy.content) {
                <div class="markdown-preview" [innerHTML]="renderedPreview()"></div>
              } @else {
                <p class="empty-preview">{{ 'privacy_policy.admin.empty_preview' | translate }}</p>
              }
            </div>
            <div class="preview-actions">
              <button class="btn btn-secondary" (click)="activeTab.set('editor')">
                ← {{ 'privacy_policy.admin.back_to_editor' | translate }}
              </button>
            </div>
          </div>
        }

        <!-- History Tab -->
        @if (activeTab() === 'history' && !loading() && !error()) {
          <div class="history-tab">
            <div class="history-header">
              <h2>{{ 'privacy_policy.admin.history_title' | translate }}</h2>
              <div class="history-lang-selector">
                <button 
                  class="lang-btn small"
                  [class.active]="historyLanguage() === 'pt'"
                  (click)="loadHistory('pt')">
                  🇵🇹
                </button>
                <button 
                  class="lang-btn small"
                  [class.active]="historyLanguage() === 'en'"
                  (click)="loadHistory('en')">
                  🇬🇧
                </button>
              </div>
            </div>

            @if (policyHistory().length === 0) {
              <div class="empty-history">
                <p>{{ 'privacy_policy.admin.no_history' | translate }}</p>
              </div>
            } @else {
              <div class="history-list">
                @for (policy of policyHistory(); track policy.policyId) {
                  <div class="history-item" [class.active]="policy.isActive">
                    <div class="history-item-header">
                      <span class="version-badge">v{{ policy.version }}</span>
                      @if (policy.isActive) {
                        <span class="active-badge">✓ {{ 'privacy_policy.admin.active' | translate }}</span>
                      }
                    </div>
                    <div class="history-item-details">
                      <p class="date">
                        <strong>{{ 'privacy_policy.effective_date' | translate }}:</strong> 
                        {{ policy.effectiveDate | date:'dd/MM/yyyy' }}
                      </p>
                      <p class="created">
                        <strong>{{ 'privacy_policy.admin.created_at' | translate }}:</strong> 
                        {{ policy.createdAt | date:'dd/MM/yyyy HH:mm' }}
                      </p>
                      @if (policy.changeSummary) {
                        <p class="summary">
                          <strong>{{ 'privacy_policy.admin.changes' | translate }}:</strong> 
                          {{ policy.changeSummary }}
                        </p>
                      }
                      <p class="acknowledgments">
                        <strong>{{ 'privacy_policy.admin.acknowledgments' | translate }}:</strong>
                        {{ policy.acknowledgmentCount }} {{ 'privacy_policy.admin.users' | translate }}
                      </p>
                    </div>
                    <div class="history-item-actions">
                      <button 
                        class="btn btn-sm btn-secondary"
                        (click)="viewPolicy(policy.policyId)">
                        {{ 'COMMON.VIEW' | translate }}
                      </button>
                      <button 
                        class="btn btn-sm btn-outline"
                        (click)="copyPolicyContent(policy.policyId)">
                        {{ 'privacy_policy.admin.use_as_base' | translate }}
                      </button>
                    </div>
                  </div>
                }
              </div>
            }
          </div>
        }
      </div>

      <!-- View Policy Modal -->
      @if (viewingPolicy()) {
        <div class="modal-overlay" (click)="closeViewModal()">
          <div class="modal-content" (click)="$event.stopPropagation()">
            <div class="modal-header">
              <h3>{{ 'privacy_policy.title' | translate }} - v{{ viewingPolicy()!.version }}</h3>
              <button class="modal-close" (click)="closeViewModal()">×</button>
            </div>
            <div class="modal-body">
              <div class="policy-meta">
                <span>{{ 'privacy_policy.effective_date' | translate }}: {{ viewingPolicy()!.effectiveDate | date:'dd/MM/yyyy' }}</span>
              </div>
              <div class="markdown-preview" [innerHTML]="renderMarkdown(viewingPolicy()!.content)"></div>
            </div>
            <div class="modal-footer">
              <button class="btn btn-secondary" (click)="closeViewModal()">
                {{ 'COMMON.CLOSE' | translate }}
              </button>
            </div>
          </div>
        </div>
      }

      <!-- Success Message -->
      @if (successMessage()) {
        <div class="toast toast-success">
          <span class="toast-icon">✓</span>
          {{ successMessage() }}
        </div>
      }
    </div>
  `,
  styles: [`
    .admin-container {
      min-height: 100vh;
      padding: 2rem;
      background: #f8f9fa;
    }

    .admin-header {
      max-width: 1200px;
      margin: 0 auto 2rem;
      text-align: center;
    }

    .admin-header h1 {
      color: #2c3e50;
      font-size: 2rem;
      margin-bottom: 0.5rem;
    }

    .subtitle {
      color: #6c757d;
      font-size: 1rem;
    }

    .admin-content {
      max-width: 1200px;
      margin: 0 auto;
      background: white;
      border-radius: 12px;
      box-shadow: 0 2px 10px rgba(0, 0, 0, 0.08);
      overflow: hidden;
    }

    /* Tab Navigation */
    .tab-nav {
      display: flex;
      background: #f1f3f5;
      border-bottom: 1px solid #dee2e6;
    }

    .tab-btn {
      flex: 1;
      padding: 1rem 1.5rem;
      background: none;
      border: none;
      font-size: 1rem;
      color: #6c757d;
      cursor: pointer;
      transition: all 0.2s;
      display: flex;
      align-items: center;
      justify-content: center;
      gap: 0.5rem;
    }

    .tab-btn:hover {
      background: #e9ecef;
      color: #495057;
    }

    .tab-btn.active {
      background: white;
      color: #2c3e50;
      font-weight: 600;
      border-bottom: 2px solid #3498db;
    }

    .tab-icon {
      font-size: 1.2rem;
    }

    /* Loading & Error States */
    .loading-state, .error-state {
      padding: 4rem 2rem;
      text-align: center;
    }

    .spinner {
      width: 40px;
      height: 40px;
      border: 3px solid #f3f3f3;
      border-top: 3px solid #3498db;
      border-radius: 50%;
      animation: spin 1s linear infinite;
      margin: 0 auto 1rem;
    }

    @keyframes spin {
      0% { transform: rotate(0deg); }
      100% { transform: rotate(360deg); }
    }

    .error-state {
      color: #e74c3c;
    }

    .error-icon {
      font-size: 3rem;
      display: block;
      margin-bottom: 1rem;
    }

    /* Editor Tab */
    .editor-tab {
      padding: 2rem;
    }

    .language-selector {
      display: flex;
      align-items: center;
      gap: 1rem;
      margin-bottom: 1.5rem;
      padding-bottom: 1.5rem;
      border-bottom: 1px solid #eee;
    }

    .language-selector label {
      font-weight: 500;
      color: #495057;
    }

    .lang-buttons {
      display: flex;
      gap: 0.5rem;
    }

    .lang-btn {
      padding: 0.5rem 1rem;
      border: 1px solid #dee2e6;
      background: white;
      border-radius: 6px;
      cursor: pointer;
      transition: all 0.2s;
    }

    .lang-btn:hover {
      border-color: #3498db;
    }

    .lang-btn.active {
      background: #3498db;
      color: white;
      border-color: #3498db;
    }

    .lang-btn.small {
      padding: 0.25rem 0.5rem;
      font-size: 1.2rem;
    }

    .current-info {
      display: flex;
      gap: 1rem;
      margin-bottom: 1.5rem;
    }

    .info-badge {
      padding: 0.5rem 1rem;
      background: #e8f4fd;
      color: #1a73e8;
      border-radius: 6px;
      font-size: 0.875rem;
    }

    .editor-form {
      display: flex;
      flex-direction: column;
      gap: 1.5rem;
    }

    .form-group {
      display: flex;
      flex-direction: column;
      gap: 0.5rem;
    }

    .form-group label {
      font-weight: 500;
      color: #495057;
    }

    .form-control {
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

    .content-editor {
      font-family: 'Monaco', 'Menlo', 'Ubuntu Mono', monospace;
      font-size: 0.9rem;
      line-height: 1.6;
      resize: vertical;
      min-height: 400px;
    }

    .help-text {
      color: #6c757d;
      font-size: 0.875rem;
    }

    .editor-toolbar {
      display: flex;
      gap: 0.5rem;
      flex-wrap: wrap;
      padding-top: 0.5rem;
    }

    .toolbar-btn {
      padding: 0.4rem 0.8rem;
      border: 1px solid #dee2e6;
      background: #f8f9fa;
      border-radius: 4px;
      cursor: pointer;
      font-size: 0.875rem;
      transition: all 0.2s;
    }

    .toolbar-btn:hover {
      background: #e9ecef;
      border-color: #adb5bd;
    }

    .form-actions {
      display: flex;
      gap: 1rem;
      justify-content: flex-end;
      padding-top: 1rem;
      border-top: 1px solid #eee;
    }

    /* Buttons */
    .btn {
      padding: 0.75rem 1.5rem;
      border: none;
      border-radius: 6px;
      font-size: 1rem;
      cursor: pointer;
      transition: all 0.2s;
      display: inline-flex;
      align-items: center;
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

    .btn-outline {
      background: transparent;
      border: 1px solid #3498db;
      color: #3498db;
    }

    .btn-outline:hover {
      background: #3498db;
      color: white;
    }

    .btn-sm {
      padding: 0.5rem 1rem;
      font-size: 0.875rem;
    }

    .btn-spinner {
      width: 16px;
      height: 16px;
      border: 2px solid rgba(255,255,255,0.3);
      border-top-color: white;
      border-radius: 50%;
      animation: spin 0.8s linear infinite;
    }

    /* Preview Tab */
    .preview-tab {
      padding: 2rem;
    }

    .preview-header {
      display: flex;
      justify-content: space-between;
      align-items: center;
      margin-bottom: 1.5rem;
      padding-bottom: 1rem;
      border-bottom: 1px solid #eee;
    }

    .preview-header h2 {
      color: #2c3e50;
      margin: 0;
    }

    .preview-meta {
      display: flex;
      gap: 0.5rem;
    }

    .meta-badge {
      padding: 0.25rem 0.75rem;
      background: #f1f3f5;
      border-radius: 4px;
      font-size: 0.875rem;
      color: #6c757d;
    }

    .preview-content {
      background: #fafafa;
      border: 1px solid #eee;
      border-radius: 8px;
      padding: 2rem;
      min-height: 400px;
    }

    .empty-preview {
      color: #6c757d;
      text-align: center;
      padding: 4rem;
      font-style: italic;
    }

    .preview-actions {
      margin-top: 1.5rem;
      text-align: left;
    }

    /* Markdown Preview Styles */
    .markdown-preview {
      line-height: 1.8;
      color: #333;
    }

    .markdown-preview h2 {
      color: #2c3e50;
      margin-top: 2rem;
      margin-bottom: 1rem;
      padding-bottom: 0.5rem;
      border-bottom: 2px solid #ecf0f1;
    }

    .markdown-preview h3 {
      color: #34495e;
      margin-top: 1.5rem;
      margin-bottom: 0.75rem;
    }

    .markdown-preview p {
      margin: 1rem 0;
    }

    .markdown-preview ul, .markdown-preview ol {
      margin: 1rem 0;
      padding-left: 2rem;
    }

    .markdown-preview li {
      margin: 0.5rem 0;
    }

    /* History Tab */
    .history-tab {
      padding: 2rem;
    }

    .history-header {
      display: flex;
      justify-content: space-between;
      align-items: center;
      margin-bottom: 1.5rem;
    }

    .history-header h2 {
      color: #2c3e50;
      margin: 0;
    }

    .history-lang-selector {
      display: flex;
      gap: 0.5rem;
    }

    .empty-history {
      padding: 4rem;
      text-align: center;
      color: #6c757d;
    }

    .history-list {
      display: flex;
      flex-direction: column;
      gap: 1rem;
    }

    .history-item {
      padding: 1.5rem;
      border: 1px solid #dee2e6;
      border-radius: 8px;
      background: #fafafa;
      transition: all 0.2s;
    }

    .history-item:hover {
      border-color: #3498db;
      box-shadow: 0 2px 8px rgba(0, 0, 0, 0.05);
    }

    .history-item.active {
      border-color: #27ae60;
      background: #f0fff4;
    }

    .history-item-header {
      display: flex;
      align-items: center;
      gap: 1rem;
      margin-bottom: 1rem;
    }

    .version-badge {
      padding: 0.25rem 0.75rem;
      background: #2c3e50;
      color: white;
      border-radius: 4px;
      font-weight: 600;
    }

    .active-badge {
      padding: 0.25rem 0.75rem;
      background: #27ae60;
      color: white;
      border-radius: 4px;
      font-size: 0.875rem;
    }

    .history-item-details {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
      gap: 0.5rem;
      font-size: 0.9rem;
      color: #495057;
    }

    .history-item-details p {
      margin: 0;
    }

    .history-item-actions {
      display: flex;
      gap: 0.5rem;
      margin-top: 1rem;
      padding-top: 1rem;
      border-top: 1px solid #eee;
    }

    /* Modal */
    .modal-overlay {
      position: fixed;
      top: 0;
      left: 0;
      right: 0;
      bottom: 0;
      background: rgba(0, 0, 0, 0.5);
      display: flex;
      align-items: center;
      justify-content: center;
      z-index: 1000;
    }

    .modal-content {
      background: white;
      border-radius: 12px;
      width: 90%;
      max-width: 800px;
      max-height: 80vh;
      overflow: hidden;
      display: flex;
      flex-direction: column;
    }

    .modal-header {
      display: flex;
      justify-content: space-between;
      align-items: center;
      padding: 1.5rem;
      background: #2c3e50;
      color: white;
    }

    .modal-header h3 {
      margin: 0;
    }

    .modal-close {
      background: none;
      border: none;
      color: white;
      font-size: 1.5rem;
      cursor: pointer;
      padding: 0;
      line-height: 1;
    }

    .modal-body {
      padding: 1.5rem;
      overflow-y: auto;
      flex: 1;
    }

    .policy-meta {
      margin-bottom: 1rem;
      padding-bottom: 1rem;
      border-bottom: 1px solid #eee;
      color: #6c757d;
    }

    .modal-footer {
      padding: 1rem 1.5rem;
      background: #f8f9fa;
      border-top: 1px solid #eee;
      display: flex;
      justify-content: flex-end;
    }

    /* Toast */
    .toast {
      position: fixed;
      bottom: 2rem;
      right: 2rem;
      padding: 1rem 1.5rem;
      border-radius: 8px;
      display: flex;
      align-items: center;
      gap: 0.5rem;
      animation: slideIn 0.3s ease;
      z-index: 1100;
    }

    .toast-success {
      background: #27ae60;
      color: white;
    }

    .toast-icon {
      font-size: 1.2rem;
    }

    @keyframes slideIn {
      from {
        transform: translateX(100%);
        opacity: 0;
      }
      to {
        transform: translateX(0);
        opacity: 1;
      }
    }
  `]
})
export class PrivacyPolicyAdminComponent implements OnInit {
  private policyService = inject(PrivacyPolicyService);
  private translationService = inject(TranslationService);
  private modalService = inject(MessageModalService);

  // State signals
  loading = signal(false);
  publishing = signal(false);
  error = signal<string | null>(null);
  successMessage = signal<string | null>(null);

  activeTab = signal<'editor' | 'preview' | 'history'>('editor');
  editingLanguage = signal<'pt' | 'en'>('pt');
  historyLanguage = signal<'pt' | 'en'>('pt');

  currentPolicy = signal<PrivacyPolicy | null>(null);
  policyHistory = signal<PrivacyPolicySummary[]>([]);
  viewingPolicy = signal<PrivacyPolicy | null>(null);

  // Form data
  newPolicy: PublishPrivacyPolicyRequest = {
    version: '',
    content: '',
    changeSummary: '',
    effectiveDate: undefined,
    languageCode: 'pt'
  };

  // Computed
  canPublish = computed(() => {
    return this.newPolicy.version.trim() !== '' && 
           this.newPolicy.content.trim() !== '' &&
           !this.publishing();
  });

  renderedPreview = computed(() => {
    return this.renderMarkdown(this.newPolicy.content);
  });

  ngOnInit(): void {
    this.loadData();
  }

  loadData(): void {
    this.loading.set(true);
    this.error.set(null);

    // Load current policy for the editing language
    this.policyService.getCurrentPolicy(this.editingLanguage()).subscribe({
      next: (policy) => {
        this.currentPolicy.set(policy);
        this.loading.set(false);
      },
      error: (err) => {
        console.error('Error loading current policy:', err);
        // Not an error if no policy exists
        this.currentPolicy.set(null);
        this.loading.set(false);
      }
    });

    // Load history
    this.loadHistory(this.historyLanguage());
  }

  loadHistory(lang: 'pt' | 'en'): void {
    this.historyLanguage.set(lang);
    this.policyService.getPolicyHistory(lang).subscribe({
      next: (history) => {
        this.policyHistory.set(history);
      },
      error: (err) => {
        console.error('Error loading policy history:', err);
      }
    });
  }

  switchLanguage(lang: 'pt' | 'en'): void {
    this.editingLanguage.set(lang);
    this.newPolicy.languageCode = lang;
    
    // Load current policy for new language
    this.policyService.getCurrentPolicy(lang).subscribe({
      next: (policy) => {
        this.currentPolicy.set(policy);
      },
      error: () => {
        this.currentPolicy.set(null);
      }
    });
  }

  loadCurrentContent(): void {
    if (this.currentPolicy()) {
      this.newPolicy.content = this.currentPolicy()!.content;
      this.showSuccess('Current policy content loaded into editor');
    } else {
      this.modalService.showInfo(
        'No Current Policy',
        'There is no current policy for this language. Start fresh!'
      );
    }
  }

  insertMarkdown(prefix: string, suffix: string = ''): void {
    const textarea = document.getElementById('content') as HTMLTextAreaElement;
    if (textarea) {
      const start = textarea.selectionStart;
      const end = textarea.selectionEnd;
      const selectedText = this.newPolicy.content.substring(start, end);
      const newText = prefix + selectedText + suffix;
      
      this.newPolicy.content = 
        this.newPolicy.content.substring(0, start) + 
        newText + 
        this.newPolicy.content.substring(end);
      
      // Reset cursor position
      setTimeout(() => {
        textarea.focus();
        textarea.setSelectionRange(start + prefix.length, start + prefix.length + selectedText.length);
      }, 0);
    }
  }

  confirmPublish(): void {
    this.modalService.showConfirm(
      'Publish New Policy Version',
      `Are you sure you want to publish version ${this.newPolicy.version} (${this.newPolicy.languageCode.toUpperCase()})?\n\nThis will:\n• Make this the active privacy policy\n• Require all users to acknowledge the new policy\n• Record this version in history`,
      () => this.publishPolicy(),
      () => console.log('Publish cancelled')
    );
  }

  publishPolicy(): void {
    if (!this.canPublish()) return;

    this.publishing.set(true);
    
    const request: PublishPrivacyPolicyRequest = {
      ...this.newPolicy,
      effectiveDate: this.newPolicy.effectiveDate || new Date()
    };

    this.policyService.publishPolicy(request).subscribe({
      next: (policy) => {
        this.publishing.set(false);
        this.showSuccess(`Privacy Policy v${policy.version} published successfully!`);
        
        // Reset form
        this.newPolicy = {
          version: '',
          content: '',
          changeSummary: '',
          effectiveDate: undefined,
          languageCode: this.editingLanguage()
        };
        
        // Refresh data
        this.loadData();
      },
      error: (err) => {
        this.publishing.set(false);
        console.error('Error publishing policy:', err);
        this.modalService.showError(
          'Publish Failed',
          'Failed to publish the new policy. Please try again.'
        );
      }
    });
  }

  viewPolicy(policyId: string): void {
    this.policyService.getPolicyById(policyId).subscribe({
      next: (policy) => {
        this.viewingPolicy.set(policy);
      },
      error: (err) => {
        console.error('Error loading policy:', err);
        this.modalService.showError('Error', 'Failed to load policy details');
      }
    });
  }

  closeViewModal(): void {
    this.viewingPolicy.set(null);
  }

  copyPolicyContent(policyId: string): void {
    this.policyService.getPolicyById(policyId).subscribe({
      next: (policy) => {
        if (policy) {
          this.newPolicy.content = policy.content;
          this.activeTab.set('editor');
          this.showSuccess('Policy content copied to editor. Update and publish as new version.');
        }
      },
      error: (err) => {
        console.error('Error copying policy:', err);
      }
    });
  }

  renderMarkdown(content: string): string {
    if (!content) return '';
    
    // Simple markdown rendering (basic implementation)
    // In production, use a proper markdown library like marked
    return content
      // Headers
      .replace(/^### (.*$)/gim, '<h3>$1</h3>')
      .replace(/^## (.*$)/gim, '<h2>$1</h2>')
      .replace(/^# (.*$)/gim, '<h1>$1</h1>')
      // Bold
      .replace(/\*\*(.*?)\*\*/g, '<strong>$1</strong>')
      // Italic
      .replace(/\*(.*?)\*/g, '<em>$1</em>')
      // Unordered lists
      .replace(/^\s*- (.*$)/gim, '<li>$1</li>')
      // Ordered lists
      .replace(/^\s*\d+\. (.*$)/gim, '<li>$1</li>')
      // Paragraphs
      .replace(/\n\n/g, '</p><p>')
      // Line breaks
      .replace(/\n/g, '<br>')
      // Wrap in paragraph
      .replace(/^(.*)$/, '<p>$1</p>')
      // Clean up list items
      .replace(/<\/li><br><li>/g, '</li><li>')
      .replace(/<p><li>/g, '<ul><li>')
      .replace(/<\/li><\/p>/g, '</li></ul>');
  }

  private showSuccess(message: string): void {
    this.successMessage.set(message);
    setTimeout(() => this.successMessage.set(null), 4000);
  }
}
