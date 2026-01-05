import { Injectable, inject } from '@angular/core';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';
import { ApiService } from './api.service';
import {
  PrivacyPolicy,
  PrivacyPolicySummary,
  PublishPrivacyPolicyRequest,
  AcknowledgmentStatus,
  AcknowledgePolicyRequest,
  AcknowledgmentResponse,
  PolicyAcknowledgment
} from '../models/privacy-policy.model';

/**
 * Service for Privacy Policy operations
 * US 4.5.1: Privacy Policy management by administrators
 * US 4.5.2: Privacy information access for all users
 */
@Injectable({
  providedIn: 'root'
})
export class PrivacyPolicyService {
  private api = inject(ApiService);
  private readonly basePath = 'PrivacyPolicy';

  // ===== Public Operations (US 4.5.2) =====

  /**
   * Get the current active privacy policy
   * Public - no authentication required
   */
  getCurrentPolicy(lang: string = 'pt'): Observable<PrivacyPolicy | null> {
    return this.api.get<PrivacyPolicy>(`${this.basePath}/current`, { lang });
  }

  // ===== User Operations =====

  /**
   * Check if the current user needs to acknowledge a new policy
   * Called on login to determine if modal should be shown
   */
  checkAcknowledgmentRequired(lang: string = 'pt'): Observable<AcknowledgmentStatus> {
    return this.api.get<AcknowledgmentStatus>(`${this.basePath}/acknowledgment-required`, { lang });
  }

  /**
   * Record user's acknowledgment of a privacy policy
   */
  acknowledgePolicy(policyId: string): Observable<AcknowledgmentResponse> {
    return this.api.post<AcknowledgmentResponse>(`${this.basePath}/acknowledge`, {
      policyId
    } as AcknowledgePolicyRequest);
  }

  // ===== Admin Operations (US 4.5.1) =====

  /**
   * Get all privacy policy versions (history)
   * Admin only
   */
  getPolicyHistory(lang?: string): Observable<PrivacyPolicySummary[]> {
    const params = lang ? { lang } : {};
    return this.api.get<PrivacyPolicySummary[]>(`${this.basePath}/history`, params);
  }

  /**
   * Get a specific policy by ID
   * Admin only
   */
  getPolicyById(policyId: string): Observable<PrivacyPolicy | null> {
    return this.api.get<PrivacyPolicy>(`${this.basePath}/${policyId}`);
  }

  /**
   * Publish a new privacy policy version
   * Admin only - triggers notification requirement for all users
   */
  publishPolicy(request: PublishPrivacyPolicyRequest): Observable<PrivacyPolicy> {
    return this.api.post<PrivacyPolicy>(this.basePath, request);
  }

  /**
   * Get acknowledgments for a specific policy
   * Admin only - for reporting purposes
   */
  getPolicyAcknowledgments(policyId: string): Observable<PolicyAcknowledgment[]> {
    return this.api.get<PolicyAcknowledgment[]>(`${this.basePath}/${policyId}/acknowledgments`);
  }
}
