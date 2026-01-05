/**
 * Privacy Policy DTOs and interfaces
 * US 4.5.1: Privacy Policy management
 * US 4.5.2: Privacy information access
 */

export interface PrivacyPolicy {
  policyId: string;
  version: string;
  content: string;
  changeSummary?: string;
  effectiveDate: Date;
  createdAt: Date;
  languageCode: string;
  isActive: boolean;
}

export interface PrivacyPolicySummary {
  policyId: string;
  version: string;
  changeSummary?: string;
  effectiveDate: Date;
  createdAt: Date;
  languageCode: string;
  isActive: boolean;
  acknowledgmentCount: number;
}

export interface PublishPrivacyPolicyRequest {
  version: string;
  content: string;
  changeSummary?: string;
  effectiveDate?: Date;
  languageCode: string;
}

export interface AcknowledgmentStatus {
  acknowledgmentRequired: boolean;
  currentPolicyVersion?: string;
  currentPolicyId?: string;
  lastAcknowledgedAt?: Date;
  lastAcknowledgedVersion?: string;
}

export interface AcknowledgePolicyRequest {
  policyId: string;
}

export interface AcknowledgmentResponse {
  acknowledgmentId: string;
  policyId: string;
  policyVersion: string;
  acknowledgedAt: Date;
}

export interface PolicyAcknowledgment {
  acknowledgmentId: string;
  userId: string;
  policyId: string;
  policyVersion: string;
  acknowledgedAt: Date;
  ipAddress?: string;
}
