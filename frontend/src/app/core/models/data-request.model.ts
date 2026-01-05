/**
 * Non-User Data Request DTOs and interfaces
 * US 4.5.4: Non-User Data Rights Information
 * 
 * For individuals whose data is processed via VVN but who are not registered users
 * (crew members, captains, vessel representatives)
 */

export enum DataRequestType {
  ACCESS = 'ACCESS',
  RECTIFICATION = 'RECTIFICATION',
  DELETION = 'DELETION',
  OBJECTION = 'OBJECTION',
  OTHER = 'OTHER'
}

export interface NonUserDataRequestDto {
  fullName: string;
  email: string;
  phone?: string;
  requestType: DataRequestType;
  vesselReference?: string;
  vvnReference?: string;
  description: string;
  /** Consent to process this request */
  consentGiven: boolean;
}

export interface NonUserDataRequestResponse {
  requestId: string;
  status: 'SUBMITTED' | 'PENDING' | 'IN_PROGRESS' | 'COMPLETED' | 'REJECTED';
  submittedAt: Date;
  referenceNumber: string;
  estimatedResponseDays: number;
  message: string;
}
