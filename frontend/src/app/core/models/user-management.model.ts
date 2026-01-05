export interface User {
  userId: string;
  name: string;
  email: string;
  organizationId: string;
  organizationName: string;
  role: string;
  profilePictureUrl?: string;
  emailVerified: boolean;
  isActive: boolean;
}

export interface UpdateUserRequest {
  name: string;
  organizationId: string;
  role: string;
}

export interface Organization {
  id: string;
  organizationId: string;
  legalName: string;
  type: string;
}

export const AVAILABLE_ROLES = [
  { value: 'SHIPPING_AGENT_REPRESENTATIVE', label: 'Shipping Agent Representative' },
  { value: 'PORT_AUTHORITY_OFFICER', label: 'Port Authority Officer' },
  { value: 'LOGISTICS_OPERATOR', label: 'Logistics Operator' },
  { value: 'ADMINISTRATOR', label: 'Administrator' }
];
