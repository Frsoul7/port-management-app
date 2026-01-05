export enum UserRole {
  PORT_AUTHORITY_OFFICER = 'PORT_AUTHORITY_OFFICER',
  SHIPPING_AGENT_REPRESENTATIVE = 'SHIPPING_AGENT_REPRESENTATIVE',
  LOGISTICS_OPERATOR = 'LOGISTICS_OPERATOR',
  ADMINISTRATOR = 'ADMINISTRATOR'
}

export interface User {
  id: string;
  username: string;
  email: string;
  fullName: string;
  role: UserRole;
  organizationId?: string;
  organizationName?: string;
  profilePictureUrl?: string;
}

export interface AuthToken {
  access_token: string;
  refresh_token?: string;
  expires_in: number;
  token_type: string;
}

export interface AuthHeaders {
  [key: string]: string;
}
