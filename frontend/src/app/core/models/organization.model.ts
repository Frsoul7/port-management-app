export enum OrganizationType {
  SHIPPING_AGENT = 0,
  PORT_AUTHORITY = 1,
  LOGISTICS_OPERATOR = 2,
  ADMINISTRATOR = 3
}

export interface Representative {
  id: string;
  name: string;
  citizenId: string;
  nationality: string;
  email: string;
  phone: string;
  isActive: boolean;
  createdAt: string;
}

export interface Organization {
  id: string;
  organizationId: string;
  identifier: string;
  legalName: string;
  alternativeName: string;
  address: string;
  taxNumber: string;
  type: string;
  representatives: Representative[];
}

export interface RepresentativeInput {
  name: string;
  citizenId: string;
  nationality: string;
  email: string;
  phone: string;
}

export interface CreateOrganizationRequest {
  identifier: string;
  legalName: string;
  alternativeName: string;
  address: string;
  taxNumber: string;
  type: OrganizationType;
  representatives?: RepresentativeInput[];
}

export interface AddRepresentativesRequest {
  representatives: RepresentativeInput[];
}

export interface UpdateRepresentativeRequest {
  name?: string;
  citizenId?: string;
  nationality?: string;
  email?: string;
  phone?: string;
}

export interface PatchRepresentativeStatusRequest {
  isActive: boolean;
}

export interface OrganizationListItem {
  id: string;
  organizationId: string;
  identifier: string;
  legalName: string;
  type: string;
}
