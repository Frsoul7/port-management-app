export interface Vessel {
  imoNumber: string;
  name: string;
  vesselTypeId: string;
  vesselTypeName?: string;
  organizationId: string;
  organizationName?: string;
  capacityTEU?: number;
}

export interface CreateVesselRequest {
  imoNumber: string;
  name: string;
  vesselTypeId: string;
  organizationId: string;
  capacityTEU?: number;
}

export interface UpdateVesselRequest {
  name: string;
  vesselTypeId: string;
  organizationId: string;
  capacityTEU?: number;
}

export interface VesselType {
  vesselTypeId: string;
  name: string;
  description?: string;
  capacityTEU?: number;
  maxRows?: number;
  maxBays?: number;
  maxTiers?: number;
  operationalConstraints?: string;
}

export interface Organization {
  id: string;
  legalName: string;
  type: string;
}

export interface VesselSearchParams {
  imo?: string;
  name?: string;
  organizationName?: string;
}
