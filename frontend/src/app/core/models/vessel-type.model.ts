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

export interface CreateVesselTypeRequest {
  name: string;
  description?: string;
  capacityTEU?: number;
  maxRows?: number;
  maxBays?: number;
  maxTiers?: number;
  operationalConstraints?: string;
  vesselTypeId?: string; // Optional: if not provided, backend will generate
}

export interface UpdateVesselTypeRequest {
  name?: string;
  description?: string;
  capacityTEU?: number;
  maxRows?: number;
  maxBays?: number;
  maxTiers?: number;
  operationalConstraints?: string;
}

export interface VesselTypeSearchParams {
  name?: string;
  description?: string;
  page?: number;
  pageSize?: number;
}

export interface VesselTypeSearchResponse {
  total: number;
  page: number;
  pageSize: number;
  items: VesselType[];
}
