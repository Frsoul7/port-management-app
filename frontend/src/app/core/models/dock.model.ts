export interface Dock {
  code: string;
  name: string;
  location: string;
  lengthM: number;
  depthM: number;
  maxDraftM: number;
  allowedVesselTypeIds: string[];
  allowedVesselTypes: VesselTypeInfo[];
}

export interface VesselTypeInfo {
  vesselTypeId: string;
  name: string;
  description?: string;
}

export interface CreateDockRequest {
  code: string;
  name: string;
  location: string;
  lengthM: number;
  depthM: number;
  maxDraftM: number;
  allowedVesselTypeIds?: string[];
}

export interface UpdateDockRequest {
  name: string;
  location: string;
  lengthM: number;
  depthM: number;
  maxDraftM: number;
  allowedVesselTypeIds?: string[];
}

export interface DockSearchParams {
  name?: string;
  location?: string;
  vesselTypeId?: string;
}
