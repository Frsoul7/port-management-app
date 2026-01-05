export enum PhysicalResourceAvailability {
  AVAILABLE = 'AVAILABLE',
  MAINTENANCE = 'MAINTENANCE',
  TEMP_OUT_OF_SERVICE = 'TEMP_OUT_OF_SERVICE'
}

export enum MobileEquipmentType {
  TRUCK = 'TRUCK',
  YARD_GANTRY_CRANE = 'YARD_GANTRY_CRANE'
}

export interface QualificationInfo {
  qualificationId: string;
  name: string;
}

export interface PhysicalResource {
  resourceId: string;
  code: string;
  description: string | null;
  resourceType: 'STS_CRANE' | 'MOBILE_EQUIPMENT';
  availability: PhysicalResourceAvailability;
  setupTimeSeconds: number;
  requiredQualifications: QualificationInfo[];
  createdAt: string;
  deactivatedAt: string | null;
  deactivationReason: string | null;
  // STS Crane specific
  avgContainersPerHour: number | null;
  installedAtDockCode: string | null;
  // Mobile Equipment specific
  mobileEquipmentType: MobileEquipmentType | null;
  maxSpeedKph: number | null;
  containersPerTrip: number | null;
  currentDockCode: string | null;
}

export interface CreatePhysicalResourceRequest {
  code: string;
  description: string | null;
  setupTimeSeconds: number;
  resourceType: 'STS_CRANE' | 'MOBILE_EQUIPMENT';
  // STS Crane specific
  avgContainersPerHour?: number;
  installedAtDockCode?: string;
  // Mobile Equipment specific
  mobileType?: MobileEquipmentType;
  maxSpeedKph?: number;
  containersPerTrip?: number;
  // Common
  requiredQualificationIds?: string[];
}

export interface UpdatePhysicalResourceRequest {
  description: string | null;
  setupTimeSeconds: number;
  // STS Crane specific
  avgContainersPerHour?: number;
  installedAtDockCode?: string;
  // Mobile Equipment specific
  mobileType?: MobileEquipmentType;
  maxSpeedKph?: number;
  containersPerTrip?: number;
  // Common
  requiredQualificationIds?: string[];
}

export interface UpdateAvailabilityRequest {
  availability: PhysicalResourceAvailability;
}

export interface PhysicalResourceSearchParams {
  code?: string;
  description?: string;
  availability?: PhysicalResourceAvailability;
}

export interface DockInfo {
  code: string;
  name: string;
}
