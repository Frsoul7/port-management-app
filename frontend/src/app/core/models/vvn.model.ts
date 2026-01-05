export enum VvnStatus {
  IN_PROGRESS = 'IN_PROGRESS',
  SUBMITTED = 'SUBMITTED',
  APPROVED = 'APPROVED',
  REJECTED = 'REJECTED'
}

export enum VvnPurpose {
  LOAD = 'LOAD',
  UNLOAD = 'UNLOAD',
  BOTH = 'BOTH',
  MAINTENANCE = 'MAINTENANCE'
}

export enum ManifestType {
  Load = 'Load',
  Unload = 'Unload'
}

export enum DockAssignmentStatus {
  ASSIGNED = 'ASSIGNED',
  IN_PROGRESS = 'IN_PROGRESS',
  COMPLETED = 'COMPLETED',
  CANCELLED = 'CANCELLED'
}

export interface DockAssignmentInfo {
  dockAssignmentId: string;
  dockCode: string;
  berthFrom: string;
  berthTo: string;
  status: string;
}

export interface VvnStatusResponse {
  id: string;
  vvnBusinessId: string;
  status: string;
  vesselImo: string;
  purpose: string;
  eta: string;
  etd: string;
  createdAt: string;
  submittedAt?: string;
  approvedAt?: string;
  rejectedAt?: string;
  submittedById?: string;
  captainName: string;
  captainCitizenId: string;
  captainNationality: string;
  crewCount: number;
  crewMembers: CrewMemberDto[];
  loadingCount: number;
  unloadingCount: number;
  dockAssignment?: DockAssignmentInfo;
  approvedById?: string;
  rejectionReason?: string;
  rejectedById?: string;
}

export interface VvnSummaryResponse {
  id: string;
  vvnBusinessId: string;
  status: string;
  purpose: string;
  eta: string;
  etd: string;
  submittedAt?: string;
  vesselImo?: string;
  vesselName?: string;
  vesselType?: string;
  assignedDock?: string;
}

export interface CreateVvnRequest {
  vesselImo: string;
  purpose: string;
  eta: string;
  etd: string;
  captainName: string;
  captainCitizenId: string;
  captainNationality: string;
  crewCount: number;
}

export interface UpdateVvnRequest {
  purpose?: string;
  eta?: string;
  etd?: string;
  captainName?: string;
  captainCitizenId?: string;
  captainNationality?: string;
  crewCount?: number;
}

export interface ManifestEntryDto {
  containerCode: string;
  bay: number;
  row: number;
  tier: number;
  goodsDescription?: string;
  hazardous: boolean;
}

export interface ManifestDto {
  entries: ManifestEntryDto[];
}

export interface UpsertManifestsRequest {
  loading?: ManifestDto;
  unloading?: ManifestDto;
}

export interface AddManifestEntryRequest {
  type: ManifestType;
  containerCode: string;
  bay: number;
  row: number;
  tier: number;
  goodsDescription?: string;
  hazardous: boolean;
}

export interface CrewMemberDto {
  name: string;
  citizenId: string;
  nationality: string;
}

export interface SetCrewRequest {
  captainName?: string;
  members: CrewMemberDto[];
}

export interface VvnApprovalRequest {
  dockCode: string;
  berthFrom: string;
  berthTo: string;
  notes?: string;
}

export interface VvnApprovalResponse {
  vvnId: string;
  vvnBusinessId: string;
  status: string;
  assignedDock: string;
  berthFrom: string;
  berthTo: string;
  dockAssignmentId: string;
  approvedByOrgId: string;
  approvedByOrgName: string;
  approvedByUserId?: string;
  approvedAt: string;
  submittedByOrgId: string;
  submittedByOrgName: string;
  submittedByUserId?: string;
  submittedAt?: string;
}

export interface VvnRejectionRequest {
  reason: string;
  notes?: string;
}

export interface VvnRejectionResponse {
  vvnId: string;
  vvnBusinessId: string;
  status: string;
  rejectionReason: string;
  rejectedByOrgId: string;
  rejectedByOrgName: string;
  rejectedByUserId?: string;
  rejectedAt: string;
  submittedByOrgId: string;
  submittedByOrgName: string;
  submittedByUserId?: string;
  submittedAt?: string;
}

export interface VvnEntryResponse {
  id: string;
  type: string; // "Load" or "Unload"
  containerCode: string;
  bay: number;
  row: number;
  tier: number;
  goodsDescription?: string;
  hazardous: boolean;
}

export interface VvnSearchParams {
  vesselImo?: string;
  status?: string;
  submittedById?: string;
  fromDate?: string;
  toDate?: string;
}
