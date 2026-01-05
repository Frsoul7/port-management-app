/**
 * Shared TypeScript types and interfaces
 */

// Status enums for different aggregates
export enum OperationPlanStatus {
  GENERATED = 'GENERATED',
  APPROVED = 'APPROVED',
  IN_EXECUTION = 'IN_EXECUTION',
  COMPLETED = 'COMPLETED',
  OUTDATED = 'OUTDATED',
}

export enum VesselVisitExecutionStatus {
  PLANNED = 'PLANNED',
  IN_PROGRESS = 'IN_PROGRESS',
  COMPLETED = 'COMPLETED',
  DISRUPTED = 'DISRUPTED',
}

export enum IncidentStatus {
  REPORTED = 'REPORTED',
  UNDER_INVESTIGATION = 'UNDER_INVESTIGATION',
  RESOLVED = 'RESOLVED',
  CLOSED = 'CLOSED',
}

export enum IncidentSeverity {
  LOW = 'LOW',
  MEDIUM = 'MEDIUM',
  HIGH = 'HIGH',
  CRITICAL = 'CRITICAL',
}

export enum IncidentCategory {
  ENVIRONMENTAL = 'ENVIRONMENTAL',
  OPERATIONAL = 'OPERATIONAL',
  SAFETY = 'SAFETY',
  OTHER = 'OTHER',
}

export enum ComplementaryTaskStatus {
  PLANNED = 'PLANNED',
  IN_PROGRESS = 'IN_PROGRESS',
  COMPLETED = 'COMPLETED',
  CANCELLED = 'CANCELLED',
}

export enum ExecutedOperationStatus {
  STARTED = 'STARTED',
  COMPLETED = 'COMPLETED',
  DELAYED = 'DELAYED',
}

export enum OperationType {
  LOAD = 'LOAD',
  UNLOAD = 'UNLOAD',
  BOTH = 'BOTH',
  MAINTENANCE = 'MAINTENANCE',
}

export enum PlanningAlgorithm {
  OPTIMAL = 'optimal',
  WEIGHTED = 'weighted',
  MULTI_CRANES = 'multi_cranes',
}

// JWT Payload interface (matches Core Backend)
export interface JwtPayload {
  sub: string; // User ID (Core Backend uses 'sub' instead of 'userId')
  email: string;
  name: string;
  role: string; // "ADMINISTRATOR" | "LOGISTICS_OPERATOR" | "SHIPPING_AGENT" | "PORT_AUTHORITY"
  organizationId?: string;
  org_id?: string; // Alternative organization ID field
  organizationName?: string;
  org_name?: string; // Alternative organization name field
  nbf?: number; // Not before
  iat: number; // Issued at
  exp: number; // Expiration
}

// Request with authenticated user
export interface AuthenticatedRequest extends Request {
  user?: JwtPayload;
}

// API Response wrappers
export interface ApiResponse<T = unknown> {
  success: boolean;
  data?: T;
  error?: string;
  message?: string;
}

export interface PaginatedResponse<T> extends ApiResponse<T[]> {
  pagination: {
    page: number;
    limit: number;
    total: number;
    totalPages: number;
  };
}

// Common filter and sort options
export interface QueryOptions {
  page?: number;
  limit?: number;
  offset?: number;
  sortBy?: string;
  sortOrder?: 'asc' | 'desc';
}

// Pagination metadata
export interface PaginationMetadata {
  page: number;
  limit: number;
  total: number;
  totalPages: number;
}

// Generic paginated result for service layer
export interface PaginatedResult<T> {
  data: T[];
  pagination: PaginationMetadata;
}

// External VVN reference (from Core Backend)
export interface VvnReference {
  vvnId: string;
  vesselImo: string;
  eta: Date;
  etd: Date;
  loadingCount: number;
  unloadingCount: number;
  purpose: OperationType;
  state: string; // "APPROVED"
}

// For future Prolog integration
export interface PlanningVesselInput {
  name: string; // VVN ID
  arrival: number; // Hours since midnight
  departure: number; // Hours since midnight
  unload: number; // Unloading time in hours
  load: number; // Loading time in hours
}

export interface PlanningSequenceResult {
  vessel: string; // VVN ID
  start: number; // Start time in hours
  end: number; // End time in hours
  cranes?: number; // Number of cranes (for multi_cranes algorithm)
}

export interface PlanningResponse {
  sequence: PlanningSequenceResult[];
  total_delay: number;
  hours_with_2_cranes?: number; // For multi_cranes only
  total_cranes_needed?: number; // For multi_cranes only
}

// Vessel Visit Execution Metrics
export interface VveMetrics {
  vveId: string;
  totalTurnaroundTime: number | null; // Hours from port arrival to departure
  berthOccupancyTime: number | null; // Hours from berth to unberth
  waitingTimeForBerthing: number | null; // Hours from arrival to berth
  totalOperationsTime: number | null; // Hours spent on operations
  operationsCount: number;
  incidentsCount: number;
  totalContainersProcessed: number;
  status: VesselVisitExecutionStatus;
}
