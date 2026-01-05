import { IncidentStatus, IncidentSeverity, IncidentCategory } from '@shared/types';

/**
 * DTO for Incident note
 */
export interface IncidentNoteDto {
  timestamp: string; // ISO 8601 date-time string
  author: string;
  content: string;
}

/**
 * DTO for creating a new Incident
 */
export interface CreateIncidentDto {
  incidentTypeId: string;
  vveId?: string; // Optional: link to VesselVisitExecution
  title: string;
  description: string;
  severity: IncidentSeverity;
  reportedBy: string;
  impactDescription?: string;
  externalEntitiesInvolved?: string[];
  affectedEntities?: string[]; // e.g., ['CRANE-1', 'DOCK-A']
}

/**
 * DTO for updating an Incident
 */
export interface UpdateIncidentDto {
  title?: string;
  description?: string;
  severity?: IncidentSeverity;
  impactDescription?: string;
}

/**
 * DTO for starting investigation on an incident
 */
export interface StartInvestigationDto {
  investigatedBy: string;
}

/**
 * DTO for Incident response
 */
export interface IncidentDto {
  incidentId: string;
  incidentTypeId: string;
  vveId?: string;
  title: string;
  description: string;
  severity: IncidentSeverity;
  status: IncidentStatus;
  reportedAt: string; // ISO 8601 date-time string
  reportedBy: string;
  assignedTo?: string;
  investigatedAt?: string;
  investigatedBy?: string;
  affectedEntities: string[];
  externalEntitiesInvolved?: string[];
  resolutionNotes?: string;
  resolutionSummary?: string;
  resolvedAt?: string;
  resolvedBy?: string;
  closedAt?: string;
  closedBy?: string;
  impactDescription?: string;
  durationMinutes?: number;
  notes: IncidentNoteDto[];
  createdAt: string;
}

/**
 * DTO for updating incident assignment
 */
export interface AssignIncidentDto {
  assignedTo: string;
}

/**
 * DTO for adding a note to an incident
 */
export interface AddIncidentNoteDto {
  content: string;
  author: string;
}

/**
 * DTO for resolving an incident
 */
export interface ResolveIncidentDto {
  resolutionNotes?: string;
  resolutionSummary: string;
  resolvedBy: string;
}

/**
 * DTO for closing an incident
 */
export interface CloseIncidentDto {
  closedBy: string;
}

/**
 * DTO for filtering incidents
 */
export interface IncidentFilterDto {
  status?: IncidentStatus;
  severity?: IncidentSeverity;
  incidentTypeId?: string;
  vveId?: string;
  reportedBy?: string;
  fromDate?: Date;
  toDate?: Date;
  externalEntity?: string;
}

/**
 * DTO for incident statistics
 */
export interface IncidentStatisticsDto {
  total: number;
  byStatus: Record<IncidentStatus, number>;
  bySeverity: Record<IncidentSeverity, number>;
  averageResolutionTimeHours: number | null;
}

// Note: IncidentTypeDto, CreateIncidentTypeDto, UpdateIncidentTypeDto are defined in IncidentTypeDto.ts

/**
 * Query parameters for listing incidents with pagination
 */
export interface ListIncidentsQuery {
  // Filter parameters
  status?: string;
  severity?: string;
  incidentTypeId?: string;
  vveId?: string;
  reportedBy?: string;
  fromDate?: string;
  toDate?: string;
  externalEntity?: string;
  // Pagination parameters
  page?: number;
  limit?: number;
  sortBy?: string;
  sortOrder?: 'asc' | 'desc';
}
