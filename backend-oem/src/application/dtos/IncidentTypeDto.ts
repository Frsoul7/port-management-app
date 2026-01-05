/**
 * DTOs for IncidentType aggregate
 * US 4.1.12: Manage hierarchical incident type catalog
 */

/**
 * DTO for creating a new IncidentType
 */
export interface CreateIncidentTypeDto {
  code: string;
  typeName: string;
  description: string;
  defaultSeverity: string;
  categoryCode: string;
  parentTypeId?: string;
  requiresExternalEntities?: boolean;
  estimatedResolutionTimeHours: number;
}

/**
 * DTO for updating an IncidentType
 */
export interface UpdateIncidentTypeDto {
  typeName?: string;
  description?: string;
  defaultSeverity?: string;
  categoryCode?: string;
  parentTypeId?: string;
  requiresExternalEntities?: boolean;
  estimatedResolutionTimeHours?: number;
}

/**
 * DTO for IncidentType response
 */
export interface IncidentTypeDto {
  incidentTypeId: string;
  code: string;
  typeName: string;
  description: string;
  defaultSeverity: string;
  categoryCode: string;
  categoryName: string;
  parentTypeId: string | null;
  requiresExternalEntities: boolean;
  estimatedResolutionTimeHours: number;
  isActive: boolean;
  isRootType: boolean;
  createdAt: string;
  updatedAt: string;
}

/**
 * Query parameters for listing incident types
 */
export interface ListIncidentTypesQuery {
  // Filter parameters
  activeOnly?: boolean;
  category?: string;
  // Pagination parameters
  page?: number;
  limit?: number;
  sortBy?: string;
  sortOrder?: 'asc' | 'desc';
}
