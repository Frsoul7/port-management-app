import { IncidentType } from '@domain/entities/IncidentType';
import { IncidentSeverity, QueryOptions } from '@shared/types';

/**
 * Repository interface for IncidentType aggregate
 * Follows Dependency Inversion Principle (DIP)
 */
export interface IIncidentTypeRepository {
  /**
   * Find incident type by ID
   * @param incidentTypeId - Incident type ID
   * @returns IncidentType or null if not found
   */
  findById(incidentTypeId: string): Promise<IncidentType | null>;

  /**
   * Find incident type by name
   * Type names should be unique
   * @param typeName - Type name to search
   * @returns IncidentType or null if not found
   */
  findByName(typeName: string): Promise<IncidentType | null>;

  /**
   * Find all incident types with optional filtering
   * @param options - Query options
   * @returns Array of incident types
   */
  findAll(options?: QueryOptions): Promise<IncidentType[]>;

  /**
   * Find all active incident types
   * Used for dropdown lists when creating incidents
   * @param options - Query options
   * @returns Array of active incident types
   */
  findActive(options?: QueryOptions): Promise<IncidentType[]>;

  /**
   * Find incident types by default severity
   * @param severity - Severity level to filter by
   * @param options - Query options
   * @returns Array of incident types with given default severity
   */
  findBySeverity(severity: IncidentSeverity, options?: QueryOptions): Promise<IncidentType[]>;

  /**
   * Find incident types that require external entities
   * @param options - Query options
   * @returns Array of incident types requiring external support
   */
  findRequiringExternalEntities(options?: QueryOptions): Promise<IncidentType[]>;

  /**
   * Save new incident type
   * @param incidentType - Incident type to save
   * @returns Saved incident type
   */
  save(incidentType: IncidentType): Promise<IncidentType>;

  /**
   * Update existing incident type
   * @param incidentType - Incident type to update
   * @returns Updated incident type
   */
  update(incidentType: IncidentType): Promise<IncidentType>;

  /**
   * Delete incident type by ID
   * Note: Should check if type is used in existing incidents before deleting
   * @param incidentTypeId - Incident type ID to delete
   */
  delete(incidentTypeId: string): Promise<void>;

  /**
   * Count total incident types
   * @param activeOnly - If true, count only active types
   * @returns Total count
   */
  count(activeOnly?: boolean): Promise<number>;

  /**
   * Check if incident type name already exists
   * Used for uniqueness validation
   * @param typeName - Type name to check
   * @param excludeId - Optional ID to exclude from check (for updates)
   * @returns True if name exists
   */
  existsByName(typeName: string, excludeId?: string): Promise<boolean>;

  /**
   * Check if incident type is used in any incidents
   * Used before deleting a type
   * @param incidentTypeId - Incident type ID to check
   * @returns True if type is used in incidents
   */
  isUsedInIncidents(incidentTypeId: string): Promise<boolean>;

  /**
   * Find incident type by code
   * @param code - Type code (e.g., "T-INC001")
   * @returns IncidentType or null if not found
   */
  findByCode(code: string): Promise<IncidentType | null>;

  /**
   * Find incident types by category
   * @param categoryCode - Category code ("ENV", "OPS", "SAF")
   * @param options - Query options
   * @returns Array of incident types in the category
   */
  findByCategory(categoryCode: string, options?: QueryOptions): Promise<IncidentType[]>;

  /**
   * Find root incident types (those without parents)
   * @param options - Query options
   * @returns Array of root incident types
   */
  findRootTypes(options?: QueryOptions): Promise<IncidentType[]>;

  /**
   * Find children of a specific incident type
   * @param parentTypeId - Parent type ID
   * @param options - Query options
   * @returns Array of child incident types
   */
  findChildrenOf(parentTypeId: string, options?: QueryOptions): Promise<IncidentType[]>;

  /**
   * Check if incident type code already exists
   * @param code - Type code to check
   * @param excludeId - Optional ID to exclude from check (for updates)
   * @returns True if code exists
   */
  existsByCode(code: string, excludeId?: string): Promise<boolean>;
}
