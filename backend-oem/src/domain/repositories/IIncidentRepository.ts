import { Incident } from '@domain/entities/Incident';
import { IncidentStatus, IncidentSeverity, QueryOptions } from '@shared/types';

/**
 * Repository interface for Incident aggregate
 * Follows Dependency Inversion Principle (DIP)
 */
export interface IIncidentRepository {
  /**
   * Find incident by ID
   * @param incidentId - Incident ID
   * @returns Incident or null if not found
   */
  findById(incidentId: string): Promise<Incident | null>;

  /**
   * Find all incidents with optional filtering
   * @param options - Query options
   * @returns Array of incidents
   */
  findAll(options?: QueryOptions): Promise<Incident[]>;

  /**
   * Find incidents by status
   * @param status - Incident status to filter by
   * @param options - Query options
   * @returns Array of incidents with given status
   */
  findByStatus(status: IncidentStatus, options?: QueryOptions): Promise<Incident[]>;

  /**
   * Find incidents by severity
   * @param severity - Incident severity to filter by
   * @param options - Query options
   * @returns Array of incidents with given severity
   */
  findBySeverity(severity: IncidentSeverity, options?: QueryOptions): Promise<Incident[]>;

  /**
   * Find incidents by incident type
   * @param incidentTypeId - Incident type ID
   * @param options - Query options
   * @returns Array of incidents of this type
   */
  findByIncidentType(incidentTypeId: string, options?: QueryOptions): Promise<Incident[]>;

  /**
   * Find incidents linked to a vessel visit execution
   * @param vveId - VVE ID
   * @param options - Query options
   * @returns Array of incidents affecting this VVE
   */
  findByVveId(vveId: string, options?: QueryOptions): Promise<Incident[]>;

  /**
   * Find incidents reported by a specific user
   * @param reportedBy - Reporter name/ID
   * @param options - Query options
   * @returns Array of incidents reported by this user
   */
  findByReporter(reportedBy: string, options?: QueryOptions): Promise<Incident[]>;

  /**
   * Find incidents within a date range
   * Based on reported date
   * @param fromDate - Start date (inclusive)
   * @param toDate - End date (inclusive)
   * @param options - Query options
   * @returns Array of incidents within date range
   */
  findByDateRange(fromDate: Date, toDate: Date, options?: QueryOptions): Promise<Incident[]>;

  /**
   * Find open incidents (REPORTED or UNDER_INVESTIGATION)
   * @param options - Query options
   * @returns Array of open incidents
   */
  findOpen(options?: QueryOptions): Promise<Incident[]>;

  /**
   * Find critical incidents (severity = CRITICAL)
   * @param options - Query options
   * @returns Array of critical incidents
   */
  findCritical(options?: QueryOptions): Promise<Incident[]>;

  /**
   * Find incidents involving external entities
   * @param options - Query options
   * @returns Array of incidents with external entity involvement
   */
  findWithExternalEntities(options?: QueryOptions): Promise<Incident[]>;

  /**
   * Find incidents by external entity name
   * @param entityName - External entity name (e.g., "Coast Guard")
   * @param options - Query options
   * @returns Array of incidents involving this entity
   */
  findByExternalEntity(entityName: string, options?: QueryOptions): Promise<Incident[]>;

  /**
   * Save new incident
   * @param incident - Incident to save
   * @returns Saved incident
   */
  save(incident: Incident): Promise<Incident>;

  /**
   * Update existing incident
   * @param incident - Incident to update
   * @returns Updated incident
   */
  update(incident: Incident): Promise<Incident>;

  /**
   * Delete incident by ID
   * @param incidentId - Incident ID to delete
   */
  delete(incidentId: string): Promise<void>;

  /**
   * Count total incidents
   * @param status - Optional status filter
   * @param severity - Optional severity filter
   * @returns Total count
   */
  count(status?: IncidentStatus, severity?: IncidentSeverity): Promise<number>;

  /**
   * Get average time to resolution for resolved incidents
   * @param fromDate - Optional start date for filtering
   * @param toDate - Optional end date for filtering
   * @returns Average resolution time in hours
   */
  getAverageResolutionTime(fromDate?: Date, toDate?: Date): Promise<number | null>;

  /**
   * Get incident statistics
   * @param fromDate - Start date for stats
   * @param toDate - End date for stats
   * @returns Incident statistics (count by status, severity, etc.)
   */
  getStatistics(
    fromDate: Date,
    toDate: Date
  ): Promise<{
    total: number;
    byStatus: Record<IncidentStatus, number>;
    bySeverity: Record<IncidentSeverity, number>;
    averageResolutionTimeHours: number | null;
  }>;
}
