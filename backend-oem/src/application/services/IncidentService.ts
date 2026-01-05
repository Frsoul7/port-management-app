import { IIncidentRepository } from '@domain/repositories/IIncidentRepository';
import { Incident } from '@domain/entities/Incident';
import { IncidentStatus, IncidentSeverity, QueryOptions } from '@shared/types';
import { logger } from '@shared/utils/logger';
import { ListIncidentsQuery } from '@application/dtos/IncidentDto';
import { PaginatedResult } from '@shared/types';
import {
  CreateIncidentDto,
  UpdateIncidentDto,
  StartInvestigationDto,
  ResolveIncidentDto,
  CloseIncidentDto,
  AddIncidentNoteDto,
  IncidentDto,
  IncidentFilterDto,
} from '@application/dtos';

/**
 * Application Service for Incident aggregate
 * Handles incident lifecycle and management
 */
export class IncidentService {
  constructor(private incidentRepository: IIncidentRepository) {}

  /**
   * Create a new incident
   */
  async createIncident(dto: CreateIncidentDto): Promise<IncidentDto> {
    try {
      logger.info('Creating new incident', { dto });

      // Create domain entity
      const incident = new Incident({
        incidentTypeId: dto.incidentTypeId,
        vveId: dto.vveId,
        title: dto.title,
        description: dto.description,
        severity: dto.severity,
        reportedBy: dto.reportedBy,
        impactDescription: dto.impactDescription,
        externalEntitiesInvolved: dto.externalEntitiesInvolved,
      });

      // Save to repository
      const saved = await this.incidentRepository.save(incident);

      logger.info('Incident created successfully', { incidentId: saved.incidentId });

      return this.toDto(saved);
    } catch (error) {
      logger.error('Error creating incident', { error, dto });
      throw error;
    }
  }

  /**
   * List all incidents with optional filtering
   */
  async listIncidents(filter?: IncidentFilterDto, options?: QueryOptions): Promise<IncidentDto[]> {
    try {
      logger.info('Listing incidents', { filter, options });

      let incidents: Incident[];

      // Apply filters
      if (filter?.status) {
        incidents = await this.incidentRepository.findByStatus(filter.status, options);
      } else if (filter?.severity) {
        incidents = await this.incidentRepository.findBySeverity(filter.severity, options);
      } else if (filter?.incidentTypeId) {
        incidents = await this.incidentRepository.findByIncidentType(filter.incidentTypeId, options);
      } else if (filter?.vveId) {
        incidents = await this.incidentRepository.findByVveId(filter.vveId, options);
      } else if (filter?.reportedBy) {
        incidents = await this.incidentRepository.findByReporter(filter.reportedBy, options);
      } else if (filter?.fromDate && filter?.toDate) {
        incidents = await this.incidentRepository.findByDateRange(
          filter.fromDate,
          filter.toDate,
          options
        );
      } else if (filter?.externalEntity) {
        incidents = await this.incidentRepository.findByExternalEntity(
          filter.externalEntity,
          options
        );
      } else {
        incidents = await this.incidentRepository.findAll(options);
      }

      logger.info('Incidents listed successfully', { count: incidents.length });

      return incidents.map((incident) => this.toDto(incident));
    } catch (error) {
      logger.error('Error listing incidents', { error, filter });
      throw error;
    }
  }

  /**
   * Get active incidents (REPORTED or UNDER_INVESTIGATION)
   */
  async getActiveIncidents(options?: QueryOptions): Promise<IncidentDto[]> {
    try {
      logger.info('Getting active incidents');

      const incidents = await this.incidentRepository.findOpen(options);

      logger.info('Active incidents retrieved successfully', { count: incidents.length });

      return incidents.map((incident) => this.toDto(incident));
    } catch (error) {
      logger.error('Error getting active incidents', { error });
      throw error;
    }
  }

  /**
   * Get critical incidents
   */
  async getCriticalIncidents(options?: QueryOptions): Promise<IncidentDto[]> {
    try {
      logger.info('Getting critical incidents');

      const incidents = await this.incidentRepository.findCritical(options);

      logger.info('Critical incidents retrieved successfully', { count: incidents.length });

      return incidents.map((incident) => this.toDto(incident));
    } catch (error) {
      logger.error('Error getting critical incidents', { error });
      throw error;
    }
  }

  /**
   * Get incident by ID
   */
  async getIncidentById(incidentId: string): Promise<IncidentDto> {
    try {
      logger.info('Getting incident by ID', { incidentId });

      const incident = await this.incidentRepository.findById(incidentId);

      if (!incident) {
        throw new Error(`Incident with ID ${incidentId} not found`);
      }

      logger.info('Incident retrieved successfully', { incidentId });

      return this.toDto(incident);
    } catch (error) {
      logger.error('Error getting incident by ID', { error, incidentId });
      throw error;
    }
  }

  /**
   * Update incident details
   */
  async updateIncident(incidentId: string, dto: UpdateIncidentDto): Promise<IncidentDto> {
    try {
      logger.info('Updating incident', { incidentId, dto });

      const incident = await this.incidentRepository.findById(incidentId);

      if (!incident) {
        throw new Error(`Incident with ID ${incidentId} not found`);
      }

      // Update fields
      // Note: Title and description are readonly after creation (business rule)
      // Only severity and impact description can be updated

      if (dto.severity !== undefined) {
        incident.updateSeverity(dto.severity);
      }

      if (dto.impactDescription !== undefined) {
        incident.setImpactDescription(dto.impactDescription);
      }

      // Save to repository
      const updated = await this.incidentRepository.update(incident);

      logger.info('Incident updated successfully', { incidentId });

      return this.toDto(updated);
    } catch (error) {
      logger.error('Error updating incident', { error, incidentId, dto });
      throw error;
    }
  }

  /**
   * Delete incident
   */
  async deleteIncident(incidentId: string): Promise<void> {
    try {
      logger.info('Deleting incident', { incidentId });

      const incident = await this.incidentRepository.findById(incidentId);

      if (!incident) {
        throw new Error(`Incident with ID ${incidentId} not found`);
      }

      await this.incidentRepository.delete(incidentId);

      logger.info('Incident deleted successfully', { incidentId });
    } catch (error) {
      logger.error('Error deleting incident', { error, incidentId });
      throw error;
    }
  }

  /**
   * Start investigation
   */
  async startInvestigation(incidentId: string, dto: StartInvestigationDto): Promise<IncidentDto> {
    try {
      logger.info('Starting investigation', { incidentId, dto });

      const incident = await this.incidentRepository.findById(incidentId);

      if (!incident) {
        throw new Error(`Incident with ID ${incidentId} not found`);
      }

      // Call domain method
      incident.startInvestigation(dto.investigatedBy);

      // Save to repository
      const updated = await this.incidentRepository.update(incident);

      logger.info('Investigation started successfully', { incidentId });

      return this.toDto(updated);
    } catch (error) {
      logger.error('Error starting investigation', { error, incidentId, dto });
      throw error;
    }
  }

  /**
   * Resolve incident
   */
  async resolveIncident(incidentId: string, dto: ResolveIncidentDto): Promise<IncidentDto> {
    try {
      logger.info('Resolving incident', { incidentId, dto });

      const incident = await this.incidentRepository.findById(incidentId);

      if (!incident) {
        throw new Error(`Incident with ID ${incidentId} not found`);
      }

      // Call domain method
      incident.resolve(dto.resolvedBy, dto.resolutionSummary);

      // Save to repository
      const updated = await this.incidentRepository.update(incident);

      logger.info('Incident resolved successfully', { incidentId });

      return this.toDto(updated);
    } catch (error) {
      logger.error('Error resolving incident', { error, incidentId, dto });
      throw error;
    }
  }

  /**
   * Close incident
   */
  async closeIncident(incidentId: string, dto: CloseIncidentDto): Promise<IncidentDto> {
    try {
      logger.info('Closing incident', { incidentId, dto });

      const incident = await this.incidentRepository.findById(incidentId);

      if (!incident) {
        throw new Error(`Incident with ID ${incidentId} not found`);
      }

      // Call domain method
      incident.close(dto.closedBy);

      // Save to repository
      const updated = await this.incidentRepository.update(incident);

      logger.info('Incident closed successfully', { incidentId });

      return this.toDto(updated);
    } catch (error) {
      logger.error('Error closing incident', { error, incidentId, dto });
      throw error;
    }
  }

  /**
   * Add note to incident
   */
  async addNote(incidentId: string, dto: AddIncidentNoteDto): Promise<IncidentDto> {
    try {
      logger.info('Adding note to incident', { incidentId, dto });

      const incident = await this.incidentRepository.findById(incidentId);

      if (!incident) {
        throw new Error(`Incident with ID ${incidentId} not found`);
      }

      // Call domain method
      incident.addNote(dto.author, dto.content);

      // Save to repository
      const updated = await this.incidentRepository.update(incident);

      logger.info('Note added successfully', { incidentId });

      return this.toDto(updated);
    } catch (error) {
      logger.error('Error adding note', { error, incidentId, dto });
      throw error;
    }
  }

  /**
   * Involve external entity
   */
  async involveExternalEntity(incidentId: string, entityName: string): Promise<IncidentDto> {
    try {
      logger.info('Involving external entity', { incidentId, entityName });

      const incident = await this.incidentRepository.findById(incidentId);

      if (!incident) {
        throw new Error(`Incident with ID ${incidentId} not found`);
      }

      // Call domain method
      incident.involveExternalEntity(entityName);

      // Save to repository
      const updated = await this.incidentRepository.update(incident);

      logger.info('External entity involved successfully', { incidentId, entityName });

      return this.toDto(updated);
    } catch (error) {
      logger.error('Error involving external entity', { error, incidentId, entityName });
      throw error;
    }
  }

  /**
   * Convert domain entity to DTO
   */
  private toDto(incident: Incident): IncidentDto {
    return {
      incidentId: incident.incidentId,
      incidentTypeId: incident.incidentTypeId,
      vveId: incident.vveId || undefined,
      title: incident.title,
      description: incident.description,
      severity: incident.severity,
      status: incident.status,
      reportedAt: incident.reportedAt.toISOString(),
      reportedBy: incident.reportedBy,
      investigatedAt: incident.investigatedAt ? incident.investigatedAt.toISOString() : undefined,
      investigatedBy: incident.investigatedBy || undefined,
      affectedEntities: [], // Incident entity doesn't track affected entities separately
      resolvedAt: incident.resolvedAt ? incident.resolvedAt.toISOString() : undefined,
      resolvedBy: incident.resolvedBy || undefined,
      closedAt: incident.closedAt ? incident.closedAt.toISOString() : undefined,
      closedBy: incident.closedBy || undefined,
      externalEntitiesInvolved: [...incident.externalEntitiesInvolved],
      notes: incident.notes.map((note) => ({
        timestamp: note.timestamp.toISOString(),
        author: note.author,
        content: note.content,
      })),
      resolutionSummary: incident.resolutionSummary || undefined,
      impactDescription: incident.impactDescription || undefined,
      durationMinutes: this.calculateDurationInMinutes(incident) || undefined,
      createdAt: incident.reportedAt.toISOString(), // Using reportedAt as createdAt
    };
  }

  /**
   * List incidents with pagination and filtering
   * Unified method that handles all filter combinations
   */
  async listIncidentsPaginated(query: ListIncidentsQuery): Promise<PaginatedResult<IncidentDto>> {
    try {
      logger.info('Listing incidents with pagination', { query });

      // Build filter from query
      const filter = this.buildFilterFromQuery(query);

      // Pagination parameters
      const page = query.page || 1;
      const limit = query.limit || 10;
      const skip = (page - 1) * limit;
      const sortBy = query.sortBy || 'reportedAt';
      const sortOrder = query.sortOrder || 'desc';

      // Build query options
      const options: QueryOptions = {
        limit,
        offset: skip,
      };

      // Get incidents based on filter
      let incidents: Incident[];
      let total: number;

      if (filter.status) {
        incidents = await this.incidentRepository.findByStatus(filter.status, options);
        total = await this.countByStatus(filter.status);
      } else if (filter.severity) {
        incidents = await this.incidentRepository.findBySeverity(filter.severity, options);
        total = await this.countBySeverity(filter.severity);
      } else if (filter.incidentTypeId) {
        incidents = await this.incidentRepository.findByIncidentType(filter.incidentTypeId, options);
        total = await this.countByIncidentType(filter.incidentTypeId);
      } else if (filter.vveId) {
        incidents = await this.incidentRepository.findByVveId(filter.vveId, options);
        total = await this.countByVveId(filter.vveId);
      } else if (filter.reportedBy) {
        incidents = await this.incidentRepository.findByReporter(filter.reportedBy, options);
        total = await this.countByReporter(filter.reportedBy);
      } else if (filter.fromDate && filter.toDate) {
        incidents = await this.incidentRepository.findByDateRange(
          filter.fromDate,
          filter.toDate,
          options
        );
        total = await this.countByDateRange(filter.fromDate, filter.toDate);
      } else if (filter.externalEntity) {
        incidents = await this.incidentRepository.findByExternalEntity(filter.externalEntity, options);
        total = await this.countByExternalEntity(filter.externalEntity);
      } else {
        incidents = await this.incidentRepository.findAll(options);
        total = await this.countAll();
      }

      // Apply sorting
      incidents = this.sortIncidents(incidents, sortBy, sortOrder);

      const incidentDtos = incidents.map((incident) => this.toDto(incident));

      return {
        data: incidentDtos,
        pagination: {
          page,
          limit,
          total,
          totalPages: Math.ceil(total / limit),
        },
      };
    } catch (error) {
      logger.error('Error listing incidents with pagination', { error, query });
      throw error;
    }
  }

  /**
   * Build filter object from query parameters
   */
  private buildFilterFromQuery(query: ListIncidentsQuery): any {
    const filter: any = {};

    if (query.status) filter.status = query.status as IncidentStatus;
    if (query.severity) filter.severity = query.severity as IncidentSeverity;
    if (query.incidentTypeId) filter.incidentTypeId = query.incidentTypeId;
    if (query.vveId) filter.vveId = query.vveId;
    if (query.reportedBy) filter.reportedBy = query.reportedBy;
    if (query.fromDate && query.toDate) {
      filter.fromDate = new Date(query.fromDate);
      filter.toDate = new Date(query.toDate);
    }
    if (query.externalEntity) filter.externalEntity = query.externalEntity;

    return filter;
  }

  /**
   * Sort incidents based on field and order
   */
  private sortIncidents(incidents: Incident[], sortBy: string, sortOrder: string): Incident[] {
    return incidents.sort((a, b) => {
      let aValue: any;
      let bValue: any;

      switch (sortBy) {
        case 'reportedAt':
          aValue = a.reportedAt.getTime();
          bValue = b.reportedAt.getTime();
          break;
        case 'severity':
          aValue = a.severity;
          bValue = b.severity;
          break;
        case 'status':
          aValue = a.status;
          bValue = b.status;
          break;
        default:
          aValue = a.reportedAt.getTime();
          bValue = b.reportedAt.getTime();
      }

      if (sortOrder === 'asc') {
        return aValue > bValue ? 1 : -1;
      } else {
        return aValue < bValue ? 1 : -1;
      }
    });
  }

  // Count methods for different filters
  private async countByStatus(status: IncidentStatus): Promise<number> {
    const incidents = await this.incidentRepository.findByStatus(status);
    return incidents.length;
  }

  private async countBySeverity(severity: IncidentSeverity): Promise<number> {
    const incidents = await this.incidentRepository.findBySeverity(severity);
    return incidents.length;
  }

  private async countByIncidentType(incidentTypeId: string): Promise<number> {
    const incidents = await this.incidentRepository.findByIncidentType(incidentTypeId);
    return incidents.length;
  }

  private async countByVveId(vveId: string): Promise<number> {
    const incidents = await this.incidentRepository.findByVveId(vveId);
    return incidents.length;
  }

  private async countByReporter(reportedBy: string): Promise<number> {
    const incidents = await this.incidentRepository.findByReporter(reportedBy);
    return incidents.length;
  }

  private async countByDateRange(fromDate: Date, toDate: Date): Promise<number> {
    const incidents = await this.incidentRepository.findByDateRange(fromDate, toDate);
    return incidents.length;
  }

  private async countByExternalEntity(externalEntity: string): Promise<number> {
    const incidents = await this.incidentRepository.findByExternalEntity(externalEntity);
    return incidents.length;
  }

  private async countAll(): Promise<number> {
    const incidents = await this.incidentRepository.findAll();
    return incidents.length;
  }

  /**
   * Calculate incident duration in minutes
   * Returns null if incident is not yet closed
   */
  private calculateDurationInMinutes(incident: Incident): number | null {
    if (!incident.closedAt) {
      return null;
    }

    const diffMs = incident.closedAt.getTime() - incident.reportedAt.getTime();
    return Math.round(diffMs / (1000 * 60));
  }
}
