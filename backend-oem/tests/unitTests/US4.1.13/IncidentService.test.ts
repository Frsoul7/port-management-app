import { IncidentService } from '@application/services/IncidentService';
import { IIncidentRepository } from '@domain/repositories/IIncidentRepository';
import { Incident } from '@domain/entities/Incident';
import { IncidentSeverity } from '@shared/types';

/**
 * Unit Tests for IncidentService
 * US 4.1.13: Incident Management - Application Service Layer
 * 
 * Test Coverage:
 * - Incident creation with validation
 * - Incident lifecycle management (investigate, resolve, close)
 * - Querying and filtering incidents
 * - Note management
 * - External entity involvement
 * - Error handling
 */

describe('IncidentService', () => {
  let incidentService: IncidentService;
  let mockRepository: jest.Mocked<IIncidentRepository>;
  let mockIncident: Incident;

  beforeEach(() => {
    // Create mock repository
    mockRepository = {
      save: jest.fn(),
      update: jest.fn(),
      delete: jest.fn(),
      findById: jest.fn(),
      findAll: jest.fn(),
      findByStatus: jest.fn(),
      findBySeverity: jest.fn(),
      findByIncidentType: jest.fn(),
      findByVveId: jest.fn(),
      findByReporter: jest.fn(),
      findByDateRange: jest.fn(),
      findOpen: jest.fn(),
      findCritical: jest.fn(),
      findWithExternalEntities: jest.fn(),
      findByExternalEntity: jest.fn(),
      count: jest.fn(),
      getAverageResolutionTime: jest.fn(),
      getStatistics: jest.fn(),
    } as any;

    incidentService = new IncidentService(mockRepository);

    // Create mock incident
    mockIncident = new Incident({
      incidentTypeId: 'type-001',
      vveId: 'vve-001',
      title: 'Crane Malfunction',
      description: 'Crane #3 stopped responding',
      severity: 'MAJOR' as IncidentSeverity,
      reportedBy: 'operator@port.com',
      impactDescription: 'Operations delayed',
    });
  });

  /**
   * Create Incident Tests
   */
  describe('createIncident', () => {
    /**
     * Test: Creates a new incident with all required fields
     * Business rule: Incident must have type, title, description, severity, and reporter
     */
    it('should create a new incident successfully', async () => {
      const createDto = {
        incidentTypeId: 'type-001',
        vveId: 'vve-001',
        title: 'Equipment Failure',
        description: 'Crane malfunction',
        severity: 'MAJOR' as IncidentSeverity,
        reportedBy: 'operator@port.com',
        impactDescription: 'Operations delayed',
      };

      mockRepository.save.mockResolvedValue(mockIncident);

      const result = await incidentService.createIncident(createDto);

      expect(mockRepository.save).toHaveBeenCalledTimes(1);
      expect(result).toBeDefined();
      expect(result.title).toBe('Crane Malfunction');
      expect(result.severity).toBe('MAJOR');
      expect(result.status).toBe('REPORTED');
    });

    /**
     * Test: Creates incident not linked to specific vessel visit
     * Business rule: VVE ID is optional - allows port-wide incidents
     */
    it('should create incident without VVE ID', async () => {
      const createDto = {
        incidentTypeId: 'type-001',
        title: 'General Port Issue',
        description: 'Access gate malfunction',
        severity: 'MINOR' as IncidentSeverity,
        reportedBy: 'security@port.com',
      };

      const incidentWithoutVve = new Incident({
        incidentTypeId: 'type-001',
        title: 'General Port Issue',
        description: 'Access gate malfunction',
        severity: 'MINOR' as IncidentSeverity,
        reportedBy: 'security@port.com',
      });

      mockRepository.save.mockResolvedValue(incidentWithoutVve);

      const result = await incidentService.createIncident(createDto);

      expect(result.vveId).toBeUndefined();
      expect(mockRepository.save).toHaveBeenCalled();
    });

    /**
     * Test: Creates incident with external emergency services
     * Business rule: External entities (Fire Dept, Coast Guard) can be involved
     */
    it('should create incident with external entities involved', async () => {
      const createDto = {
        incidentTypeId: 'type-001',
        title: 'Fire Emergency',
        description: 'Fire detected in container yard',
        severity: 'CRITICAL' as IncidentSeverity,
        reportedBy: 'security@port.com',
        externalEntitiesInvolved: ['Fire Department', 'Coast Guard'],
      };

      const criticalIncident = new Incident({
        incidentTypeId: 'type-001',
        title: 'Fire Emergency',
        description: 'Fire detected in container yard',
        severity: 'CRITICAL' as IncidentSeverity,
        reportedBy: 'security@port.com',
        externalEntitiesInvolved: ['Fire Department', 'Coast Guard'],
      });

      mockRepository.save.mockResolvedValue(criticalIncident);

      const result = await incidentService.createIncident(createDto);

      expect(result.externalEntitiesInvolved).toContain('Fire Department');
      expect(result.externalEntitiesInvolved).toContain('Coast Guard');
    });

    /**
     * Test: Propagates database errors during incident creation
     * Business rule: Service layer errors bubble up to caller
     */
    it('should handle creation errors gracefully', async () => {
      const createDto = {
        incidentTypeId: 'type-001',
        title: 'Test Incident',
        description: 'Test description',
        severity: 'MAJOR' as IncidentSeverity,
        reportedBy: 'test@port.com',
      };

      mockRepository.save.mockRejectedValue(new Error('Database error'));

      await expect(incidentService.createIncident(createDto)).rejects.toThrow('Database error');
    });
  });

  /**
   * Get Incident By ID Tests
   */
  describe('getIncidentById', () => {
    /**
     * Test: Retrieves incident by unique identifier
     * Business rule: Each incident has unique ID for tracking
     */
    it('should retrieve incident by ID successfully', async () => {
      mockRepository.findById.mockResolvedValue(mockIncident);

      const result = await incidentService.getIncidentById('incident-001');

      expect(mockRepository.findById).toHaveBeenCalledWith('incident-001');
      expect(result.incidentId).toBe(mockIncident.incidentId);
    });

    /**
     * Test: Throws descriptive error when incident doesn't exist
     * Business rule: Invalid incident IDs result in clear error messages
     */
    it('should throw error when incident not found', async () => {
      mockRepository.findById.mockResolvedValue(null);

      await expect(incidentService.getIncidentById('non-existent')).rejects.toThrow(
        'Incident with ID non-existent not found'
      );
    });
  });

  /**
   * List Incidents Tests
   */
  describe('listIncidents', () => {
    /**
     * Test: Returns all incidents without filtering
     * Business rule: Default behavior lists entire incident catalog
     */
    it('should list all incidents when no filter provided', async () => {
      const incidents = [mockIncident];
      mockRepository.findAll.mockResolvedValue(incidents);

      const result = await incidentService.listIncidents();

      expect(mockRepository.findAll).toHaveBeenCalled();
      expect(result).toHaveLength(1);
    });

    /**
     * Test: Filters incidents by lifecycle status (REPORTED, INVESTIGATING, etc.)
     * Business rule: Supports workflow-based incident views
     */
    it('should filter incidents by status', async () => {
      mockRepository.findByStatus.mockResolvedValue([mockIncident]);

      const result = await incidentService.listIncidents({ status: 'REPORTED' as any });

      expect(mockRepository.findByStatus).toHaveBeenCalledWith('REPORTED', undefined);
      expect(result).toHaveLength(1);
    });

    /**
     * Test: Filters incidents by severity level (MINOR, MAJOR, CRITICAL)
     * Business rule: Enables priority-based incident management
     */
    it('should filter incidents by severity', async () => {
      mockRepository.findBySeverity.mockResolvedValue([mockIncident]);

      await incidentService.listIncidents({ severity: 'MAJOR' as any });

      expect(mockRepository.findBySeverity).toHaveBeenCalledWith('MAJOR', undefined);
    });

    /**
     * Test: Filters incidents by type category
     * Business rule: Groups incidents by classification (equipment, safety, etc.)
     */
    it('should filter incidents by incident type', async () => {
      mockRepository.findByIncidentType.mockResolvedValue([mockIncident]);

      await incidentService.listIncidents({ incidentTypeId: 'type-001' });

      expect(mockRepository.findByIncidentType).toHaveBeenCalledWith('type-001', undefined);
    });

    /**
     * Test: Filters incidents by vessel visit execution
     * Business rule: Links incidents to specific vessel operations
     */
    it('should filter incidents by VVE ID', async () => {
      mockRepository.findByVveId.mockResolvedValue([mockIncident]);

      await incidentService.listIncidents({ vveId: 'vve-001' });

      expect(mockRepository.findByVveId).toHaveBeenCalledWith('vve-001', undefined);
    });

    /**
     * Test: Filters incidents by person who reported them
     * Business rule: Tracks incident reporters for accountability
     */
    it('should filter incidents by reporter', async () => {
      mockRepository.findByReporter.mockResolvedValue([mockIncident]);

      await incidentService.listIncidents({ reportedBy: 'operator@port.com' });

      expect(mockRepository.findByReporter).toHaveBeenCalledWith('operator@port.com', undefined);
    });

    /**
     * Test: Filters incidents within specific time period
     * Business rule: Supports date-based incident analysis and reporting
     */
    it('should filter incidents by date range', async () => {
      const fromDate = new Date('2026-01-01');
      const toDate = new Date('2026-01-31');

      mockRepository.findByDateRange.mockResolvedValue([mockIncident]);

      await incidentService.listIncidents({ fromDate, toDate });

      expect(mockRepository.findByDateRange).toHaveBeenCalledWith(fromDate, toDate, undefined);
    });

    /**
     * Test: Filters incidents involving specific external organization
     * Business rule: Tracks incidents requiring external coordination
     */
    it('should filter incidents by external entity', async () => {
      mockRepository.findByExternalEntity.mockResolvedValue([mockIncident]);

      await incidentService.listIncidents({ externalEntity: 'Coast Guard' });

      expect(mockRepository.findByExternalEntity).toHaveBeenCalledWith('Coast Guard', undefined);
    });

    /**
     * Test: Applies pagination to incident queries
     * Business rule: Supports paginated views for large incident lists
     */
    it('should support pagination options', async () => {
      mockRepository.findAll.mockResolvedValue([mockIncident]);

      const options = { limit: 10, offset: 0 };
      await incidentService.listIncidents(undefined, options);

      expect(mockRepository.findAll).toHaveBeenCalledWith(options);
    });
  });

  /**
   * Active and Critical Incidents Tests
   */
  describe('getActiveIncidents', () => {
    /**
     * Test: Returns all open incidents (not closed)
     * Business rule: Active incidents require ongoing attention
     */
    it('should retrieve active incidents', async () => {
      mockRepository.findOpen.mockResolvedValue([mockIncident]);

      const result = await incidentService.getActiveIncidents();

      expect(mockRepository.findOpen).toHaveBeenCalled();
      expect(result).toHaveLength(1);
    });
  });

  describe('getCriticalIncidents', () => {
    /**
     * Test: Returns highest priority incidents
     * Business rule: Critical incidents need immediate response
     */
    it('should retrieve critical incidents', async () => {
      const criticalIncident = new Incident({
        incidentTypeId: 'type-001',
        title: 'Critical Issue',
        description: 'Critical failure',
        severity: 'CRITICAL' as IncidentSeverity,
        reportedBy: 'operator@port.com',
      });

      mockRepository.findCritical.mockResolvedValue([criticalIncident]);

      const result = await incidentService.getCriticalIncidents();

      expect(mockRepository.findCritical).toHaveBeenCalled();
      expect(result).toHaveLength(1);
    });
  });

  /**
   * Update Incident Tests
   */
  describe('updateIncident', () => {
    /**
     * Test: Updates incident severity level
     * Business rule: Severity can change as situation evolves
     */
    it('should update incident severity', async () => {
      mockRepository.findById.mockResolvedValue(mockIncident);
      mockRepository.update.mockResolvedValue(mockIncident);

      const updateDto = { severity: 'CRITICAL' as IncidentSeverity };

      const result = await incidentService.updateIncident('incident-001', updateDto);

      expect(mockRepository.findById).toHaveBeenCalledWith('incident-001');
      expect(mockRepository.update).toHaveBeenCalled();
      expect(result).toBeDefined();
    });

    /**
     * Test: Updates operational impact assessment
     * Business rule: Impact description documents consequences
     */
    it('should update impact description', async () => {
      mockRepository.findById.mockResolvedValue(mockIncident);
      mockRepository.update.mockResolvedValue(mockIncident);

      const updateDto = { impactDescription: 'Severe operational impact' };

      const result = await incidentService.updateIncident('incident-001', updateDto);

      expect(mockRepository.update).toHaveBeenCalled();
      expect(result).toBeDefined();
    });

    /**
     * Test: Validates incident existence before update
     * Business rule: Cannot update non-existent incidents
     */
    it('should throw error when updating non-existent incident', async () => {
      mockRepository.findById.mockResolvedValue(null);

      await expect(
        incidentService.updateIncident('non-existent', { severity: 'MAJOR' as any })
      ).rejects.toThrow('Incident with ID non-existent not found');
    });
  });

  /**
   * Incident Lifecycle Tests
   */
  describe('startInvestigation', () => {
    /**
     * Test: Transitions incident from REPORTED to INVESTIGATING status
     * Business rule: Investigation assigns investigator and records start time
     */
    it('should start investigation successfully', async () => {
      mockRepository.findById.mockResolvedValue(mockIncident);
      mockRepository.update.mockResolvedValue(mockIncident);

      const dto = { investigatedBy: 'supervisor@port.com' };

      const result = await incidentService.startInvestigation('incident-001', dto);

      expect(mockRepository.findById).toHaveBeenCalledWith('incident-001');
      expect(mockRepository.update).toHaveBeenCalled();
      expect(result).toBeDefined();
    });

    /**
     * Test: Validates incident exists before starting investigation
     * Business rule: Cannot investigate non-existent incidents
     */
    it('should throw error when starting investigation on non-existent incident', async () => {
      mockRepository.findById.mockResolvedValue(null);

      await expect(
        incidentService.startInvestigation('non-existent', { investigatedBy: 'test@port.com' })
      ).rejects.toThrow('Incident with ID non-existent not found');
    });
  });

  describe('resolveIncident', () => {
    /**
     * Test: Marks incident as resolved with resolution details
     * Business rule: Resolution requires summary and resolver information
     */
    it('should resolve incident successfully', async () => {
      // Start investigation first
      mockIncident.startInvestigation('supervisor@port.com');

      mockRepository.findById.mockResolvedValue(mockIncident);
      mockRepository.update.mockResolvedValue(mockIncident);

      const dto = {
        resolvedBy: 'technician@port.com',
        resolutionSummary: 'Crane repaired and operational',
      };

      const result = await incidentService.resolveIncident('incident-001', dto);

      expect(mockRepository.update).toHaveBeenCalled();
      expect(result).toBeDefined();
    });

    /**
     * Test: Validates incident exists before resolution
     * Business rule: Cannot resolve non-existent incidents
     */
    it('should throw error when resolving non-existent incident', async () => {
      mockRepository.findById.mockResolvedValue(null);

      await expect(
        incidentService.resolveIncident('non-existent', {
          resolvedBy: 'tech@port.com',
          resolutionSummary: 'Fixed',
        })
      ).rejects.toThrow('Incident with ID non-existent not found');
    });
  });

  describe('closeIncident', () => {
    /**
     * Test: Finalizes incident lifecycle after resolution
     * Business rule: Only resolved incidents can be closed
     */
    it('should close incident successfully', async () => {
      // Progress incident to resolved state
      mockIncident.startInvestigation('supervisor@port.com');
      mockIncident.resolve('technician@port.com', 'Issue resolved');

      mockRepository.findById.mockResolvedValue(mockIncident);
      mockRepository.update.mockResolvedValue(mockIncident);

      const dto = { closedBy: 'manager@port.com' };

      const result = await incidentService.closeIncident('incident-001', dto);

      expect(mockRepository.update).toHaveBeenCalled();
      expect(result).toBeDefined();
    });

    /**
     * Test: Validates incident exists before closing
     * Business rule: Cannot close non-existent incidents
     */
    it('should throw error when closing non-existent incident', async () => {
      mockRepository.findById.mockResolvedValue(null);

      await expect(
        incidentService.closeIncident('non-existent', { closedBy: 'test@port.com' })
      ).rejects.toThrow('Incident with ID non-existent not found');
    });
  });

  /**
   * Note Management Tests
   */
  describe('addNote', () => {
    /**
     * Test: Adds timestamped note to incident
     * Business rule: Notes track investigation progress and findings
     */
    it('should add note to incident successfully', async () => {
      mockRepository.findById.mockResolvedValue(mockIncident);
      mockRepository.update.mockResolvedValue(mockIncident);

      const dto = {
        author: 'technician@port.com',
        content: 'Inspected crane, found electrical issue',
      };

      const result = await incidentService.addNote('incident-001', dto);

      expect(mockRepository.update).toHaveBeenCalled();
      expect(result).toBeDefined();
    });

    /**
     * Test: Validates incident exists before adding note
     * Business rule: Cannot add notes to non-existent incidents
     */
    it('should throw error when adding note to non-existent incident', async () => {
      mockRepository.findById.mockResolvedValue(null);

      await expect(
        incidentService.addNote('non-existent', {
          author: 'test@port.com',
          content: 'Test note',
        })
      ).rejects.toThrow('Incident with ID non-existent not found');
    });

    /**
     * Test: Supports multiple notes per incident
     * Business rule: Incidents accumulate notes throughout lifecycle
     */
    it('should allow multiple notes on same incident', async () => {
      mockRepository.findById.mockResolvedValue(mockIncident);
      mockRepository.update.mockResolvedValue(mockIncident);

      await incidentService.addNote('incident-001', {
        author: 'tech1@port.com',
        content: 'First inspection',
      });

      await incidentService.addNote('incident-001', {
        author: 'tech2@port.com',
        content: 'Second inspection',
      });

      expect(mockRepository.update).toHaveBeenCalledTimes(2);
    });
  });

  /**
   * External Entity Management Tests
   */
  describe('involveExternalEntity', () => {
    /**
     * Test: Adds external organization to incident
     * Business rule: External entities can be added as incident progresses
     */
    it('should involve external entity successfully', async () => {
      mockRepository.findById.mockResolvedValue(mockIncident);
      mockRepository.update.mockResolvedValue(mockIncident);

      const result = await incidentService.involveExternalEntity('incident-001', 'Coast Guard');

      expect(mockRepository.update).toHaveBeenCalled();
      expect(result).toBeDefined();
    });

    /**
     * Test: Validates incident exists before adding external entity
     * Business rule: Cannot involve entities in non-existent incidents
     */
    it('should throw error when involving entity in non-existent incident', async () => {
      mockRepository.findById.mockResolvedValue(null);

      await expect(
        incidentService.involveExternalEntity('non-existent', 'Coast Guard')
      ).rejects.toThrow('Incident with ID non-existent not found');
    });

    /**
     * Test: Supports multiple external organizations per incident
     * Business rule: Complex incidents may involve multiple agencies
     */
    it('should allow multiple external entities', async () => {
      mockRepository.findById.mockResolvedValue(mockIncident);
      mockRepository.update.mockResolvedValue(mockIncident);

      await incidentService.involveExternalEntity('incident-001', 'Fire Department');
      await incidentService.involveExternalEntity('incident-001', 'Coast Guard');

      expect(mockRepository.update).toHaveBeenCalledTimes(2);
    });
  });

  /**
   * Delete Incident Tests
   */
  describe('deleteIncident', () => {
    /**
     * Test: Removes incident from system
     * Business rule: Incidents can be deleted (e.g., duplicate entries)
     */
    it('should delete incident successfully', async () => {
      mockRepository.findById.mockResolvedValue(mockIncident);
      mockRepository.delete.mockResolvedValue(undefined);

      await incidentService.deleteIncident('incident-001');

      expect(mockRepository.findById).toHaveBeenCalledWith('incident-001');
      expect(mockRepository.delete).toHaveBeenCalledWith('incident-001');
    });

    /**
     * Test: Validates incident exists before deletion
     * Business rule: Cannot delete non-existent incidents
     */
    it('should throw error when deleting non-existent incident', async () => {
      mockRepository.findById.mockResolvedValue(null);

      await expect(incidentService.deleteIncident('non-existent')).rejects.toThrow(
        'Incident with ID non-existent not found'
      );
    });
  });

  /**
   * DTO Conversion Tests
   */
  describe('DTO conversion', () => {
    /**
     * Test: Converts complete incident entity to DTO format
     * Business rule: DTO includes all lifecycle fields and nested data
     */
    it('should convert incident to DTO with all fields', async () => {
      mockIncident.startInvestigation('supervisor@port.com');
      mockIncident.resolve('technician@port.com', 'Fixed the issue');
      mockIncident.addNote('tech@port.com', 'Additional details');
      mockIncident.close('manager@port.com');

      mockRepository.save.mockResolvedValue(mockIncident);

      const createDto = {
        incidentTypeId: 'type-001',
        vveId: 'vve-001',
        title: 'Test',
        description: 'Test',
        severity: 'MAJOR' as IncidentSeverity,
        reportedBy: 'test@port.com',
      };

      const result = await incidentService.createIncident(createDto);

      // Verify all DTO fields are present
      expect(result).toHaveProperty('incidentId');
      expect(result).toHaveProperty('incidentTypeId');
      expect(result).toHaveProperty('vveId');
      expect(result).toHaveProperty('title');
      expect(result).toHaveProperty('description');
      expect(result).toHaveProperty('severity');
      expect(result).toHaveProperty('status');
      expect(result).toHaveProperty('reportedAt');
      expect(result).toHaveProperty('reportedBy');
      expect(result).toHaveProperty('notes');
      expect(result).toHaveProperty('externalEntitiesInvolved');
    });

    /**
     * Test: Handles optional fields correctly in DTO
     * Business rule: DTO properly represents null/undefined optional fields
     */
    it('should handle null optional fields in DTO', async () => {
      const minimalIncident = new Incident({
        incidentTypeId: 'type-001',
        title: 'Minimal Incident',
        description: 'Test',
        severity: 'MINOR' as any,
        reportedBy: 'test@port.com',
      });

      mockRepository.save.mockResolvedValue(minimalIncident);

      const createDto = {
        incidentTypeId: 'type-001',
        title: 'Minimal Incident',
        description: 'Test',
        severity: 'MINOR' as IncidentSeverity,
        reportedBy: 'test@port.com',
      };

      const result = await incidentService.createIncident(createDto);

      expect(result.vveId).toBeUndefined();
      expect(result.investigatedAt).toBeUndefined();
      expect(result.resolvedAt).toBeUndefined();
      expect(result.closedAt).toBeUndefined();
    });
  });

  /**
   * Error Handling Tests
   */
  describe('error handling', () => {
    /**
     * Test: Propagates repository errors during queries
     * Business rule: Database errors bubble up to service layer caller
     */
    it('should handle repository errors during list operations', async () => {
      mockRepository.findAll.mockRejectedValue(new Error('Database connection error'));

      await expect(incidentService.listIncidents()).rejects.toThrow('Database connection error');
    });

    /**
     * Test: Propagates repository errors during updates
     * Business rule: Update failures are reported to caller
     */
    it('should handle repository errors during update operations', async () => {
      mockRepository.findById.mockResolvedValue(mockIncident);
      mockRepository.update.mockRejectedValue(new Error('Update failed'));

      await expect(
        incidentService.updateIncident('incident-001', { severity: 'CRITICAL' as any })
      ).rejects.toThrow('Update failed');
    });
  });
});
