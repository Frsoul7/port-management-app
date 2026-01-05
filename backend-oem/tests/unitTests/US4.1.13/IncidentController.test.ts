import { Request, Response, NextFunction } from 'express';
import { IncidentController } from '@presentation/controllers/IncidentController';
import { IncidentService } from '@application/services/IncidentService';

/**
 * Unit Tests for IncidentController
 * US 4.1.13: Incident Management - Presentation Layer
 * 
 * Test Coverage:
 * - HTTP request handling
 * - Request validation
 * - Response formatting
 * - Error handling and status codes
 * - Query parameter parsing
 * - Filter building
 */

describe('IncidentController', () => {
  let incidentController: IncidentController;
  let mockIncidentService: jest.Mocked<IncidentService>;
  let mockRequest: Partial<Request>;
  let mockResponse: Partial<Response>;
  let mockNext: jest.MockedFunction<NextFunction>;

  beforeEach(() => {
    // Create mock service
    mockIncidentService = {
      createIncident: jest.fn(),
      getIncidentById: jest.fn(),
      listIncidents: jest.fn(),
      listIncidentsPaginated: jest.fn(),
      getActiveIncidents: jest.fn(),
      getCriticalIncidents: jest.fn(),
      updateIncident: jest.fn(),
      deleteIncident: jest.fn(),
      startInvestigation: jest.fn(),
      resolveIncident: jest.fn(),
      closeIncident: jest.fn(),
      addNote: jest.fn(),
      involveExternalEntity: jest.fn(),
    } as any;

    incidentController = new IncidentController(mockIncidentService);

    // Create mock request and response
    mockRequest = {
      params: {},
      query: {},
      body: {},
    };

    mockResponse = {
      status: jest.fn().mockReturnThis(),
      json: jest.fn().mockReturnThis(),
    };

    mockNext = jest.fn();
  });

  /**
   * Create Incident Tests
   */
  describe('createIncident', () => {
    it('should create incident and return 201 status', async () => {
      const incidentDto = {
        incidentTypeId: 'type-001',
        vveId: 'vve-001',
        title: 'Crane Malfunction',
        description: 'Crane #3 not working',
        severity: 'MAJOR',
        reportedBy: 'operator@port.com',
      };

      mockRequest.body = incidentDto;

      const mockResult = {
        incidentId: 'incident-001',
        ...incidentDto,
        severity: 'MAJOR' as any,
        status: 'REPORTED' as any,
        reportedAt: new Date().toISOString(),
        investigatedAt: undefined,
        resolvedAt: undefined,
        closedAt: undefined,
        externalEntitiesInvolved: [],
        notes: [],
        resolutionSummary: undefined,
        impactDescription: undefined,
        durationMinutes: undefined,
        investigatedBy: undefined,
        resolvedBy: undefined,
        closedBy: undefined,
        affectedEntities: [],
        createdAt: new Date().toISOString(),
      };

      mockIncidentService.createIncident.mockResolvedValue(mockResult);

      await incidentController.createIncident(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockIncidentService.createIncident).toHaveBeenCalledWith(incidentDto);
      expect(mockResponse.status).toHaveBeenCalledWith(201);
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        message: 'Incident created successfully',
        data: mockResult,
      });
      expect(mockNext).not.toHaveBeenCalled();
    });

    it('should call next with error on service failure', async () => {
      mockRequest.body = {
        title: 'Test',
        description: 'Test',
        severity: 'MAJOR',
        reportedBy: 'test@port.com',
      };

      const error = new Error('Service error');
      mockIncidentService.createIncident.mockRejectedValue(error);

      await incidentController.createIncident(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockNext).toHaveBeenCalledWith(error);
      expect(mockResponse.status).not.toHaveBeenCalled();
    });
  });

  /**
   * Get Incident By ID Tests
   */
  describe('getIncidentById', () => {
    it('should return incident when found', async () => {
      mockRequest.params = { id: 'incident-001' };

      const mockIncident = {
        incidentId: 'incident-001',
        incidentTypeId: 'type-001',
        vveId: 'vve-001',
        title: 'Test Incident',
        description: 'Test description',
        severity: 'MAJOR' as any,
        status: 'REPORTED' as any,
        reportedAt: new Date().toISOString(),
        reportedBy: 'test@port.com',
        investigatedAt: undefined,
        investigatedBy: undefined,
        resolvedAt: undefined,
        resolvedBy: undefined,
        closedAt: undefined,
        closedBy: undefined,
        externalEntitiesInvolved: [],
        notes: [],
        resolutionSummary: undefined,
        impactDescription: undefined,
        durationMinutes: undefined,
        affectedEntities: [],
        createdAt: new Date().toISOString(),
      };

      mockIncidentService.getIncidentById.mockResolvedValue(mockIncident);

      await incidentController.getIncidentById(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockIncidentService.getIncidentById).toHaveBeenCalledWith('incident-001');
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        data: mockIncident,
      });
    });

    it('should return 400 when ID is missing', async () => {
      mockRequest.params = {};

      await incidentController.getIncidentById(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockResponse.status).toHaveBeenCalledWith(400);
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: false,
        message: 'Incident ID is required',
      });
      expect(mockIncidentService.getIncidentById).not.toHaveBeenCalled();
    });

    it('should call next with error when service fails', async () => {
      mockRequest.params = { id: 'incident-001' };

      const error = new Error('Not found');
      mockIncidentService.getIncidentById.mockRejectedValue(error);

      await incidentController.getIncidentById(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockNext).toHaveBeenCalledWith(error);
    });
  });

  /**
   * List Incidents Tests
   */
  describe('listIncidents', () => {
    it('should list all incidents without filters', async () => {
      mockRequest.query = {};

      const mockIncidents = [
        {
          incidentId: 'incident-001',
          title: 'Test 1',
          status: 'REPORTED',
        },
        {
          incidentId: 'incident-002',
          title: 'Test 2',
          status: 'RESOLVED',
        },
      ];

      mockIncidentService.listIncidentsPaginated.mockResolvedValue({
        data: mockIncidents as any,
        pagination: { page: 1, limit: 10, total: 2, totalPages: 1 },
      });

      await incidentController.listIncidents(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockIncidentService.listIncidentsPaginated).toHaveBeenCalledWith({});
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        data: mockIncidents,
        pagination: { page: 1, limit: 10, total: 2, totalPages: 1 },
      });
    });

    it('should filter by status', async () => {
      mockRequest.query = { status: 'REPORTED' };

      mockIncidentService.listIncidentsPaginated.mockResolvedValue({
        data: [],
        pagination: { page: 1, limit: 10, total: 0, totalPages: 0 },
      });

      await incidentController.listIncidents(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockIncidentService.listIncidentsPaginated).toHaveBeenCalledWith(
        { status: 'REPORTED' }
      );
    });

    it('should filter by severity', async () => {
      mockRequest.query = { severity: 'CRITICAL' };

      mockIncidentService.listIncidentsPaginated.mockResolvedValue({
        data: [],
        pagination: { page: 1, limit: 10, total: 0, totalPages: 0 },
      });

      await incidentController.listIncidents(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockIncidentService.listIncidentsPaginated).toHaveBeenCalledWith(
        { severity: 'CRITICAL' }
      );
    });

    it('should filter by incident type ID', async () => {
      mockRequest.query = { incidentTypeId: 'type-001' };

      mockIncidentService.listIncidentsPaginated.mockResolvedValue({
        data: [],
        pagination: { page: 1, limit: 10, total: 0, totalPages: 0 },
      });

      await incidentController.listIncidents(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockIncidentService.listIncidentsPaginated).toHaveBeenCalledWith(
        { incidentTypeId: 'type-001' }
      );
    });

    it('should filter by VVE ID', async () => {
      mockRequest.query = { vveId: 'vve-001' };

      mockIncidentService.listIncidentsPaginated.mockResolvedValue({
        data: [],
        pagination: { page: 1, limit: 10, total: 0, totalPages: 0 },
      });

      await incidentController.listIncidents(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockIncidentService.listIncidentsPaginated).toHaveBeenCalledWith(
        { vveId: 'vve-001' }
      );
    });

    it('should filter by reporter', async () => {
      mockRequest.query = { reportedBy: 'operator@port.com' };

      mockIncidentService.listIncidentsPaginated.mockResolvedValue({
        data: [],
        pagination: { page: 1, limit: 10, total: 0, totalPages: 0 },
      });

      await incidentController.listIncidents(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockIncidentService.listIncidentsPaginated).toHaveBeenCalledWith(
        { reportedBy: 'operator@port.com' }
      );
    });

    it('should filter by date range', async () => {
      mockRequest.query = {
        fromDate: '2026-01-01',
        toDate: '2026-01-31',
      };

      mockIncidentService.listIncidentsPaginated.mockResolvedValue({
        data: [],
        pagination: { page: 1, limit: 10, total: 0, totalPages: 0 },
      });

      await incidentController.listIncidents(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockIncidentService.listIncidentsPaginated).toHaveBeenCalledWith({
        fromDate: '2026-01-01',
        toDate: '2026-01-31',
      });
    });

    it('should filter by external entity', async () => {
      mockRequest.query = { externalEntity: 'Coast Guard' };

      mockIncidentService.listIncidentsPaginated.mockResolvedValue({
        data: [],
        pagination: { page: 1, limit: 10, total: 0, totalPages: 0 },
      });

      await incidentController.listIncidents(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockIncidentService.listIncidentsPaginated).toHaveBeenCalledWith(
        { externalEntity: 'Coast Guard' }
      );
    });

    it('should support pagination with limit and offset', async () => {
      mockRequest.query = { limit: '10', page: '2' };

      mockIncidentService.listIncidentsPaginated.mockResolvedValue({
        data: [],
        pagination: { page: 2, limit: 10, total: 0, totalPages: 0 },
      });

      await incidentController.listIncidents(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockIncidentService.listIncidentsPaginated).toHaveBeenCalledWith({
        limit: '10',
        page: '2',
      });
    });

    it('should combine multiple filters', async () => {
      mockRequest.query = {
        status: 'REPORTED',
        severity: 'MAJOR',
        limit: '5',
      };

      mockIncidentService.listIncidentsPaginated.mockResolvedValue({
        data: [],
        pagination: { page: 1, limit: 5, total: 0, totalPages: 0 },
      });

      await incidentController.listIncidents(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockIncidentService.listIncidentsPaginated).toHaveBeenCalledWith({
        status: 'REPORTED',
        severity: 'MAJOR',
        limit: '5',
      });
    });

    it('should handle service errors', async () => {
      mockRequest.query = {};

      const error = new Error('Database error');
      mockIncidentService.listIncidentsPaginated.mockRejectedValue(error);

      await incidentController.listIncidents(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockNext).toHaveBeenCalledWith(error);
    });
  });

  /**
   * Get Active Incidents Tests
   */
  describe('getActiveIncidents', () => {
    it('should return active incidents', async () => {
      const mockActiveIncidents = [
        { incidentId: 'incident-001', status: 'REPORTED' },
        { incidentId: 'incident-002', status: 'UNDER_INVESTIGATION' },
      ];

      mockIncidentService.getActiveIncidents.mockResolvedValue(mockActiveIncidents as any);

      await incidentController.getActiveIncidents(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockIncidentService.getActiveIncidents).toHaveBeenCalled();
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        data: mockActiveIncidents,
        count: 2,
      });
    });

    it('should handle errors', async () => {
      const error = new Error('Service error');
      mockIncidentService.getActiveIncidents.mockRejectedValue(error);

      await incidentController.getActiveIncidents(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockNext).toHaveBeenCalledWith(error);
    });
  });

  /**
   * Get Critical Incidents Tests
   */
  describe('getCriticalIncidents', () => {
    it('should return critical incidents', async () => {
      const mockCriticalIncidents = [
        { incidentId: 'incident-003', severity: 'CRITICAL' },
      ];

      mockIncidentService.getCriticalIncidents.mockResolvedValue(mockCriticalIncidents as any);

      await incidentController.getCriticalIncidents(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockIncidentService.getCriticalIncidents).toHaveBeenCalled();
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        data: mockCriticalIncidents,
        count: 1,
      });
    });

    it('should handle errors', async () => {
      const error = new Error('Service error');
      mockIncidentService.getCriticalIncidents.mockRejectedValue(error);

      await incidentController.getCriticalIncidents(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockNext).toHaveBeenCalledWith(error);
    });
  });

  /**
   * Update Incident Tests
   */
  describe('updateIncident', () => {
    it('should update incident successfully', async () => {
      mockRequest.params = { id: 'incident-001' };
      mockRequest.body = {
        severity: 'CRITICAL',
        impactDescription: 'Severe impact',
      };

      const mockUpdated = {
        incidentId: 'incident-001',
        severity: 'CRITICAL',
        impactDescription: 'Severe impact',
      };

      mockIncidentService.updateIncident.mockResolvedValue(mockUpdated as any);

      await incidentController.updateIncident(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockIncidentService.updateIncident).toHaveBeenCalledWith('incident-001', {
        severity: 'CRITICAL',
        impactDescription: 'Severe impact',
      });
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        message: 'Incident updated successfully',
        data: mockUpdated,
      });
    });

    it('should return 400 when ID is missing', async () => {
      mockRequest.params = {};
      mockRequest.body = { severity: 'MAJOR' };

      await incidentController.updateIncident(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockResponse.status).toHaveBeenCalledWith(400);
      expect(mockIncidentService.updateIncident).not.toHaveBeenCalled();
    });

    it('should handle service errors', async () => {
      mockRequest.params = { id: 'incident-001' };
      mockRequest.body = { severity: 'MAJOR' };

      const error = new Error('Update failed');
      mockIncidentService.updateIncident.mockRejectedValue(error);

      await incidentController.updateIncident(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockNext).toHaveBeenCalledWith(error);
    });
  });

  /**
   * Start Investigation Tests
   */
  describe('startInvestigation', () => {
    it('should start investigation successfully', async () => {
      mockRequest.params = { id: 'incident-001' };
      mockRequest.body = { investigatedBy: 'supervisor@port.com' };

      const mockResult = {
        incidentId: 'incident-001',
        status: 'UNDER_INVESTIGATION',
      };

      mockIncidentService.startInvestigation.mockResolvedValue(mockResult as any);

      await incidentController.startInvestigation(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockIncidentService.startInvestigation).toHaveBeenCalledWith('incident-001', {
        investigatedBy: 'supervisor@port.com',
      });
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        message: 'Investigation started successfully',
        data: mockResult,
      });
    });

    it('should return 400 when ID is missing', async () => {
      mockRequest.params = {};
      mockRequest.body = { investigatedBy: 'test@port.com' };

      await incidentController.startInvestigation(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockResponse.status).toHaveBeenCalledWith(400);
      expect(mockIncidentService.startInvestigation).not.toHaveBeenCalled();
    });
  });

  /**
   * Resolve Incident Tests
   */
  describe('resolveIncident', () => {
    it('should resolve incident successfully', async () => {
      mockRequest.params = { id: 'incident-001' };
      mockRequest.body = {
        resolvedBy: 'technician@port.com',
        resolutionSummary: 'Issue fixed',
      };

      const mockResult = {
        incidentId: 'incident-001',
        status: 'RESOLVED',
      };

      mockIncidentService.resolveIncident.mockResolvedValue(mockResult as any);

      await incidentController.resolveIncident(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockIncidentService.resolveIncident).toHaveBeenCalledWith('incident-001', {
        resolvedBy: 'technician@port.com',
        resolutionSummary: 'Issue fixed',
      });
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        message: 'Incident resolved successfully',
        data: mockResult,
      });
    });
  });

  /**
   * Close Incident Tests
   */
  describe('closeIncident', () => {
    it('should close incident successfully', async () => {
      mockRequest.params = { id: 'incident-001' };
      mockRequest.body = { closedBy: 'manager@port.com' };

      const mockResult = {
        incidentId: 'incident-001',
        status: 'CLOSED',
      };

      mockIncidentService.closeIncident.mockResolvedValue(mockResult as any);

      await incidentController.closeIncident(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockIncidentService.closeIncident).toHaveBeenCalledWith('incident-001', {
        closedBy: 'manager@port.com',
      });
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        message: 'Incident closed successfully',
        data: mockResult,
      });
    });
  });

  /**
   * Add Note Tests
   */
  describe('addNote', () => {
    it('should add note successfully', async () => {
      mockRequest.params = { id: 'incident-001' };
      mockRequest.body = {
        author: 'tech@port.com',
        content: 'Inspection complete',
      };

      const mockResult = {
        incidentId: 'incident-001',
        notes: [{ author: 'tech@port.com', content: 'Inspection complete' }],
      };

      mockIncidentService.addNote.mockResolvedValue(mockResult as any);

      await incidentController.addNote(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockIncidentService.addNote).toHaveBeenCalledWith('incident-001', {
        author: 'tech@port.com',
        content: 'Inspection complete',
      });
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        message: 'Note added successfully',
        data: mockResult,
      });
    });
  });

  /**
   * Involve External Entity Tests
   */
  describe('involveExternalEntity', () => {
    it('should involve external entity successfully', async () => {
      mockRequest.params = { id: 'incident-001' };
      mockRequest.body = { entityName: 'Coast Guard' };

      const mockResult = {
        incidentId: 'incident-001',
        externalEntitiesInvolved: ['Coast Guard'],
      };

      mockIncidentService.involveExternalEntity.mockResolvedValue(mockResult as any);

      await incidentController.involveExternalEntity(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockIncidentService.involveExternalEntity).toHaveBeenCalledWith(
        'incident-001',
        'Coast Guard'
      );
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        message: 'External entity involved successfully',
        data: mockResult,
      });
    });
  });

  /**
   * Delete Incident Tests
   */
  describe('deleteIncident', () => {
    it('should delete incident successfully', async () => {
      mockRequest.params = { id: 'incident-001' };

      mockIncidentService.deleteIncident.mockResolvedValue(undefined);

      await incidentController.deleteIncident(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockIncidentService.deleteIncident).toHaveBeenCalledWith('incident-001');
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        message: 'Incident deleted successfully',
      });
    });

    it('should return 400 when ID is missing', async () => {
      mockRequest.params = {};

      await incidentController.deleteIncident(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockResponse.status).toHaveBeenCalledWith(400);
      expect(mockIncidentService.deleteIncident).not.toHaveBeenCalled();
    });
  });
});
