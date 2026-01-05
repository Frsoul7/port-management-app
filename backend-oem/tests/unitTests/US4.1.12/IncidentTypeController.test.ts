import { Request, Response, NextFunction } from 'express';
import { IncidentTypeController } from '../../../src/presentation/controllers/IncidentTypeController';
import { IncidentTypeService } from '../../../src/application/services/IncidentTypeService';

describe('US 4.1.12 - IncidentTypeController', () => {
  let controller: IncidentTypeController;
  let mockService: jest.Mocked<IncidentTypeService>;
  let mockRequest: Partial<Request>;
  let mockResponse: Partial<Response>;
  let mockNext: NextFunction;

  beforeEach(() => {
    mockService = {
      createIncidentType: jest.fn(),
      getIncidentTypeById: jest.fn(),
      getIncidentTypeByCode: jest.fn(),
      listAllIncidentTypes: jest.fn(),
      listActiveIncidentTypes: jest.fn(),
      listIncidentTypesByCategory: jest.fn(),
      listIncidentTypes: jest.fn(),
      getRootIncidentTypes: jest.fn(),
      getChildIncidentTypes: jest.fn(),
      updateIncidentType: jest.fn(),
      deactivateIncidentType: jest.fn(),
    } as any;

    controller = new IncidentTypeController(mockService);

    mockResponse = {
      json: jest.fn(),
      status: jest.fn().mockReturnThis(),
    };

    mockNext = jest.fn();
  });

  describe('Create Incident Type', () => {
    /**
     * Test: Creates incident type successfully
     * Business rule: Should return 201 with created data
     */
    it('should create incident type with valid data', async () => {
      mockRequest = {
        body: {
          code: 'T-INC001',
          typeName: 'Equipment Failure',
          description: 'Mechanical or electrical equipment malfunction',
          defaultSeverity: 'Major',
          categoryCode: 'OPS',
          estimatedResolutionTimeHours: 4,
        },
      };

      const mockResult = {
        incidentTypeId: 'type-id-123',
        code: 'T-INC001',
        typeName: 'Equipment Failure',
        description: 'Mechanical or electrical equipment malfunction',
        defaultSeverity: 'Major',
        categoryCode: 'OPS',
        categoryName: 'Operational Failures',
        parentTypeId: null,
        requiresExternalEntities: false,
        estimatedResolutionTimeHours: 4,
        isActive: true,
        isRootType: true,
        createdAt: '2026-01-03T10:00:00Z',
        updatedAt: '2026-01-03T10:00:00Z',
      };

      mockService.createIncidentType.mockResolvedValue(mockResult);

      await controller.createIncidentType(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.createIncidentType).toHaveBeenCalledWith(mockRequest.body);
      expect(mockResponse.status).toHaveBeenCalledWith(201);
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        message: 'Incident type created successfully',
        data: mockResult,
      });
    });

    /**
     * Test: Passes errors to error handler
     * Business rule: Validation errors should be handled by middleware
     */
    it('should handle creation errors', async () => {
      mockRequest = {
        body: {
          code: 'T-INC001',
          typeName: 'Duplicate Name',
          description: 'Should fail',
          defaultSeverity: 'Major',
          categoryCode: 'OPS',
          estimatedResolutionTimeHours: 3,
        },
      };

      const mockError = new Error('Incident type with code T-INC001 already exists');
      mockService.createIncidentType.mockRejectedValue(mockError);

      await controller.createIncidentType(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockNext).toHaveBeenCalledWith(mockError);
      expect(mockResponse.json).not.toHaveBeenCalled();
    });
  });

  describe('Get Incident Type', () => {
    /**
     * Test: Gets incident type by ID
     * Business rule: Should return type data
     */
    it('should get incident type by ID', async () => {
      mockRequest = {
        params: { id: 'type-id-123' },
      };

      const mockResult = {
        incidentTypeId: 'type-id-123',
        code: 'T-INC001',
        typeName: 'Equipment Failure',
        description: 'Mechanical failure',
        defaultSeverity: 'Major',
        categoryCode: 'OPS',
        categoryName: 'Operational Failures',
        parentTypeId: null,
        requiresExternalEntities: false,
        estimatedResolutionTimeHours: 4,
        isActive: true,
        isRootType: true,
        createdAt: '2026-01-03T10:00:00Z',
        updatedAt: '2026-01-03T10:00:00Z',
      };

      mockService.getIncidentTypeById.mockResolvedValue(mockResult);

      await controller.getIncidentTypeById(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.getIncidentTypeById).toHaveBeenCalledWith('type-id-123');
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        data: mockResult,
      });
    });

    /**
     * Test: Gets incident type by code
     * Business rule: Should retrieve by unique code
     */
    it('should get incident type by code', async () => {
      mockRequest = {
        params: { code: 'T-INC001' },
      };

      const mockResult = {
        incidentTypeId: 'type-id-123',
        code: 'T-INC001',
        typeName: 'Equipment Failure',
        description: 'Mechanical failure',
        defaultSeverity: 'Major',
        categoryCode: 'OPS',
        categoryName: 'Operational Failures',
        parentTypeId: null,
        requiresExternalEntities: false,
        estimatedResolutionTimeHours: 4,
        isActive: true,
        isRootType: true,
        createdAt: '2026-01-03T10:00:00Z',
        updatedAt: '2026-01-03T10:00:00Z',
      };

      mockService.getIncidentTypeByCode.mockResolvedValue(mockResult);

      await controller.getIncidentTypeByCode(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.getIncidentTypeByCode).toHaveBeenCalledWith('T-INC001');
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        data: mockResult,
      });
    });

    /**
     * Test: Handles not found errors
     * Business rule: Should pass 404 errors to handler
     */
    it('should handle not found error', async () => {
      mockRequest = {
        params: { id: 'nonexistent' },
      };

      const mockError = new Error('Incident type nonexistent not found');
      mockService.getIncidentTypeById.mockRejectedValue(mockError);

      await controller.getIncidentTypeById(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockNext).toHaveBeenCalledWith(mockError);
    });
  });

  describe('List Incident Types', () => {
    /**
     * Test: Lists all incident types
     * Business rule: Should return all types when no filters
     */
    it('should list all incident types without filters', async () => {
      mockRequest = {
        query: {},
      };

      const mockResults = [
        {
          incidentTypeId: 'type-id-1',
          code: 'T-INC001',
          typeName: 'Equipment Failure',
          description: 'Mechanical failure',
          defaultSeverity: 'Major',
          categoryCode: 'OPS',
          categoryName: 'Operational Failures',
          parentTypeId: null,
          requiresExternalEntities: false,
          estimatedResolutionTimeHours: 4,
          isActive: true,
          isRootType: true,
          createdAt: '2026-01-03T10:00:00Z',
          updatedAt: '2026-01-03T10:00:00Z',
        },
        {
          incidentTypeId: 'type-id-2',
          code: 'T-ENV001',
          typeName: 'Fog',
          description: 'Dense fog',
          defaultSeverity: 'Major',
          categoryCode: 'ENV',
          categoryName: 'Environmental Conditions',
          parentTypeId: null,
          requiresExternalEntities: false,
          estimatedResolutionTimeHours: 3,
          isActive: true,
          isRootType: true,
          createdAt: '2026-01-03T10:00:00Z',
          updatedAt: '2026-01-03T10:00:00Z',
        },
      ];

      mockService.listIncidentTypes.mockResolvedValue(mockResults);

      await controller.listIncidentTypes(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.listIncidentTypes).toHaveBeenCalledWith(
        { activeOnly: false, category: undefined },
        { sortBy: undefined, sortOrder: undefined }
      );
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        data: mockResults,
        count: 2,
      });
    });

    /**
     * Test: Filters active types only
     * Business rule: Should call listActiveIncidentTypes when activeOnly=true
     */
    it('should list active incident types only', async () => {
      mockRequest = {
        query: { activeOnly: 'true' },
      };

      const mockResults = [
        {
          incidentTypeId: 'type-id-1',
          code: 'T-INC001',
          typeName: 'Equipment Failure',
          isActive: true,
        } as any,
      ];

      mockService.listIncidentTypes.mockResolvedValue(mockResults);

      await controller.listIncidentTypes(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.listIncidentTypes).toHaveBeenCalledWith(
        { activeOnly: true, category: undefined },
        { sortBy: undefined, sortOrder: undefined }
      );
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        data: mockResults,
        count: 1,
      });
    });

    /**
     * Test: Filters by category
     * Business rule: Should filter ENV/OPS/SAF categories
     */
    it('should list incident types by category', async () => {
      mockRequest = {
        query: { category: 'ENV' },
      };

      const mockResults = [
        {
          incidentTypeId: 'type-id-1',
          code: 'T-ENV001',
          typeName: 'Fog',
          categoryCode: 'ENV',
        } as any,
      ];

      mockService.listIncidentTypes.mockResolvedValue(mockResults);

      await controller.listIncidentTypes(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.listIncidentTypes).toHaveBeenCalledWith(
        { activeOnly: false, category: 'ENV' },
        { sortBy: undefined, sortOrder: undefined }
      );
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        data: mockResults,
        count: 1,
      });
    });

    /**
     * Test: Applies sorting options
     * Business rule: Should pass sortBy and sortOrder to service
     */
    it('should list with sorting options', async () => {
      mockRequest = {
        query: { sortBy: 'typeName', sortOrder: 'asc' },
      };

      mockService.listIncidentTypes.mockResolvedValue([]);

      await controller.listIncidentTypes(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.listIncidentTypes).toHaveBeenCalledWith(
        { activeOnly: false, category: undefined },
        { sortBy: 'typeName', sortOrder: 'asc' }
      );
    });

    /**
     * Test: Returns empty array when no results
     * Business rule: Should handle empty results gracefully
     */
    it('should return empty array when no incident types found', async () => {
      mockRequest = {
        query: {},
      };

      mockService.listIncidentTypes.mockResolvedValue([]);

      await controller.listIncidentTypes(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        data: [],
        count: 0,
      });
    });
  });

  describe('Get Root Incident Types', () => {
    /**
     * Test: Gets root types (no parents)
     * Business rule: Should return types with parentTypeId=null
     */
    it('should get root incident types', async () => {
      mockRequest = {};

      const mockResults = [
        {
          incidentTypeId: 'type-id-1',
          code: 'T-ENV001',
          typeName: 'Environmental Conditions',
          parentTypeId: null,
          isRootType: true,
        } as any,
        {
          incidentTypeId: 'type-id-2',
          code: 'T-OPS001',
          typeName: 'Operational Failures',
          parentTypeId: null,
          isRootType: true,
        } as any,
      ];

      mockService.getRootIncidentTypes.mockResolvedValue(mockResults);

      await controller.getRootIncidentTypes(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.getRootIncidentTypes).toHaveBeenCalled();
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        data: mockResults,
        count: 2,
      });
    });
  });

  describe('Get Child Incident Types', () => {
    /**
     * Test: Gets children of parent type
     * Business rule: Should return types with matching parentTypeId
     */
    it('should get child incident types of a parent', async () => {
      mockRequest = {
        params: { id: 'parent-type-id' },
      };

      const mockResults = [
        {
          incidentTypeId: 'child-id-1',
          code: 'T-ENV002',
          typeName: 'Fog',
          parentTypeId: 'parent-type-id',
          isRootType: false,
        } as any,
        {
          incidentTypeId: 'child-id-2',
          code: 'T-ENV003',
          typeName: 'Strong Winds',
          parentTypeId: 'parent-type-id',
          isRootType: false,
        } as any,
      ];

      mockService.getChildIncidentTypes.mockResolvedValue(mockResults);

      await controller.getChildIncidentTypes(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.getChildIncidentTypes).toHaveBeenCalledWith('parent-type-id');
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        data: mockResults,
        count: 2,
      });
    });

    /**
     * Test: Returns empty array when no children
     * Business rule: Should handle parent with no children
     */
    it('should return empty array when parent has no children', async () => {
      mockRequest = {
        params: { id: 'parent-type-id' },
      };

      mockService.getChildIncidentTypes.mockResolvedValue([]);

      await controller.getChildIncidentTypes(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        data: [],
        count: 0,
      });
    });
  });

  describe('Update Incident Type', () => {
    /**
     * Test: Updates incident type successfully
     * Business rule: Should update and return modified data
     */
    it('should update incident type with valid data', async () => {
      mockRequest = {
        params: { id: 'type-id-123' },
        body: {
          description: 'Updated description',
          estimatedResolutionTimeHours: 5,
        },
      };

      const mockResult = {
        incidentTypeId: 'type-id-123',
        code: 'T-INC001',
        typeName: 'Equipment Failure',
        description: 'Updated description',
        defaultSeverity: 'Major',
        categoryCode: 'OPS',
        categoryName: 'Operational Failures',
        parentTypeId: null,
        requiresExternalEntities: false,
        estimatedResolutionTimeHours: 5,
        isActive: true,
        isRootType: true,
        createdAt: '2026-01-03T10:00:00Z',
        updatedAt: '2026-01-03T11:00:00Z',
      };

      mockService.updateIncidentType.mockResolvedValue(mockResult);

      await controller.updateIncidentType(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.updateIncidentType).toHaveBeenCalledWith('type-id-123', mockRequest.body);
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        message: 'Incident type updated successfully',
        data: mockResult,
      });
    });

    /**
     * Test: Handles validation errors
     * Business rule: Should pass validation errors to handler
     */
    it('should handle update validation errors', async () => {
      mockRequest = {
        params: { id: 'type-id-123' },
        body: {
          typeName: 'Duplicate Name',
        },
      };

      const mockError = new Error('Incident type with name Duplicate Name already exists');
      mockService.updateIncidentType.mockRejectedValue(mockError);

      await controller.updateIncidentType(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockNext).toHaveBeenCalledWith(mockError);
      expect(mockResponse.json).not.toHaveBeenCalled();
    });

    /**
     * Test: Handles circular reference errors
     * Business rule: Should prevent hierarchy loops
     */
    it('should handle circular reference errors', async () => {
      mockRequest = {
        params: { id: 'type-id-123' },
        body: {
          parentTypeId: 'descendant-type-id',
        },
      };

      const mockError = new Error('Circular reference detected in incident type hierarchy');
      mockService.updateIncidentType.mockRejectedValue(mockError);

      await controller.updateIncidentType(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockNext).toHaveBeenCalledWith(mockError);
    });
  });

  describe('Deactivate Incident Type', () => {
    /**
     * Test: Deactivates incident type successfully
     * Business rule: Should soft delete by setting isActive=false
     */
    it('should deactivate incident type', async () => {
      mockRequest = {
        params: { id: 'type-id-123' },
      };

      const mockResult = {
        incidentTypeId: 'type-id-123',
        code: 'T-INC001',
        typeName: 'Equipment Failure',
        description: 'Mechanical failure',
        defaultSeverity: 'Major',
        categoryCode: 'OPS',
        categoryName: 'Operational Failures',
        parentTypeId: null,
        requiresExternalEntities: false,
        estimatedResolutionTimeHours: 4,
        isActive: false,
        isRootType: true,
        createdAt: '2026-01-03T10:00:00Z',
        updatedAt: '2026-01-03T11:00:00Z',
      };

      mockService.deactivateIncidentType.mockResolvedValue(mockResult);

      await controller.deactivateIncidentType(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.deactivateIncidentType).toHaveBeenCalledWith('type-id-123');
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        message: 'Incident type deactivated successfully',
        data: mockResult,
      });
    });

    /**
     * Test: Handles deactivation of used type
     * Business rule: Cannot deactivate if used in incidents
     */
    it('should handle error when deactivating used type', async () => {
      mockRequest = {
        params: { id: 'type-id-123' },
      };

      const mockError = new Error(
        'Cannot deactivate incident type that is used in existing incidents'
      );
      mockService.deactivateIncidentType.mockRejectedValue(mockError);

      await controller.deactivateIncidentType(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockNext).toHaveBeenCalledWith(mockError);
      expect(mockResponse.json).not.toHaveBeenCalled();
    });
  });

  describe('Response Structure', () => {
    /**
     * Test: Success responses include success flag
     * Business rule: All responses should indicate success
     */
    it('should include success flag in all responses', async () => {
      mockRequest = {
        params: { id: 'type-id-123' },
      };

      const mockResult = {
        incidentTypeId: 'type-id-123',
        code: 'T-INC001',
      } as any;

      mockService.getIncidentTypeById.mockResolvedValue(mockResult);

      await controller.getIncidentTypeById(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockResponse.json).toHaveBeenCalledWith(
        expect.objectContaining({
          success: true,
        })
      );
    });

    /**
     * Test: List responses include count
     * Business rule: Should provide count for pagination
     */
    it('should include count in list responses', async () => {
      mockRequest = {
        query: {},
      };

      const mockResults = [{} as any, {} as any, {} as any];

      mockService.listIncidentTypes.mockResolvedValue(mockResults);

      await controller.listIncidentTypes(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockResponse.json).toHaveBeenCalledWith(
        expect.objectContaining({
          count: 3,
        })
      );
    });
  });

  describe('Error Handling', () => {
    /**
     * Test: Passes all service errors to error handler
     * Business rule: Errors should be handled by middleware
     */
    it('should pass all service errors to error handler', async () => {
      const endpoints = [
        {
          method: 'createIncidentType',
          request: { body: {} },
        },
        {
          method: 'getIncidentTypeById',
          request: { params: { id: 'test' } },
        },
        {
          method: 'updateIncidentType',
          request: { params: { id: 'test' }, body: {} },
        },
        {
          method: 'deactivateIncidentType',
          request: { params: { id: 'test' } },
        },
      ];

      for (const endpoint of endpoints) {
        const mockError = new Error('Test error');
        (mockService as any)[endpoint.method].mockRejectedValue(mockError);

        mockRequest = endpoint.request;

        await (controller as any)[endpoint.method](
          mockRequest as Request,
          mockResponse as Response,
          mockNext
        );

        expect(mockNext).toHaveBeenCalledWith(mockError);
      }
    });
  });
});
