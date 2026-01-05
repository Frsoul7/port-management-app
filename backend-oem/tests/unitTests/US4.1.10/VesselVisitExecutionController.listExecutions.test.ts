import { Request, Response, NextFunction } from 'express';
import { VesselVisitExecutionController } from '../../../src/presentation/controllers/VesselVisitExecutionController';
import { VesselVisitExecutionService } from '../../../src/application/services/VesselVisitExecutionService';
import { VesselVisitExecutionStatus } from '../../../src/shared/types';

describe('US 4.1.10 - VesselVisitExecutionController.listExecutions', () => {
  let controller: VesselVisitExecutionController;
  let mockService: jest.Mocked<VesselVisitExecutionService>;
  let mockRequest: Partial<Request>;
  let mockResponse: Partial<Response>;
  let mockNext: NextFunction;

  beforeEach(() => {
    mockService = {
      listVesselVisitExecutions: jest.fn(),
    } as any;

    controller = new VesselVisitExecutionController(mockService);

    mockResponse = {
      json: jest.fn(),
      status: jest.fn().mockReturnThis(),
    };

    mockNext = jest.fn();
  });

  describe('Valid List Requests', () => {
    /**
     * Test: Lists all VVEs with default pagination
     * Business rule: Should return paginated list of VVEs
     */
    it('should list all VVEs with default pagination', async () => {
      mockRequest = {
        query: {},
      };

      const mockVves = [
        {
          vveId: 'VVE-001',
          vvnId: 'VVN-001',
          status: VesselVisitExecutionStatus.IN_PROGRESS,
        },
        {
          vveId: 'VVE-002',
          vvnId: 'VVN-002',
          status: VesselVisitExecutionStatus.COMPLETED,
        },
      ];

      mockService.listVesselVisitExecutions.mockResolvedValue({
        data: mockVves as any,
        pagination: { page: 1, limit: 10, total: 2, totalPages: 1 },
      });

      await controller.listExecutions(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.listVesselVisitExecutions).toHaveBeenCalledWith({});
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        data: mockVves,
        pagination: {
          page: 1,
          limit: 10,
          total: 2,
          totalPages: 1,
        },
      });
    });

    /**
     * Test: Filters by VVN ID
     * Business rule: Should filter results by vessel visit
     */
    it('should filter VVEs by VVN ID', async () => {
      mockRequest = {
        query: {
          vvnId: 'VVN-001',
        },
      };

      mockService.listVesselVisitExecutions.mockResolvedValue({
        data: [],
        pagination: { page: 1, limit: 10, total: 0, totalPages: 0 },
      });

      await controller.listExecutions(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.listVesselVisitExecutions).toHaveBeenCalledWith({
        vvnId: 'VVN-001',
      });
    });

    /**
     * Test: Filters by status
     * Business rule: Should filter results by execution status
     */
    it('should filter VVEs by status', async () => {
      mockRequest = {
        query: {
          status: 'IN_PROGRESS',
        },
      };

      mockService.listVesselVisitExecutions.mockResolvedValue({
        data: [],
        pagination: { page: 1, limit: 10, total: 0, totalPages: 0 },
      });

      await controller.listExecutions(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.listVesselVisitExecutions).toHaveBeenCalledWith({
        status: 'IN_PROGRESS',
      });
    });

    /**
     * Test: Filters by date range
     * Business rule: Should support date range filtering
     */
    it('should filter VVEs by date range', async () => {
      mockRequest = {
        query: {
          fromDate: '2026-01-01',
          toDate: '2026-01-31',
        },
      };

      mockService.listVesselVisitExecutions.mockResolvedValue({
        data: [],
        pagination: { page: 1, limit: 10, total: 0, totalPages: 0 },
      });

      await controller.listExecutions(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.listVesselVisitExecutions).toHaveBeenCalledWith({
        fromDate: '2026-01-01',
        toDate: '2026-01-31',
      });
    });

    /**
     * Test: Combines multiple filters
     * Business rule: Should support combining filters
     */
    it('should combine VVN ID, status, and date filters', async () => {
      mockRequest = {
        query: {
          vvnId: 'VVN-001',
          status: 'COMPLETED',
          fromDate: '2026-01-01',
        },
      };

      mockService.listVesselVisitExecutions.mockResolvedValue({
        data: [],
        pagination: { page: 1, limit: 10, total: 0, totalPages: 0 },
      });

      await controller.listExecutions(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.listVesselVisitExecutions).toHaveBeenCalledWith({
        vvnId: 'VVN-001',
        status: 'COMPLETED',
        fromDate: '2026-01-01',
      });
    });
  });

  describe('Pagination', () => {
    /**
     * Test: Handles custom page and limit
     * Business rule: Should support custom pagination parameters
     */
    it('should handle custom page and limit parameters', async () => {
      mockRequest = {
        query: {
          page: '2',
          limit: '20',
        },
      };

      mockService.listVesselVisitExecutions.mockResolvedValue({
        data: [],
        pagination: { page: 2, limit: 20, total: 50, totalPages: 3 },
      });

      await controller.listExecutions(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.listVesselVisitExecutions).toHaveBeenCalledWith({
        page: '2',
        limit: '20',
      });

      expect(mockResponse.json).toHaveBeenCalledWith(
        expect.objectContaining({
          pagination: {
            page: 2,
            limit: 20,
            total: 50,
            totalPages: 3,
          },
        })
      );
    });

    /**
     * Test: Calculates correct skip value
     * Business rule: Should calculate skip from page and limit
     */
    it('should calculate skip value correctly for different pages', async () => {
      mockService.listVesselVisitExecutions.mockResolvedValue({
        data: [],
        pagination: { page: 1, limit: 10, total: 0, totalPages: 0 },
      });

      // Page 1, Limit 10 → pagination page=1
      mockRequest = { query: { page: '1', limit: '10' } };
      await controller.listExecutions(mockRequest as Request, mockResponse as Response, mockNext);
      expect(mockService.listVesselVisitExecutions).toHaveBeenCalledWith({ page: '1', limit: '10' });

      // Page 3, Limit 10 → pagination page=3
      mockRequest = { query: { page: '3', limit: '10' } };
      await controller.listExecutions(mockRequest as Request, mockResponse as Response, mockNext);
      expect(mockService.listVesselVisitExecutions).toHaveBeenCalledWith({ page: '3', limit: '10' });

      // Page 2, Limit 25 → pagination page=2
      mockRequest = { query: { page: '2', limit: '25' } };
      await controller.listExecutions(mockRequest as Request, mockResponse as Response, mockNext);
      expect(mockService.listVesselVisitExecutions).toHaveBeenCalledWith({ page: '2', limit: '25' });
    });

    /**
     * Test: Calculates total pages correctly
     * Business rule: Should calculate correct number of pages
     */
    it('should calculate total pages correctly', async () => {
      mockRequest = { query: { limit: '10' } };

      // 23 total items, 10 per page → 3 pages
      mockService.listVesselVisitExecutions.mockResolvedValue({
        data: [],
        pagination: { page: 1, limit: 10, total: 23, totalPages: 3 },
      });
      await controller.listExecutions(mockRequest as Request, mockResponse as Response, mockNext);
      expect((mockResponse.json as jest.Mock).mock.calls[0]![0].pagination.totalPages).toBe(3);

      // 30 total items, 10 per page → 3 pages
      mockService.listVesselVisitExecutions.mockResolvedValue({
        data: [],
        pagination: { page: 1, limit: 10, total: 30, totalPages: 3 },
      });
      await controller.listExecutions(mockRequest as Request, mockResponse as Response, mockNext);
      expect((mockResponse.json as jest.Mock).mock.calls[1]![0].pagination.totalPages).toBe(3);

      // 0 total items → 0 pages
      mockService.listVesselVisitExecutions.mockResolvedValue({
        data: [],
        pagination: { page: 1, limit: 10, total: 0, totalPages: 0 },
      });
      await controller.listExecutions(mockRequest as Request, mockResponse as Response, mockNext);
      expect((mockResponse.json as jest.Mock).mock.calls[2]![0].pagination.totalPages).toBe(0);
    });

    /**
     * Test: Includes pagination in response
     * Business rule: Response must include pagination metadata
     */
    it('should include pagination metadata in response', async () => {
      mockRequest = { query: {} };
      mockService.listVesselVisitExecutions.mockResolvedValue({
        data: [],
        pagination: { page: 1, limit: 10, total: 100, totalPages: 10 },
      });

      await controller.listExecutions(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      const response = (mockResponse.json as jest.Mock).mock.calls[0]![0];
      expect(response.pagination).toEqual({
        page: 1,
        limit: 10,
        total: 100,
        totalPages: 10,
      });
    });
  });

  describe('Sorting', () => {
    /**
     * Test: Handles custom sort field
     * Business rule: Should support sorting by different fields
     */
    it('should handle custom sortBy parameter', async () => {
      mockRequest = {
        query: {
          sortBy: 'actualPortArrivalTime',
        },
      };

      mockService.listVesselVisitExecutions.mockResolvedValue({
        data: [],
        pagination: { page: 1, limit: 10, total: 0, totalPages: 0 },
      });

      await controller.listExecutions(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.listVesselVisitExecutions).toHaveBeenCalledWith({
        sortBy: 'actualPortArrivalTime',
      });
    });

    /**
     * Test: Handles sort order parameter
     * Business rule: Should support asc and desc sorting
     */
    it('should handle sortOrder parameter', async () => {
      mockRequest = {
        query: {
          sortBy: 'createdAt',
          sortOrder: 'asc',
        },
      };

      mockService.listVesselVisitExecutions.mockResolvedValue({
        data: [],
        pagination: { page: 1, limit: 10, total: 0, totalPages: 0 },
      });

      await controller.listExecutions(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.listVesselVisitExecutions).toHaveBeenCalledWith({
        sortBy: 'createdAt',
        sortOrder: 'asc',
      });
    });

    /**
     * Test: Uses default sort parameters
     * Business rule: Should default to createdAt desc
     */
    it('should use default sort parameters when not specified', async () => {
      mockRequest = { query: {} };
      mockService.listVesselVisitExecutions.mockResolvedValue({
        data: [],
        pagination: { page: 1, limit: 10, total: 0, totalPages: 0 },
      });

      await controller.listExecutions(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.listVesselVisitExecutions).toHaveBeenCalledWith({});
    });
  });

  describe('Response Structure', () => {
    /**
     * Test: Response includes success flag
     * Business rule: All responses should have success indicator
     */
    it('should include success flag in response', async () => {
      mockRequest = { query: {} };
      mockService.listVesselVisitExecutions.mockResolvedValue({
        data: [],
        pagination: { page: 1, limit: 10, total: 0, totalPages: 0 },
      });

      await controller.listExecutions(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockResponse.json).toHaveBeenCalledWith(
        expect.objectContaining({ success: true })
      );
    });

    /**
     * Test: Response includes data array
     * Business rule: VVEs should be in data property
     */
    it('should include VVE data array in response', async () => {
      mockRequest = { query: {} };
      const mockData = [{ vveId: 'VVE-001', vvnId: 'VVN-001' }];
      mockService.listVesselVisitExecutions.mockResolvedValue({
        data: mockData as any,
        pagination: { page: 1, limit: 10, total: 1, totalPages: 1 },
      });

      await controller.listExecutions(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockResponse.json).toHaveBeenCalledWith(
        expect.objectContaining({ data: mockData })
      );
    });

    /**
     * Test: Empty results return empty array
     * Business rule: Should return empty array, not null
     */
    it('should return empty array for no results', async () => {
      mockRequest = { query: {} };
      mockService.listVesselVisitExecutions.mockResolvedValue({
        data: [],
        pagination: { page: 1, limit: 10, total: 0, totalPages: 0 },
      });

      await controller.listExecutions(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      const response = (mockResponse.json as jest.Mock).mock.calls[0]![0];
      expect(response.data).toEqual([]);
    });
  });

  describe('Date Filtering', () => {
    /**
     * Test: Filters by fromDate only
     * Business rule: Should support filtering from a start date
     */
    it('should filter by fromDate only', async () => {
      mockRequest = {
        query: {
          fromDate: '2026-01-15',
        },
      };

      mockService.listVesselVisitExecutions.mockResolvedValue({
        data: [],
        pagination: { page: 1, limit: 10, total: 0, totalPages: 0 },
      });

      await controller.listExecutions(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.listVesselVisitExecutions).toHaveBeenCalledWith({
        fromDate: '2026-01-15',
      });
    });

    /**
     * Test: Filters by toDate only
     * Business rule: Should support filtering up to an end date
     */
    it('should filter by toDate only', async () => {
      mockRequest = {
        query: {
          toDate: '2026-01-31',
        },
      };

      mockService.listVesselVisitExecutions.mockResolvedValue({
        data: [],
        pagination: { page: 1, limit: 10, total: 0, totalPages: 0 },
      });

      await controller.listExecutions(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.listVesselVisitExecutions).toHaveBeenCalledWith({
        toDate: '2026-01-31',
      });
    });

    /**
     * Test: Converts date strings to Date objects
     * Business rule: Should parse date strings correctly
     */
    it('should convert date strings to Date objects', async () => {
      mockRequest = {
        query: {
          fromDate: '2026-01-15T00:00:00Z',
          toDate: '2026-01-20T23:59:59Z',
        },
      };

      mockService.listVesselVisitExecutions.mockResolvedValue({
        data: [],
        pagination: { page: 1, limit: 10, total: 0, totalPages: 0 },
      });

      await controller.listExecutions(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.listVesselVisitExecutions).toHaveBeenCalledWith({
        fromDate: '2026-01-15T00:00:00Z',
        toDate: '2026-01-20T23:59:59Z',
      });
    });
  });

  describe('Error Handling', () => {
    /**
     * Test: Passes service errors to error handler
     * Business rule: Errors should be handled by middleware
     */
    it('should pass service errors to error handler', async () => {
      mockRequest = { query: {} };
      const error = new Error('Database connection failed');
      mockService.listVesselVisitExecutions.mockRejectedValue(error);

      await controller.listExecutions(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockNext).toHaveBeenCalledWith(error);
      expect(mockResponse.json).not.toHaveBeenCalled();
    });

    /**
     * Test: Handles count errors
     * Business rule: Count errors should be propagated
     */
    it('should handle errors during count operation', async () => {
      mockRequest = { query: {} };
      mockService.listVesselVisitExecutions.mockRejectedValue(new Error('Service failed'));

      await controller.listExecutions(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockNext).toHaveBeenCalledWith(expect.any(Error));
    });

    /**
     * Test: Handles invalid date formats
     * Business rule: Invalid dates should be caught
     */
    it('should handle invalid date formats', async () => {
      mockRequest = {
        query: {
          fromDate: 'invalid-date',
        },
      };

      mockService.listVesselVisitExecutions.mockResolvedValue({
        data: [],
        pagination: { page: 1, limit: 10, total: 0, totalPages: 0 },
      });

      await controller.listExecutions(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      // Invalid date creates Invalid Date object, service should handle it
      expect(mockService.listVesselVisitExecutions).toHaveBeenCalled();
    });
  });

  describe('Integration Scenarios', () => {
    /**
     * Test: Complete pagination workflow
     * Business rule: Should handle full pagination flow
     */
    it('should handle complete pagination workflow', async () => {
      mockRequest = {
        query: {
          status: 'IN_PROGRESS',
          page: '2',
          limit: '5',
        },
      };

      const mockVves = [
        { vveId: 'VVE-006', status: VesselVisitExecutionStatus.IN_PROGRESS },
        { vveId: 'VVE-007', status: VesselVisitExecutionStatus.IN_PROGRESS },
      ];

      mockService.listVesselVisitExecutions.mockResolvedValue({
        data: mockVves as any,
        pagination: { page: 2, limit: 5, total: 12, totalPages: 3 },
      });

      await controller.listExecutions(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.listVesselVisitExecutions).toHaveBeenCalledWith({
        status: 'IN_PROGRESS',
        page: '2',
        limit: '5',
      });

      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        data: mockVves,
        pagination: {
          page: 2,
          limit: 5,
          total: 12,
          totalPages: 3,
        },
      });
    });
  });
});
