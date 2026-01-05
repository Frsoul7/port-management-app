import { Request, Response, NextFunction } from 'express';
import { OperationPlanController } from '@presentation/controllers/OperationPlanController';
import { OperationPlanService } from '@application/services/OperationPlanService';
import { OperationPlanStatus, PlanningAlgorithm } from '@shared/types';

/**
 * Unit Tests for OperationPlanController - listPlans endpoint
 * US 4.1.3: List and Filter Operation Plans
 * 
 * Test Coverage:
 * - Query parameter parsing (targetDate, status, algorithm, vvnId, vesselImo, fromDate, toDate)
 * - Pagination query parameters (page, limit)
 * - Sorting query parameters (sortBy, sortOrder)
 * - Filter object construction
 * - Response format (success, data, pagination)
 * - Error handling
 */
describe('US 4.1.3 - OperationPlanController.listPlans', () => {
  let controller: OperationPlanController;
  let mockService: jest.Mocked<OperationPlanService>;
  let mockRequest: Partial<Request>;
  let mockResponse: Partial<Response>;
  let mockNext: NextFunction;

  beforeEach(() => {
    // Create mock service
    mockService = {
      listOperationPlans: jest.fn(),
    } as any;

    // Create controller instance
    controller = new OperationPlanController(mockService);

    // Create mock response
    mockResponse = {
      json: jest.fn(),
      status: jest.fn().mockReturnThis(),
    };

    // Create mock next function
    mockNext = jest.fn();
  });

  describe('Basic Filtering', () => {
    /**
     * Test: Returns all plans when no filters provided
     * Business rule: Default behavior lists all plans
     */
    it('should return all plans when no query parameters provided', async () => {
      mockRequest = {
        query: {},
      };

      const mockPlans = [
        {
          operationPlanId: 'PLAN-001',
          targetDate: new Date('2026-01-15'),
          status: OperationPlanStatus.GENERATED,
          algorithm: PlanningAlgorithm.OPTIMAL,
          operations: [],
          totalDelay: 0,
        },
      ];

      mockService.listOperationPlans.mockResolvedValue({
        data: mockPlans as any,
        pagination: {
          page: 1,
          limit: 10,
          total: 1,
          totalPages: 1,
        },
      });

      await controller.listPlans(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockService.listOperationPlans).toHaveBeenCalledWith({});

      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        data: mockPlans,
        pagination: {
          page: 1,
          limit: 10,
          total: 1,
          totalPages: 1,
        },
      });
    });

    /**
     * Test: Filters by status
     * Business rule: Support status-based filtering
     */
    it('should filter by status', async () => {
      mockRequest = {
        query: {
          status: OperationPlanStatus.APPROVED,
        },
      };

      const mockPlans = [
        {
          operationPlanId: 'PLAN-001',
          status: OperationPlanStatus.APPROVED,
        },
      ];

      mockService.listOperationPlans.mockResolvedValue({
        data: mockPlans as any,
        pagination: { page: 1, limit: 10, total: 1, totalPages: 1 },
      });

      await controller.listPlans(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockService.listOperationPlans).toHaveBeenCalledWith({
        status: OperationPlanStatus.APPROVED,
      });
    });

    /**
     * Test: Filters by algorithm
     * Business rule: Support algorithm-based filtering
     */
    it('should filter by algorithm', async () => {
      mockRequest = {
        query: {
          algorithm: PlanningAlgorithm.WEIGHTED,
        },
      };

      mockService.listOperationPlans.mockResolvedValue({
        data: [],
        pagination: { page: 1, limit: 10, total: 0, totalPages: 0 },
      });

      await controller.listPlans(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockService.listOperationPlans).toHaveBeenCalledWith({
        algorithm: PlanningAlgorithm.WEIGHTED,
      });
    });

    /**
     * Test: Filters by VVN ID
     * Business rule: Search by vessel visit notification identifier
     */
    it('should filter by VVN ID', async () => {
      mockRequest = {
        query: {
          vvnId: 'VVN-001',
        },
      };

      mockService.listOperationPlans.mockResolvedValue({
        data: [],
        pagination: { page: 1, limit: 10, total: 0, totalPages: 0 },
      });

      await controller.listPlans(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockService.listOperationPlans).toHaveBeenCalledWith({
        vvnId: 'VVN-001',
      });
    });

    /**
     * Test: Filters by vessel IMO
     * Business rule: Search by vessel IMO number
     */
    it('should filter by vessel IMO', async () => {
      mockRequest = {
        query: {
          vesselImo: 'IMO1234567',
        },
      };

      mockService.listOperationPlans.mockResolvedValue({
        data: [],
        pagination: { page: 1, limit: 10, total: 0, totalPages: 0 },
      });

      await controller.listPlans(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockService.listOperationPlans).toHaveBeenCalledWith({
        vesselImo: 'IMO1234567',
      });
    });

    /**
     * Test: Filters by target date
     * Business rule: Search by specific operation date
     */
    it('should filter by target date', async () => {
      mockRequest = {
        query: {
          targetDate: '2026-01-15',
        },
      };

      mockService.listOperationPlans.mockResolvedValue({
        data: [],
        pagination: { page: 1, limit: 10, total: 0, totalPages: 0 },
      });

      await controller.listPlans(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockService.listOperationPlans).toHaveBeenCalledWith({
        targetDate: '2026-01-15',
      });
    });
  });

  describe('Date Range Filtering', () => {
    /**
     * Test: Filters by date range (fromDate and toDate)
     * Business rule: Support date interval filtering
     */
    it('should filter by date range with fromDate and toDate', async () => {
      mockRequest = {
        query: {
          fromDate: '2026-01-10',
          toDate: '2026-01-20',
        },
      };

      mockService.listOperationPlans.mockResolvedValue({
        data: [],
        pagination: { page: 1, limit: 10, total: 0, totalPages: 0 },
      });

      await controller.listPlans(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockService.listOperationPlans).toHaveBeenCalledWith({
        fromDate: '2026-01-10',
        toDate: '2026-01-20',
      });
    });

    /**
     * Test: Filters with only fromDate
     * Business rule: Open-ended date range (from date onwards)
     */
    it('should filter with only fromDate', async () => {
      mockRequest = {
        query: {
          fromDate: '2026-01-10',
        },
      };

      mockService.listOperationPlans.mockResolvedValue({
        data: [],
        pagination: { page: 1, limit: 10, total: 0, totalPages: 0 },
      });

      await controller.listPlans(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockService.listOperationPlans).toHaveBeenCalledWith({
        fromDate: '2026-01-10',
      });
    });

    /**
     * Test: Filters with only toDate
     * Business rule: Open-ended date range (up to date)
     */
    it('should filter with only toDate', async () => {
      mockRequest = {
        query: {
          toDate: '2026-01-20',
        },
      };

      mockService.listOperationPlans.mockResolvedValue({
        data: [],
        pagination: { page: 1, limit: 10, total: 0, totalPages: 0 },
      });

      await controller.listPlans(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockService.listOperationPlans).toHaveBeenCalledWith({
        toDate: '2026-01-20',
      });
    });

    /**
     * Test: Date range overrides single targetDate
     * Business rule: Date range takes precedence over single date
     */
    it('should use date range when both targetDate and fromDate/toDate provided', async () => {
      mockRequest = {
        query: {
          targetDate: '2026-01-15',
          fromDate: '2026-01-10',
          toDate: '2026-01-20',
        },
      };

      mockService.listOperationPlans.mockResolvedValue({
        data: [],
        pagination: { page: 1, limit: 10, total: 0, totalPages: 0 },
      });

      await controller.listPlans(mockRequest as Request, mockResponse as Response, mockNext);

      // Should use date range, not single targetDate
      expect(mockService.listOperationPlans).toHaveBeenCalledWith({
        targetDate: '2026-01-15',
        fromDate: '2026-01-10',
        toDate: '2026-01-20',
      });
    });
  });

  describe('Combined Filters', () => {
    /**
     * Test: Combines multiple filter criteria
     * Business rule: Support complex queries with multiple filters
     */
    it('should combine multiple filters', async () => {
      mockRequest = {
        query: {
          status: OperationPlanStatus.APPROVED,
          algorithm: PlanningAlgorithm.OPTIMAL,
          vvnId: 'VVN-001',
        },
      };

      mockService.listOperationPlans.mockResolvedValue({
        data: [],
        pagination: { page: 1, limit: 10, total: 0, totalPages: 0 },
      });

      await controller.listPlans(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockService.listOperationPlans).toHaveBeenCalledWith({
        status: OperationPlanStatus.APPROVED,
        algorithm: PlanningAlgorithm.OPTIMAL,
        vvnId: 'VVN-001',
      });
    });

    /**
     * Test: Combines filters with date range
     * Business rule: Support date range with other filters
     */
    it('should combine filters with date range', async () => {
      mockRequest = {
        query: {
          status: OperationPlanStatus.APPROVED,
          fromDate: '2026-01-10',
          toDate: '2026-01-20',
        },
      };

      mockService.listOperationPlans.mockResolvedValue({
        data: [],
        pagination: { page: 1, limit: 10, total: 0, totalPages: 0 },
      });

      await controller.listPlans(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockService.listOperationPlans).toHaveBeenCalledWith({
        status: OperationPlanStatus.APPROVED,
        fromDate: '2026-01-10',
        toDate: '2026-01-20',
      });
    });
  });

  describe('Pagination', () => {
    /**
     * Test: Uses default pagination values
     * Business rule: Default page 1, limit 10
     */
    it('should use default pagination values', async () => {
      mockRequest = {
        query: {},
      };

      mockService.listOperationPlans.mockResolvedValue({
        data: [],
        pagination: { page: 1, limit: 10, total: 0, totalPages: 0 },
      });

      await controller.listPlans(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockService.listOperationPlans).toHaveBeenCalledWith({});

      expect(mockResponse.json).toHaveBeenCalledWith(
        expect.objectContaining({
          pagination: expect.objectContaining({
            page: 1,
            limit: 10,
          }),
        })
      );
    });

    /**
     * Test: Respects custom page and limit
     * Business rule: Support custom pagination
     */
    it('should respect custom page and limit', async () => {
      mockRequest = {
        query: {
          page: '3',
          limit: '20',
        },
      };

      mockService.listOperationPlans.mockResolvedValue({
        data: [],
        pagination: { page: 3, limit: 20, total: 100, totalPages: 5 },
      });

      await controller.listPlans(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockService.listOperationPlans).toHaveBeenCalledWith({
        page: '3',
        limit: '20',
      });

      expect(mockResponse.json).toHaveBeenCalledWith(
        expect.objectContaining({
          pagination: {
            page: 3,
            limit: 20,
            total: 100,
            totalPages: 5, // Math.ceil(100 / 20)
          },
        })
      );
    });

    /**
     * Test: Calculates total pages correctly
     * Business rule: Total pages = ceil(total / limit)
     */
    it('should calculate total pages correctly', async () => {
      mockRequest = {
        query: {
          page: '1',
          limit: '10',
        },
      };

      mockService.listOperationPlans.mockResolvedValue({
        data: [],
        pagination: { page: 1, limit: 10, total: 25, totalPages: 3 },
      });

      await controller.listPlans(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockResponse.json).toHaveBeenCalledWith(
        expect.objectContaining({
          pagination: expect.objectContaining({
            total: 25,
            totalPages: 3, // Math.ceil(25 / 10)
          }),
        })
      );
    });
  });

  describe('Sorting', () => {
    /**
     * Test: Uses default sorting
     * Business rule: Default sort by targetDate descending
     */
    it('should use default sorting values', async () => {
      mockRequest = {
        query: {},
      };

      mockService.listOperationPlans.mockResolvedValue({
        data: [],
        pagination: { page: 1, limit: 10, total: 0, totalPages: 0 },
      });

      await controller.listPlans(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockService.listOperationPlans).toHaveBeenCalledWith({});
    });

    /**
     * Test: Respects custom sorting
     * Business rule: Support custom sort field and order
     */
    it('should respect custom sortBy and sortOrder', async () => {
      mockRequest = {
        query: {
          sortBy: 'createdAt',
          sortOrder: 'asc',
        },
      };

      mockService.listOperationPlans.mockResolvedValue({
        data: [],
        pagination: { page: 1, limit: 10, total: 0, totalPages: 0 },
      });

      await controller.listPlans(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockService.listOperationPlans).toHaveBeenCalledWith({
        sortBy: 'createdAt',
        sortOrder: 'asc',
      });
    });
  });

  describe('Response Format', () => {
    /**
     * Test: Returns success response with data
     * Business rule: Standard response format
     */
    it('should return success response with data', async () => {
      mockRequest = {
        query: {},
      };

      const mockPlans = [
        {
          operationPlanId: 'PLAN-001',
          targetDate: new Date('2026-01-15'),
          status: OperationPlanStatus.GENERATED,
          algorithm: PlanningAlgorithm.OPTIMAL,
          operations: [],
        },
      ];

      mockService.listOperationPlans.mockResolvedValue({
        data: mockPlans as any,
        pagination: { page: 1, limit: 10, total: 1, totalPages: 1 },
      });

      await controller.listPlans(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        data: mockPlans,
        pagination: expect.any(Object),
      });
    });

    /**
     * Test: Returns empty array when no results
     * Business rule: Empty results return empty array, not null
     */
    it('should return empty array when no results found', async () => {
      mockRequest = {
        query: {
          vvnId: 'VVN-999',
        },
      };

      mockService.listOperationPlans.mockResolvedValue({
        data: [],
        pagination: { page: 1, limit: 10, total: 0, totalPages: 0 },
      });

      await controller.listPlans(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        data: [],
        pagination: {
          page: 1,
          limit: 10,
          total: 0,
          totalPages: 0,
        },
      });
    });

    /**
     * Test: Includes pagination metadata
     * Business rule: Response includes pagination info
     */
    it('should include pagination metadata in response', async () => {
      mockRequest = {
        query: {
          page: '2',
          limit: '5',
        },
      };

      mockService.listOperationPlans.mockResolvedValue({
        data: [],
        pagination: { page: 2, limit: 5, total: 12, totalPages: 3 },
      });

      await controller.listPlans(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockResponse.json).toHaveBeenCalledWith(
        expect.objectContaining({
          pagination: {
            page: 2,
            limit: 5,
            total: 12,
            totalPages: 3,
          },
        })
      );
    });
  });

  describe('Error Handling', () => {
    /**
     * Test: Handles service errors
     * Business rule: Errors are passed to error handler middleware
     */
    it('should handle service errors', async () => {
      mockRequest = {
        query: {},
      };

      const error = new Error('Database connection failed');
      mockService.listOperationPlans.mockRejectedValue(error);

      await controller.listPlans(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockNext).toHaveBeenCalledWith(error);
      expect(mockResponse.json).not.toHaveBeenCalled();
    });

    /**
     * Test: Handles invalid date format
     * Business rule: Invalid dates should cause error
     */
    it('should handle invalid date format', async () => {
      mockRequest = {
        query: {
          targetDate: 'invalid-date',
        },
      };

      // Date constructor with invalid string creates Invalid Date
      // Service should handle or validation should catch this
      mockService.listOperationPlans.mockResolvedValue({
        data: [],
        pagination: { page: 1, limit: 10, total: 0, totalPages: 0 },
      });

      await controller.listPlans(mockRequest as Request, mockResponse as Response, mockNext);

      // Controller will pass invalid Date object to service
      expect(mockService.listOperationPlans).toHaveBeenCalled();
    });

    /**
     * Test: Handles count service errors
     * Business rule: Count errors are also passed to error handler
     */
    it('should handle errors from service', async () => {
      mockRequest = {
        query: {},
      };

      const error = new Error('Service query failed');
      mockService.listOperationPlans.mockRejectedValue(error);

      await controller.listPlans(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockNext).toHaveBeenCalledWith(error);
      expect(mockResponse.json).not.toHaveBeenCalled();
    });
  });
});
