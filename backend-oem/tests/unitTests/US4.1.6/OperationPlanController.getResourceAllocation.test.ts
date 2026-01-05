import { Request, Response, NextFunction } from 'express';
import { OperationPlanController } from '../../../src/presentation/controllers/OperationPlanController';
import { OperationPlanService } from '../../../src/application/services/OperationPlanService';

describe('US 4.1.6 - OperationPlanController.getResourceAllocation', () => {
  let controller: OperationPlanController;
  let mockService: jest.Mocked<OperationPlanService>;
  let mockRequest: Partial<Request>;
  let mockResponse: Partial<Response>;
  let mockNext: NextFunction;

  beforeEach(() => {
    mockService = {
      getResourceAllocation: jest.fn(),
    } as any;

    controller = new OperationPlanController(mockService);

    mockResponse = {
      json: jest.fn(),
      status: jest.fn().mockReturnThis(),
    };

    mockNext = jest.fn();
  });

  describe('Valid Query Parameters', () => {
    /**
     * Test: Returns resource allocation for valid parameters
     * Business rule: Should call service with correct params and return result
     */
    it('should return resource allocation for valid crane query', async () => {
      mockRequest = {
        query: {
          resourceType: 'crane',
          resourceId: 'CRANE-1',
          fromDate: '2026-01-15',
          toDate: '2026-01-16',
        },
      };

      const mockAllocation = {
        resourceType: 'crane',
        resourceId: 'CRANE-1',
        fromDate: '2026-01-15T00:00:00.000Z',
        toDate: '2026-01-16T00:00:00.000Z',
        totalAllocatedTime: 12.5,
        operationCount: 3,
        operations: [],
      };

      mockService.getResourceAllocation.mockResolvedValue(mockAllocation);

      await controller.getResourceAllocation(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.getResourceAllocation).toHaveBeenCalledWith(
        'crane',
        'CRANE-1',
        new Date('2026-01-15'),
        new Date('2026-01-16')
      );
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        data: mockAllocation,
      });
    });

    /**
     * Test: Handles dock resource type
     * Business rule: Should work for dock queries
     */
    it('should handle dock resource allocation query', async () => {
      mockRequest = {
        query: {
          resourceType: 'dock',
          resourceId: 'DOCK-A',
          fromDate: '2026-01-15T00:00:00Z',
          toDate: '2026-01-15T23:59:59Z',
        },
      };

      const mockAllocation = {
        resourceType: 'dock',
        resourceId: 'DOCK-A',
        fromDate: '2026-01-15T00:00:00.000Z',
        toDate: '2026-01-15T23:59:59.000Z',
        totalAllocatedTime: 8,
        operationCount: 2,
        operations: [],
      };

      mockService.getResourceAllocation.mockResolvedValue(mockAllocation);

      await controller.getResourceAllocation(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.getResourceAllocation).toHaveBeenCalledWith(
        'dock',
        'DOCK-A',
        expect.any(Date),
        expect.any(Date)
      );
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        data: mockAllocation,
      });
    });

    /**
     * Test: Handles staff resource type
     * Business rule: Should work for staff queries
     */
    it('should handle staff resource allocation query', async () => {
      mockRequest = {
        query: {
          resourceType: 'staff',
          resourceId: 'STAFF-001',
          fromDate: '2026-01-15',
          toDate: '2026-01-20',
        },
      };

      const mockAllocation = {
        resourceType: 'staff',
        resourceId: 'STAFF-001',
        fromDate: '2026-01-15T00:00:00.000Z',
        toDate: '2026-01-20T00:00:00.000Z',
        totalAllocatedTime: 40,
        operationCount: 10,
        operations: [],
      };

      mockService.getResourceAllocation.mockResolvedValue(mockAllocation);

      await controller.getResourceAllocation(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.getResourceAllocation).toHaveBeenCalledWith(
        'staff',
        'STAFF-001',
        expect.any(Date),
        expect.any(Date)
      );
      expect(mockResponse.json).toHaveBeenCalled();
    });

    /**
     * Test: Returns empty result when no allocations found
     * Business rule: Zero allocations should return success with zero counts
     */
    it('should return empty result when no allocations found', async () => {
      mockRequest = {
        query: {
          resourceType: 'crane',
          resourceId: 'CRANE-99',
          fromDate: '2026-01-15',
          toDate: '2026-01-16',
        },
      };

      const mockAllocation = {
        resourceType: 'crane',
        resourceId: 'CRANE-99',
        fromDate: '2026-01-15T00:00:00.000Z',
        toDate: '2026-01-16T00:00:00.000Z',
        totalAllocatedTime: 0,
        operationCount: 0,
        operations: [],
      };

      mockService.getResourceAllocation.mockResolvedValue(mockAllocation);

      await controller.getResourceAllocation(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        data: mockAllocation,
      });
    });
  });

  describe('Missing Parameters', () => {
    /**
     * Test: Returns 400 when resourceType missing
     * Business rule: All four params are required
     */
    it('should return 400 when resourceType is missing', async () => {
      mockRequest = {
        query: {
          resourceId: 'CRANE-1',
          fromDate: '2026-01-15',
          toDate: '2026-01-16',
        },
      };

      await controller.getResourceAllocation(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockResponse.status).toHaveBeenCalledWith(400);
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: false,
        message: 'resourceType, resourceId, fromDate, and toDate are required',
      });
    });

    /**
     * Test: Returns 400 when resourceId missing
     * Business rule: All four params are required
     */
    it('should return 400 when resourceId is missing', async () => {
      mockRequest = {
        query: {
          resourceType: 'crane',
          fromDate: '2026-01-15',
          toDate: '2026-01-16',
        },
      };

      await controller.getResourceAllocation(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockResponse.status).toHaveBeenCalledWith(400);
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: false,
        message: 'resourceType, resourceId, fromDate, and toDate are required',
      });
    });

    /**
     * Test: Returns 400 when fromDate missing
     * Business rule: All four params are required
     */
    it('should return 400 when fromDate is missing', async () => {
      mockRequest = {
        query: {
          resourceType: 'crane',
          resourceId: 'CRANE-1',
          toDate: '2026-01-16',
        },
      };

      await controller.getResourceAllocation(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockResponse.status).toHaveBeenCalledWith(400);
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: false,
        message: 'resourceType, resourceId, fromDate, and toDate are required',
      });
    });

    /**
     * Test: Returns 400 when toDate missing
     * Business rule: All four params are required
     */
    it('should return 400 when toDate is missing', async () => {
      mockRequest = {
        query: {
          resourceType: 'crane',
          resourceId: 'CRANE-1',
          fromDate: '2026-01-15',
        },
      };

      await controller.getResourceAllocation(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockResponse.status).toHaveBeenCalledWith(400);
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: false,
        message: 'resourceType, resourceId, fromDate, and toDate are required',
      });
    });

    /**
     * Test: Returns 400 when all parameters missing
     * Business rule: All four params are required
     */
    it('should return 400 when all parameters are missing', async () => {
      mockRequest = {
        query: {},
      };

      await controller.getResourceAllocation(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockResponse.status).toHaveBeenCalledWith(400);
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: false,
        message: 'resourceType, resourceId, fromDate, and toDate are required',
      });
    });
  });

  describe('Date Parsing and Formats', () => {
    /**
     * Test: Accepts ISO 8601 date format
     * Business rule: Should handle standard ISO date strings
     */
    it('should accept ISO 8601 date format', async () => {
      mockRequest = {
        query: {
          resourceType: 'crane',
          resourceId: 'CRANE-1',
          fromDate: '2026-01-15T08:00:00Z',
          toDate: '2026-01-16T18:00:00Z',
        },
      };

      mockService.getResourceAllocation.mockResolvedValue({
        resourceType: 'crane',
        resourceId: 'CRANE-1',
        fromDate: '2026-01-15T08:00:00.000Z',
        toDate: '2026-01-16T18:00:00.000Z',
        totalAllocatedTime: 0,
        operationCount: 0,
        operations: [],
      });

      await controller.getResourceAllocation(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.getResourceAllocation).toHaveBeenCalledWith(
        'crane',
        'CRANE-1',
        new Date('2026-01-15T08:00:00Z'),
        new Date('2026-01-16T18:00:00Z')
      );
    });

    /**
     * Test: Accepts YYYY-MM-DD format
     * Business rule: Should handle simple date strings
     */
    it('should accept YYYY-MM-DD format', async () => {
      mockRequest = {
        query: {
          resourceType: 'dock',
          resourceId: 'DOCK-A',
          fromDate: '2026-01-15',
          toDate: '2026-01-16',
        },
      };

      mockService.getResourceAllocation.mockResolvedValue({
        resourceType: 'dock',
        resourceId: 'DOCK-A',
        fromDate: '2026-01-15T00:00:00.000Z',
        toDate: '2026-01-16T00:00:00.000Z',
        totalAllocatedTime: 0,
        operationCount: 0,
        operations: [],
      });

      await controller.getResourceAllocation(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.getResourceAllocation).toHaveBeenCalled();
    });

    /**
     * Test: Handles single-day date range
     * Business rule: fromDate and toDate can be the same day
     */
    it('should handle single-day date range', async () => {
      mockRequest = {
        query: {
          resourceType: 'staff',
          resourceId: 'STAFF-001',
          fromDate: '2026-01-15T00:00:00Z',
          toDate: '2026-01-15T23:59:59Z',
        },
      };

      mockService.getResourceAllocation.mockResolvedValue({
        resourceType: 'staff',
        resourceId: 'STAFF-001',
        fromDate: '2026-01-15T00:00:00.000Z',
        toDate: '2026-01-15T23:59:59.000Z',
        totalAllocatedTime: 8,
        operationCount: 1,
        operations: [],
      });

      await controller.getResourceAllocation(
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
  });

  describe('Response Structure', () => {
    /**
     * Test: Response includes success flag
     * Business rule: All responses should have success boolean
     */
    it('should include success flag in response', async () => {
      mockRequest = {
        query: {
          resourceType: 'crane',
          resourceId: 'CRANE-1',
          fromDate: '2026-01-15',
          toDate: '2026-01-16',
        },
      };

      mockService.getResourceAllocation.mockResolvedValue({
        resourceType: 'crane',
        resourceId: 'CRANE-1',
        fromDate: '2026-01-15T00:00:00.000Z',
        toDate: '2026-01-16T00:00:00.000Z',
        totalAllocatedTime: 10,
        operationCount: 2,
        operations: [],
      });

      await controller.getResourceAllocation(
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
     * Test: Response wraps data in data field
     * Business rule: Service result should be in data property
     */
    it('should wrap allocation data in data field', async () => {
      mockRequest = {
        query: {
          resourceType: 'dock',
          resourceId: 'DOCK-B',
          fromDate: '2026-01-15',
          toDate: '2026-01-16',
        },
      };

      const mockAllocation = {
        resourceType: 'dock',
        resourceId: 'DOCK-B',
        fromDate: '2026-01-15T00:00:00.000Z',
        toDate: '2026-01-16T00:00:00.000Z',
        totalAllocatedTime: 16,
        operationCount: 4,
        operations: [
          { planId: 'PLAN-001', vvnId: 'VVN-001', duration: 4 },
          { planId: 'PLAN-001', vvnId: 'VVN-002', duration: 4 },
          { planId: 'PLAN-002', vvnId: 'VVN-003', duration: 4 },
          { planId: 'PLAN-002', vvnId: 'VVN-004', duration: 4 },
        ],
      };

      mockService.getResourceAllocation.mockResolvedValue(mockAllocation);

      await controller.getResourceAllocation(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        data: mockAllocation,
      });
    });

    /**
     * Test: Preserves all service response fields
     * Business rule: Should include resource info, dates, totals, operations
     */
    it('should preserve all service response fields', async () => {
      mockRequest = {
        query: {
          resourceType: 'crane',
          resourceId: 'CRANE-1',
          fromDate: '2026-01-15',
          toDate: '2026-01-16',
        },
      };

      const mockAllocation = {
        resourceType: 'crane',
        resourceId: 'CRANE-1',
        fromDate: '2026-01-15T00:00:00.000Z',
        toDate: '2026-01-16T00:00:00.000Z',
        totalAllocatedTime: 12.5,
        operationCount: 3,
        operations: [
          {
            planId: 'PLAN-001',
            vvnId: 'VVN-001',
            vesselImo: 'IMO1234567',
            plannedStart: new Date('2026-01-15T08:00:00Z'),
            plannedEnd: new Date('2026-01-15T12:00:00Z'),
            duration: 4,
          },
        ],
      };

      mockService.getResourceAllocation.mockResolvedValue(mockAllocation);

      await controller.getResourceAllocation(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      const callArgs = (mockResponse.json as jest.Mock).mock.calls[0]![0];
      expect(callArgs.data).toHaveProperty('resourceType');
      expect(callArgs.data).toHaveProperty('resourceId');
      expect(callArgs.data).toHaveProperty('fromDate');
      expect(callArgs.data).toHaveProperty('toDate');
      expect(callArgs.data).toHaveProperty('totalAllocatedTime');
      expect(callArgs.data).toHaveProperty('operationCount');
      expect(callArgs.data).toHaveProperty('operations');
    });
  });

  describe('Error Handling', () => {
    /**
     * Test: Passes service errors to error handler
     * Business rule: Service errors should be handled by middleware
     */
    it('should pass service errors to error handler', async () => {
      mockRequest = {
        query: {
          resourceType: 'crane',
          resourceId: 'CRANE-1',
          fromDate: '2026-01-15',
          toDate: '2026-01-16',
        },
      };

      const mockError = new Error('Database connection failed');
      mockService.getResourceAllocation.mockRejectedValue(mockError);

      await controller.getResourceAllocation(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockNext).toHaveBeenCalledWith(mockError);
      expect(mockResponse.json).not.toHaveBeenCalled();
    });

    /**
     * Test: Handles unknown resource type error
     * Business rule: Service should throw for invalid resource types
     */
    it('should handle unknown resource type error', async () => {
      mockRequest = {
        query: {
          resourceType: 'truck',
          resourceId: 'TRUCK-1',
          fromDate: '2026-01-15',
          toDate: '2026-01-16',
        },
      };

      const mockError = new Error('Unknown resource type: truck');
      mockService.getResourceAllocation.mockRejectedValue(mockError);

      await controller.getResourceAllocation(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockNext).toHaveBeenCalledWith(mockError);
    });

    /**
     * Test: Handles invalid date format errors
     * Business rule: Invalid dates should be caught by Date constructor
     */
    it('should handle invalid date format gracefully', async () => {
      mockRequest = {
        query: {
          resourceType: 'dock',
          resourceId: 'DOCK-A',
          fromDate: 'invalid-date',
          toDate: '2026-01-16',
        },
      };

      // Service receives Invalid Date object and may throw or handle it
      mockService.getResourceAllocation.mockResolvedValue({
        resourceType: 'dock',
        resourceId: 'DOCK-A',
        fromDate: 'Invalid Date',
        toDate: '2026-01-16T00:00:00.000Z',
        totalAllocatedTime: 0,
        operationCount: 0,
        operations: [],
      });

      await controller.getResourceAllocation(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      // Controller doesn't validate dates, passes to service
      expect(mockService.getResourceAllocation).toHaveBeenCalled();
    });

    /**
     * Test: Handles repository errors
     * Business rule: Database errors should propagate through service
     */
    it('should handle repository errors', async () => {
      mockRequest = {
        query: {
          resourceType: 'staff',
          resourceId: 'STAFF-001',
          fromDate: '2026-01-15',
          toDate: '2026-01-16',
        },
      };

      const dbError = new Error('Connection timeout');
      mockService.getResourceAllocation.mockRejectedValue(dbError);

      await controller.getResourceAllocation(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockNext).toHaveBeenCalledWith(dbError);
    });

    /**
     * Test: Handles unexpected errors
     * Business rule: All errors should be caught and passed to error handler
     */
    it('should handle unexpected errors', async () => {
      mockRequest = {
        query: {
          resourceType: 'crane',
          resourceId: 'CRANE-1',
          fromDate: '2026-01-15',
          toDate: '2026-01-16',
        },
      };

      const unexpectedError = new Error('Something went wrong');
      mockService.getResourceAllocation.mockRejectedValue(unexpectedError);

      await controller.getResourceAllocation(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockNext).toHaveBeenCalledWith(unexpectedError);
    });
  });

  describe('Query Parameter Edge Cases', () => {
    /**
     * Test: Handles empty string parameters
     * Business rule: Empty strings should be treated as missing
     */
    it('should treat empty string parameters as missing', async () => {
      mockRequest = {
        query: {
          resourceType: '',
          resourceId: 'CRANE-1',
          fromDate: '2026-01-15',
          toDate: '2026-01-16',
        },
      };

      await controller.getResourceAllocation(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockResponse.status).toHaveBeenCalledWith(400);
    });

    /**
     * Test: Ignores additional query parameters
     * Business rule: Extra params should not affect processing
     */
    it('should ignore additional query parameters', async () => {
      mockRequest = {
        query: {
          resourceType: 'crane',
          resourceId: 'CRANE-1',
          fromDate: '2026-01-15',
          toDate: '2026-01-16',
          extraParam: 'ignored',
          anotherExtra: '123',
        },
      };

      mockService.getResourceAllocation.mockResolvedValue({
        resourceType: 'crane',
        resourceId: 'CRANE-1',
        fromDate: '2026-01-15T00:00:00.000Z',
        toDate: '2026-01-16T00:00:00.000Z',
        totalAllocatedTime: 0,
        operationCount: 0,
        operations: [],
      });

      await controller.getResourceAllocation(
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
     * Test: Handles whitespace in parameters
     * Business rule: Parameters are used as-is (trimming is service's responsibility)
     */
    it('should pass parameters with whitespace to service', async () => {
      mockRequest = {
        query: {
          resourceType: ' crane ',
          resourceId: ' CRANE-1 ',
          fromDate: '2026-01-15',
          toDate: '2026-01-16',
        },
      };

      mockService.getResourceAllocation.mockResolvedValue({
        resourceType: ' crane ',
        resourceId: ' CRANE-1 ',
        fromDate: '2026-01-15T00:00:00.000Z',
        toDate: '2026-01-16T00:00:00.000Z',
        totalAllocatedTime: 0,
        operationCount: 0,
        operations: [],
      });

      await controller.getResourceAllocation(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.getResourceAllocation).toHaveBeenCalledWith(
        ' crane ',
        ' CRANE-1 ',
        expect.any(Date),
        expect.any(Date)
      );
    });
  });

  describe('Integration Scenarios', () => {
    /**
     * Test: Complete workflow with multiple operations
     * Business rule: Should handle realistic allocation query
     */
    it('should handle complete workflow with multiple operations', async () => {
      mockRequest = {
        query: {
          resourceType: 'dock',
          resourceId: 'DOCK-A',
          fromDate: '2026-01-15T00:00:00Z',
          toDate: '2026-01-20T23:59:59Z',
        },
      };

      const mockAllocation = {
        resourceType: 'dock',
        resourceId: 'DOCK-A',
        fromDate: '2026-01-15T00:00:00.000Z',
        toDate: '2026-01-20T23:59:59.000Z',
        totalAllocatedTime: 48,
        operationCount: 12,
        operations: Array.from({ length: 12 }, (_, i) => ({
          planId: `PLAN-00${Math.floor(i / 3) + 1}`,
          vvnId: `VVN-00${i + 1}`,
          vesselImo: `IMO${1000000 + i}`,
          plannedStart: new Date(`2026-01-${15 + Math.floor(i / 3)}T08:00:00Z`),
          plannedEnd: new Date(`2026-01-${15 + Math.floor(i / 3)}T12:00:00Z`),
          duration: 4,
        })),
      };

      mockService.getResourceAllocation.mockResolvedValue(mockAllocation);

      await controller.getResourceAllocation(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        data: mockAllocation,
      });
      expect(mockService.getResourceAllocation).toHaveBeenCalledTimes(1);
    });

    /**
     * Test: Handles zero allocation result
     * Business rule: Should successfully return empty result
     */
    it('should handle zero allocation result successfully', async () => {
      mockRequest = {
        query: {
          resourceType: 'staff',
          resourceId: 'STAFF-999',
          fromDate: '2026-01-15',
          toDate: '2026-01-16',
        },
      };

      const mockAllocation = {
        resourceType: 'staff',
        resourceId: 'STAFF-999',
        fromDate: '2026-01-15T00:00:00.000Z',
        toDate: '2026-01-16T00:00:00.000Z',
        totalAllocatedTime: 0,
        operationCount: 0,
        operations: [],
      };

      mockService.getResourceAllocation.mockResolvedValue(mockAllocation);

      await controller.getResourceAllocation(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        data: mockAllocation,
      });
      expect(mockNext).not.toHaveBeenCalled();
    });
  });
});
