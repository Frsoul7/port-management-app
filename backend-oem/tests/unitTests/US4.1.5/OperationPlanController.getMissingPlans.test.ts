import { Request, Response, NextFunction } from 'express';
import { OperationPlanController } from '@presentation/controllers/OperationPlanController';
import { OperationPlanService } from '@application/services/OperationPlanService';
import { VvnReference, OperationType } from '@shared/types';

/**
 * Unit Tests for OperationPlanController - getMissingPlans endpoint
 * US 4.1.5: Identify VVNs without Operation Plans
 * 
 * Test Coverage:
 * - Valid date parameter returns missing VVNs
 * - Missing date parameter returns 400 error
 * - Empty results handling
 * - Multiple missing VVNs
 * - Response format (success, data, count)
 * - Error handling from service
 * - Date format validation
 */
describe('US 4.1.5 - OperationPlanController.getMissingPlans', () => {
  let controller: OperationPlanController;
  let mockService: jest.Mocked<OperationPlanService>;
  let mockRequest: Partial<Request>;
  let mockResponse: Partial<Response>;
  let mockNext: NextFunction;

  /**
   * Create a mock VVN reference for testing
   */
  const createMockVvn = (vvnId: string, vesselImo: string): VvnReference => ({
    vvnId,
    vesselImo,
    eta: new Date('2026-01-15T08:00:00.000Z'),
    etd: new Date('2026-01-15T20:00:00.000Z'),
    loadingCount: 50,
    unloadingCount: 30,
    purpose: OperationType.UNLOAD,
    state: 'APPROVED',
  });

  beforeEach(() => {
    // Create mock service
    mockService = {
      getVvnsWithoutPlans: jest.fn(),
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

  describe('Valid Date Parameter', () => {
    /**
     * Test: Returns missing VVNs for valid date
     * Business rule: Endpoint returns list of VVNs without operation plans
     */
    it('should return missing VVNs for valid date', async () => {
      const targetDate = '2026-01-15';
      const mockVvns = [
        createMockVvn('VVN-001', 'IMO1111111'),
        createMockVvn('VVN-002', 'IMO2222222'),
        createMockVvn('VVN-003', 'IMO3333333'),
      ];

      mockRequest = {
        query: { date: targetDate },
      };

      mockService.getVvnsWithoutPlans.mockResolvedValue(mockVvns);

      await controller.getMissingPlans(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.getVvnsWithoutPlans).toHaveBeenCalledWith(new Date(targetDate));
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        data: mockVvns,
        count: 3,
      });
    });

    /**
     * Test: Returns empty array when no missing VVNs
     * Business rule: If all VVNs have plans, return empty list with count 0
     */
    it('should return empty array when no missing VVNs', async () => {
      mockRequest = {
        query: { date: '2026-01-15' },
      };

      mockService.getVvnsWithoutPlans.mockResolvedValue([]);

      await controller.getMissingPlans(
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

    /**
     * Test: Returns single missing VVN
     * Business rule: Should handle single result correctly
     */
    it('should handle single missing VVN', async () => {
      const mockVvn = createMockVvn('VVN-001', 'IMO1111111');

      mockRequest = {
        query: { date: '2026-01-15' },
      };

      mockService.getVvnsWithoutPlans.mockResolvedValue([mockVvn]);

      await controller.getMissingPlans(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        data: [mockVvn],
        count: 1,
      });
    });

    /**
     * Test: Returns many missing VVNs
     * Business rule: Should handle large result sets
     */
    it('should handle many missing VVNs', async () => {
      const mockVvns: VvnReference[] = [];
      for (let i = 1; i <= 50; i++) {
        mockVvns.push(createMockVvn(`VVN-${i.toString().padStart(3, '0')}`, `IMO${i}111111`));
      }

      mockRequest = {
        query: { date: '2026-01-15' },
      };

      mockService.getVvnsWithoutPlans.mockResolvedValue(mockVvns);

      await controller.getMissingPlans(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        data: mockVvns,
        count: 50,
      });
    });
  });

  describe('Date Format Handling', () => {
    /**
     * Test: Accepts ISO 8601 date format
     * Business rule: Should support standard date formats
     */
    it('should accept ISO 8601 date format', async () => {
      mockRequest = {
        query: { date: '2026-01-15T00:00:00.000Z' },
      };

      mockService.getVvnsWithoutPlans.mockResolvedValue([]);

      await controller.getMissingPlans(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.getVvnsWithoutPlans).toHaveBeenCalledWith(
        new Date('2026-01-15T00:00:00.000Z')
      );
      expect(mockResponse.json).toHaveBeenCalled();
    });

    /**
     * Test: Accepts YYYY-MM-DD format
     * Business rule: Should support simple date string format
     */
    it('should accept YYYY-MM-DD format', async () => {
      mockRequest = {
        query: { date: '2026-01-15' },
      };

      mockService.getVvnsWithoutPlans.mockResolvedValue([]);

      await controller.getMissingPlans(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.getVvnsWithoutPlans).toHaveBeenCalledWith(new Date('2026-01-15'));
      expect(mockResponse.json).toHaveBeenCalled();
    });

    /**
     * Test: Date with time component
     * Business rule: Should accept date with time and pass to service
     */
    it('should handle date with time component', async () => {
      mockRequest = {
        query: { date: '2026-01-15T14:30:00.000Z' },
      };

      mockService.getVvnsWithoutPlans.mockResolvedValue([]);

      await controller.getMissingPlans(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.getVvnsWithoutPlans).toHaveBeenCalledWith(
        new Date('2026-01-15T14:30:00.000Z')
      );
    });
  });

  describe('Missing or Invalid Date Parameter', () => {
    /**
     * Test: Missing date parameter returns 400
     * Business rule: Date parameter is required
     */
    it('should return 400 when date parameter is missing', async () => {
      mockRequest = {
        query: {}, // No date parameter
      };

      await controller.getMissingPlans(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockResponse.status).toHaveBeenCalledWith(400);
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: false,
        message: 'Date parameter is required',
      });
      expect(mockService.getVvnsWithoutPlans).not.toHaveBeenCalled();
    });

    /**
     * Test: Undefined date parameter returns 400
     * Business rule: Explicit undefined should be treated as missing
     */
    it('should return 400 when date is undefined', async () => {
      mockRequest = {
        query: { date: undefined },
      };

      await controller.getMissingPlans(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockResponse.status).toHaveBeenCalledWith(400);
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: false,
        message: 'Date parameter is required',
      });
    });

    /**
     * Test: Empty string date parameter returns 400
     * Business rule: Empty string should be treated as missing
     */
    it('should return 400 when date is empty string', async () => {
      mockRequest = {
        query: { date: '' },
      };

      await controller.getMissingPlans(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockResponse.status).toHaveBeenCalledWith(400);
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: false,
        message: 'Date parameter is required',
      });
    });

    /**
     * Test: Null date parameter returns 400
     * Business rule: Null should be treated as missing
     */
    it('should return 400 when date is null', async () => {
      mockRequest = {
        query: { date: null },
      } as any;

      await controller.getMissingPlans(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockResponse.status).toHaveBeenCalledWith(400);
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: false,
        message: 'Date parameter is required',
      });
    });
  });

  describe('Response Format', () => {
    /**
     * Test: Response includes success flag
     * Business rule: All responses should include success indicator
     */
    it('should include success flag in response', async () => {
      mockRequest = {
        query: { date: '2026-01-15' },
      };

      mockService.getVvnsWithoutPlans.mockResolvedValue([]);

      await controller.getMissingPlans(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      const responseCall = (mockResponse.json as jest.Mock).mock.calls[0][0];
      expect(responseCall).toHaveProperty('success', true);
    });

    /**
     * Test: Response includes data array
     * Business rule: Missing VVNs should be in data property
     */
    it('should include data array in response', async () => {
      const mockVvns = [createMockVvn('VVN-001', 'IMO1111111')];

      mockRequest = {
        query: { date: '2026-01-15' },
      };

      mockService.getVvnsWithoutPlans.mockResolvedValue(mockVvns);

      await controller.getMissingPlans(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      const responseCall = (mockResponse.json as jest.Mock).mock.calls[0][0];
      expect(responseCall).toHaveProperty('data');
      expect(Array.isArray(responseCall.data)).toBe(true);
      expect(responseCall.data).toEqual(mockVvns);
    });

    /**
     * Test: Response includes count
     * Business rule: Count should match data array length
     */
    it('should include count matching data length', async () => {
      const mockVvns = [
        createMockVvn('VVN-001', 'IMO1111111'),
        createMockVvn('VVN-002', 'IMO2222222'),
        createMockVvn('VVN-003', 'IMO3333333'),
      ];

      mockRequest = {
        query: { date: '2026-01-15' },
      };

      mockService.getVvnsWithoutPlans.mockResolvedValue(mockVvns);

      await controller.getMissingPlans(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      const responseCall = (mockResponse.json as jest.Mock).mock.calls[0][0];
      expect(responseCall).toHaveProperty('count', 3);
      expect(responseCall.count).toBe(responseCall.data.length);
    });

    /**
     * Test: Response preserves VVN metadata
     * Business rule: All VVN properties should be returned
     */
    it('should preserve complete VVN metadata in response', async () => {
      const mockVvn = createMockVvn('VVN-001', 'IMO1111111');
      // Override properties
      (mockVvn as any).loadingCount = 150;
      (mockVvn as any).unloadingCount = 200;

      mockRequest = {
        query: { date: '2026-01-15' },
      };

      mockService.getVvnsWithoutPlans.mockResolvedValue([mockVvn]);

      await controller.getMissingPlans(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      const responseCall = (mockResponse.json as jest.Mock).mock.calls[0][0];
      expect(responseCall.data[0]).toMatchObject({
        vvnId: 'VVN-001',
        vesselImo: 'IMO1111111',
        eta: expect.any(Date),
        etd: expect.any(Date),
        loadingCount: 150,
        unloadingCount: 200,
        state: 'APPROVED',
      });
    });
  });

  describe('Error Handling', () => {
    /**
     * Test: Service error is passed to next()
     * Business rule: Controller should not catch service errors
     */
    it('should pass service errors to error handler', async () => {
      const serviceError = new Error('Service error occurred');

      mockRequest = {
        query: { date: '2026-01-15' },
      };

      mockService.getVvnsWithoutPlans.mockRejectedValue(serviceError);

      await controller.getMissingPlans(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockNext).toHaveBeenCalledWith(serviceError);
      expect(mockResponse.json).not.toHaveBeenCalled();
    });

    /**
     * Test: Repository error is passed to next()
     * Business rule: Database errors should be handled by error middleware
     */
    it('should handle repository errors', async () => {
      const dbError = new Error('Database connection failed');

      mockRequest = {
        query: { date: '2026-01-15' },
      };

      mockService.getVvnsWithoutPlans.mockRejectedValue(dbError);

      await controller.getMissingPlans(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockNext).toHaveBeenCalledWith(dbError);
    });

    /**
     * Test: Core Backend error is passed to next()
     * Business rule: External service errors should be handled by error middleware
     */
    it('should handle Core Backend errors', async () => {
      const coreBackendError = new Error('Core Backend unavailable');

      mockRequest = {
        query: { date: '2026-01-15' },
      };

      mockService.getVvnsWithoutPlans.mockRejectedValue(coreBackendError);

      await controller.getMissingPlans(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockNext).toHaveBeenCalledWith(coreBackendError);
    });

    /**
     * Test: Invalid date format error is passed to next()
     * Business rule: Date parsing errors should be handled by error middleware
     */
    it('should handle invalid date format errors', async () => {
      const invalidDateError = new Error('Invalid date format');

      mockRequest = {
        query: { date: 'invalid-date' },
      };

      mockService.getVvnsWithoutPlans.mockRejectedValue(invalidDateError);

      await controller.getMissingPlans(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockNext).toHaveBeenCalledWith(invalidDateError);
    });

    /**
     * Test: Generic error is passed to next()
     * Business rule: Unexpected errors should be handled by error middleware
     */
    it('should handle unexpected errors', async () => {
      const unexpectedError = new Error('Unexpected error');

      mockRequest = {
        query: { date: '2026-01-15' },
      };

      mockService.getVvnsWithoutPlans.mockRejectedValue(unexpectedError);

      await controller.getMissingPlans(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockNext).toHaveBeenCalledWith(unexpectedError);
      expect(mockResponse.json).not.toHaveBeenCalled();
      expect(mockResponse.status).not.toHaveBeenCalled();
    });
  });

  describe('Query Parameter Variations', () => {
    /**
     * Test: Ignores additional query parameters
     * Business rule: Only date parameter is used, others are ignored
     */
    it('should ignore additional query parameters', async () => {
      mockRequest = {
        query: {
          date: '2026-01-15',
          status: 'APPROVED', // Extra parameter
          algorithm: 'OPTIMAL', // Extra parameter
        },
      };

      mockService.getVvnsWithoutPlans.mockResolvedValue([]);

      await controller.getMissingPlans(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.getVvnsWithoutPlans).toHaveBeenCalledWith(new Date('2026-01-15'));
      expect(mockResponse.json).toHaveBeenCalled();
    });

    /**
     * Test: Handles date as array (takes first element via casting)
     * Business rule: When Express parses query string with duplicate names,
     * the controller casts to string which uses first element
     */
    it('should handle date as array parameter', async () => {
      mockRequest = {
        query: {
          date: ['2026-01-15', '2026-01-16'] as any,
        },
      };

      mockService.getVvnsWithoutPlans.mockResolvedValue([]);

      await controller.getMissingPlans(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      // When date is array, TypeScript cast to string gets first element
      // Array.toString() joins with commas: ['2026-01-15', '2026-01-16'].toString() = '2026-01-15,2026-01-16'
      // But Express typically handles this and we get the first value
      const calls = mockService.getVvnsWithoutPlans.mock.calls;
      expect(calls).toHaveLength(1);
      expect(calls[0]![0]).toBeInstanceOf(Date);
    });
  });

  describe('Integration Scenarios', () => {
    /**
     * Test: Complete flow with multiple missing VVNs
     * Business rule: End-to-end test of typical usage
     */
    it('should handle complete flow with results', async () => {
      const mockVvns = [
        createMockVvn('VVN-001', 'IMO1111111'),
        createMockVvn('VVN-002', 'IMO2222222'),
      ];

      mockRequest = {
        query: { date: '2026-01-15' },
      };

      mockService.getVvnsWithoutPlans.mockResolvedValue(mockVvns);

      await controller.getMissingPlans(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      // Verify service called correctly
      expect(mockService.getVvnsWithoutPlans).toHaveBeenCalledTimes(1);
      expect(mockService.getVvnsWithoutPlans).toHaveBeenCalledWith(new Date('2026-01-15'));

      // Verify response structure
      expect(mockResponse.json).toHaveBeenCalledTimes(1);
      const response = (mockResponse.json as jest.Mock).mock.calls[0][0];
      expect(response.success).toBe(true);
      expect(response.data).toHaveLength(2);
      expect(response.count).toBe(2);

      // Verify error handler not called
      expect(mockNext).not.toHaveBeenCalled();
    });

    /**
     * Test: Complete flow with no results
     * Business rule: End-to-end test when all VVNs have plans
     */
    it('should handle complete flow with no missing VVNs', async () => {
      mockRequest = {
        query: { date: '2026-01-15' },
      };

      mockService.getVvnsWithoutPlans.mockResolvedValue([]);

      await controller.getMissingPlans(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      // Verify service called
      expect(mockService.getVvnsWithoutPlans).toHaveBeenCalledTimes(1);

      // Verify empty response
      const response = (mockResponse.json as jest.Mock).mock.calls[0][0];
      expect(response.success).toBe(true);
      expect(response.data).toEqual([]);
      expect(response.count).toBe(0);
    });
  });
});
