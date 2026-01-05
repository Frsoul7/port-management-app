import { Request, Response, NextFunction } from 'express';
import { VesselVisitExecutionController } from '../../../src/presentation/controllers/VesselVisitExecutionController';
import { VesselVisitExecutionService } from '../../../src/application/services/VesselVisitExecutionService';
import { VesselVisitExecutionStatus } from '../../../src/shared/types';

describe('US 4.1.11 - VesselVisitExecutionController.recordCompletion', () => {
  let controller: VesselVisitExecutionController;
  let mockService: jest.Mocked<VesselVisitExecutionService>;
  let mockRequest: Partial<Request>;
  let mockResponse: Partial<Response>;
  let mockNext: NextFunction;

  beforeEach(() => {
    mockService = {
      completeVve: jest.fn(),
    } as any;

    controller = new VesselVisitExecutionController(mockService);

    mockResponse = {
      json: jest.fn(),
      status: jest.fn().mockReturnThis(),
    };

    mockNext = jest.fn();
  });

  describe('Valid Completion Requests', () => {
    /**
     * Test: Completes VVE with valid parameters
     * Business rule: Should call service and return success
     */
    it('should complete VVE with valid departure time', async () => {
      mockRequest = {
        params: { id: 'VVE-001' },
        body: {
          departureTime: '2026-01-15T13:00:00Z',
        },
        user: { sub: 'operator-1', id: 'operator-1' } as any,
      };

      const mockResult = {
        vveId: 'VVE-001',
        vvnId: 'VVN-001',
        status: VesselVisitExecutionStatus.COMPLETED,
        actualPortDepartureTime: '2026-01-15T13:00:00Z',
        completedBy: 'operator-1',
        completedAt: '2026-01-15T13:00:00Z',
      };

      mockService.completeVve.mockResolvedValue(mockResult as any);

      await controller.recordCompletion(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.completeVve).toHaveBeenCalledWith('VVE-001', {
        departureTime: '2026-01-15T13:00:00Z',
        completedBy: 'operator-1',
      });

      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        message: 'Vessel visit execution completed successfully',
        data: mockResult,
      });
    });

    /**
     * Test: Handles different departure time formats
     * Business rule: Should accept ISO 8601 datetime strings
     */
    it('should handle various departure time formats', async () => {
      const departureTimes = [
        '2026-01-15T13:00:00Z',
        '2026-01-15T13:30:45.123Z',
        '2026-01-15T23:59:59Z',
      ];

      for (const departureTime of departureTimes) {
        mockRequest = {
          params: { id: 'VVE-001' },
          body: { departureTime },
          user: { sub: 'operator-1', id: 'operator-1' } as any,
        };

        mockService.completeVve.mockResolvedValue({
          vveId: 'VVE-001',
          status: VesselVisitExecutionStatus.COMPLETED,
        } as any);

        await controller.recordCompletion(
          mockRequest as Request,
          mockResponse as Response,
          mockNext
        );

        expect(mockService.completeVve).toHaveBeenCalledWith('VVE-001', {
          departureTime,
          completedBy: 'operator-1',
        });
      }
    });

    /**
     * Test: Returns complete VVE data
     * Business rule: Response should include all VVE fields
     */
    it('should return complete VVE data after completion', async () => {
      mockRequest = {
        params: { id: 'VVE-001' },
        body: {
          departureTime: '2026-01-15T13:00:00Z',
        },
        user: { sub: 'operator-1', id: 'operator-1' } as any,
      };

      const mockResult = {
        vveId: 'VVE-001',
        vvnId: 'VVN-001',
        status: VesselVisitExecutionStatus.COMPLETED,
        actualPortArrivalTime: '2026-01-15T06:00:00Z',
        actualBerthTime: '2026-01-15T08:00:00Z',
        actualUnberthTime: '2026-01-15T12:00:00Z',
        actualPortDepartureTime: '2026-01-15T13:00:00Z',
        completedBy: 'operator-1',
        completedAt: '2026-01-15T13:00:00Z',
        operations: [],
        turnaroundTimeHours: 7,
        berthOccupancyHours: 4,
        waitingTimeHours: 2,
      };

      mockService.completeVve.mockResolvedValue(mockResult as any);

      await controller.recordCompletion(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      const responseData = (mockResponse.json as jest.Mock).mock.calls[0]![0].data;
      expect(responseData).toMatchObject({
        vveId: 'VVE-001',
        status: VesselVisitExecutionStatus.COMPLETED,
        actualPortDepartureTime: '2026-01-15T13:00:00Z',
        completedBy: 'operator-1',
        turnaroundTimeHours: 7,
      });
    });
  });

  describe('Request Parameter Extraction', () => {
    /**
     * Test: Extracts VVE ID from URL params
     * Business rule: VVE ID comes from route parameter
     */
    it('should extract VVE ID from URL parameters', async () => {
      mockRequest = {
        params: { id: 'VVE-123' },
        body: {
          departureTime: '2026-01-15T13:00:00Z',
        },
        user: { sub: 'operator-1', id: 'operator-1' } as any,
      };

      mockService.completeVve.mockResolvedValue({} as any);

      await controller.recordCompletion(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.completeVve).toHaveBeenCalledWith(
        'VVE-123',
        expect.any(Object)
      );
    });

    /**
     * Test: Extracts departure time from request body
     * Business rule: Departure time comes from POST body
     */
    it('should extract departure time from request body', async () => {
      mockRequest = {
        params: { id: 'VVE-001' },
        body: {
          departureTime: '2026-01-15T14:30:00Z',
        },
        user: { sub: 'operator-1', id: 'operator-1' } as any,
      };

      mockService.completeVve.mockResolvedValue({} as any);

      await controller.recordCompletion(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.completeVve).toHaveBeenCalledWith('VVE-001', {
        departureTime: '2026-01-15T14:30:00Z',
        completedBy: 'operator-1',
      });
    });

    /**
     * Test: Extracts user ID from JWT middleware
     * Business rule: User ID comes from authentication
     */
    it('should extract user ID from JWT token', async () => {
      mockRequest = {
        params: { id: 'VVE-001' },
        body: {
          departureTime: '2026-01-15T13:00:00Z',
        },
        user: { sub: 'operator-123', id: 'operator-123' } as any,
      };

      mockService.completeVve.mockResolvedValue({} as any);

      await controller.recordCompletion(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.completeVve).toHaveBeenCalledWith('VVE-001', {
        departureTime: '2026-01-15T13:00:00Z',
        completedBy: 'operator-123',
      });
    });

    /**
     * Test: Handles undefined user ID gracefully
     * Business rule: Should pass undefined if no user
     */
    it('should handle missing user ID', async () => {
      mockRequest = {
        params: { id: 'VVE-001' },
        body: {
          departureTime: '2026-01-15T13:00:00Z',
        },
        // No user property
      };

      mockService.completeVve.mockResolvedValue({} as any);

      await controller.recordCompletion(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.completeVve).toHaveBeenCalledWith('VVE-001', {
        departureTime: '2026-01-15T13:00:00Z',
        completedBy: undefined,
      });
    });
  });

  describe('Response Structure', () => {
    /**
     * Test: Response includes success flag
     * Business rule: All responses should indicate success
     */
    it('should include success flag in response', async () => {
      mockRequest = {
        params: { id: 'VVE-001' },
        body: {
          departureTime: '2026-01-15T13:00:00Z',
        },
        user: { sub: 'operator-1', id: 'operator-1' } as any,
      };

      mockService.completeVve.mockResolvedValue({} as any);

      await controller.recordCompletion(
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
     * Test: Response includes success message
     * Business rule: Should provide user-friendly message
     */
    it('should include success message in response', async () => {
      mockRequest = {
        params: { id: 'VVE-001' },
        body: {
          departureTime: '2026-01-15T13:00:00Z',
        },
        user: { sub: 'operator-1', id: 'operator-1' } as any,
      };

      mockService.completeVve.mockResolvedValue({} as any);

      await controller.recordCompletion(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockResponse.json).toHaveBeenCalledWith(
        expect.objectContaining({
          message: 'Vessel visit execution completed successfully',
        })
      );
    });

    /**
     * Test: Response wraps VVE data in data field
     * Business rule: Service result should be in data property
     */
    it('should wrap VVE data in data field', async () => {
      mockRequest = {
        params: { id: 'VVE-001' },
        body: {
          departureTime: '2026-01-15T13:00:00Z',
        },
        user: { sub: 'operator-1', id: 'operator-1' } as any,
      };

      const mockResult = {
        vveId: 'VVE-001',
        status: VesselVisitExecutionStatus.COMPLETED,
        completedBy: 'operator-1',
      };

      mockService.completeVve.mockResolvedValue(mockResult as any);

      await controller.recordCompletion(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        message: 'Vessel visit execution completed successfully',
        data: mockResult,
      });
    });

    /**
     * Test: Preserves all service response fields
     * Business rule: Should not lose any data from service
     */
    it('should preserve all VVE fields from service', async () => {
      mockRequest = {
        params: { id: 'VVE-001' },
        body: {
          departureTime: '2026-01-15T13:00:00Z',
        },
        user: { sub: 'operator-1', id: 'operator-1' } as any,
      };

      const mockResult = {
        vveId: 'VVE-001',
        vvnId: 'VVN-001',
        status: VesselVisitExecutionStatus.COMPLETED,
        actualPortArrivalTime: '2026-01-15T06:00:00Z',
        actualBerthTime: '2026-01-15T08:00:00Z',
        actualUnberthTime: '2026-01-15T12:00:00Z',
        actualPortDepartureTime: '2026-01-15T13:00:00Z',
        completedBy: 'operator-1',
        completedAt: '2026-01-15T13:00:00Z',
        operations: [],
      };

      mockService.completeVve.mockResolvedValue(mockResult as any);

      await controller.recordCompletion(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      const responseData = (mockResponse.json as jest.Mock).mock.calls[0]![0].data;
      expect(responseData).toEqual(mockResult);
    });
  });

  describe('Error Handling', () => {
    /**
     * Test: Passes service errors to error handler
     * Business rule: Errors should be handled by middleware
     */
    it('should pass service errors to error handler', async () => {
      mockRequest = {
        params: { id: 'VVE-001' },
        body: {
          departureTime: '2026-01-15T13:00:00Z',
        },
        user: { sub: 'operator-1', id: 'operator-1' } as any,
      };

      const mockError = new Error('VVE not found');
      mockService.completeVve.mockRejectedValue(mockError);

      await controller.recordCompletion(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockNext).toHaveBeenCalledWith(mockError);
      expect(mockResponse.json).not.toHaveBeenCalled();
    });

    /**
     * Test: Handles VVE not found error
     * Business rule: Should propagate not found errors
     */
    it('should handle VVE not found error', async () => {
      mockRequest = {
        params: { id: 'VVE-NONEXISTENT' },
        body: {
          departureTime: '2026-01-15T13:00:00Z',
        },
        user: { sub: 'operator-1', id: 'operator-1' } as any,
      };

      const mockError = new Error('VVE VVE-NONEXISTENT not found');
      mockService.completeVve.mockRejectedValue(mockError);

      await controller.recordCompletion(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockNext).toHaveBeenCalledWith(mockError);
    });

    /**
     * Test: Handles missing unberth time error
     * Business rule: Prerequisite errors should be propagated
     */
    it('should handle missing unberth time error', async () => {
      mockRequest = {
        params: { id: 'VVE-001' },
        body: {
          departureTime: '2026-01-15T13:00:00Z',
        },
        user: { sub: 'operator-1', id: 'operator-1' } as any,
      };

      const mockError = new Error('Cannot complete VVE without unberth time');
      mockService.completeVve.mockRejectedValue(mockError);

      await controller.recordCompletion(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockNext).toHaveBeenCalledWith(mockError);
    });

    /**
     * Test: Handles incomplete operations error
     * Business rule: Should propagate business rule errors
     */
    it('should handle incomplete operations error', async () => {
      mockRequest = {
        params: { id: 'VVE-001' },
        body: {
          departureTime: '2026-01-15T13:00:00Z',
        },
        user: { sub: 'operator-1', id: 'operator-1' } as any,
      };

      const mockError = new Error('Cannot complete VVE with incomplete operations');
      mockService.completeVve.mockRejectedValue(mockError);

      await controller.recordCompletion(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockNext).toHaveBeenCalledWith(mockError);
    });

    /**
     * Test: Handles invalid departure time error
     * Business rule: Validation errors should be propagated
     */
    it('should handle invalid departure time error', async () => {
      mockRequest = {
        params: { id: 'VVE-001' },
        body: {
          departureTime: '2026-01-15T11:00:00Z', // Before unberth
        },
        user: { sub: 'operator-1', id: 'operator-1' } as any,
      };

      const mockError = new Error('Port departure time must be after unberth time');
      mockService.completeVve.mockRejectedValue(mockError);

      await controller.recordCompletion(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockNext).toHaveBeenCalledWith(mockError);
    });

    /**
     * Test: Handles repository errors
     * Business rule: Database errors should be propagated
     */
    it('should handle repository errors', async () => {
      mockRequest = {
        params: { id: 'VVE-001' },
        body: {
          departureTime: '2026-01-15T13:00:00Z',
        },
        user: { sub: 'operator-1', id: 'operator-1' } as any,
      };

      const dbError = new Error('Database connection failed');
      mockService.completeVve.mockRejectedValue(dbError);

      await controller.recordCompletion(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockNext).toHaveBeenCalledWith(dbError);
    });

    /**
     * Test: Handles unexpected errors
     * Business rule: All errors should be caught and passed to middleware
     */
    it('should handle unexpected errors', async () => {
      mockRequest = {
        params: { id: 'VVE-001' },
        body: {
          departureTime: '2026-01-15T13:00:00Z',
        },
        user: { sub: 'operator-1', id: 'operator-1' } as any,
      };

      const unexpectedError = new Error('Unexpected error occurred');
      mockService.completeVve.mockRejectedValue(unexpectedError);

      await controller.recordCompletion(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockNext).toHaveBeenCalledWith(unexpectedError);
    });
  });

  describe('Request Body Edge Cases', () => {
    /**
     * Test: Handles missing departure time
     * Business rule: Service should validate required fields
     */
    it('should pass through missing departureTime to service', async () => {
      mockRequest = {
        params: { id: 'VVE-001' },
        body: {
          // departureTime missing
        },
        user: { sub: 'operator-1', id: 'operator-1' } as any,
      };

      mockService.completeVve.mockRejectedValue(
        new Error('Departure time is required')
      );

      await controller.recordCompletion(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.completeVve).toHaveBeenCalledWith('VVE-001', {
        departureTime: undefined,
        completedBy: 'operator-1',
      });
      expect(mockNext).toHaveBeenCalled();
    });

    /**
     * Test: Ignores extra body fields
     * Business rule: Should only extract relevant fields
     */
    it('should ignore extra request body fields', async () => {
      mockRequest = {
        params: { id: 'VVE-001' },
        body: {
          departureTime: '2026-01-15T13:00:00Z',
          extraField: 'should-be-ignored',
          anotherExtra: 123,
        },
        user: { sub: 'operator-1', id: 'operator-1' } as any,
      };

      mockService.completeVve.mockResolvedValue({} as any);

      await controller.recordCompletion(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.completeVve).toHaveBeenCalledWith('VVE-001', {
        departureTime: '2026-01-15T13:00:00Z',
        completedBy: 'operator-1',
      });
    });
  });

  describe('Integration Scenarios', () => {
    /**
     * Test: Complete VVE workflow
     * Business rule: Should handle full completion process
     */
    it('should handle complete VVE completion workflow', async () => {
      mockRequest = {
        params: { id: 'VVE-001' },
        body: {
          departureTime: '2026-01-15T13:00:00Z',
        },
        user: { sub: 'operator-1', id: 'operator-1' } as any,
      };

      const mockResult = {
        vveId: 'VVE-001',
        vvnId: 'VVN-001',
        status: VesselVisitExecutionStatus.COMPLETED,
        actualPortArrivalTime: '2026-01-15T06:00:00Z',
        actualBerthTime: '2026-01-15T08:00:00Z',
        actualUnberthTime: '2026-01-15T12:00:00Z',
        actualPortDepartureTime: '2026-01-15T13:00:00Z',
        completedBy: 'operator-1',
        completedAt: '2026-01-15T13:00:00Z',
        operations: [],
      };

      mockService.completeVve.mockResolvedValue(mockResult as any);

      await controller.recordCompletion(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.completeVve).toHaveBeenCalledWith('VVE-001', {
        departureTime: '2026-01-15T13:00:00Z',
        completedBy: 'operator-1',
      });

      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        message: 'Vessel visit execution completed successfully',
        data: mockResult,
      });

      expect(mockNext).not.toHaveBeenCalled();
    });
  });
});
