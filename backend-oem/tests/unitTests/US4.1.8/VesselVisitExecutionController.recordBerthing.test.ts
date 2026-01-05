import { Request, Response, NextFunction } from 'express';
import { VesselVisitExecutionController } from '../../../src/presentation/controllers/VesselVisitExecutionController';
import { VesselVisitExecutionService } from '../../../src/application/services/VesselVisitExecutionService';
import { VesselVisitExecutionStatus } from '../../../src/shared/types';

describe('US 4.1.8 - VesselVisitExecutionController.recordBerthing', () => {
  let controller: VesselVisitExecutionController;
  let mockService: jest.Mocked<VesselVisitExecutionService>;
  let mockRequest: Partial<Request>;
  let mockResponse: Partial<Response>;
  let mockNext: NextFunction;

  beforeEach(() => {
    mockService = {
      recordBerthing: jest.fn(),
    } as any;

    controller = new VesselVisitExecutionController(mockService);

    mockResponse = {
      json: jest.fn(),
      status: jest.fn().mockReturnThis(),
    };

    mockNext = jest.fn();
  });

  describe('Valid Berthing Requests', () => {
    /**
     * Test: Records berthing with valid parameters
     * Business rule: Should call service and return success response
     */
    it('should record berthing with valid berth time and dock ID', async () => {
      mockRequest = {
        params: { id: 'VVE-001' },
        body: {
          berthTime: '2026-01-15T08:00:00Z',
          dockId: 'DOCK-A',
        },
      };

      const mockResult = {
        vveId: 'VVE-001',
        vvnId: 'VVN-001',
        vesselImo: 'IMO1234567',
        status: VesselVisitExecutionStatus.IN_PROGRESS,
        actualBerthTime: '2026-01-15T08:00:00Z',
        assignedDock: 'DOCK-A',
        operations: [],
      };

      mockService.recordBerthing.mockResolvedValue(mockResult as any);

      await controller.recordBerthing(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.recordBerthing).toHaveBeenCalledWith('VVE-001', {
        berthTime: '2026-01-15T08:00:00Z',
        dockId: 'DOCK-A',
      });
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        message: 'Berthing recorded successfully',
        data: mockResult,
      });
    });

    /**
     * Test: Handles different dock identifiers
     * Business rule: Should accept various dock ID formats
     */
    it('should accept various dock ID formats', async () => {
      const dockIds = ['DOCK-A', 'BERTH-101', 'TERMINAL-5', 'B-12'];

      for (const dockId of dockIds) {
        mockRequest = {
          params: { id: 'VVE-001' },
          body: {
            berthTime: '2026-01-15T08:00:00Z',
            dockId,
          },
        };

        mockService.recordBerthing.mockResolvedValue({
          vveId: 'VVE-001',
          assignedDock: dockId,
        } as any);

        await controller.recordBerthing(
          mockRequest as Request,
          mockResponse as Response,
          mockNext
        );

        expect(mockService.recordBerthing).toHaveBeenCalledWith('VVE-001', {
          berthTime: '2026-01-15T08:00:00Z',
          dockId,
        });
      }
    });

    /**
     * Test: Handles ISO 8601 datetime formats
     * Business rule: Should accept standard datetime strings
     */
    it('should handle ISO 8601 datetime formats', async () => {
      mockRequest = {
        params: { id: 'VVE-001' },
        body: {
          berthTime: '2026-01-15T08:23:45.123Z',
          dockId: 'DOCK-A',
        },
      };

      mockService.recordBerthing.mockResolvedValue({
        vveId: 'VVE-001',
        actualBerthTime: '2026-01-15T08:23:45.123Z',
      } as any);

      await controller.recordBerthing(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.recordBerthing).toHaveBeenCalledWith('VVE-001', {
        berthTime: '2026-01-15T08:23:45.123Z',
        dockId: 'DOCK-A',
      });
    });

    /**
     * Test: Returns complete VVE data
     * Business rule: Response should include updated VVE state
     */
    it('should return complete VVE data after berthing', async () => {
      mockRequest = {
        params: { id: 'VVE-001' },
        body: {
          berthTime: '2026-01-15T08:00:00Z',
          dockId: 'DOCK-A',
        },
      };

      const mockResult = {
        vveId: 'VVE-001',
        vvnId: 'VVN-001',
        vesselImo: 'IMO1234567',
        status: VesselVisitExecutionStatus.IN_PROGRESS,
        createdBy: 'test-user',
        createdAt: '2026-01-15T00:00:00Z',
        actualPortArrivalTime: '2026-01-15T06:00:00Z',
        actualBerthTime: '2026-01-15T08:00:00Z',
        assignedDock: 'DOCK-A',
        operations: [],
      };

      mockService.recordBerthing.mockResolvedValue(mockResult as any);

      await controller.recordBerthing(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      const responseData = (mockResponse.json as jest.Mock).mock.calls[0]![0];
      expect(responseData.data).toMatchObject({
        vveId: 'VVE-001',
        actualBerthTime: '2026-01-15T08:00:00Z',
        assignedDock: 'DOCK-A',
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
          berthTime: '2026-01-15T08:00:00Z',
          dockId: 'DOCK-A',
        },
      };

      mockService.recordBerthing.mockResolvedValue({} as any);

      await controller.recordBerthing(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.recordBerthing).toHaveBeenCalledWith(
        'VVE-123',
        expect.any(Object)
      );
    });

    /**
     * Test: Extracts berth time from request body
     * Business rule: Berth time comes from POST body
     */
    it('should extract berth time from request body', async () => {
      mockRequest = {
        params: { id: 'VVE-001' },
        body: {
          berthTime: '2026-01-15T10:30:00Z',
          dockId: 'DOCK-A',
        },
      };

      mockService.recordBerthing.mockResolvedValue({} as any);

      await controller.recordBerthing(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.recordBerthing).toHaveBeenCalledWith('VVE-001', {
        berthTime: '2026-01-15T10:30:00Z',
        dockId: 'DOCK-A',
      });
    });

    /**
     * Test: Extracts dock ID from request body
     * Business rule: Dock ID comes from POST body
     */
    it('should extract dock ID from request body', async () => {
      mockRequest = {
        params: { id: 'VVE-001' },
        body: {
          berthTime: '2026-01-15T08:00:00Z',
          dockId: 'TERMINAL-NORTH',
        },
      };

      mockService.recordBerthing.mockResolvedValue({} as any);

      await controller.recordBerthing(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.recordBerthing).toHaveBeenCalledWith('VVE-001', {
        berthTime: '2026-01-15T08:00:00Z',
        dockId: 'TERMINAL-NORTH',
      });
    });
  });

  describe('Response Structure', () => {
    /**
     * Test: Response includes success flag
     * Business rule: All responses should indicate success/failure
     */
    it('should include success flag in response', async () => {
      mockRequest = {
        params: { id: 'VVE-001' },
        body: {
          berthTime: '2026-01-15T08:00:00Z',
          dockId: 'DOCK-A',
        },
      };

      mockService.recordBerthing.mockResolvedValue({} as any);

      await controller.recordBerthing(
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
          berthTime: '2026-01-15T08:00:00Z',
          dockId: 'DOCK-A',
        },
      };

      mockService.recordBerthing.mockResolvedValue({} as any);

      await controller.recordBerthing(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockResponse.json).toHaveBeenCalledWith(
        expect.objectContaining({
          message: 'Berthing recorded successfully',
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
          berthTime: '2026-01-15T08:00:00Z',
          dockId: 'DOCK-A',
        },
      };

      const mockResult = {
        vveId: 'VVE-001',
        actualBerthTime: '2026-01-15T08:00:00Z',
        assignedDock: 'DOCK-A',
      };

      mockService.recordBerthing.mockResolvedValue(mockResult as any);

      await controller.recordBerthing(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        message: 'Berthing recorded successfully',
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
          berthTime: '2026-01-15T08:00:00Z',
          dockId: 'DOCK-A',
        },
      };

      const mockResult = {
        vveId: 'VVE-001',
        vvnId: 'VVN-001',
        vesselImo: 'IMO1234567',
        status: VesselVisitExecutionStatus.IN_PROGRESS,
        createdBy: 'operator-1',
        createdAt: '2026-01-15T00:00:00Z',
        actualPortArrivalTime: '2026-01-15T06:00:00Z',
        actualBerthTime: '2026-01-15T08:00:00Z',
        assignedDock: 'DOCK-A',
        operations: [],
      };

      mockService.recordBerthing.mockResolvedValue(mockResult as any);

      await controller.recordBerthing(
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
          berthTime: '2026-01-15T08:00:00Z',
          dockId: 'DOCK-A',
        },
      };

      const mockError = new Error('VVE not found');
      mockService.recordBerthing.mockRejectedValue(mockError);

      await controller.recordBerthing(
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
          berthTime: '2026-01-15T08:00:00Z',
          dockId: 'DOCK-A',
        },
      };

      const mockError = new Error('VVE VVE-NONEXISTENT not found');
      mockService.recordBerthing.mockRejectedValue(mockError);

      await controller.recordBerthing(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockNext).toHaveBeenCalledWith(mockError);
    });

    /**
     * Test: Handles invalid berth time error
     * Business rule: Domain validation errors should be propagated
     */
    it('should handle invalid berth time error', async () => {
      mockRequest = {
        params: { id: 'VVE-001' },
        body: {
          berthTime: '2026-01-15T05:00:00Z', // Before arrival
          dockId: 'DOCK-A',
        },
      };

      const mockError = new Error('Berth time must be after port arrival time');
      mockService.recordBerthing.mockRejectedValue(mockError);

      await controller.recordBerthing(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockNext).toHaveBeenCalledWith(mockError);
    });

    /**
     * Test: Handles invalid VVE status error
     * Business rule: Status validation errors should be propagated
     */
    it('should handle invalid VVE status error', async () => {
      mockRequest = {
        params: { id: 'VVE-001' },
        body: {
          berthTime: '2026-01-15T08:00:00Z',
          dockId: 'DOCK-A',
        },
      };

      const mockError = new Error('Cannot record berthing for VVE with status COMPLETED');
      mockService.recordBerthing.mockRejectedValue(mockError);

      await controller.recordBerthing(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockNext).toHaveBeenCalledWith(mockError);
    });

    /**
     * Test: Handles port arrival prerequisite error
     * Business rule: Should propagate prerequisite errors
     */
    it('should handle missing port arrival error', async () => {
      mockRequest = {
        params: { id: 'VVE-001' },
        body: {
          berthTime: '2026-01-15T08:00:00Z',
          dockId: 'DOCK-A',
        },
      };

      const mockError = new Error('Cannot record berthing without port arrival time');
      mockService.recordBerthing.mockRejectedValue(mockError);

      await controller.recordBerthing(
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
          berthTime: '2026-01-15T08:00:00Z',
          dockId: 'DOCK-A',
        },
      };

      const dbError = new Error('Database connection failed');
      mockService.recordBerthing.mockRejectedValue(dbError);

      await controller.recordBerthing(
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
          berthTime: '2026-01-15T08:00:00Z',
          dockId: 'DOCK-A',
        },
      };

      const unexpectedError = new Error('Unexpected error occurred');
      mockService.recordBerthing.mockRejectedValue(unexpectedError);

      await controller.recordBerthing(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockNext).toHaveBeenCalledWith(unexpectedError);
    });
  });

  describe('Request Body Edge Cases', () => {
    /**
     * Test: Handles missing berthTime
     * Business rule: Service should validate required fields
     */
    it('should pass through missing berthTime to service', async () => {
      mockRequest = {
        params: { id: 'VVE-001' },
        body: {
          dockId: 'DOCK-A',
          // berthTime missing
        },
      };

      mockService.recordBerthing.mockRejectedValue(
        new Error('Berth time is required')
      );

      await controller.recordBerthing(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.recordBerthing).toHaveBeenCalledWith('VVE-001', {
        berthTime: undefined,
        dockId: 'DOCK-A',
      });
      expect(mockNext).toHaveBeenCalled();
    });

    /**
     * Test: Handles missing dockId
     * Business rule: Service should validate required fields
     */
    it('should pass through missing dockId to service', async () => {
      mockRequest = {
        params: { id: 'VVE-001' },
        body: {
          berthTime: '2026-01-15T08:00:00Z',
          // dockId missing
        },
      };

      mockService.recordBerthing.mockRejectedValue(new Error('Dock ID is required'));

      await controller.recordBerthing(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.recordBerthing).toHaveBeenCalledWith('VVE-001', {
        berthTime: '2026-01-15T08:00:00Z',
        dockId: undefined,
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
          berthTime: '2026-01-15T08:00:00Z',
          dockId: 'DOCK-A',
          extraField: 'should-be-ignored',
          anotherExtra: 123,
        },
      };

      mockService.recordBerthing.mockResolvedValue({} as any);

      await controller.recordBerthing(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.recordBerthing).toHaveBeenCalledWith('VVE-001', {
        berthTime: '2026-01-15T08:00:00Z',
        dockId: 'DOCK-A',
      });
    });
  });

  describe('Integration Scenarios', () => {
    /**
     * Test: Complete berthing workflow
     * Business rule: Should handle full berthing process
     */
    it('should handle complete berthing workflow', async () => {
      mockRequest = {
        params: { id: 'VVE-001' },
        body: {
          berthTime: '2026-01-15T08:00:00Z',
          dockId: 'DOCK-A',
        },
      };

      const mockResult = {
        vveId: 'VVE-001',
        vvnId: 'VVN-001',
        vesselImo: 'IMO1234567',
        status: VesselVisitExecutionStatus.IN_PROGRESS,
        actualPortArrivalTime: '2026-01-15T06:00:00Z',
        actualBerthTime: '2026-01-15T08:00:00Z',
        assignedDock: 'DOCK-A',
        operations: [],
      };

      mockService.recordBerthing.mockResolvedValue(mockResult as any);

      await controller.recordBerthing(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.recordBerthing).toHaveBeenCalledWith('VVE-001', {
        berthTime: '2026-01-15T08:00:00Z',
        dockId: 'DOCK-A',
      });
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        message: 'Berthing recorded successfully',
        data: mockResult,
      });
      expect(mockNext).not.toHaveBeenCalled();
    });

    /**
     * Test: Handles berthing updates (dock change)
     * Business rule: Should support updating berthing information
     */
    it('should handle berthing updates', async () => {
      mockRequest = {
        params: { id: 'VVE-001' },
        body: {
          berthTime: '2026-01-15T08:00:00Z',
          dockId: 'DOCK-B', // Changed from DOCK-A
        },
      };

      const mockResult = {
        vveId: 'VVE-001',
        actualBerthTime: '2026-01-15T08:00:00Z',
        assignedDock: 'DOCK-B',
      };

      mockService.recordBerthing.mockResolvedValue(mockResult as any);

      await controller.recordBerthing(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockResponse.json).toHaveBeenCalledWith(
        expect.objectContaining({
          success: true,
          data: expect.objectContaining({
            assignedDock: 'DOCK-B',
          }),
        })
      );
    });
  });
});
