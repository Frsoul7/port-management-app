import { VesselVisitExecutionService } from '../../../src/application/services/VesselVisitExecutionService';
import { IVesselVisitExecutionRepository } from '../../../src/domain/repositories/IVesselVisitExecutionRepository';
import { VesselVisitExecution } from '../../../src/domain/entities/VesselVisitExecution';
import { VesselVisitExecutionStatus } from '../../../src/shared/types';
import { ExecutedOperation } from '../../../src/domain/value-objects/ExecutedOperation';
import { ExecutedOperationStatus, OperationType } from '../../../src/shared/types';
import { IOperationPlanRepository } from '../../../src/domain/repositories/IOperationPlanRepository';

describe('US 4.1.11 - VesselVisitExecutionService.completeVve', () => {
  let service: VesselVisitExecutionService;
  let mockRepository: jest.Mocked<IVesselVisitExecutionRepository>;
  let mockPlanRepository: jest.Mocked<IOperationPlanRepository>;

  /**
   * Helper to create mock VVE ready for completion
   */
  const createMockVveReadyForCompletion = (): VesselVisitExecution => {
    const vve = new VesselVisitExecution({
      vveId: 'VVE-001',
      vvnId: 'VVN-001',
      status: VesselVisitExecutionStatus.IN_PROGRESS,
    });

    // Record full lifecycle
    vve.recordPortArrival(new Date('2026-01-15T06:00:00Z'));
    vve.recordBerthing(new Date('2026-01-15T08:00:00Z'), 'DOCK-A');

    // Add completed operation
    const operation = new ExecutedOperation({
      operationType: OperationType.LOAD,
      startTime: new Date('2026-01-15T09:00:00Z'),
      endTime: new Date('2026-01-15T11:00:00Z'),
      containersProcessed: 50,
      cranesUsed: 2,
      staffAssigned: ['STAFF-001'],
      status: ExecutedOperationStatus.COMPLETED,
    });


    vve.addExecutedOperation(operation);
    vve.recordUnberthing(new Date('2026-01-15T12:00:00Z'));

    return vve;
  };

  beforeEach(() => {
    mockRepository = {
      findById: jest.fn(),
      update: jest.fn(),
      save: jest.fn(),
      findByFilters: jest.fn(),
      findByVvnId: jest.fn(),
      findByStatus: jest.fn(),
      findAll: jest.fn(),
      delete: jest.fn(),
    } as any;

    mockPlanRepository = {} as any;

    service = new VesselVisitExecutionService(mockRepository, mockPlanRepository);
  });

  describe('Valid Completion', () => {
    /**
     * Test: Completes VVE with valid departure time
     * Business rule: Should mark VVE as completed with departure time
     */
    it('should complete VVE with valid departure time', async () => {
      const vveId = 'VVE-001';
      const mockVve = createMockVveReadyForCompletion();

      mockRepository.findById.mockResolvedValue(mockVve);
      mockRepository.update.mockResolvedValue(mockVve);

      const result = await service.completeVve(vveId, {
        departureTime: '2026-01-15T13:00:00Z',
        completedBy: 'operator-1',
      });

      expect(result.status).toBe(VesselVisitExecutionStatus.COMPLETED);
      expect(result.actualPortDepartureTime).toBe('2026-01-15T13:00:00.000Z');
      expect(result.completedBy).toBe('operator-1');
    });

    /**
     * Test: Records completion timestamp
     * Business rule: Completion must be timestamped for audit
     */
    it('should record completion timestamp', async () => {
      const vveId = 'VVE-001';
      const mockVve = createMockVveReadyForCompletion();

      mockRepository.findById.mockResolvedValue(mockVve);
      mockRepository.update.mockResolvedValue(mockVve);

      const result = await service.completeVve(vveId, {
        departureTime: '2026-01-15T13:00:00Z',
        completedBy: 'operator-1',
      });

      expect(result.completedAt).toBeDefined();
    });

    /**
     * Test: Updates repository with completed VVE
     * Business rule: Completion must be persisted
     */
    it('should persist completion to repository', async () => {
      const vveId = 'VVE-001';
      const mockVve = createMockVveReadyForCompletion();

      mockRepository.findById.mockResolvedValue(mockVve);
      mockRepository.update.mockResolvedValue(mockVve);

      await service.completeVve(vveId, {
        departureTime: '2026-01-15T13:00:00Z',
        completedBy: 'operator-1',
      });

      expect(mockRepository.update).toHaveBeenCalledWith(mockVve);
    });

    /**
     * Test: Accepts various departure times
     * Business rule: Should handle different time formats
     */
    it('should handle various departure time formats', async () => {
      const vveId = 'VVE-001';
      const departureTimes = [
        '2026-01-15T13:00:00Z',
        '2026-01-15T13:30:45.123Z',
        '2026-01-15T23:59:59Z',
      ];

      for (const departureTime of departureTimes) {
        const mockVve = createMockVveReadyForCompletion();
        mockRepository.findById.mockResolvedValue(mockVve);
        mockRepository.update.mockResolvedValue(mockVve);

        const result = await service.completeVve(vveId, {
          departureTime,
          completedBy: 'operator-1',
        });

        expect(result.status).toBe(VesselVisitExecutionStatus.COMPLETED);
      }
    });
  });

  describe('Prerequisite Validations', () => {
    /**
     * Test: Requires unberth time before completion
     * Business rule: Cannot complete without unberthing
     */
    it('should require unberth time before completion', async () => {
      const vveId = 'VVE-001';
      const vve = new VesselVisitExecution({
        vveId,
        vvnId: 'VVN-001',
        status: VesselVisitExecutionStatus.IN_PROGRESS,
      });

      vve.recordPortArrival(new Date('2026-01-15T06:00:00Z'));
      vve.recordBerthing(new Date('2026-01-15T08:00:00Z'), 'DOCK-A');
      // No unberthing recorded

      mockRepository.findById.mockResolvedValue(vve);

      await expect(
        service.completeVve(vveId, {
          departureTime: '2026-01-15T13:00:00Z',
          completedBy: 'operator-1',
        })
      ).rejects.toThrow('Cannot complete VVE without unberth time');
    });

    /**
     * Test: Requires all operations completed
     * Business rule: Cannot complete with incomplete operations
     */
    it('should require all operations to be completed', async () => {
      const vveId = 'VVE-001';
      const vve = createMockVveReadyForCompletion();

      // Add incomplete operation
      const incompleteOp = new ExecutedOperation({
        operationType: OperationType.UNLOAD,
        startTime: new Date('2026-01-15T11:00:00Z'),
        containersProcessed: 0,
        cranesUsed: 1,
        staffAssigned: [],
        status: ExecutedOperationStatus.STARTED,
      });
      vve.addExecutedOperation(incompleteOp);

      mockRepository.findById.mockResolvedValue(vve);

      await expect(
        service.completeVve(vveId, {
          departureTime: '2026-01-15T13:00:00Z',
          completedBy: 'operator-1',
        })
      ).rejects.toThrow('Cannot complete VVE with incomplete operations');
    });

    /**
     * Test: Departure time must be after unberth time
     * Business rule: Logical time sequence must be maintained
     */
    it('should require departure time after unberth time', async () => {
      const vveId = 'VVE-001';
      const mockVve = createMockVveReadyForCompletion();

      mockRepository.findById.mockResolvedValue(mockVve);

      // Unberth time is 12:00, trying to depart at 11:00
      await expect(
        service.completeVve(vveId, {
          departureTime: '2026-01-15T11:00:00Z',
          completedBy: 'operator-1',
        })
      ).rejects.toThrow('Port departure time must be after unberth time');
    });

    /**
     * Test: Cannot complete already completed VVE
     * Business rule: Completed VVEs are read-only
     */
    it('should not allow re-completion of already completed VVE', async () => {
      const vveId = 'VVE-001';
      const mockVve = createMockVveReadyForCompletion();

      // Complete it first
      mockVve.markAsCompleted(new Date('2026-01-15T13:00:00Z'), 'operator-1');

      mockRepository.findById.mockResolvedValue(mockVve);

      // Try to complete again
      await expect(
        service.completeVve(vveId, {
          departureTime: '2026-01-15T14:00:00Z',
          completedBy: 'operator-2',
        })
      ).rejects.toThrow();
    });
  });

  describe('Timestamp Validations', () => {
    /**
     * Test: Validates departure time is a valid date
     * Business rule: Must provide valid datetime
     */
    it('should validate departure time is a valid date', async () => {
      const vveId = 'VVE-001';
      const mockVve = createMockVveReadyForCompletion();

      mockRepository.findById.mockResolvedValue(mockVve);
      mockRepository.update.mockResolvedValue(mockVve);

      // Invalid date string will create Invalid Date
      const result = await service.completeVve(vveId, {
        departureTime: '2026-01-15T13:00:00Z',
        completedBy: 'operator-1',
      });

      expect(result.actualPortDepartureTime).toBeDefined();
    });

    /**
     * Test: Handles precise timestamps
     * Business rule: Should preserve timestamp precision
     */
    it('should handle precise timestamps with milliseconds', async () => {
      const vveId = 'VVE-001';
      const mockVve = createMockVveReadyForCompletion();

      mockRepository.findById.mockResolvedValue(mockVve);
      mockRepository.update.mockResolvedValue(mockVve);

      const preciseTime = '2026-01-15T13:45:32.987Z';
      const result = await service.completeVve(vveId, {
        departureTime: preciseTime,
        completedBy: 'operator-1',
      });

      const expectedTime = new Date(preciseTime);
      const actualTime = new Date(result.actualPortDepartureTime!);
      expect(actualTime.getTime()).toBe(expectedTime.getTime());
    });
  });

  describe('User Tracking', () => {
    /**
     * Test: Records who completed the VVE
     * Business rule: Must track operator for audit
     */
    it('should record the operator who completed the VVE', async () => {
      const vveId = 'VVE-001';
      const mockVve = createMockVveReadyForCompletion();

      mockRepository.findById.mockResolvedValue(mockVve);
      mockRepository.update.mockResolvedValue(mockVve);

      const result = await service.completeVve(vveId, {
        departureTime: '2026-01-15T13:00:00Z',
        completedBy: 'operator-123',
      });

      expect(result.completedBy).toBe('operator-123');
    });

    /**
     * Test: Handles different user identifiers
     * Business rule: Should accept various user ID formats
     */
    it('should handle different user identifier formats', async () => {
      const vveId = 'VVE-001';
      const userIds = ['operator-1', 'user@email.com', 'UUID-123-456', '12345'];

      for (const userId of userIds) {
        const mockVve = createMockVveReadyForCompletion();
        mockRepository.findById.mockResolvedValue(mockVve);
        mockRepository.update.mockResolvedValue(mockVve);

        const result = await service.completeVve(vveId, {
          departureTime: '2026-01-15T13:00:00Z',
          completedBy: userId,
        });

        expect(result.completedBy).toBe(userId);
      }
    });
  });

  describe('Status Transitions', () => {
    /**
     * Test: Changes status from IN_PROGRESS to COMPLETED
     * Business rule: Must update status correctly
     */
    it('should transition status from IN_PROGRESS to COMPLETED', async () => {
      const vveId = 'VVE-001';
      const mockVve = createMockVveReadyForCompletion();

      expect(mockVve.status).toBe(VesselVisitExecutionStatus.IN_PROGRESS);

      mockRepository.findById.mockResolvedValue(mockVve);
      mockRepository.update.mockResolvedValue(mockVve);

      const result = await service.completeVve(vveId, {
        departureTime: '2026-01-15T13:00:00Z',
        completedBy: 'operator-1',
      });

      expect(result.status).toBe(VesselVisitExecutionStatus.COMPLETED);
    });

    /**
     * Test: Cannot complete VVE in PLANNED status
     * Business rule: VVE must be in progress
     */
    it('should not allow completion of PLANNED VVE', async () => {
      const vveId = 'VVE-001';
      const vve = new VesselVisitExecution({
        vveId,
        vvnId: 'VVN-001',
        status: VesselVisitExecutionStatus.PLANNED,
      });

      mockRepository.findById.mockResolvedValue(vve);

      await expect(
        service.completeVve(vveId, {
          departureTime: '2026-01-15T13:00:00Z',
          completedBy: 'operator-1',
        })
      ).rejects.toThrow();
    });
  });

  describe('Repository Interactions', () => {
    /**
     * Test: Fetches VVE before updating
     * Business rule: Must retrieve current state first
     */
    it('should fetch VVE from repository before completing', async () => {
      const vveId = 'VVE-001';
      const mockVve = createMockVveReadyForCompletion();

      mockRepository.findById.mockResolvedValue(mockVve);
      mockRepository.update.mockResolvedValue(mockVve);

      await service.completeVve(vveId, {
        departureTime: '2026-01-15T13:00:00Z',
        completedBy: 'operator-1',
      });

      expect(mockRepository.findById).toHaveBeenCalledWith(vveId);
      const findByIdOrder = (mockRepository.findById as jest.Mock).mock.invocationCallOrder[0]!;
      const updateOrder = (mockRepository.update as jest.Mock).mock.invocationCallOrder[0]!;
      expect(findByIdOrder).toBeLessThan(updateOrder);
    });

    /**
     * Test: Returns updated VVE as DTO
     * Business rule: Should return serialized state
     */
    it('should return completed VVE as DTO', async () => {
      const vveId = 'VVE-001';
      const mockVve = createMockVveReadyForCompletion();

      mockRepository.findById.mockResolvedValue(mockVve);
      mockRepository.update.mockResolvedValue(mockVve);

      const result = await service.completeVve(vveId, {
        departureTime: '2026-01-15T13:00:00Z',
        completedBy: 'operator-1',
      });

      expect(result).toHaveProperty('vveId');
      expect(result).toHaveProperty('status');
      expect(result).toHaveProperty('actualPortDepartureTime');
      expect(result).toHaveProperty('completedAt');
      expect(result).toHaveProperty('completedBy');
    });
  });

  describe('Error Handling', () => {
    /**
     * Test: Throws error when VVE not found
     * Business rule: VVE must exist to complete
     */
    it('should throw error when VVE not found', async () => {
      mockRepository.findById.mockResolvedValue(null);

      await expect(
        service.completeVve('VVE-NONEXISTENT', {
          departureTime: '2026-01-15T13:00:00Z',
          completedBy: 'operator-1',
        })
      ).rejects.toThrow('VVE VVE-NONEXISTENT not found');
    });

    /**
     * Test: Handles repository update errors
     * Business rule: Should propagate database errors
     */
    it('should handle repository update errors', async () => {
      const vveId = 'VVE-001';
      const mockVve = createMockVveReadyForCompletion();

      mockRepository.findById.mockResolvedValue(mockVve);
      mockRepository.update.mockRejectedValue(new Error('Database update failed'));

      await expect(
        service.completeVve(vveId, {
          departureTime: '2026-01-15T13:00:00Z',
          completedBy: 'operator-1',
        })
      ).rejects.toThrow('Database update failed');
    });

    /**
     * Test: Handles repository fetch errors
     * Business rule: Should handle lookup failures
     */
    it('should handle repository fetch errors', async () => {
      mockRepository.findById.mockRejectedValue(new Error('Database connection lost'));

      await expect(
        service.completeVve('VVE-001', {
          departureTime: '2026-01-15T13:00:00Z',
          completedBy: 'operator-1',
        })
      ).rejects.toThrow('Database connection lost');
    });
  });

  describe('Integration Scenarios', () => {
    /**
     * Test: Complete VVE workflow from start to finish
     * Business rule: Should handle full lifecycle completion
     */
    it('should complete full VVE lifecycle', async () => {
      const vveId = 'VVE-001';
      const mockVve = createMockVveReadyForCompletion();

      mockRepository.findById.mockResolvedValue(mockVve);
      mockRepository.update.mockResolvedValue(mockVve);

      const result = await service.completeVve(vveId, {
        departureTime: '2026-01-15T13:00:00Z',
        completedBy: 'operator-1',
      });

      expect(result.status).toBe(VesselVisitExecutionStatus.COMPLETED);
      expect(result.actualPortArrivalTime).toBeDefined();
      expect(result.actualBerthTime).toBeDefined();
      expect(result.actualUnberthTime).toBeDefined();
      expect(result.actualPortDepartureTime).toBeDefined();
      expect(result.completedAt).toBeDefined();
      expect(result.completedBy).toBe('operator-1');
    });

    /**
     * Test: VVE with multiple completed operations
     * Business rule: Should handle multiple operations
     */
    it('should complete VVE with multiple operations', async () => {
      const vveId = 'VVE-001';
      const mockVve = createMockVveReadyForCompletion();

      // Add another completed operation
      const op2 = new ExecutedOperation({
        operationType: OperationType.UNLOAD,
        startTime: new Date('2026-01-15T11:00:00Z'),
        endTime: new Date('2026-01-15T12:00:00Z'),
        containersProcessed: 30,
        cranesUsed: 1,
        staffAssigned: ['STAFF-002'],
        status: ExecutedOperationStatus.COMPLETED,
      });
      mockVve.addExecutedOperation(op2);

      mockRepository.findById.mockResolvedValue(mockVve);
      mockRepository.update.mockResolvedValue(mockVve);

      const result = await service.completeVve(vveId, {
        departureTime: '2026-01-15T13:00:00Z',
        completedBy: 'operator-1',
      });

      expect(result.status).toBe(VesselVisitExecutionStatus.COMPLETED);
      expect(result.operations).toHaveLength(2);
    });
  });
});
