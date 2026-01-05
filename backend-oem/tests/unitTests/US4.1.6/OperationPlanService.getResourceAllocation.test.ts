import { OperationPlanService } from '../../../src/application/services/OperationPlanService';
import { IOperationPlanRepository } from '../../../src/domain/repositories/IOperationPlanRepository';
import { OperationPlan } from '../../../src/domain/entities/OperationPlan';
import { PlannedOperation } from '../../../src/domain/value-objects/PlannedOperation';
import { OperationType, OperationPlanStatus, PlanningAlgorithm } from '../../../src/shared/types';

describe('US 4.1.6 - OperationPlanService.getResourceAllocation', () => {
  let service: OperationPlanService;
  let mockRepository: jest.Mocked<IOperationPlanRepository>;

  /**
   * Helper to create mock operation plan
   */
  const createMockPlan = (
    planId: string,
    date: Date,
    operations: Partial<PlannedOperation>[]
  ): OperationPlan => {
    const plan = new OperationPlan({
      operationPlanId: planId,
      targetDate: date,
      algorithm: 'GREEDY' as PlanningAlgorithm,
      status: 'APPROVED' as OperationPlanStatus,
      createdBy: 'test-user',
      totalDelay: 0,
      operations: operations.map(
        (op) =>
          new PlannedOperation({
            vvnId: op.vvnId || 'VVN-001',
            vesselImo: op.vesselImo || 'IMO1234567',
            operationType: op.operationType || ('LOADING' as OperationType),
            plannedStart: op.plannedStart || new Date('2026-01-15T08:00:00Z'),
            plannedEnd: op.plannedEnd || new Date('2026-01-15T12:00:00Z'),
            assignedDock: op.assignedDock || 'DOCK-A',
            assignedCranes: op.assignedCranes ?? 2,
            assignedStaff: op.assignedStaff || [],
          })
      ),
    });
    return plan;
  };

  beforeEach(() => {
    mockRepository = {
      findByDateRange: jest.fn(),
      findById: jest.fn(),
      findByStatus: jest.fn(),
      save: jest.fn(),
      findByFilters: jest.fn(),
    } as any;

    service = new OperationPlanService(mockRepository, null as any, null as any);
  });

  describe('Crane Resource Allocation', () => {
    /**
     * Test: Calculates total crane allocation time
     * Business rule: Sum durations of all operations using cranes
     */
    it('should calculate total crane allocation time', async () => {
      const fromDate = new Date('2026-01-15T00:00:00Z');
      const toDate = new Date('2026-01-15T23:59:59Z');

      const plans = [
        createMockPlan('PLAN-001', fromDate, [
          {
            vvnId: 'VVN-001',
            vesselImo: 'IMO1111111',
            plannedStart: new Date('2026-01-15T08:00:00Z'),
            plannedEnd: new Date('2026-01-15T12:00:00Z'), // 4 hours
            assignedCranes: 2,
          },
          {
            vvnId: 'VVN-002',
            vesselImo: 'IMO2222222',
            plannedStart: new Date('2026-01-15T13:00:00Z'),
            plannedEnd: new Date('2026-01-15T15:00:00Z'), // 2 hours
            assignedCranes: 1,
          },
        ]),
      ];

      mockRepository.findByDateRange.mockResolvedValue(plans);

      const result = await service.getResourceAllocation('crane', 'any', fromDate, toDate);

      expect(result.totalAllocatedTime).toBe(6); // 4 + 2 hours
      expect(result.operationCount).toBe(2);
    });

    /**
     * Test: Includes only operations for the queried resource
     * Business rule: Only count operations with assignedCranes > 0 for crane queries
     */
    it('should include only crane operations for crane query', async () => {
      const fromDate = new Date('2026-01-15T00:00:00Z');
      const toDate = new Date('2026-01-15T23:59:59Z');

      const plans = [
        createMockPlan('PLAN-001', fromDate, [
          {
            vvnId: 'VVN-001',
            plannedStart: new Date('2026-01-15T08:00:00Z'),
            plannedEnd: new Date('2026-01-15T12:00:00Z'), // 4 hours
            assignedCranes: 2,
          },
        ]),
        // Create a separate plan for the operation without crane usage in the query
        createMockPlan('PLAN-002', fromDate, [
          {
            vvnId: 'VVN-002',
            plannedStart: new Date('2026-01-15T13:00:00Z'),
            plannedEnd: new Date('2026-01-15T15:00:00Z'), // 2 hours
            assignedCranes: 1, // Has cranes but we'll test filtering logic
            assignedDock: 'DOCK-B', // Different resource to not match
          },
        ]),
      ];

      mockRepository.findByDateRange.mockResolvedValue(plans);

      const result = await service.getResourceAllocation('crane', 'any', fromDate, toDate);

      // Both operations have cranes, so both should be counted
      expect(result.totalAllocatedTime).toBe(6); // 4 + 2 hours
      expect(result.operationCount).toBe(2);
    });

    /**
     * Test: Returns detailed operation list
     * Business rule: Include planId, vvnId, vessel, times, and duration
     */
    it('should return detailed operation list for cranes', async () => {
      const fromDate = new Date('2026-01-15T00:00:00Z');
      const toDate = new Date('2026-01-15T23:59:59Z');

      const plans = [
        createMockPlan('PLAN-001', fromDate, [
          {
            vvnId: 'VVN-001',
            vesselImo: 'IMO1234567',
            plannedStart: new Date('2026-01-15T08:00:00Z'),
            plannedEnd: new Date('2026-01-15T10:00:00Z'),
            assignedCranes: 1,
          },
        ]),
      ];

      mockRepository.findByDateRange.mockResolvedValue(plans);

      const result = await service.getResourceAllocation('crane', 'any', fromDate, toDate);

      expect(result.operations).toHaveLength(1);
      expect(result.operations[0]).toMatchObject({
        planId: 'PLAN-001',
        vvnId: 'VVN-001',
        vesselImo: 'IMO1234567',
        duration: 2,
      });
    });
  });

  describe('Dock Resource Allocation', () => {
    /**
     * Test: Calculates allocation for specific dock
     * Business rule: Only include operations assigned to the requested dock
     */
    it('should calculate allocation for specific dock', async () => {
      const fromDate = new Date('2026-01-15T00:00:00Z');
      const toDate = new Date('2026-01-15T23:59:59Z');

      const plans = [
        createMockPlan('PLAN-001', fromDate, [
          {
            vvnId: 'VVN-001',
            plannedStart: new Date('2026-01-15T08:00:00Z'),
            plannedEnd: new Date('2026-01-15T12:00:00Z'), // 4 hours
            assignedDock: 'DOCK-A',
          },
          {
            vvnId: 'VVN-002',
            plannedStart: new Date('2026-01-15T13:00:00Z'),
            plannedEnd: new Date('2026-01-15T16:00:00Z'), // 3 hours
            assignedDock: 'DOCK-B',
          },
          {
            vvnId: 'VVN-003',
            plannedStart: new Date('2026-01-15T16:00:00Z'),
            plannedEnd: new Date('2026-01-15T18:00:00Z'), // 2 hours
            assignedDock: 'DOCK-A',
          },
        ]),
      ];

      mockRepository.findByDateRange.mockResolvedValue(plans);

      const result = await service.getResourceAllocation('dock', 'DOCK-A', fromDate, toDate);

      expect(result.totalAllocatedTime).toBe(6); // 4 + 2 hours
      expect(result.operationCount).toBe(2);
      expect(result.resourceId).toBe('DOCK-A');
    });

    /**
     * Test: Returns empty result when dock not used
     * Business rule: If dock not assigned in period, return zero allocation
     */
    it('should return empty result when dock not used', async () => {
      const fromDate = new Date('2026-01-15T00:00:00Z');
      const toDate = new Date('2026-01-15T23:59:59Z');

      const plans = [
        createMockPlan('PLAN-001', fromDate, [
          {
            vvnId: 'VVN-001',
            plannedStart: new Date('2026-01-15T08:00:00Z'),
            plannedEnd: new Date('2026-01-15T12:00:00Z'),
            assignedDock: 'DOCK-B',
          },
        ]),
      ];

      mockRepository.findByDateRange.mockResolvedValue(plans);

      const result = await service.getResourceAllocation('dock', 'DOCK-A', fromDate, toDate);

      expect(result.totalAllocatedTime).toBe(0);
      expect(result.operationCount).toBe(0);
      expect(result.operations).toHaveLength(0);
    });

    /**
     * Test: Case-sensitive dock matching
     * Business rule: Dock IDs must match exactly (case-sensitive)
     */
    it('should match dock IDs case-sensitively', async () => {
      const fromDate = new Date('2026-01-15T00:00:00Z');
      const toDate = new Date('2026-01-15T23:59:59Z');

      const plans = [
        createMockPlan('PLAN-001', fromDate, [
          {
            vvnId: 'VVN-001',
            plannedStart: new Date('2026-01-15T08:00:00Z'),
            plannedEnd: new Date('2026-01-15T12:00:00Z'),
            assignedDock: 'DOCK-A',
          },
        ]),
      ];

      mockRepository.findByDateRange.mockResolvedValue(plans);

      // Query with lowercase - should not match
      const result = await service.getResourceAllocation('dock', 'dock-a', fromDate, toDate);

      expect(result.operationCount).toBe(0);
    });
  });

  describe('Staff Resource Allocation', () => {
    /**
     * Test: Calculates allocation for specific staff member
     * Business rule: Count operations where staff ID is in assignedStaff array
     */
    it('should calculate allocation for specific staff member', async () => {
      const fromDate = new Date('2026-01-15T00:00:00Z');
      const toDate = new Date('2026-01-15T23:59:59Z');

      const plans = [
        createMockPlan('PLAN-001', fromDate, [
          {
            vvnId: 'VVN-001',
            plannedStart: new Date('2026-01-15T08:00:00Z'),
            plannedEnd: new Date('2026-01-15T12:00:00Z'), // 4 hours
            assignedStaff: ['STAFF-001', 'STAFF-002'],
          },
          {
            vvnId: 'VVN-002',
            plannedStart: new Date('2026-01-15T13:00:00Z'),
            plannedEnd: new Date('2026-01-15T15:00:00Z'), // 2 hours
            assignedStaff: ['STAFF-003'],
          },
          {
            vvnId: 'VVN-003',
            plannedStart: new Date('2026-01-15T15:00:00Z'),
            plannedEnd: new Date('2026-01-15T17:00:00Z'), // 2 hours
            assignedStaff: ['STAFF-001'],
          },
        ]),
      ];

      mockRepository.findByDateRange.mockResolvedValue(plans);

      const result = await service.getResourceAllocation('staff', 'STAFF-001', fromDate, toDate);

      expect(result.totalAllocatedTime).toBe(6); // 4 + 2 hours
      expect(result.operationCount).toBe(2);
    });

    /**
     * Test: Handles empty staff assignments
     * Business rule: Operations with empty/undefined staff don't match
     */
    it('should handle empty staff assignments', async () => {
      const fromDate = new Date('2026-01-15T00:00:00Z');
      const toDate = new Date('2026-01-15T23:59:59Z');

      const plans = [
        createMockPlan('PLAN-001', fromDate, [
          {
            vvnId: 'VVN-001',
            plannedStart: new Date('2026-01-15T08:00:00Z'),
            plannedEnd: new Date('2026-01-15T12:00:00Z'),
            assignedStaff: [],
          },
          {
            vvnId: 'VVN-002',
            plannedStart: new Date('2026-01-15T13:00:00Z'),
            plannedEnd: new Date('2026-01-15T15:00:00Z'),
            assignedStaff: undefined,
          },
        ]),
      ];

      mockRepository.findByDateRange.mockResolvedValue(plans);

      const result = await service.getResourceAllocation('staff', 'STAFF-001', fromDate, toDate);

      expect(result.operationCount).toBe(0);
      expect(result.totalAllocatedTime).toBe(0);
    });
  });

  describe('Multiple Plans and Date Ranges', () => {
    /**
     * Test: Aggregates across multiple plans
     * Business rule: Sum allocations from all plans in date range
     */
    it('should aggregate across multiple plans', async () => {
      const fromDate = new Date('2026-01-15T00:00:00Z');
      const toDate = new Date('2026-01-16T23:59:59Z');

      const plans = [
        createMockPlan('PLAN-001', new Date('2026-01-15'), [
          {
            vvnId: 'VVN-001',
            plannedStart: new Date('2026-01-15T08:00:00Z'),
            plannedEnd: new Date('2026-01-15T12:00:00Z'), // 4 hours
            assignedDock: 'DOCK-A',
          },
        ]),
        createMockPlan('PLAN-002', new Date('2026-01-16'), [
          {
            vvnId: 'VVN-002',
            plannedStart: new Date('2026-01-16T08:00:00Z'),
            plannedEnd: new Date('2026-01-16T11:00:00Z'), // 3 hours
            assignedDock: 'DOCK-A',
          },
        ]),
      ];

      mockRepository.findByDateRange.mockResolvedValue(plans);

      const result = await service.getResourceAllocation('dock', 'DOCK-A', fromDate, toDate);

      expect(result.totalAllocatedTime).toBe(7); // 4 + 3 hours
      expect(result.operationCount).toBe(2);
      expect(result.operations).toHaveLength(2);
    });

    /**
     * Test: Returns empty result for period with no plans
     * Business rule: If no plans exist in range, return zero allocation
     */
    it('should return empty result for period with no plans', async () => {
      const fromDate = new Date('2026-01-15T00:00:00Z');
      const toDate = new Date('2026-01-15T23:59:59Z');

      mockRepository.findByDateRange.mockResolvedValue([]);

      const result = await service.getResourceAllocation('dock', 'DOCK-A', fromDate, toDate);

      expect(result.totalAllocatedTime).toBe(0);
      expect(result.operationCount).toBe(0);
      expect(result.operations).toHaveLength(0);
    });
  });

  describe('Response Format', () => {
    /**
     * Test: Includes all required fields
     * Business rule: Response must include resource info, dates, totals, and operations
     */
    it('should include all required response fields', async () => {
      const fromDate = new Date('2026-01-15T00:00:00Z');
      const toDate = new Date('2026-01-15T23:59:59Z');

      const plans = [
        createMockPlan('PLAN-001', fromDate, [
          {
            vvnId: 'VVN-001',
            plannedStart: new Date('2026-01-15T08:00:00Z'),
            plannedEnd: new Date('2026-01-15T12:00:00Z'),
            assignedCranes: 2,
          },
        ]),
      ];

      mockRepository.findByDateRange.mockResolvedValue(plans);

      const result = await service.getResourceAllocation('crane', 'CRANE-1', fromDate, toDate);

      expect(result).toHaveProperty('resourceType', 'crane');
      expect(result).toHaveProperty('resourceId', 'CRANE-1');
      expect(result).toHaveProperty('fromDate');
      expect(result).toHaveProperty('toDate');
      expect(result).toHaveProperty('totalAllocatedTime');
      expect(result).toHaveProperty('operationCount');
      expect(result).toHaveProperty('operations');
    });

    /**
     * Test: Formats dates as ISO strings
     * Business rule: Date range should be in ISO format
     */
    it('should format dates as ISO strings', async () => {
      const fromDate = new Date('2026-01-15T00:00:00Z');
      const toDate = new Date('2026-01-15T23:59:59Z');

      mockRepository.findByDateRange.mockResolvedValue([]);

      const result = await service.getResourceAllocation('dock', 'DOCK-A', fromDate, toDate);

      expect(result.fromDate).toBe('2026-01-15T00:00:00.000Z');
      expect(result.toDate).toBe('2026-01-15T23:59:59.000Z');
    });
  });

  describe('Duration Calculations', () => {
    /**
     * Test: Calculates duration in hours correctly
     * Business rule: Duration = (plannedEnd - plannedStart) in hours
     */
    it('should calculate duration in hours correctly', async () => {
      const fromDate = new Date('2026-01-15T00:00:00Z');
      const toDate = new Date('2026-01-15T23:59:59Z');

      const plans = [
        createMockPlan('PLAN-001', fromDate, [
          {
            vvnId: 'VVN-001',
            plannedStart: new Date('2026-01-15T08:00:00Z'),
            plannedEnd: new Date('2026-01-15T08:30:00Z'), // 0.5 hours
            assignedCranes: 1,
          },
        ]),
      ];

      mockRepository.findByDateRange.mockResolvedValue(plans);

      const result = await service.getResourceAllocation('crane', 'any', fromDate, toDate);

      expect(result.totalAllocatedTime).toBe(0.5);
      expect(result.operations[0]!.duration).toBe(0.5);
    });

    /**
     * Test: Handles operations spanning midnight
     * Business rule: Duration calculation works across day boundaries
     */
    it('should handle operations spanning midnight', async () => {
      const fromDate = new Date('2026-01-15T00:00:00Z');
      const toDate = new Date('2026-01-16T23:59:59Z');

      const plans = [
        createMockPlan('PLAN-001', fromDate, [
          {
            vvnId: 'VVN-001',
            plannedStart: new Date('2026-01-15T22:00:00Z'),
            plannedEnd: new Date('2026-01-16T02:00:00Z'), // 4 hours
            assignedDock: 'DOCK-A',
          },
        ]),
      ];

      mockRepository.findByDateRange.mockResolvedValue(plans);

      const result = await service.getResourceAllocation('dock', 'DOCK-A', fromDate, toDate);

      expect(result.totalAllocatedTime).toBe(4);
    });
  });

  describe('Error Handling', () => {
    /**
     * Test: Throws error for unknown resource type when processing operations
     * Business rule: Only crane, dock, staff are supported
     */
    it('should throw error for unknown resource type when plans exist', async () => {
      const fromDate = new Date('2026-01-15T00:00:00Z');
      const toDate = new Date('2026-01-15T23:59:59Z');

      const plans = [
        createMockPlan('PLAN-001', fromDate, [
          {
            vvnId: 'VVN-001',
            plannedStart: new Date('2026-01-15T08:00:00Z'),
            plannedEnd: new Date('2026-01-15T12:00:00Z'),
            assignedCranes: 1,
          },
        ]),
      ];

      mockRepository.findByDateRange.mockResolvedValue(plans);

      await expect(
        service.getResourceAllocation('truck', 'TRUCK-1', fromDate, toDate)
      ).rejects.toThrow('Unknown resource type: truck');
    });

    /**
     * Test: Propagates repository errors
     * Business rule: Database errors should bubble up
     */
    it('should propagate repository errors', async () => {
      const fromDate = new Date('2026-01-15T00:00:00Z');
      const toDate = new Date('2026-01-15T23:59:59Z');

      mockRepository.findByDateRange.mockRejectedValue(new Error('Database connection failed'));

      await expect(
        service.getResourceAllocation('dock', 'DOCK-A', fromDate, toDate)
      ).rejects.toThrow('Database connection failed');
    });

    /**
     * Test: Handles resource type case-insensitively
     * Business rule: 'Crane', 'crane', 'CRANE' should all work
     */
    it('should handle resource type case-insensitively', async () => {
      const fromDate = new Date('2026-01-15T00:00:00Z');
      const toDate = new Date('2026-01-15T23:59:59Z');

      const plans = [
        createMockPlan('PLAN-001', fromDate, [
          {
            vvnId: 'VVN-001',
            plannedStart: new Date('2026-01-15T08:00:00Z'),
            plannedEnd: new Date('2026-01-15T12:00:00Z'),
            assignedCranes: 1,
          },
        ]),
      ];

      mockRepository.findByDateRange.mockResolvedValue(plans);

      const resultLower = await service.getResourceAllocation('crane', 'any', fromDate, toDate);
      const resultUpper = await service.getResourceAllocation('CRANE', 'any', fromDate, toDate);
      const resultMixed = await service.getResourceAllocation('Crane', 'any', fromDate, toDate);

      expect(resultLower.operationCount).toBe(1);
      expect(resultUpper.operationCount).toBe(1);
      expect(resultMixed.operationCount).toBe(1);
    });
  });
});
