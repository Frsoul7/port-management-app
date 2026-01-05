import { OperationPlanService } from '@application/services/OperationPlanService';
import { IOperationPlanRepository } from '@domain/repositories/IOperationPlanRepository';
import { OperationPlan } from '@domain/entities/OperationPlan';
import { PlannedOperation } from '@domain/value-objects/PlannedOperation';
import { CoreBackendClient } from '@infrastructure/http-clients/CoreBackendClient';
import { PlanningClient } from '@infrastructure/http-clients/PlanningClient';
import {
  OperationPlanStatus,
  PlanningAlgorithm,
  OperationType,
  VvnReference,
} from '@shared/types';

/**
 * Unit Tests for OperationPlanService - getVvnsWithoutPlans method
 * US 4.1.5: Identify Missing Operation Plans
 * 
 * Test Coverage:
 * - No existing plan (all VVNs missing)
 * - Existing plan with all VVNs covered (no missing)
 * - Existing plan with some VVNs missing
 * - Empty VVN list from Core Backend
 * - Multiple missing VVNs
 * - Date range handling (start/end of day)
 */
describe('US 4.1.5 - OperationPlanService.getVvnsWithoutPlans', () => {
  let service: OperationPlanService;
  let mockRepository: jest.Mocked<IOperationPlanRepository>;
  let mockCoreBackendClient: jest.Mocked<CoreBackendClient>;
  let mockPlanningClient: jest.Mocked<PlanningClient>;

  const targetDate = new Date('2026-01-15T00:00:00.000Z');

  /**
   * Create a mock VVN reference for testing
   */
  const createMockVvn = (vvnId: string, vesselImo: string): VvnReference => ({
    vvnId,
    vesselImo,
    eta: new Date(targetDate.getTime() + 8 * 60 * 60 * 1000), // 8am
    etd: new Date(targetDate.getTime() + 20 * 60 * 60 * 1000), // 8pm
    loadingCount: 50,
    unloadingCount: 30,
    purpose: OperationType.UNLOAD,
    state: 'APPROVED',
  });

  /**
   * Create a mock planned operation
   */
  const createMockOperation = (vvnId: string): PlannedOperation => {
    return new PlannedOperation({
      vvnId,
      vesselImo: 'IMO1234567',
      operationType: OperationType.UNLOAD,
      plannedStart: new Date(targetDate.getTime() + 8 * 60 * 60 * 1000),
      plannedEnd: new Date(targetDate.getTime() + 12 * 60 * 60 * 1000),
      assignedCranes: 2,
    });
  };

  /**
   * Create a mock operation plan
   */
  const createMockPlan = (
    targetDate: Date,
    operations: PlannedOperation[]
  ): OperationPlan => {
    return new OperationPlan({
      operationPlanId: 'PLAN-001',
      targetDate,
      status: OperationPlanStatus.GENERATED,
      algorithm: PlanningAlgorithm.OPTIMAL,
      createdBy: 'user-001',
      operations,
      totalDelay: 0,
    });
  };

  beforeEach(() => {
    // Create mock repository
    mockRepository = {
      findByTargetDate: jest.fn(),
      findById: jest.fn(),
      findAll: jest.fn(),
      findByStatus: jest.fn(),
      findByDateRange: jest.fn(),
      save: jest.fn(),
      update: jest.fn(),
      delete: jest.fn(),
    } as any;

    // Create mock Core Backend client
    mockCoreBackendClient = {
      getVvnsByDateRange: jest.fn(),
      getVvnById: jest.fn(),
    } as any;

    // Create mock Planning client
    mockPlanningClient = {
      generatePlan: jest.fn(),
    } as any;

    // Create service instance
    service = new OperationPlanService(
      mockRepository,
      mockCoreBackendClient,
      mockPlanningClient
    );
  });

  describe('No Existing Plan Scenario', () => {
    /**
     * Test: When no plan exists for the date, all VVNs are missing
     * Business rule: If no operation plan exists, all approved VVNs need plans
     */
    it('should return all VVNs when no operation plan exists', async () => {
      const mockVvns = [
        createMockVvn('VVN-001', 'IMO1111111'),
        createMockVvn('VVN-002', 'IMO2222222'),
        createMockVvn('VVN-003', 'IMO3333333'),
      ];

      mockRepository.findByTargetDate.mockResolvedValue(null);
      mockCoreBackendClient.getVvnsByDateRange.mockResolvedValue(mockVvns);

      const result = await service.getVvnsWithoutPlansWithAuth(targetDate);

      expect(result).toHaveLength(3);
      expect(result).toEqual(mockVvns);
      expect(mockRepository.findByTargetDate).toHaveBeenCalledWith(targetDate);
      expect(mockCoreBackendClient.getVvnsByDateRange).toHaveBeenCalledWith(
        expect.any(Date), // start of day
        expect.any(Date)  // end of day
      );

      // Verify date range bounds
      const callArgs = mockCoreBackendClient.getVvnsByDateRange.mock.calls[0];
      if (callArgs) {
        const startOfDay = callArgs[0] as Date;
        const endOfDay = callArgs[1] as Date;
        expect(startOfDay.getDate()).toBe(targetDate.getDate());
        expect(endOfDay.getDate()).toBe(targetDate.getDate());
      }
    });

    /**
     * Test: Verify date range is properly calculated (start and end of day)
     * Business rule: Query must include entire day (00:00:00 to 23:59:59.999)
     */
    it('should query Core Backend with correct date range', async () => {
      mockRepository.findByTargetDate.mockResolvedValue(null);
      mockCoreBackendClient.getVvnsByDateRange.mockResolvedValue([]);

      await service.getVvnsWithoutPlansWithAuth(targetDate);

      const callArgs = mockCoreBackendClient.getVvnsByDateRange.mock.calls[0];
      expect(callArgs).toBeDefined();
      
      const startOfDay = callArgs![0] as Date;
      const endOfDay = callArgs![1] as Date;

      // Verify start of day (00:00:00.000)
      expect(startOfDay.getHours()).toBe(0);
      expect(startOfDay.getMinutes()).toBe(0);
      expect(startOfDay.getSeconds()).toBe(0);
      expect(startOfDay.getMilliseconds()).toBe(0);

      // Verify end of day (23:59:59.999)
      expect(endOfDay.getHours()).toBe(23);
      expect(endOfDay.getMinutes()).toBe(59);
      expect(endOfDay.getSeconds()).toBe(59);
      expect(endOfDay.getMilliseconds()).toBe(999);
    });

    /**
     * Test: When Core Backend returns empty list, result should be empty
     * Business rule: If no VVNs exist for the date, there are no missing plans
     */
    it('should return empty array when no VVNs exist for date', async () => {
      mockRepository.findByTargetDate.mockResolvedValue(null);
      mockCoreBackendClient.getVvnsByDateRange.mockResolvedValue([]);

      const result = await service.getVvnsWithoutPlansWithAuth(targetDate);

      expect(result).toHaveLength(0);
      expect(result).toEqual([]);
    });
  });

  describe('Existing Plan - All VVNs Covered', () => {
    /**
     * Test: When plan includes all VVNs, no missing VVNs
     * Business rule: If all VVNs have operations in the plan, nothing is missing
     */
    it('should return empty array when all VVNs have operations', async () => {
      const mockVvns = [
        createMockVvn('VVN-001', 'IMO1111111'),
        createMockVvn('VVN-002', 'IMO2222222'),
      ];

      const operations = [
        createMockOperation('VVN-001'),
        createMockOperation('VVN-002'),
      ];

      const existingPlan = createMockPlan(targetDate, operations);

      mockRepository.findByTargetDate.mockResolvedValue(existingPlan);
      mockCoreBackendClient.getVvnsByDateRange.mockResolvedValue(mockVvns);

      const result = await service.getVvnsWithoutPlansWithAuth(targetDate);

      expect(result).toHaveLength(0);
      expect(result).toEqual([]);
    });

    /**
     * Test: Plan with more operations than VVNs (outdated/extra operations)
     * Business rule: If plan has operations for VVNs that no longer exist, ignore them
     */
    it('should handle plan with extra operations not in VVN list', async () => {
      const mockVvns = [
        createMockVvn('VVN-001', 'IMO1111111'),
      ];

      const operations = [
        createMockOperation('VVN-001'),
        createMockOperation('VVN-999'), // Extra operation not in VVN list
      ];

      const existingPlan = createMockPlan(targetDate, operations);

      mockRepository.findByTargetDate.mockResolvedValue(existingPlan);
      mockCoreBackendClient.getVvnsByDateRange.mockResolvedValue(mockVvns);

      const result = await service.getVvnsWithoutPlansWithAuth(targetDate);

      expect(result).toHaveLength(0);
      expect(result).toEqual([]);
    });
  });

  describe('Existing Plan - Partial Coverage', () => {
    /**
     * Test: When plan has some but not all VVNs
     * Business rule: Missing VVNs = All VVNs - VVNs with operations
     */
    it('should return VVNs missing from operation plan', async () => {
      const mockVvns = [
        createMockVvn('VVN-001', 'IMO1111111'),
        createMockVvn('VVN-002', 'IMO2222222'),
        createMockVvn('VVN-003', 'IMO3333333'),
        createMockVvn('VVN-004', 'IMO4444444'),
      ];

      // Plan only has operations for VVN-001 and VVN-003
      const operations = [
        createMockOperation('VVN-001'),
        createMockOperation('VVN-003'),
      ];

      const existingPlan = createMockPlan(targetDate, operations);

      mockRepository.findByTargetDate.mockResolvedValue(existingPlan);
      mockCoreBackendClient.getVvnsByDateRange.mockResolvedValue(mockVvns);

      const result = await service.getVvnsWithoutPlansWithAuth(targetDate);

      expect(result).toHaveLength(2);
      expect(result).toEqual([
        mockVvns[1], // VVN-002
        mockVvns[3], // VVN-004
      ]);

      // Verify the missing VVN IDs
      const missingIds = result.map((v) => v.vvnId);
      expect(missingIds).toContain('VVN-002');
      expect(missingIds).toContain('VVN-004');
      expect(missingIds).not.toContain('VVN-001');
      expect(missingIds).not.toContain('VVN-003');
    });

    /**
     * Test: Single missing VVN among many
     * Business rule: Even if only one VVN is missing, it should be identified
     */
    it('should identify single missing VVN among many', async () => {
      const mockVvns = [
        createMockVvn('VVN-001', 'IMO1111111'),
        createMockVvn('VVN-002', 'IMO2222222'),
        createMockVvn('VVN-003', 'IMO3333333'),
        createMockVvn('VVN-004', 'IMO4444444'),
        createMockVvn('VVN-005', 'IMO5555555'),
      ];

      // Plan has all except VVN-003
      const operations = [
        createMockOperation('VVN-001'),
        createMockOperation('VVN-002'),
        createMockOperation('VVN-004'),
        createMockOperation('VVN-005'),
      ];

      const existingPlan = createMockPlan(targetDate, operations);

      mockRepository.findByTargetDate.mockResolvedValue(existingPlan);
      mockCoreBackendClient.getVvnsByDateRange.mockResolvedValue(mockVvns);

      const result = await service.getVvnsWithoutPlansWithAuth(targetDate);

      expect(result).toHaveLength(1);
      expect(result[0]?.vvnId).toBe('VVN-003');
    });
  });

  describe('Multiple Missing VVNs', () => {
    /**
     * Test: Large number of missing VVNs
     * Business rule: Should handle bulk missing VVNs efficiently
     */
    it('should handle many missing VVNs', async () => {
      const mockVvns: VvnReference[] = [];
      for (let i = 1; i <= 20; i++) {
        mockVvns.push(createMockVvn(`VVN-${i.toString().padStart(3, '0')}`, `IMO${i}111111`));
      }

      // Plan only covers first 5 VVNs
      const operations = [
        createMockOperation('VVN-001'),
        createMockOperation('VVN-002'),
        createMockOperation('VVN-003'),
        createMockOperation('VVN-004'),
        createMockOperation('VVN-005'),
      ];

      const existingPlan = createMockPlan(targetDate, operations);

      mockRepository.findByTargetDate.mockResolvedValue(existingPlan);
      mockCoreBackendClient.getVvnsByDateRange.mockResolvedValue(mockVvns);

      const result = await service.getVvnsWithoutPlansWithAuth(targetDate);

      expect(result).toHaveLength(15); // 20 total - 5 covered = 15 missing

      // Verify all missing VVNs are from VVN-006 to VVN-020
      const missingIds = result.map((v) => v.vvnId);
      for (let i = 6; i <= 20; i++) {
        expect(missingIds).toContain(`VVN-${i.toString().padStart(3, '0')}`);
      }
    });
  });

  describe('VVN Filtering Logic', () => {
    /**
     * Test: Case-sensitive VVN ID matching
     * Business rule: VVN ID comparison must be exact
     */
    it('should perform case-sensitive VVN ID matching', async () => {
      const mockVvns = [
        createMockVvn('VVN-001', 'IMO1111111'),
        createMockVvn('vvn-002', 'IMO2222222'), // lowercase
      ];

      const operations = [
        createMockOperation('VVN-001'), // exact match
        createMockOperation('VVN-002'), // uppercase (doesn't match vvn-002)
      ];

      const existingPlan = createMockPlan(targetDate, operations);

      mockRepository.findByTargetDate.mockResolvedValue(existingPlan);
      mockCoreBackendClient.getVvnsByDateRange.mockResolvedValue(mockVvns);

      const result = await service.getVvnsWithoutPlansWithAuth(targetDate);

      // vvn-002 (lowercase) should be missing because plan has VVN-002 (uppercase)
      expect(result).toHaveLength(1);
      expect(result[0]?.vvnId).toBe('vvn-002');
    });

    /**
     * Test: Duplicate VVN IDs in VVN list (edge case)
     * Business rule: Should handle duplicate VVNs gracefully
     */
    it('should handle duplicate VVN IDs in VVN list', async () => {
      const mockVvns = [
        createMockVvn('VVN-001', 'IMO1111111'),
        createMockVvn('VVN-001', 'IMO2222222'), // Duplicate ID
        createMockVvn('VVN-002', 'IMO3333333'),
      ];

      const operations = [
        createMockOperation('VVN-001'),
      ];

      const existingPlan = createMockPlan(targetDate, operations);

      mockRepository.findByTargetDate.mockResolvedValue(existingPlan);
      mockCoreBackendClient.getVvnsByDateRange.mockResolvedValue(mockVvns);

      const result = await service.getVvnsWithoutPlansWithAuth(targetDate);

      // Only VVN-002 should be missing
      expect(result).toHaveLength(1);
      expect(result[0]?.vvnId).toBe('VVN-002');
    });
  });

  describe('Result Ordering and Metadata', () => {
    /**
     * Test: Missing VVNs preserve original order from Core Backend
     * Business rule: Result order should match VVN list order
     */
    it('should preserve VVN order from Core Backend', async () => {
      const mockVvns = [
        createMockVvn('VVN-003', 'IMO3333333'),
        createMockVvn('VVN-001', 'IMO1111111'),
        createMockVvn('VVN-002', 'IMO2222222'),
      ];

      const operations = [
        createMockOperation('VVN-002'),
      ];

      const existingPlan = createMockPlan(targetDate, operations);

      mockRepository.findByTargetDate.mockResolvedValue(existingPlan);
      mockCoreBackendClient.getVvnsByDateRange.mockResolvedValue(mockVvns);

      const result = await service.getVvnsWithoutPlansWithAuth(targetDate);

      expect(result).toHaveLength(2);
      expect(result[0]?.vvnId).toBe('VVN-003'); // First in original order
      expect(result[1]?.vvnId).toBe('VVN-001'); // Second in original order
    });

    /**
     * Test: Missing VVNs include complete metadata
     * Business rule: Each VVN should include all Core Backend metadata
     */
    it('should include complete VVN metadata in results', async () => {
      const mockVvn = createMockVvn('VVN-001', 'IMO1111111');
      // Modify VVN properties
      (mockVvn as any).loadingCount = 150;
      (mockVvn as any).unloadingCount = 200;

      mockRepository.findByTargetDate.mockResolvedValue(null);
      mockCoreBackendClient.getVvnsByDateRange.mockResolvedValue([mockVvn]);

      const result = await service.getVvnsWithoutPlansWithAuth(targetDate);

      expect(result).toHaveLength(1);
      expect(result[0]).toMatchObject({
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

  describe('Edge Cases and Error Scenarios', () => {
    /**
     * Test: Repository throws error
     * Business rule: Should propagate repository errors
     */
    it('should propagate repository errors', async () => {
      mockRepository.findByTargetDate.mockRejectedValue(
        new Error('Database connection failed')
      );

      await expect(service.getVvnsWithoutPlansWithAuth(targetDate)).rejects.toThrow(
        'Database connection failed'
      );
    });

    /**
     * Test: Core Backend client throws error
     * Business rule: Should propagate Core Backend errors
     */
    it('should propagate Core Backend errors', async () => {
      mockRepository.findByTargetDate.mockResolvedValue(null);
      mockCoreBackendClient.getVvnsByDateRange.mockRejectedValue(
        new Error('Core Backend unavailable')
      );

      await expect(service.getVvnsWithoutPlansWithAuth(targetDate)).rejects.toThrow(
        'Core Backend unavailable'
      );
    });

    /**
     * Test: Date at midnight boundary
     * Business rule: Should handle midnight dates correctly
     */
    it('should handle date at midnight correctly', async () => {
      const midnightDate = new Date('2026-01-15T00:00:00.000Z');

      mockRepository.findByTargetDate.mockResolvedValue(null);
      mockCoreBackendClient.getVvnsByDateRange.mockResolvedValue([]);

      await service.getVvnsWithoutPlansWithAuth(midnightDate);

      const callArgs = mockCoreBackendClient.getVvnsByDateRange.mock.calls[0];
      expect(callArgs).toBeDefined();
      const startOfDay = callArgs![0] as Date;

      expect(startOfDay.getTime()).toBe(midnightDate.getTime());
    });

    /**
     * Test: Date with non-zero time component
     * Business rule: Should normalize date to start/end of day regardless of input time
     */
    it('should normalize date with time component', async () => {
      const dateWithTime = new Date('2026-01-15T14:30:45.123Z');

      mockRepository.findByTargetDate.mockResolvedValue(null);
      mockCoreBackendClient.getVvnsByDateRange.mockResolvedValue([]);

      await service.getVvnsWithoutPlansWithAuth(dateWithTime);

      const callArgs = mockCoreBackendClient.getVvnsByDateRange.mock.calls[0];
      expect(callArgs).toBeDefined();
      const startOfDay = callArgs![0] as Date;
      const endOfDay = callArgs![1] as Date;

      // Should still query for entire day
      expect(startOfDay.getHours()).toBe(0);
      expect(endOfDay.getHours()).toBe(23);
    });
  });

  describe('getVvnsWithoutPlans (without auth)', () => {
    /**
     * Test: Method without auth token throws error
     * Business rule: This method requires authentication and should not be called directly
     */
    it('should throw error when called without auth', async () => {
      await expect(service.getVvnsWithoutPlans(targetDate)).rejects.toThrow(
        'getVvnsWithoutPlans requires authentication token'
      );
    });
  });
});
