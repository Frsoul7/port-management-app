import { OperationPlanService } from '@application/services/OperationPlanService';
import { IOperationPlanRepository } from '@domain/repositories/IOperationPlanRepository';
import { OperationPlan } from '@domain/entities/OperationPlan';
import { PlannedOperation } from '@domain/value-objects/PlannedOperation';
import { OperationPlanStatus, PlanningAlgorithm, OperationType } from '@shared/types';

/**
 * Unit Tests for OperationPlanService - findByFilters method
 * US 4.1.3: Search and List Operation Plans with Filtering
 * 
 * Test Coverage:
 * - Filter by VVN ID (vessel visit notification identifier)
 * - Filter by status (GENERATED, APPROVED, IN_EXECUTION, COMPLETED)
 * - Filter by algorithm (OPTIMAL, WEIGHTED, MULTI_CRANES)
 * - Filter by target date (single date)
 * - Filter by date range (fromDate/toDate)
 * - Pagination (skip, limit)
 * - Sorting (sortBy, sortOrder)
 * - Combined filters (multiple criteria)
 * - Empty results handling
 */
describe('US 4.1.3 - OperationPlanService.findByFilters', () => {
  let service: OperationPlanService;
  let mockRepository: jest.Mocked<IOperationPlanRepository>;

  const futureDate = new Date('2026-01-15');
  const futureDate2 = new Date('2026-01-20');
  const futureDate3 = new Date('2026-01-25');

  /**
   * Create a sample operation plan for testing
   */
  const createMockPlan = (
    id: string,
    targetDate: Date,
    status: OperationPlanStatus,
    algorithm: PlanningAlgorithm,
    operations: PlannedOperation[]
  ): OperationPlan => {
    return new OperationPlan({
      operationPlanId: id,
      targetDate,
      status,
      algorithm,
      createdBy: 'user-001',
      operations,
      totalDelay: 0,
    });
  };

  /**
   * Create a sample planned operation
   */
  const createMockOperation = (
    vvnId: string,
    vesselImo: string = 'IMO1234567'
  ): PlannedOperation => {
    return new PlannedOperation({
      vvnId,
      vesselImo,
      operationType: OperationType.UNLOAD,
      plannedStart: new Date(futureDate.getTime() + 8 * 60 * 60 * 1000),
      plannedEnd: new Date(futureDate.getTime() + 12 * 60 * 60 * 1000),
      assignedCranes: 2,
    });
  };

  beforeEach(() => {
    // Create mock repository
    mockRepository = {
      findAll: jest.fn(),
      findById: jest.fn(),
      findByTargetDate: jest.fn(),
      findByStatus: jest.fn(),
      findByDateRange: jest.fn(),
      save: jest.fn(),
      update: jest.fn(),
      delete: jest.fn(),
    } as any;

    // Create service instance
    service = new OperationPlanService(mockRepository, {} as any, {} as any);
  });

  describe('Filter by VVN ID', () => {
    /**
     * Test: Can find plans containing a specific VVN ID
     * Business rule: Search by vessel visit notification identifier
     */
    it('should find plans containing specific VVN ID', async () => {
      const plan1 = createMockPlan(
        'PLAN-001',
        futureDate,
        OperationPlanStatus.GENERATED,
        PlanningAlgorithm.OPTIMAL,
        [createMockOperation('VVN-001'), createMockOperation('VVN-002')]
      );

      const plan2 = createMockPlan(
        'PLAN-002',
        futureDate2,
        OperationPlanStatus.APPROVED,
        PlanningAlgorithm.WEIGHTED,
        [createMockOperation('VVN-003'), createMockOperation('VVN-004')]
      );

      const plan3 = createMockPlan(
        'PLAN-003',
        futureDate3,
        OperationPlanStatus.GENERATED,
        PlanningAlgorithm.OPTIMAL,
        [createMockOperation('VVN-001'), createMockOperation('VVN-005')]
      );

      mockRepository.findAll.mockResolvedValue([plan1, plan2, plan3]);

      const result = await service.findByFilters({ vvnId: 'VVN-001' }, 0, 10, 'targetDate', 'desc');

      expect(result).toHaveLength(2);
      expect(result[0]!.operationPlanId).toBe('PLAN-001');
      expect(result[1]!.operationPlanId).toBe('PLAN-003');
    });

    /**
     * Test: Returns empty array when VVN ID not found
     * Business rule: Graceful handling of no results
     */
    it('should return empty array when VVN ID not found', async () => {
      const plan1 = createMockPlan(
        'PLAN-001',
        futureDate,
        OperationPlanStatus.GENERATED,
        PlanningAlgorithm.OPTIMAL,
        [createMockOperation('VVN-001')]
      );

      mockRepository.findAll.mockResolvedValue([plan1]);

      const result = await service.findByFilters({ vvnId: 'VVN-999' }, 0, 10, 'targetDate', 'desc');

      expect(result).toHaveLength(0);
    });

    /**
     * Test: VVN ID filter combined with status filter
     * Business rule: Support multiple filter criteria
     */
    it('should filter by VVN ID and status', async () => {
      const plan1 = createMockPlan(
        'PLAN-001',
        futureDate,
        OperationPlanStatus.GENERATED,
        PlanningAlgorithm.OPTIMAL,
        [createMockOperation('VVN-001')]
      );

      const plan2 = createMockPlan(
        'PLAN-002',
        futureDate2,
        OperationPlanStatus.APPROVED,
        PlanningAlgorithm.OPTIMAL,
        [createMockOperation('VVN-001')]
      );

      mockRepository.findAll.mockResolvedValue([plan1, plan2]);

      const result = await service.findByFilters(
        { vvnId: 'VVN-001', status: OperationPlanStatus.APPROVED },
        0,
        10,
        'targetDate',
        'desc'
      );

      expect(result).toHaveLength(1);
      expect(result[0]!.status).toBe(OperationPlanStatus.APPROVED);
    });

    /**
     * Test: VVN ID filter combined with algorithm filter
     * Business rule: Support multiple filter criteria
     */
    it('should filter by VVN ID and algorithm', async () => {
      const plan1 = createMockPlan(
        'PLAN-001',
        futureDate,
        OperationPlanStatus.GENERATED,
        PlanningAlgorithm.OPTIMAL,
        [createMockOperation('VVN-001')]
      );

      const plan2 = createMockPlan(
        'PLAN-002',
        futureDate2,
        OperationPlanStatus.GENERATED,
        PlanningAlgorithm.WEIGHTED,
        [createMockOperation('VVN-001')]
      );

      mockRepository.findAll.mockResolvedValue([plan1, plan2]);

      const result = await service.findByFilters(
        { vvnId: 'VVN-001', algorithm: PlanningAlgorithm.WEIGHTED },
        0,
        10,
        'targetDate',
        'desc'
      );

      expect(result).toHaveLength(1);
      expect(result[0]!.algorithm).toBe(PlanningAlgorithm.WEIGHTED);
    });
  });

  describe('Filter by Status', () => {
    /**
     * Test: Can find plans by status
     * Business rule: Search by plan lifecycle status
     */
    it('should find plans by status', async () => {
      const plan2 = createMockPlan(
        'PLAN-002',
        futureDate2,
        OperationPlanStatus.APPROVED,
        PlanningAlgorithm.OPTIMAL,
        [createMockOperation('VVN-002')]
      );

      mockRepository.findByStatus.mockResolvedValue([plan2]);

      const result = await service.findByFilters(
        { status: OperationPlanStatus.APPROVED },
        0,
        10,
        'targetDate',
        'desc'
      );

      expect(result).toHaveLength(1);
      expect(result[0]!.status).toBe(OperationPlanStatus.APPROVED);
      expect(mockRepository.findByStatus).toHaveBeenCalledWith(
        OperationPlanStatus.APPROVED,
        expect.any(Object)
      );
    });

    /**
     * Test: Status filter combined with algorithm filter
     * Business rule: Support multiple filter criteria
     */
    it('should filter by status and algorithm', async () => {
      const plan1 = createMockPlan(
        'PLAN-001',
        futureDate,
        OperationPlanStatus.APPROVED,
        PlanningAlgorithm.OPTIMAL,
        [createMockOperation('VVN-001')]
      );

      const plan2 = createMockPlan(
        'PLAN-002',
        futureDate2,
        OperationPlanStatus.APPROVED,
        PlanningAlgorithm.WEIGHTED,
        [createMockOperation('VVN-002')]
      );

      mockRepository.findByStatus.mockResolvedValue([plan1, plan2]);

      const result = await service.findByFilters(
        { status: OperationPlanStatus.APPROVED, algorithm: PlanningAlgorithm.WEIGHTED },
        0,
        10,
        'targetDate',
        'desc'
      );

      expect(result).toHaveLength(1);
      expect(result[0]!.algorithm).toBe(PlanningAlgorithm.WEIGHTED);
    });
  });

  describe('Filter by Algorithm', () => {
    /**
     * Test: Can find plans by algorithm
     * Business rule: Search by planning algorithm used
     */
    it('should find plans by algorithm', async () => {
      const plan1 = createMockPlan(
        'PLAN-001',
        futureDate,
        OperationPlanStatus.GENERATED,
        PlanningAlgorithm.OPTIMAL,
        [createMockOperation('VVN-001')]
      );

      const plan2 = createMockPlan(
        'PLAN-002',
        futureDate2,
        OperationPlanStatus.GENERATED,
        PlanningAlgorithm.WEIGHTED,
        [createMockOperation('VVN-002')]
      );

      mockRepository.findAll.mockResolvedValue([plan1, plan2]);

      const result = await service.findByFilters(
        { algorithm: PlanningAlgorithm.WEIGHTED },
        0,
        10,
        'targetDate',
        'desc'
      );

      expect(result).toHaveLength(1);
      expect(result[0]!.algorithm).toBe(PlanningAlgorithm.WEIGHTED);
    });
  });

  describe('Filter by Target Date', () => {
    /**
     * Test: Can find plan by specific target date
     * Business rule: Search by exact operation date
     */
    it('should find plan by specific target date', async () => {
      const plan = createMockPlan(
        'PLAN-001',
        futureDate,
        OperationPlanStatus.GENERATED,
        PlanningAlgorithm.OPTIMAL,
        [createMockOperation('VVN-001')]
      );

      mockRepository.findByTargetDate.mockResolvedValue(plan);

      const result = await service.findByFilters(
        { targetDate: futureDate },
        0,
        10,
        'targetDate',
        'desc'
      );

      expect(result).toHaveLength(1);
      // DTO converts targetDate to ISO string format
      expect(result[0]!.targetDate).toBe(futureDate.toISOString().split('T')[0]);
      expect(mockRepository.findByTargetDate).toHaveBeenCalledWith(futureDate);
    });

    /**
     * Test: Returns empty array when date has no plans
     * Business rule: Graceful handling of no results
     */
    it('should return empty array when date has no plans', async () => {
      mockRepository.findByTargetDate.mockResolvedValue(null);

      const result = await service.findByFilters(
        { targetDate: new Date('2026-12-31') },
        0,
        10,
        'targetDate',
        'desc'
      );

      expect(result).toHaveLength(0);
    });
  });

  describe('Filter by Date Range', () => {
    /**
     * Test: Can find plans within date range
     * Business rule: Search by date interval
     */
    it('should find plans within date range', async () => {
      const plan1 = createMockPlan(
        'PLAN-001',
        futureDate,
        OperationPlanStatus.GENERATED,
        PlanningAlgorithm.OPTIMAL,
        [createMockOperation('VVN-001')]
      );

      const plan2 = createMockPlan(
        'PLAN-002',
        futureDate2,
        OperationPlanStatus.APPROVED,
        PlanningAlgorithm.OPTIMAL,
        [createMockOperation('VVN-002')]
      );

      mockRepository.findByDateRange.mockResolvedValue([plan1, plan2]);

      const fromDate = new Date('2026-01-10');
      const toDate = new Date('2026-01-25');

      const result = await service.findByFilters(
        { targetDate: { $gte: fromDate, $lte: toDate } },
        0,
        10,
        'targetDate',
        'desc'
      );

      expect(result).toHaveLength(2);
      expect(mockRepository.findByDateRange).toHaveBeenCalledWith(
        fromDate,
        toDate,
        expect.any(Object)
      );
    });

    /**
     * Test: Date range with only fromDate
     * Business rule: Open-ended date range (from date onwards)
     */
    it('should find plans from date onwards when only fromDate provided', async () => {
      const plan = createMockPlan(
        'PLAN-001',
        futureDate2,
        OperationPlanStatus.GENERATED,
        PlanningAlgorithm.OPTIMAL,
        [createMockOperation('VVN-001')]
      );

      mockRepository.findByDateRange.mockResolvedValue([plan]);

      const fromDate = new Date('2026-01-10');

      const result = await service.findByFilters(
        { targetDate: { $gte: fromDate } },
        0,
        10,
        'targetDate',
        'desc'
      );

      expect(result).toHaveLength(1);
      expect(mockRepository.findByDateRange).toHaveBeenCalledWith(
        fromDate,
        expect.any(Date), // toDate defaults to far future
        expect.any(Object)
      );
    });

    /**
     * Test: Date range with only toDate
     * Business rule: Open-ended date range (up to date)
     */
    it('should find plans up to date when only toDate provided', async () => {
      const plan = createMockPlan(
        'PLAN-001',
        futureDate,
        OperationPlanStatus.GENERATED,
        PlanningAlgorithm.OPTIMAL,
        [createMockOperation('VVN-001')]
      );

      mockRepository.findByDateRange.mockResolvedValue([plan]);

      const toDate = new Date('2026-01-20');

      const result = await service.findByFilters(
        { targetDate: { $lte: toDate } },
        0,
        10,
        'targetDate',
        'desc'
      );

      expect(result).toHaveLength(1);
      expect(mockRepository.findByDateRange).toHaveBeenCalledWith(
        expect.any(Date), // fromDate defaults to epoch
        toDate,
        expect.any(Object)
      );
    });

    /**
     * Test: Date range combined with status filter
     * Business rule: Support multiple filter criteria
     */
    it('should filter by date range and status', async () => {
      const plan1 = createMockPlan(
        'PLAN-001',
        futureDate,
        OperationPlanStatus.GENERATED,
        PlanningAlgorithm.OPTIMAL,
        [createMockOperation('VVN-001')]
      );

      const plan2 = createMockPlan(
        'PLAN-002',
        futureDate2,
        OperationPlanStatus.APPROVED,
        PlanningAlgorithm.OPTIMAL,
        [createMockOperation('VVN-002')]
      );

      mockRepository.findByDateRange.mockResolvedValue([plan1, plan2]);

      const fromDate = new Date('2026-01-10');
      const toDate = new Date('2026-01-25');

      const result = await service.findByFilters(
        { targetDate: { $gte: fromDate, $lte: toDate }, status: OperationPlanStatus.APPROVED },
        0,
        10,
        'targetDate',
        'desc'
      );

      expect(result).toHaveLength(1);
      expect(result[0]!.status).toBe(OperationPlanStatus.APPROVED);
    });
  });

  describe('Pagination', () => {
    /**
     * Test: Respects skip and limit parameters
     * Business rule: Support paginated results
     */
    it('should respect pagination parameters', async () => {
      const plans = Array.from({ length: 25 }, (_, i) =>
        createMockPlan(
          `PLAN-${i + 1}`,
          futureDate,
          OperationPlanStatus.GENERATED,
          PlanningAlgorithm.OPTIMAL,
          [createMockOperation(`VVN-${i + 1}`)]
        )
      );

      mockRepository.findAll.mockResolvedValue(plans);

      // Get page 2 with 10 items per page
      await service.findByFilters({}, 10, 10, 'targetDate', 'desc');

      expect(mockRepository.findAll).toHaveBeenCalledWith(
        expect.objectContaining({
          page: 2, // skip 10 / limit 10 + 1 = page 2
          limit: 10,
        })
      );
    });
  });

  describe('Sorting', () => {
    /**
     * Test: Passes sorting parameters to repository
     * Business rule: Support custom sorting
     */
    it('should pass sorting parameters to repository', async () => {
      mockRepository.findAll.mockResolvedValue([]);

      await service.findByFilters({}, 0, 10, 'createdAt', 'asc');

      expect(mockRepository.findAll).toHaveBeenCalledWith(
        expect.objectContaining({
          sortBy: 'createdAt',
          sortOrder: 'asc',
        })
      );
    });

    /**
     * Test: Defaults to descending sort order
     * Business rule: Latest plans first by default
     */
    it('should default to descending sort order', async () => {
      mockRepository.findAll.mockResolvedValue([]);

      await service.findByFilters({}, 0, 10, 'targetDate', 'invalid');

      expect(mockRepository.findAll).toHaveBeenCalledWith(
        expect.objectContaining({
          sortOrder: 'desc',
        })
      );
    });
  });

  describe('No Filters', () => {
    /**
     * Test: Returns all plans when no filters provided
     * Business rule: Default behavior lists all plans
     */
    it('should return all plans when no filters provided', async () => {
      const plans = [
        createMockPlan(
          'PLAN-001',
          futureDate,
          OperationPlanStatus.GENERATED,
          PlanningAlgorithm.OPTIMAL,
          [createMockOperation('VVN-001')]
        ),
        createMockPlan(
          'PLAN-002',
          futureDate2,
          OperationPlanStatus.APPROVED,
          PlanningAlgorithm.WEIGHTED,
          [createMockOperation('VVN-002')]
        ),
      ];

      mockRepository.findAll.mockResolvedValue(plans);

      const result = await service.findByFilters({}, 0, 10, 'targetDate', 'desc');

      expect(result).toHaveLength(2);
      expect(mockRepository.findAll).toHaveBeenCalled();
    });
  });

  describe('DTO Conversion', () => {
    /**
     * Test: Returns DTOs not domain entities
     * Business rule: API returns data transfer objects
     */
    it('should convert domain entities to DTOs', async () => {
      const plan = createMockPlan(
        'PLAN-001',
        futureDate,
        OperationPlanStatus.GENERATED,
        PlanningAlgorithm.OPTIMAL,
        [createMockOperation('VVN-001')]
      );

      mockRepository.findAll.mockResolvedValue([plan]);

      const result = await service.findByFilters({}, 0, 10, 'targetDate', 'desc');

      expect(result[0]).toHaveProperty('operationPlanId');
      expect(result[0]).toHaveProperty('targetDate');
      expect(result[0]).toHaveProperty('status');
      expect(result[0]).toHaveProperty('algorithm');
      expect(result[0]).toHaveProperty('operations');
    });
  });

  describe('countByFilters', () => {
    /**
     * Test: Returns correct count for filtered results
     * Business rule: Support pagination with total count
     */
    it('should return count of filtered plans', async () => {
      const plans = Array.from({ length: 5 }, (_, i) =>
        createMockPlan(
          `PLAN-${i + 1}`,
          futureDate,
          OperationPlanStatus.GENERATED,
          PlanningAlgorithm.OPTIMAL,
          [createMockOperation(`VVN-${i + 1}`)]
        )
      );

      mockRepository.findAll.mockResolvedValue(plans);

      const count = await service.countByFilters({});

      expect(count).toBe(5);
    });

    /**
     * Test: Count respects filters
     * Business rule: Count only matching records
     */
    it('should count only matching records', async () => {
      const plans = [
        createMockPlan(
          'PLAN-001',
          futureDate,
          OperationPlanStatus.APPROVED,
          PlanningAlgorithm.OPTIMAL,
          [createMockOperation('VVN-001')]
        ),
      ];

      mockRepository.findByStatus.mockResolvedValue(plans);

      const count = await service.countByFilters({ status: OperationPlanStatus.APPROVED });

      expect(count).toBe(1);
    });
  });
});
