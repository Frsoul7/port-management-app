import { VesselVisitExecutionService } from '../../../src/application/services/VesselVisitExecutionService';
import { IVesselVisitExecutionRepository } from '../../../src/domain/repositories/IVesselVisitExecutionRepository';
import { VesselVisitExecution } from '../../../src/domain/entities/VesselVisitExecution';
import { VesselVisitExecutionStatus } from '../../../src/shared/types';
import { IOperationPlanRepository } from '../../../src/domain/repositories/IOperationPlanRepository';

describe('US 4.1.10 - VesselVisitExecutionService.findByFilters', () => {
  let service: VesselVisitExecutionService;
  let mockVveRepository: jest.Mocked<IVesselVisitExecutionRepository>;
  let mockPlanRepository: jest.Mocked<IOperationPlanRepository>;

  /**
   * Helper to create mock VVE
   */
  const createMockVve = (
    vveId: string,
    vvnId: string,
    status: VesselVisitExecutionStatus,
    arrivalTime?: Date
  ): VesselVisitExecution => {
    const vve = new VesselVisitExecution({
      vveId,
      vvnId,
      status,
      actualPortArrivalTime: arrivalTime,
    });
    return vve;
  };

  beforeEach(() => {
    mockVveRepository = {
      findById: jest.fn(),
      findByVvnId: jest.fn(),
      findByStatus: jest.fn(),
      findAll: jest.fn(),
      update: jest.fn(),
      save: jest.fn(),
      delete: jest.fn(),
      findByFilters: jest.fn(),
    } as any;

    mockPlanRepository = {} as any;

    service = new VesselVisitExecutionService(mockVveRepository, mockPlanRepository);
  });

  describe('Filter by VVN ID', () => {
    /**
     * Test: Finds VVE by specific VVN ID
     * Business rule: Should return VVE for the specified vessel visit
     */
    it('should find VVE by VVN ID', async () => {
      const vvnId = 'VVN-001';
      const mockVve = createMockVve('VVE-001', vvnId, VesselVisitExecutionStatus.IN_PROGRESS);

      mockVveRepository.findByVvnId.mockResolvedValue(mockVve);

      const result = await service.findByFilters({ vvnId }, 0, 10, 'createdAt', 'desc');

      expect(mockVveRepository.findByVvnId).toHaveBeenCalledWith(vvnId);
      expect(result).toHaveLength(1);
      expect(result[0]!.vvnId).toBe(vvnId);
    });

    /**
     * Test: Returns empty array when VVN not found
     * Business rule: Should handle non-existent VVN gracefully
     */
    it('should return empty array when VVN ID not found', async () => {
      mockVveRepository.findByVvnId.mockResolvedValue(null);

      const result = await service.findByFilters({ vvnId: 'VVN-NONEXISTENT' }, 0, 10, 'createdAt', 'desc');

      expect(result).toHaveLength(0);
    });

    /**
     * Test: VVN filter takes precedence over other filters
     * Business rule: VVN ID is the most specific filter
     */
    it('should prioritize VVN ID filter over status', async () => {
      const vvnId = 'VVN-001';
      const mockVve = createMockVve('VVE-001', vvnId, VesselVisitExecutionStatus.IN_PROGRESS);

      mockVveRepository.findByVvnId.mockResolvedValue(mockVve);

      await service.findByFilters(
        { vvnId, status: 'COMPLETED' },
        0,
        10,
        'createdAt',
        'desc'
      );

      expect(mockVveRepository.findByVvnId).toHaveBeenCalledWith(vvnId);
      expect(mockVveRepository.findByStatus).not.toHaveBeenCalled();
    });
  });

  describe('Filter by Status', () => {
    /**
     * Test: Finds VVEs by status
     * Business rule: Should return all VVEs with matching status
     */
    it('should find VVEs by status', async () => {
      const status = VesselVisitExecutionStatus.IN_PROGRESS;
      const mockVves = [
        createMockVve('VVE-001', 'VVN-001', status),
        createMockVve('VVE-002', 'VVN-002', status),
      ];

      mockVveRepository.findByStatus.mockResolvedValue(mockVves);

      const result = await service.findByFilters({ status }, 0, 10, 'createdAt', 'desc');

      expect(mockVveRepository.findByStatus).toHaveBeenCalledWith(status, {
        page: 1,
        limit: 10,
        sortBy: 'createdAt',
        sortOrder: 'desc',
      });
      expect(result).toHaveLength(2);
      expect(result.every((vve) => vve.status === status)).toBe(true);
    });

    /**
     * Test: Handles each valid status
     * Business rule: Should support all VVE statuses
     */
    it('should handle all valid VVE statuses', async () => {
      const statuses = [
        VesselVisitExecutionStatus.PLANNED,
        VesselVisitExecutionStatus.IN_PROGRESS,
        VesselVisitExecutionStatus.COMPLETED,
        VesselVisitExecutionStatus.DISRUPTED,
      ];

      for (const status of statuses) {
        mockVveRepository.findByStatus.mockResolvedValue([
          createMockVve('VVE-001', 'VVN-001', status),
        ]);

        const result = await service.findByFilters({ status }, 0, 10, 'createdAt', 'desc');

        expect(result[0]!.status).toBe(status);
      }
    });

    /**
     * Test: Returns empty array when no VVEs match status
     * Business rule: Should handle empty results gracefully
     */
    it('should return empty array when no VVEs match status', async () => {
      mockVveRepository.findByStatus.mockResolvedValue([]);

      const result = await service.findByFilters(
        { status: VesselVisitExecutionStatus.COMPLETED },
        0,
        10,
        'createdAt',
        'desc'
      );

      expect(result).toHaveLength(0);
    });
  });

  describe('No Filters (All VVEs)', () => {
    /**
     * Test: Returns all VVEs when no filters provided
     * Business rule: Should support listing all executions
     */
    it('should return all VVEs when no filters specified', async () => {
      const mockVves = [
        createMockVve('VVE-001', 'VVN-001', VesselVisitExecutionStatus.IN_PROGRESS),
        createMockVve('VVE-002', 'VVN-002', VesselVisitExecutionStatus.COMPLETED),
        createMockVve('VVE-003', 'VVN-003', VesselVisitExecutionStatus.PLANNED),
      ];

      mockVveRepository.findAll.mockResolvedValue(mockVves);

      const result = await service.findByFilters({}, 0, 10, 'createdAt', 'desc');

      expect(mockVveRepository.findAll).toHaveBeenCalledWith({
        page: 1,
        limit: 10,
        sortBy: 'createdAt',
        sortOrder: 'desc',
      });
      expect(result).toHaveLength(3);
    });

    /**
     * Test: Handles empty database
     * Business rule: Should return empty array when no VVEs exist
     */
    it('should return empty array when no VVEs exist', async () => {
      mockVveRepository.findAll.mockResolvedValue([]);

      const result = await service.findByFilters({}, 0, 10, 'createdAt', 'desc');

      expect(result).toHaveLength(0);
    });
  });

  describe('Pagination', () => {
    /**
     * Test: Calculates correct page from skip/limit
     * Business rule: Should convert skip to page number
     */
    it('should calculate page number from skip and limit', async () => {
      mockVveRepository.findAll.mockResolvedValue([]);

      // First page (skip 0, limit 10)
      await service.findByFilters({}, 0, 10, 'createdAt', 'desc');
      expect(mockVveRepository.findAll).toHaveBeenCalledWith(
        expect.objectContaining({ page: 1, limit: 10 })
      );

      // Second page (skip 10, limit 10)
      await service.findByFilters({}, 10, 10, 'createdAt', 'desc');
      expect(mockVveRepository.findAll).toHaveBeenCalledWith(
        expect.objectContaining({ page: 2, limit: 10 })
      );

      // Third page (skip 20, limit 10)
      await service.findByFilters({}, 20, 10, 'createdAt', 'desc');
      expect(mockVveRepository.findAll).toHaveBeenCalledWith(
        expect.objectContaining({ page: 3, limit: 10 })
      );
    });

    /**
     * Test: Respects limit parameter
     * Business rule: Should control result set size
     */
    it('should respect limit parameter', async () => {
      const limits = [5, 10, 20, 50];

      for (const limit of limits) {
        mockVveRepository.findAll.mockResolvedValue([]);

        await service.findByFilters({}, 0, limit, 'createdAt', 'desc');

        expect(mockVveRepository.findAll).toHaveBeenCalledWith(
          expect.objectContaining({ limit })
        );
      }
    });

    /**
     * Test: Handles non-standard skip values
     * Business rule: Should handle any skip value correctly
     */
    it('should handle non-standard skip values', async () => {
      mockVveRepository.findAll.mockResolvedValue([]);

      // skip 15, limit 10 → page 2 (floor(15/10) + 1 = 2)
      await service.findByFilters({}, 15, 10, 'createdAt', 'desc');
      expect(mockVveRepository.findAll).toHaveBeenCalledWith(
        expect.objectContaining({ page: 2 })
      );

      // skip 7, limit 5 → page 2 (floor(7/5) + 1 = 2)
      await service.findByFilters({}, 7, 5, 'createdAt', 'desc');
      expect(mockVveRepository.findAll).toHaveBeenCalledWith(
        expect.objectContaining({ page: 2 })
      );
    });
  });

  describe('Sorting', () => {
    /**
     * Test: Passes sort parameters to repository
     * Business rule: Should support sorting by specified field
     */
    it('should pass sortBy parameter to repository', async () => {
      mockVveRepository.findAll.mockResolvedValue([]);

      const sortFields = ['createdAt', 'actualPortArrivalTime', 'status', 'vvnId'];

      for (const sortBy of sortFields) {
        await service.findByFilters({}, 0, 10, sortBy, 'desc');

        expect(mockVveRepository.findAll).toHaveBeenCalledWith(
          expect.objectContaining({ sortBy })
        );
      }
    });

    /**
     * Test: Handles ascending sort order
     * Business rule: Should support asc sort direction
     */
    it('should handle ascending sort order', async () => {
      mockVveRepository.findAll.mockResolvedValue([]);

      await service.findByFilters({}, 0, 10, 'createdAt', 'asc');

      expect(mockVveRepository.findAll).toHaveBeenCalledWith(
        expect.objectContaining({ sortOrder: 'asc' })
      );
    });

    /**
     * Test: Handles descending sort order
     * Business rule: Should support desc sort direction
     */
    it('should handle descending sort order', async () => {
      mockVveRepository.findAll.mockResolvedValue([]);

      await service.findByFilters({}, 0, 10, 'createdAt', 'desc');

      expect(mockVveRepository.findAll).toHaveBeenCalledWith(
        expect.objectContaining({ sortOrder: 'desc' })
      );
    });

    /**
     * Test: Defaults to desc for invalid sort order
     * Business rule: Should default to descending if order invalid
     */
    it('should default to desc for invalid sort order', async () => {
      mockVveRepository.findAll.mockResolvedValue([]);

      await service.findByFilters({}, 0, 10, 'createdAt', 'invalid' as any);

      expect(mockVveRepository.findAll).toHaveBeenCalledWith(
        expect.objectContaining({ sortOrder: 'desc' })
      );
    });
  });

  describe('DTO Conversion', () => {
    /**
     * Test: Converts entities to DTOs
     * Business rule: Should return DTOs, not domain entities
     */
    it('should convert VVE entities to DTOs', async () => {
      const mockVves = [
        createMockVve('VVE-001', 'VVN-001', VesselVisitExecutionStatus.IN_PROGRESS),
      ];

      mockVveRepository.findAll.mockResolvedValue(mockVves);

      const result = await service.findByFilters({}, 0, 10, 'createdAt', 'desc');

      expect(result[0]).toHaveProperty('vveId');
      expect(result[0]).toHaveProperty('vvnId');
      expect(result[0]).toHaveProperty('status');
    });

    /**
     * Test: DTO Conversion - should return DTOs with all required fields
     * Business rule: DTOs should contain all VVE fields (metrics are now accessed via separate endpoint)
     */
    it('should return DTOs with all required fields', async () => {
      const arrivalTime = new Date('2026-01-15T06:00:00Z');
      const mockVve = createMockVve('VVE-001', 'VVN-001', VesselVisitExecutionStatus.IN_PROGRESS, arrivalTime);
      mockVve.recordBerthing(new Date('2026-01-15T08:00:00Z'), 'DOCK-A');

      mockVveRepository.findAll.mockResolvedValue([mockVve]);

      const result = await service.findByFilters({}, 0, 10, 'createdAt', 'desc');

      expect(result[0]).toHaveProperty('vveId');
      expect(result[0]).toHaveProperty('vvnId');
      expect(result[0]).toHaveProperty('status');
      expect(result[0]).toHaveProperty('operations');
      expect(result[0]).toHaveProperty('incidents');
    });
  });

  describe('Count by Filters', () => {
    /**
     * Test: Counts all VVEs matching filter
     * Business rule: Should return total count for pagination
     */
    it('should count VVEs by filter', async () => {
      const mockVves = [
        createMockVve('VVE-001', 'VVN-001', VesselVisitExecutionStatus.IN_PROGRESS),
        createMockVve('VVE-002', 'VVN-002', VesselVisitExecutionStatus.IN_PROGRESS),
        createMockVve('VVE-003', 'VVN-003', VesselVisitExecutionStatus.IN_PROGRESS),
      ];

      mockVveRepository.findByStatus.mockResolvedValue(mockVves);

      const count = await service.countByFilters({ status: VesselVisitExecutionStatus.IN_PROGRESS });

      expect(count).toBe(3);
    });

    /**
     * Test: Returns zero for empty results
     * Business rule: Should return 0 when no VVEs match
     */
    it('should return zero count for empty results', async () => {
      mockVveRepository.findAll.mockResolvedValue([]);

      const count = await service.countByFilters({});

      expect(count).toBe(0);
    });
  });

  describe('Error Handling', () => {
    /**
     * Test: Propagates repository errors
     * Business rule: Repository errors should bubble up
     */
    it('should propagate repository errors', async () => {
      const error = new Error('Database connection failed');
      mockVveRepository.findAll.mockRejectedValue(error);

      await expect(
        service.findByFilters({}, 0, 10, 'createdAt', 'desc')
      ).rejects.toThrow('Database connection failed');
    });

    /**
     * Test: Handles repository errors during status filter
     * Business rule: Should handle errors gracefully
     */
    it('should handle errors during status filtering', async () => {
      mockVveRepository.findByStatus.mockRejectedValue(new Error('Query failed'));

      await expect(
        service.findByFilters({ status: VesselVisitExecutionStatus.COMPLETED }, 0, 10, 'createdAt', 'desc')
      ).rejects.toThrow('Query failed');
    });

    /**
     * Test: Handles repository errors during VVN lookup
     * Business rule: Should handle lookup failures
     */
    it('should handle errors during VVN ID lookup', async () => {
      mockVveRepository.findByVvnId.mockRejectedValue(new Error('VVN lookup failed'));

      await expect(
        service.findByFilters({ vvnId: 'VVN-001' }, 0, 10, 'createdAt', 'desc')
      ).rejects.toThrow('VVN lookup failed');
    });
  });
});
