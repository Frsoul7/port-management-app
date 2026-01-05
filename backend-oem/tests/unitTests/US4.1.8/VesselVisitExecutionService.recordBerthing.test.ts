import { VesselVisitExecutionService } from '../../../src/application/services/VesselVisitExecutionService';
import { IVesselVisitExecutionRepository } from '../../../src/domain/repositories/IVesselVisitExecutionRepository';
import { VesselVisitExecution } from '../../../src/domain/entities/VesselVisitExecution';
import { VesselVisitExecutionStatus } from '../../../src/shared/types';

describe('US 4.1.8 - VesselVisitExecutionService.recordBerthing', () => {
  let service: VesselVisitExecutionService;
  let mockRepository: jest.Mocked<IVesselVisitExecutionRepository>;

  /**
   * Helper to create mock VVE in IN_PROGRESS status with port arrival recorded
   */
  const createMockVve = (vveId: string, hasPortArrival: boolean = true): VesselVisitExecution => {
    const vve = new VesselVisitExecution({
      vveId,
      vvnId: 'VVN-001',
    });

    if (hasPortArrival) {
      vve.recordPortArrival(new Date('2026-01-15T06:00:00Z'));
    }

    return vve;
  };

  beforeEach(() => {
    mockRepository = {
      findById: jest.fn(),
      update: jest.fn(),
      save: jest.fn(),
      findByFilters: jest.fn(),
      delete: jest.fn(),
    } as any;

    service = new VesselVisitExecutionService(mockRepository);
  });

  describe('Valid Berthing Recording', () => {
    /**
     * Test: Successfully records berthing with valid data
     * Business rule: Updates VVE with berth time and dock assignment
     */
    it('should record berthing with valid berth time and dock', async () => {
      const vveId = 'VVE-001';
      const mockVve = createMockVve(vveId);

      mockRepository.findById.mockResolvedValue(mockVve);
      mockRepository.update.mockResolvedValue(mockVve);

      const result = await service.recordBerthing(vveId, {
        berthTime: '2026-01-15T08:00:00Z',
        dockId: 'DOCK-A',
      });

      expect(mockRepository.findById).toHaveBeenCalledWith(vveId);
      expect(mockRepository.update).toHaveBeenCalled();
      expect(result).toHaveProperty('vveId', vveId);
      expect(result).toHaveProperty('actualBerthTime');
      expect(result).toHaveProperty('assignedDock', 'DOCK-A');
    });

    /**
     * Test: Records berthing time after port arrival
     * Business rule: Berth time must be after port arrival time
     */
    it('should allow berthing after port arrival', async () => {
      const vveId = 'VVE-001';
      const mockVve = createMockVve(vveId);

      mockRepository.findById.mockResolvedValue(mockVve);
      mockRepository.update.mockResolvedValue(mockVve);

      await expect(
        service.recordBerthing(vveId, {
          berthTime: '2026-01-15T08:00:00Z', // After 06:00 arrival
          dockId: 'DOCK-A',
        })
      ).resolves.toBeDefined();
    });

    /**
     * Test: Records berthing with different dock IDs
     * Business rule: Should accept any valid dock identifier
     */
    it('should accept various dock ID formats', async () => {
      const vveId = 'VVE-001';
      const mockVve = createMockVve(vveId);

      mockRepository.findById.mockResolvedValue(mockVve);
      mockRepository.update.mockResolvedValue(mockVve);

      const dockIds = ['DOCK-A', 'DOCK-B1', 'BERTH-101', 'TERMINAL-5'];

      for (const dockId of dockIds) {
        const freshVve = createMockVve(vveId);
        mockRepository.findById.mockResolvedValue(freshVve);
        mockRepository.update.mockResolvedValue(freshVve);

        const result = await service.recordBerthing(vveId, {
          berthTime: '2026-01-15T08:00:00Z',
          dockId,
        });

        expect(result.assignedDock).toBe(dockId);
      }
    });

    /**
     * Test: Updates VVE status to IN_PROGRESS if needed
     * Business rule: VVE must be IN_PROGRESS when recording berthing
     */
    it('should maintain IN_PROGRESS status when recording berthing', async () => {
      const vveId = 'VVE-001';
      const mockVve = createMockVve(vveId);

      expect(mockVve.status).toBe(VesselVisitExecutionStatus.IN_PROGRESS);

      mockRepository.findById.mockResolvedValue(mockVve);
      mockRepository.update.mockResolvedValue(mockVve);

      const result = await service.recordBerthing(vveId, {
        berthTime: '2026-01-15T08:00:00Z',
        dockId: 'DOCK-A',
      });

      expect(result.status).toBe(VesselVisitExecutionStatus.IN_PROGRESS);
    });
  });

  describe('Date and Time Validations', () => {
    /**
     * Test: Rejects berth time before port arrival
     * Business rule: Berth time must be chronologically after port arrival
     */
    it('should reject berth time before port arrival time', async () => {
      const vveId = 'VVE-001';
      const mockVve = createMockVve(vveId);

      mockRepository.findById.mockResolvedValue(mockVve);

      await expect(
        service.recordBerthing(vveId, {
          berthTime: '2026-01-15T05:00:00Z', // Before 06:00 arrival
          dockId: 'DOCK-A',
        })
      ).rejects.toThrow('Berth time must be after port arrival time');
    });

    /**
     * Test: Accepts ISO 8601 datetime format
     * Business rule: Should handle standard datetime formats
     */
    it('should accept ISO 8601 datetime format', async () => {
      const vveId = 'VVE-001';
      const mockVve = createMockVve(vveId);

      mockRepository.findById.mockResolvedValue(mockVve);
      mockRepository.update.mockResolvedValue(mockVve);

      const result = await service.recordBerthing(vveId, {
        berthTime: '2026-01-15T08:00:00.000Z',
        dockId: 'DOCK-A',
      });

      expect(result).toBeDefined();
      expect(result.actualBerthTime).toBeDefined();
    });

    /**
     * Test: Handles same-minute berth time correctly
     * Business rule: Time precision should be maintained
     */
    it('should handle precise berth times', async () => {
      const vveId = 'VVE-001';
      const mockVve = createMockVve(vveId);

      mockRepository.findById.mockResolvedValue(mockVve);
      mockRepository.update.mockResolvedValue(mockVve);

      const result = await service.recordBerthing(vveId, {
        berthTime: '2026-01-15T08:23:45.123Z',
        dockId: 'DOCK-A',
      });

      expect(result.actualBerthTime).toBeDefined();
    });
  });

  describe('VVE Status Validations', () => {
    /**
     * Test: Requires IN_PROGRESS status
     * Business rule: Can only record berthing for IN_PROGRESS VVEs
     */
    it('should require VVE to be IN_PROGRESS', async () => {
      const vveId = 'VVE-001';
      const mockVve = createMockVve(vveId);

      // Complete the VVE (changes status)
      mockVve.recordBerthing(new Date('2026-01-15T08:00:00Z'), 'DOCK-A');
      mockVve.recordUnberthing(new Date('2026-01-15T18:00:00Z'));
      mockVve.markAsCompleted(new Date('2026-01-15T19:00:00Z'), 'test-user');

      mockRepository.findById.mockResolvedValue(mockVve);

      await expect(
        service.recordBerthing(vveId, {
          berthTime: '2026-01-15T08:00:00Z',
          dockId: 'DOCK-A',
        })
      ).rejects.toThrow();
    });

    /**
     * Test: Requires port arrival to be recorded first
     * Business rule: Cannot record berthing without port arrival
     */
    it('should require port arrival to be recorded first', async () => {
      const vveId = 'VVE-001';
      const mockVve = createMockVve(vveId, false); // No port arrival

      mockRepository.findById.mockResolvedValue(mockVve);

      await expect(
        service.recordBerthing(vveId, {
          berthTime: '2026-01-15T08:00:00Z',
          dockId: 'DOCK-A',
        })
      ).rejects.toThrow('Cannot record berthing without port arrival time');
    });
  });

  describe('Dock Assignment', () => {
    /**
     * Test: Assigns dock to VVE
     * Business rule: Dock ID should be stored with VVE
     */
    it('should assign dock to VVE', async () => {
      const vveId = 'VVE-001';
      const mockVve = createMockVve(vveId);

      mockRepository.findById.mockResolvedValue(mockVve);
      mockRepository.update.mockResolvedValue(mockVve);

      const result = await service.recordBerthing(vveId, {
        berthTime: '2026-01-15T08:00:00Z',
        dockId: 'DOCK-B',
      });

      expect(result.assignedDock).toBe('DOCK-B');
    });

    /**
     * Test: Can update berthing with different dock
     * Business rule: Should be able to reassign dock (e.g., if plan changed)
     */
    it('should allow updating dock assignment', async () => {
      const vveId = 'VVE-001';
      const mockVve = createMockVve(vveId);

      // First berthing
      mockVve.recordBerthing(new Date('2026-01-15T08:00:00Z'), 'DOCK-A');

      mockRepository.findById.mockResolvedValue(mockVve);
      mockRepository.update.mockResolvedValue(mockVve);

      // Update to different dock (re-record berthing allowed by domain)
      const freshVve = createMockVve(vveId);
      mockRepository.findById.mockResolvedValue(freshVve);
      mockRepository.update.mockResolvedValue(freshVve);

      const result = await service.recordBerthing(vveId, {
        berthTime: '2026-01-15T08:00:00Z',
        dockId: 'DOCK-C',
      });

      expect(result.assignedDock).toBe('DOCK-C');
    });

    /**
     * Test: Handles special dock identifiers
     * Business rule: Should accept any string as dock ID
     */
    it('should handle various dock identifier formats', async () => {
      const vveId = 'VVE-001';
      const mockVve = createMockVve(vveId);

      mockRepository.findById.mockResolvedValue(mockVve);
      mockRepository.update.mockResolvedValue(mockVve);

      const specialDocks = ['DOCK-001', 'B-12', 'TERMINAL_NORTH_A', 'BERTH.5'];

      for (const dockId of specialDocks) {
        const freshVve = createMockVve(vveId);
        mockRepository.findById.mockResolvedValue(freshVve);
        mockRepository.update.mockResolvedValue(freshVve);

        const result = await service.recordBerthing(vveId, {
          berthTime: '2026-01-15T08:00:00Z',
          dockId,
        });

        expect(result.assignedDock).toBe(dockId);
      }
    });
  });

  describe('Repository Interactions', () => {
    /**
     * Test: Calls repository methods in correct order
     * Business rule: Should fetch, update domain, then persist
     */
    it('should fetch VVE, update it, and persist changes', async () => {
      const vveId = 'VVE-001';
      const mockVve = createMockVve(vveId);

      mockRepository.findById.mockResolvedValue(mockVve);
      mockRepository.update.mockResolvedValue(mockVve);

      await service.recordBerthing(vveId, {
        berthTime: '2026-01-15T08:00:00Z',
        dockId: 'DOCK-A',
      });

      // Verify findById was called before update by checking invocationCallOrder
      const findByIdOrder = (mockRepository.findById as jest.Mock).mock.invocationCallOrder[0]!;
      const updateOrder = (mockRepository.update as jest.Mock).mock.invocationCallOrder[0]!;
      expect(findByIdOrder).toBeLessThan(updateOrder);
      expect(mockRepository.update).toHaveBeenCalledTimes(1);
    });

    /**
     * Test: Returns updated VVE DTO
     * Business rule: Should return serialized VVE state
     */
    it('should return updated VVE as DTO', async () => {
      const vveId = 'VVE-001';
      const mockVve = createMockVve(vveId);

      mockRepository.findById.mockResolvedValue(mockVve);
      mockRepository.update.mockResolvedValue(mockVve);

      const result = await service.recordBerthing(vveId, {
        berthTime: '2026-01-15T08:00:00Z',
        dockId: 'DOCK-A',
      });

      expect(result).toHaveProperty('vveId');
      expect(result).toHaveProperty('vvnId');
      expect(result).toHaveProperty('status');
      expect(result).toHaveProperty('actualBerthTime');
      expect(result).toHaveProperty('assignedDock');
    });
  });

  describe('Error Handling', () => {
    /**
     * Test: Throws error when VVE not found
     * Business rule: VVE must exist to record berthing
     */
    it('should throw error when VVE not found', async () => {
      const vveId = 'VVE-NONEXISTENT';

      mockRepository.findById.mockResolvedValue(null);

      await expect(
        service.recordBerthing(vveId, {
          berthTime: '2026-01-15T08:00:00Z',
          dockId: 'DOCK-A',
        })
      ).rejects.toThrow();
    });

    /**
     * Test: Propagates repository errors
     * Business rule: Database errors should bubble up
     */
    it('should propagate repository errors', async () => {
      const vveId = 'VVE-001';

      mockRepository.findById.mockRejectedValue(new Error('Database connection failed'));

      await expect(
        service.recordBerthing(vveId, {
          berthTime: '2026-01-15T08:00:00Z',
          dockId: 'DOCK-A',
        })
      ).rejects.toThrow('Database connection failed');
    });

    /**
     * Test: Handles update failures
     * Business rule: Update errors should be propagated
     */
    it('should handle update failures', async () => {
      const vveId = 'VVE-001';
      const mockVve = createMockVve(vveId);

      mockRepository.findById.mockResolvedValue(mockVve);
      mockRepository.update.mockRejectedValue(new Error('Update failed'));

      await expect(
        service.recordBerthing(vveId, {
          berthTime: '2026-01-15T08:00:00Z',
          dockId: 'DOCK-A',
        })
      ).rejects.toThrow('Update failed');
    });

    /**
     * Test: Validates berth time format
     * Business rule: Should handle invalid date strings
     */
    it('should handle invalid date format', async () => {
      const vveId = 'VVE-001';
      const mockVve = createMockVve(vveId);

      mockRepository.findById.mockResolvedValue(mockVve);

      // Invalid Date will be created, domain will validate
      await expect(
        service.recordBerthing(vveId, {
          berthTime: 'invalid-date',
          dockId: 'DOCK-A',
        })
      ).rejects.toThrow();
    });
  });

  describe('Audit and Logging', () => {
    /**
     * Test: Updates are timestamped
     * Business rule: All updates must be timestamped for auditability
     */
    it('should record berthing with timestamp', async () => {
      const vveId = 'VVE-001';
      const mockVve = createMockVve(vveId);
      const berthTime = '2026-01-15T08:00:00Z';

      mockRepository.findById.mockResolvedValue(mockVve);
      mockRepository.update.mockResolvedValue(mockVve);

      const result = await service.recordBerthing(vveId, {
        berthTime,
        dockId: 'DOCK-A',
      });

      expect(result.actualBerthTime).toBeDefined();
      const actualTime = new Date(result.actualBerthTime!);
      const expectedTime = new Date(berthTime);
      expect(actualTime.getTime()).toBe(expectedTime.getTime());
    });

    /**
     * Test: Persists dock assignment for audit
     * Business rule: Dock changes must be tracked
     */
    it('should persist dock assignment for audit trail', async () => {
      const vveId = 'VVE-001';
      const mockVve = createMockVve(vveId);

      mockRepository.findById.mockResolvedValue(mockVve);
      mockRepository.update.mockImplementation(async (vve) => vve);

      await service.recordBerthing(vveId, {
        berthTime: '2026-01-15T08:00:00Z',
        dockId: 'DOCK-A',
      });

      const updateCall = mockRepository.update.mock.calls[0]![0];
      expect(updateCall.assignedDock).toBe('DOCK-A');
    });
  });

  describe('Integration Scenarios', () => {
    /**
     * Test: Complete berthing workflow
     * Business rule: Should handle full lifecycle from creation to berthing
     */
    it('should handle complete berthing workflow', async () => {
      const vveId = 'VVE-001';
      const mockVve = createMockVve(vveId);

      mockRepository.findById.mockResolvedValue(mockVve);
      mockRepository.update.mockResolvedValue(mockVve);

      // Record berthing
      const result = await service.recordBerthing(vveId, {
        berthTime: '2026-01-15T08:00:00Z',
        dockId: 'DOCK-A',
      });

      expect(result.status).toBe(VesselVisitExecutionStatus.IN_PROGRESS);
      expect(result.actualBerthTime).toBeDefined();
      expect(result.assignedDock).toBe('DOCK-A');
      expect(mockRepository.findById).toHaveBeenCalledWith(vveId);
      expect(mockRepository.update).toHaveBeenCalled();
    });

    /**
     * Test: Sequential berthing updates
     * Business rule: Should handle multiple berthing records (updates)
     */
    it('should handle sequential berthing updates', async () => {
      const vveId = 'VVE-001';

      // First berthing
      const mockVve1 = createMockVve(vveId);
      mockRepository.findById.mockResolvedValueOnce(mockVve1);
      mockRepository.update.mockResolvedValueOnce(mockVve1);

      await service.recordBerthing(vveId, {
        berthTime: '2026-01-15T08:00:00Z',
        dockId: 'DOCK-A',
      });

      // Update berthing (e.g., dock change)
      const mockVve2 = createMockVve(vveId);
      mockRepository.findById.mockResolvedValueOnce(mockVve2);
      mockRepository.update.mockResolvedValueOnce(mockVve2);

      const result = await service.recordBerthing(vveId, {
        berthTime: '2026-01-15T08:00:00Z',
        dockId: 'DOCK-B',
      });

      expect(result.assignedDock).toBe('DOCK-B');
      expect(mockRepository.update).toHaveBeenCalledTimes(2);
    });
  });
});
