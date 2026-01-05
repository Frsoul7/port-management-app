import { OperationPlanService } from '../OperationPlanService';
import { IOperationPlanRepository } from '@domain/repositories/IOperationPlanRepository';
import { CoreBackendClient } from '@infrastructure/http-clients/CoreBackendClient';
import { PlanningClient } from '@infrastructure/http-clients/PlanningClient';
import { OperationPlan } from '@domain/entities/OperationPlan';
import { PlannedOperation } from '@domain/value-objects/PlannedOperation';
import { VvnReference, OperationType, PlanningAlgorithm } from '@shared/types';
import { CreateOperationPlanDto } from '@application/dtos';

// Mock logger to avoid console output during tests
jest.mock('@shared/utils/logger', () => ({
  logger: {
    info: jest.fn(),
    warn: jest.fn(),
    error: jest.fn(),
    debug: jest.fn(),
  },
}));

describe('OperationPlanService', () => {
  let service: OperationPlanService;
  let mockRepository: jest.Mocked<IOperationPlanRepository>;
  let mockCoreBackendClient: jest.Mocked<CoreBackendClient>;
  let mockPlanningClient: jest.Mocked<PlanningClient>;

  const mockUserId = 'user-123';
  // Use a future date (7 days from now) to avoid date validation errors
  const targetDate = new Date();
  targetDate.setDate(targetDate.getDate() + 7);
  targetDate.setHours(0, 0, 0, 0);
  const targetDateString: string = targetDate.toISOString().split('T')[0]!;

  const mockVvn: VvnReference = {
    vvnId: 'VVN-001',
    vesselImo: 'IMO1234567',
    eta: new Date(targetDate.getTime() + 8 * 60 * 60 * 1000), // 8:00 AM on target date
    etd: new Date(targetDate.getTime() + 16 * 60 * 60 * 1000), // 4:00 PM on target date
    loadingCount: 80,
    unloadingCount: 100,
    purpose: OperationType.BOTH,
    state: 'APPROVED',
  };

  beforeEach(() => {
    // Create mocked repository
    mockRepository = {
      save: jest.fn(),
      findById: jest.fn(),
      findByTargetDate: jest.fn(),
      findAll: jest.fn(),
      findByStatus: jest.fn(),
      findByDateRange: jest.fn(),
      update: jest.fn(),
      delete: jest.fn(),
    } as any;

    // Create mocked CoreBackendClient
    mockCoreBackendClient = {
      getApprovedVvns: jest.fn(),
      getVvnById: jest.fn(),
      getVvnsByDateRange: jest.fn(),
      checkHealth: jest.fn(),
    } as any;

    // Create mocked PlanningClient
    mockPlanningClient = {
      generatePlan: jest.fn(),
      checkHealth: jest.fn(),
    } as any;

    service = new OperationPlanService(mockRepository, mockCoreBackendClient, mockPlanningClient);
  });

  describe('generatePlan', () => {
    const createPlanDto: CreateOperationPlanDto = {
      targetDate: targetDateString,
      algorithm: PlanningAlgorithm.OPTIMAL,
    };

    const mockPrologResponse = {
      sequence: [
        {
          vessel: 'VVN-001',
          start: 8.0,
          end: 13.3,
          cranes: 1,
        },
      ],
      total_delay: 0,
    };

    it('should generate a plan successfully with OPTIMAL algorithm', async () => {
      mockRepository.findByTargetDate.mockResolvedValue(null);
      mockCoreBackendClient.getVvnsByDateRange.mockResolvedValue([mockVvn]);
      mockPlanningClient.generatePlan.mockResolvedValue(mockPrologResponse);

      const mockSavedPlan = new OperationPlan({
        targetDate,
        algorithm: PlanningAlgorithm.OPTIMAL,
        createdBy: mockUserId,
        totalDelay: 0,
        operations: [
          new PlannedOperation({
            vvnId: 'VVN-001',
            vesselImo: 'IMO1234567',
            plannedStart: new Date('2025-12-10T08:00:00Z'),
            plannedEnd: new Date('2025-12-10T13:18:00Z'),
            assignedCranes: 1,
            operationType: OperationType.BOTH,
          }),
        ],
      });

      mockRepository.save.mockResolvedValue(mockSavedPlan);

      const result = await service.generatePlan(createPlanDto, mockUserId);

      expect(result).toBeDefined();
      expect(result.algorithm).toBe(PlanningAlgorithm.OPTIMAL);
      expect(result.status).toBe('GENERATED');
      expect(result.totalDelay).toBe(0);
      expect(result.operations).toHaveLength(1);
      expect(mockRepository.save).toHaveBeenCalled();
    });

    it('should throw error if APPROVED plan already exists for target date', async () => {
      const mockOperation = new PlannedOperation({
        vvnId: 'VVN-001',
        vesselImo: 'IMO1234567',
        plannedStart: new Date('2025-12-10T08:00:00Z'),
        plannedEnd: new Date('2025-12-10T13:00:00Z'),
        assignedCranes: 1,
        operationType: OperationType.BOTH,
      });

      const existingPlan = new OperationPlan({
        targetDate,
        algorithm: PlanningAlgorithm.OPTIMAL,
        createdBy: mockUserId,
        totalDelay: 0,
        operations: [mockOperation],
      });
      existingPlan.approve();

      mockRepository.findByTargetDate.mockResolvedValue(existingPlan);

      await expect(service.generatePlan(createPlanDto, mockUserId)).rejects.toThrow(
        `An APPROVED plan already exists for ${targetDateString}`
      );
    });

    it('should throw error if IN_EXECUTION plan already exists for target date', async () => {
      const mockOperation = new PlannedOperation({
        vvnId: 'VVN-001',
        vesselImo: 'IMO1234567',
        plannedStart: new Date('2025-12-10T08:00:00Z'),
        plannedEnd: new Date('2025-12-10T13:00:00Z'),
        assignedCranes: 1,
        operationType: OperationType.BOTH,
      });

      const existingPlan = new OperationPlan({
        targetDate,
        algorithm: PlanningAlgorithm.OPTIMAL,
        createdBy: mockUserId,
        totalDelay: 0,
        operations: [mockOperation],
      });
      existingPlan.approve();
      existingPlan.startExecution();

      mockRepository.findByTargetDate.mockResolvedValue(existingPlan);

      await expect(service.generatePlan(createPlanDto, mockUserId)).rejects.toThrow(
        `An IN_EXECUTION plan already exists for ${targetDateString}`
      );
    });

    it('should throw error if no VVNs found for target date', async () => {
      mockRepository.findByTargetDate.mockResolvedValue(null);
      mockCoreBackendClient.getVvnsByDateRange.mockResolvedValue([]);

      await expect(service.generatePlan(createPlanDto, mockUserId)).rejects.toThrow(
        `No approved VVNs found for ${targetDateString}`
      );
    });

    it('should filter VVNs by specific vvnIds when provided', async () => {
      const createPlanDtoWithIds: CreateOperationPlanDto = {
        targetDate: targetDateString,
        algorithm: PlanningAlgorithm.OPTIMAL,
        vvnIds: ['VVN-001'],
      };

      mockRepository.findByTargetDate.mockResolvedValue(null);
      mockCoreBackendClient.getVvnById.mockResolvedValue(mockVvn); // For filtering by specific IDs
      mockPlanningClient.generatePlan.mockResolvedValue(mockPrologResponse);

      const mockOperation = new PlannedOperation({
        vvnId: 'VVN-001',
        vesselImo: 'IMO1234567',
        plannedStart: new Date('2025-12-10T08:00:00Z'),
        plannedEnd: new Date('2025-12-10T13:00:00Z'),
        assignedCranes: 1,
        operationType: OperationType.BOTH,
      });

      const mockSavedPlan = new OperationPlan({
        targetDate,
        algorithm: PlanningAlgorithm.OPTIMAL,
        createdBy: mockUserId,
        totalDelay: 0,
        operations: [mockOperation],
      });

      mockRepository.save.mockResolvedValue(mockSavedPlan);

      await service.generatePlan(createPlanDtoWithIds, mockUserId);

      expect(mockCoreBackendClient.getVvnById).toHaveBeenCalledWith('VVN-001');
      expect(mockPlanningClient.generatePlan).toHaveBeenCalled();
    });

    it('should calculate processing times correctly', async () => {
      // VVN with 100 containers to unload (20/hr) = 5.0 hours
      // VVN with 80 containers to load (15/hr) = 5.3 hours
      mockRepository.findByTargetDate.mockResolvedValue(null);
      mockCoreBackendClient.getVvnsByDateRange.mockResolvedValue([mockVvn]);
      mockPlanningClient.generatePlan.mockResolvedValue(mockPrologResponse);

      const mockOperation = new PlannedOperation({
        vvnId: 'VVN-001',
        vesselImo: 'IMO1234567',
        plannedStart: new Date('2025-12-10T08:00:00Z'),
        plannedEnd: new Date('2025-12-10T13:00:00Z'),
        assignedCranes: 1,
        operationType: OperationType.BOTH,
      });

      const mockSavedPlan = new OperationPlan({
        targetDate,
        algorithm: PlanningAlgorithm.OPTIMAL,
        createdBy: mockUserId,
        totalDelay: 0,
        operations: [mockOperation],
      });

      mockRepository.save.mockResolvedValue(mockSavedPlan);

      await service.generatePlan(createPlanDto, mockUserId);

      // Verify that generatePlan was called with correct processing times
      const prologInput = mockPlanningClient.generatePlan.mock.calls[0]![0];
      expect(prologInput).toHaveLength(1);
      expect(prologInput[0]!.unload).toBe(5.0); // 100 / 20
      expect(prologInput[0]!.load).toBe(5.4); // 80 / 15 = 5.333... rounded to 5.4
    });
  });

  describe('getPlanById', () => {
    it('should return plan DTO when plan exists', async () => {
      const mockPlan = new OperationPlan({
        targetDate,
        algorithm: PlanningAlgorithm.OPTIMAL,
        createdBy: mockUserId,
        totalDelay: 0,
        operations: [
          new PlannedOperation({
            vvnId: 'VVN-001',
            vesselImo: 'IMO1234567',
            plannedStart: new Date('2025-12-10T08:00:00Z'),
            plannedEnd: new Date('2025-12-10T13:00:00Z'),
            assignedCranes: 1,
            operationType: OperationType.BOTH,
          }),
        ],
      });

      mockRepository.findById.mockResolvedValue(mockPlan);

      const result = await service.getPlanById('plan-123');

      expect(result).toBeDefined();
      expect(result?.algorithm).toBe(PlanningAlgorithm.OPTIMAL);
      expect(mockRepository.findById).toHaveBeenCalledWith('plan-123');
    });

    it('should return null when plan does not exist', async () => {
      mockRepository.findById.mockResolvedValue(null);

      const result = await service.getPlanById('non-existent');

      expect(result).toBeNull();
    });
  });

  describe('getAllPlans', () => {
    it('should return all plans as DTOs', async () => {
      const mockOperation = new PlannedOperation({
        vvnId: 'VVN-001',
        vesselImo: 'IMO1234567',
        plannedStart: new Date('2025-12-10T08:00:00Z'),
        plannedEnd: new Date('2025-12-10T13:00:00Z'),
        assignedCranes: 1,
        operationType: OperationType.BOTH,
      });

      const mockPlans = [
        new OperationPlan({
          targetDate,
          algorithm: PlanningAlgorithm.OPTIMAL,
          createdBy: mockUserId,
          totalDelay: 0,
          operations: [mockOperation],
        }),
        new OperationPlan({
          targetDate: new Date(targetDate.getTime() + 24 * 60 * 60 * 1000), // Next day
          algorithm: PlanningAlgorithm.WEIGHTED,
          createdBy: mockUserId,
          totalDelay: 5,
          operations: [mockOperation],
        }),
      ];

      mockRepository.findAll.mockResolvedValue(mockPlans);

      const result = await service.getAllPlans();

      expect(result).toHaveLength(2);
      expect(result[0]!.algorithm).toBe(PlanningAlgorithm.OPTIMAL);
      expect(result[1]!.algorithm).toBe(PlanningAlgorithm.WEIGHTED);
    });

    it('should return empty array when no plans exist', async () => {
      mockRepository.findAll.mockResolvedValue([]);

      const result = await service.getAllPlans();

      expect(result).toEqual([]);
    });
  });

  describe('approvePlan', () => {
    it('should approve a plan successfully', async () => {
      const mockPlan = new OperationPlan({
        targetDate,
        algorithm: PlanningAlgorithm.OPTIMAL,
        createdBy: mockUserId,
        totalDelay: 0,
        operations: [
          new PlannedOperation({
            vvnId: 'VVN-001',
            vesselImo: 'IMO1234567',
            plannedStart: new Date('2025-12-10T08:00:00Z'),
            plannedEnd: new Date('2025-12-10T13:00:00Z'),
            assignedCranes: 1,
            operationType: OperationType.BOTH,
          }),
        ],
      });

      mockRepository.findById.mockResolvedValue(mockPlan);
      mockRepository.update.mockResolvedValue(mockPlan);

      const result = await service.approvePlan('plan-123');

      expect(result.status).toBe('APPROVED');
      expect(mockRepository.update).toHaveBeenCalled();
    });

    it('should throw error when plan not found', async () => {
      mockRepository.findById.mockResolvedValue(null);

      await expect(service.approvePlan('non-existent')).rejects.toThrow(
        'Operation plan non-existent not found'
      );
    });
  });

  describe('startExecution', () => {
    it('should start execution of an approved plan', async () => {
      const mockPlan = new OperationPlan({
        targetDate,
        algorithm: PlanningAlgorithm.OPTIMAL,
        createdBy: mockUserId,
        totalDelay: 0,
        operations: [
          new PlannedOperation({
            vvnId: 'VVN-001',
            vesselImo: 'IMO1234567',
            plannedStart: new Date('2025-12-10T08:00:00Z'),
            plannedEnd: new Date('2025-12-10T13:00:00Z'),
            assignedCranes: 1,
            operationType: OperationType.BOTH,
          }),
        ],
      });
      mockPlan.approve();

      mockRepository.findById.mockResolvedValue(mockPlan);
      mockRepository.update.mockResolvedValue(mockPlan);

      const result = await service.startExecution('plan-123');

      expect(result.status).toBe('IN_EXECUTION');
      expect(mockRepository.update).toHaveBeenCalled();
    });

    it('should throw error when plan not found', async () => {
      mockRepository.findById.mockResolvedValue(null);

      await expect(service.startExecution('non-existent')).rejects.toThrow(
        'Operation plan non-existent not found'
      );
    });
  });

  describe('completePlan', () => {
    it('should complete a plan in execution', async () => {
      const mockPlan = new OperationPlan({
        targetDate,
        algorithm: PlanningAlgorithm.OPTIMAL,
        createdBy: mockUserId,
        totalDelay: 0,
        operations: [
          new PlannedOperation({
            vvnId: 'VVN-001',
            vesselImo: 'IMO1234567',
            plannedStart: new Date('2025-12-10T08:00:00Z'),
            plannedEnd: new Date('2025-12-10T13:00:00Z'),
            assignedCranes: 1,
            operationType: OperationType.BOTH,
          }),
        ],
      });
      mockPlan.approve();
      mockPlan.startExecution();

      mockRepository.findById.mockResolvedValue(mockPlan);
      mockRepository.update.mockResolvedValue(mockPlan);

      const result = await service.completePlan('plan-123');

      expect(result.status).toBe('COMPLETED');
      expect(mockRepository.update).toHaveBeenCalled();
    });

    it('should throw error when plan not found', async () => {
      mockRepository.findById.mockResolvedValue(null);

      await expect(service.completePlan('non-existent')).rejects.toThrow(
        'Operation plan non-existent not found'
      );
    });
  });

  describe('updateDockAssignment', () => {
    it('should update dock assignment for an operation', async () => {
      const mockOperation = new PlannedOperation({
        vvnId: 'VVN-001',
        vesselImo: 'IMO1234567',
        plannedStart: new Date('2025-12-10T08:00:00Z'),
        plannedEnd: new Date('2025-12-10T13:00:00Z'),
        assignedCranes: 1,
        operationType: OperationType.BOTH,
      });

      const mockPlan = new OperationPlan({
        targetDate,
        algorithm: PlanningAlgorithm.OPTIMAL,
        createdBy: mockUserId,
        totalDelay: 0,
        operations: [mockOperation],
      });

      mockRepository.findById.mockResolvedValue(mockPlan);
      mockRepository.update.mockResolvedValue(mockPlan);

      const result = await service.updateDockAssignment('plan-123', {
        vvnId: 'VVN-001',
        dockId: 'DOCK-A',
      });

      expect(result).toBeDefined();
      expect(mockRepository.update).toHaveBeenCalled();
    });

    it('should throw error when VVN not found in plan', async () => {
      const mockPlan = new OperationPlan({
        targetDate,
        algorithm: PlanningAlgorithm.OPTIMAL,
        createdBy: mockUserId,
        totalDelay: 0,
        operations: [
          new PlannedOperation({
            vvnId: 'VVN-001',
            vesselImo: 'IMO1234567',
            plannedStart: new Date('2025-12-10T08:00:00Z'),
            plannedEnd: new Date('2025-12-10T13:00:00Z'),
            assignedCranes: 1,
            operationType: OperationType.BOTH,
          }),
        ],
      });

      mockRepository.findById.mockResolvedValue(mockPlan);

      await expect(
        service.updateDockAssignment('plan-123', {
          vvnId: 'VVN-999',
          dockId: 'DOCK-A',
        })
      ).rejects.toThrow('Operation with VVN ID VVN-999 not found in plan');
    });
  });

  describe('updateStaffAssignment', () => {
    it('should update staff assignment for an operation', async () => {
      const mockOperation = new PlannedOperation({
        vvnId: 'VVN-001',
        vesselImo: 'IMO1234567',
        plannedStart: new Date('2025-12-10T08:00:00Z'),
        plannedEnd: new Date('2025-12-10T13:00:00Z'),
        assignedCranes: 1,
        operationType: OperationType.BOTH,
      });

      const mockPlan = new OperationPlan({
        targetDate,
        algorithm: PlanningAlgorithm.OPTIMAL,
        createdBy: mockUserId,
        totalDelay: 0,
        operations: [mockOperation],
      });

      mockRepository.findById.mockResolvedValue(mockPlan);
      mockRepository.update.mockResolvedValue(mockPlan);

      const result = await service.updateStaffAssignment('plan-123', {
        vvnId: 'VVN-001',
        staffIds: ['STAFF-1', 'STAFF-2'],
      });

      expect(result).toBeDefined();
      expect(mockRepository.update).toHaveBeenCalled();
    });

    it('should throw error when plan not found', async () => {
      mockRepository.findById.mockResolvedValue(null);

      await expect(
        service.updateStaffAssignment('non-existent', {
          vvnId: 'VVN-001',
          staffIds: ['STAFF-1'],
        })
      ).rejects.toThrow('Operation plan non-existent not found');
    });
  });

  describe('deletePlan', () => {
    it('should delete a draft plan', async () => {
      const mockPlan = new OperationPlan({
        targetDate,
        algorithm: PlanningAlgorithm.OPTIMAL,
        createdBy: mockUserId,
        totalDelay: 0,
        operations: [
          new PlannedOperation({
            vvnId: 'VVN-001',
            vesselImo: 'IMO1234567',
            plannedStart: new Date('2025-12-10T08:00:00Z'),
            plannedEnd: new Date('2025-12-10T13:00:00Z'),
            assignedCranes: 1,
            operationType: OperationType.BOTH,
          }),
        ],
      });

      mockRepository.findById.mockResolvedValue(mockPlan);
      mockRepository.delete.mockResolvedValue(undefined);

      await service.deletePlan('plan-123');

      expect(mockRepository.delete).toHaveBeenCalledWith('plan-123');
    });

    it('should throw error when trying to delete plan in execution', async () => {
      const mockPlan = new OperationPlan({
        targetDate,
        algorithm: PlanningAlgorithm.OPTIMAL,
        createdBy: mockUserId,
        totalDelay: 0,
        operations: [
          new PlannedOperation({
            vvnId: 'VVN-001',
            vesselImo: 'IMO1234567',
            plannedStart: new Date('2025-12-10T08:00:00Z'),
            plannedEnd: new Date('2025-12-10T13:00:00Z'),
            assignedCranes: 1,
            operationType: OperationType.BOTH,
          }),
        ],
      });
      mockPlan.approve();
      mockPlan.startExecution();

      mockRepository.findById.mockResolvedValue(mockPlan);

      await expect(service.deletePlan('plan-123')).rejects.toThrow(
        'Cannot delete plan with status IN_EXECUTION'
      );
    });

    it('should throw error when plan not found', async () => {
      mockRepository.findById.mockResolvedValue(null);

      await expect(service.deletePlan('non-existent')).rejects.toThrow(
        'Operation plan non-existent not found'
      );
    });
  });
});
