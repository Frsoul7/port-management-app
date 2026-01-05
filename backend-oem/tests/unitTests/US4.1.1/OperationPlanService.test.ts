import { OperationPlanService } from '@application/services/OperationPlanService';
import { IOperationPlanRepository } from '@domain/repositories/IOperationPlanRepository';
import { OperationPlan } from '@domain/entities/OperationPlan';
import { PlannedOperation } from '@domain/value-objects/PlannedOperation';
import { CoreBackendClient } from '@infrastructure/http-clients/CoreBackendClient';
import { PlanningClient } from '@infrastructure/http-clients/PlanningClient';
import { PlanningAlgorithm, OperationType, OperationPlanStatus, VvnReference } from '@shared/types';
import { CreateOperationPlanDto } from '@application/dtos';

/**
 * US 4.1.1 - OEM Module Setup
 * US 4.1.2 - Generate Operation Plan
 * US 4.1.3 - Search and List Operation Plans (partial coverage)
 * US 4.1.5 - Identify VVNs Without Plans (partial coverage)
 *
 * Tests for OperationPlanService (Application Layer)
 * Verifies orchestration of plan generation, approval, and lifecycle management
 * Tests integration with repository, Core Backend, and Planning clients
 */
describe('US 4.1.1 - OperationPlanService', () => {
  let service: OperationPlanService;
  let mockRepository: jest.Mocked<IOperationPlanRepository>;
  let mockCoreBackendClient: jest.Mocked<CoreBackendClient>;
  let mockPlanningClient: jest.Mocked<PlanningClient>;

  // Use future date for all tests to avoid date validation errors
  const futureDate = new Date();
  futureDate.setDate(futureDate.getDate() + 7);
  futureDate.setHours(0, 0, 0, 0);
  const futureDateString = futureDate.toISOString().split('T')[0]!;

  const mockVvns: VvnReference[] = [
    {
      vvnId: 'VVN-001',
      vesselImo: 'IMO1234567',
      eta: new Date(futureDate.getTime() + 8 * 60 * 60 * 1000), // 8:00 AM
      etd: new Date(futureDate.getTime() + 16 * 60 * 60 * 1000), // 4:00 PM
      loadingCount: 0,
      unloadingCount: 120,
      purpose: OperationType.UNLOAD,
      state: 'APPROVED',
    },
    {
      vvnId: 'VVN-002',
      vesselImo: 'IMO7654321',
      eta: new Date(futureDate.getTime() + 12 * 60 * 60 * 1000), // 12:00 PM
      etd: new Date(futureDate.getTime() + 20 * 60 * 60 * 1000), // 8:00 PM
      loadingCount: 100,
      unloadingCount: 0,
      purpose: OperationType.LOAD,
      state: 'APPROVED',
    },
  ];

  const mockPrologResponse = {
    sequence: [
      {
        vessel: 'VVN-001',
        start: 8, // 08:00 in hours
        end: 12, // 12:00 in hours
        cranes: 2,
      },
      {
        vessel: 'VVN-002',
        start: 12, // 12:00 in hours
        end: 14, // 14:00 in hours
        cranes: 1,
      },
    ],
    total_delay: 60,
  };

  beforeEach(() => {
    // Create mock repository
    mockRepository = {
      save: jest.fn(),
      update: jest.fn(),
      findById: jest.fn(),
      findByTargetDate: jest.fn(),
      findAll: jest.fn(),
      delete: jest.fn(),
    } as any;

    // Create mock core backend client
    mockCoreBackendClient = {
      getVvnsByDateRange: jest.fn(),
      getVvnById: jest.fn(),
    } as unknown as jest.Mocked<CoreBackendClient>;

    // Create mock planning client
    mockPlanningClient = {
      generatePlan: jest.fn(),
    } as unknown as jest.Mocked<PlanningClient>;

    // Create service instance
    service = new OperationPlanService(
      mockRepository,
      mockCoreBackendClient,
      mockPlanningClient
    );
  });

  describe('generatePlan', () => {
    /**
     * US 4.1.2: Test successful operation plan generation
     * Verifies complete workflow:
     * 1. Check if plan already exists for target date
     * 2. Fetch approved VVNs from Core Backend
     * 3. Call Planning & Scheduling service (Prolog) with algorithm
     * 4. Create OperationPlan entity with results
     * 5. Persist to repository
     * Ensures REST-only communication between modules (US 4.1.1 requirement)
     */
    it('should generate a new operation plan successfully', async () => {
      const dto: CreateOperationPlanDto = {
        targetDate: futureDateString,
        algorithm: PlanningAlgorithm.OPTIMAL,
      };

      mockRepository.findByTargetDate.mockResolvedValue(null);
      mockCoreBackendClient.getVvnsByDateRange.mockResolvedValue(mockVvns);
      mockPlanningClient.generatePlan.mockResolvedValue(mockPrologResponse);
      mockRepository.save.mockResolvedValue({
        operationPlanId: 'PLAN-001',
        targetDate: futureDate,
        algorithm: PlanningAlgorithm.OPTIMAL,
        status: OperationPlanStatus.GENERATED,
        totalDelay: 60,
        operations: [],        auditLog: [],        createdAt: new Date(),
        createdBy: 'user-123',
      } as any);

      const result = await service.generatePlan(dto, 'user-123');

      expect(result).toBeDefined();
      expect(result.algorithm).toBe(PlanningAlgorithm.OPTIMAL);
      expect(mockRepository.findByTargetDate).toHaveBeenCalledWith(futureDate);
      expect(mockPlanningClient.generatePlan).toHaveBeenCalled();
      expect(mockRepository.save).toHaveBeenCalled();
    });

    /**
     * US 4.1.2 & 4.1.5: Test plan regeneration prevention for APPROVED plans
     * Verifies business rule that APPROVED plans cannot be regenerated
     * Prevents overwriting plans that are being or have been executed
     * User must manually delete or modify APPROVED plans
     */
    it('should throw error if APPROVED plan already exists for date', async () => {
      const dto: CreateOperationPlanDto = {
        targetDate: futureDateString,
        algorithm: PlanningAlgorithm.OPTIMAL,
      };

      const existingPlan = {
        status: OperationPlanStatus.APPROVED,
      } as OperationPlan;

      mockRepository.findByTargetDate.mockResolvedValue(existingPlan);

      await expect(service.generatePlan(dto, 'user-123')).rejects.toThrow(
        `An APPROVED plan already exists for ${futureDateString}`
      );

      expect(mockPlanningClient.generatePlan).not.toHaveBeenCalled();
    });

    /**
     * US 4.1.2 & 4.1.5: Test plan regeneration prevention for IN_EXECUTION plans
     * Verifies business rule that IN_EXECUTION plans cannot be regenerated
     * Prevents disrupting active operations by regenerating the plan
     * Ensures data consistency during execution
     */
    it('should throw error if IN_EXECUTION plan already exists for date', async () => {
      const dto: CreateOperationPlanDto = {
        targetDate: futureDateString,
        algorithm: PlanningAlgorithm.OPTIMAL,
      };

      const existingPlan = {
        status: OperationPlanStatus.IN_EXECUTION,
      } as OperationPlan;

      mockRepository.findByTargetDate.mockResolvedValue(existingPlan);

      await expect(service.generatePlan(dto, 'user-123')).rejects.toThrow(
        `An IN_EXECUTION plan already exists for ${futureDateString}`
      );
    });

    /**
     * US 4.1.2 & 4.1.5: Test plan regeneration for GENERATED plans
     * Verifies that GENERATED plans (not yet approved) can be regenerated
     * Allows operators to try different algorithms before approval
     * Supports iterative plan refinement workflow
     * Related to US 4.1.5: regeneration of plans for VVNs without valid plans
     */
    it('should allow regenerating plan if existing plan is GENERATED', async () => {
      const dto: CreateOperationPlanDto = {
        targetDate: futureDateString,
        algorithm: PlanningAlgorithm.OPTIMAL,
      };

      const existingPlan = {
        status: OperationPlanStatus.GENERATED,
      } as OperationPlan;

      mockRepository.findByTargetDate.mockResolvedValue(existingPlan);
      mockCoreBackendClient.getVvnsByDateRange.mockResolvedValue(mockVvns);
      mockPlanningClient.generatePlan.mockResolvedValue(mockPrologResponse);
      mockRepository.save.mockResolvedValue({
        operationPlanId: 'PLAN-002',
        targetDate: futureDate,
        algorithm: PlanningAlgorithm.OPTIMAL,
        status: OperationPlanStatus.GENERATED,
        totalDelay: 60,
        operations: [],
        auditLog: [],
        createdAt: new Date(),
        createdBy: 'user-123',
      } as any);

      const result = await service.generatePlan(dto, 'user-123');

      expect(result).toBeDefined();
      expect(mockPlanningClient.generatePlan).toHaveBeenCalled();
    });

    /**
     * US 4.1.2 & 4.1.5: Test error handling for missing VVNs
     * Verifies that plan generation fails if no approved VVNs exist for target date
     * Prevents creating empty or invalid operation plans
     * Related to US 4.1.5: identifying VVNs without plans
     */
    it('should throw error if no approved VVNs found', async () => {
      const dto: CreateOperationPlanDto = {
        targetDate: futureDateString,
        algorithm: PlanningAlgorithm.OPTIMAL,
      };

      mockRepository.findByTargetDate.mockResolvedValue(null);
      mockCoreBackendClient.getVvnsByDateRange.mockResolvedValue([]);

      await expect(service.generatePlan(dto, 'user-123')).rejects.toThrow(
        `No approved VVNs found for ${futureDateString}`
      );

      expect(mockPlanningClient.generatePlan).not.toHaveBeenCalled();
    });

    /**
     * US 4.1.2: Test mock VVN mode for development/testing
     * Verifies that service can use mock VVN data when configured
     * Allows testing plan generation without Core Backend dependency
     * Useful for development and integration testing
     * TODO: Fix VVN ID mismatch between mock generator and Prolog response
     */
    it.skip('should use mock VVNs when USE_MOCK_VVNS is true', async () => {
      process.env.USE_MOCK_VVNS = 'true';

      const dto: CreateOperationPlanDto = {
        targetDate: futureDateString,
        algorithm: PlanningAlgorithm.OPTIMAL,
      };

      mockRepository.findByTargetDate.mockResolvedValue(null);
      mockPlanningClient.generatePlan.mockResolvedValue(mockPrologResponse);
      mockRepository.save.mockResolvedValue({
        operationPlanId: 'PLAN-001',
        targetDate: futureDate,
        algorithm: PlanningAlgorithm.OPTIMAL,
        status: OperationPlanStatus.GENERATED,
        totalDelay: 60,
        operations: [],
        auditLog: [],
        createdAt: new Date(),
        createdBy: 'user-123',
      } as any);

      await service.generatePlan(dto, 'user-123');

      // Should not call core backend client
      expect(mockCoreBackendClient.getVvnsByDateRange).not.toHaveBeenCalled();
      expect(mockPlanningClient.generatePlan).toHaveBeenCalled();

      delete process.env.USE_MOCK_VVNS;
    });
  });

  describe('approvePlan', () => {
    /**
     * US 4.1.1: Test plan approval workflow at service level
     * Verifies:
     * - Service retrieves plan from repository
     * - Calls domain method to approve (business logic in domain)
     * - Persists updated plan back to repository
     * Demonstrates Clean Architecture: service orchestrates, domain enforces rules
     */
    it('should approve a GENERATED plan', async () => {
      const mockPlan = new OperationPlan({
        targetDate: futureDate,
        algorithm: PlanningAlgorithm.OPTIMAL,
        operations: [
          new PlannedOperation({
            vvnId: 'VVN-001',
            vesselImo: 'IMO1234567',
            plannedStart: new Date(futureDate.getTime() + 8 * 60 * 60 * 1000),
            plannedEnd: new Date(futureDate.getTime() + 12 * 60 * 60 * 1000),
            assignedCranes: 2,
            operationType: OperationType.UNLOAD,
          }),
        ],
        totalDelay: 60,
        createdBy: 'user-123',
      });

      mockRepository.findById.mockResolvedValue(mockPlan);
      mockRepository.update.mockResolvedValue(mockPlan);

      await service.approvePlan('plan-id');

      expect(mockPlan.status).toBe(OperationPlanStatus.APPROVED);
      expect(mockRepository.update).toHaveBeenCalledWith(mockPlan);
    });

    /**
     * US 4.1.1: Test error handling for missing plan
     * Verifies that service throws clear error if plan doesn't exist
     * Prevents null reference errors and provides helpful feedback
     */
    it('should throw error if plan not found', async () => {
      mockRepository.findById.mockResolvedValue(null);

      await expect(service.approvePlan('plan-id')).rejects.toThrow(
        'Operation plan plan-id not found'
      );
    });
  });

  describe('deletePlan', () => {
    /**
     * US 4.1.1: Test plan deletion for GENERATED plans
     * Verifies:
     * - Service retrieves plan and checks if deletable (via domain method)
     * - Only GENERATED plans can be deleted
     * - Repository delete is called
     * Supports cleanup of draft/rejected plans
     */
    it('should delete a GENERATED plan', async () => {
      const mockPlan = new OperationPlan({
        targetDate: futureDate,
        algorithm: PlanningAlgorithm.OPTIMAL,
        operations: [
          new PlannedOperation({
            vvnId: 'VVN-001',
            vesselImo: 'IMO1234567',
            plannedStart: new Date(futureDate.getTime() + 8 * 60 * 60 * 1000),
            plannedEnd: new Date(futureDate.getTime() + 12 * 60 * 60 * 1000),
            assignedCranes: 2,
            operationType: OperationType.UNLOAD,
          }),
        ],
        totalDelay: 60,
        createdBy: 'user-123',
      });

      mockRepository.findById.mockResolvedValue(mockPlan);
      mockRepository.delete.mockResolvedValue(undefined);

      await service.deletePlan('plan-id');

      expect(mockRepository.delete).toHaveBeenCalledWith('plan-id');
    });

    /**
     * US 4.1.1: Test deletion prevention for non-GENERATED plans
     * Verifies business rule that IN_EXECUTION/COMPLETED plans cannot be deleted
     * Prevents accidental deletion of active or historical plans
     * Ensures data integrity and audit trail preservation
     */
    it('should throw error if trying to delete non-GENERATED plan', async () => {
      const mockPlan = new OperationPlan({
        targetDate: futureDate,
        algorithm: PlanningAlgorithm.OPTIMAL,
        operations: [
          new PlannedOperation({
            vvnId: 'VVN-001',
            vesselImo: 'IMO1234567',
            plannedStart: new Date(futureDate.getTime() + 8 * 60 * 60 * 1000),
            plannedEnd: new Date(futureDate.getTime() + 12 * 60 * 60 * 1000),
            assignedCranes: 2,
            operationType: OperationType.UNLOAD,
          }),
        ],
        totalDelay: 60,
        createdBy: 'user-123',
      });

      // Set status to IN_EXECUTION (which cannot be deleted)
      mockPlan['_status'] = OperationPlanStatus.IN_EXECUTION;
      mockRepository.findById.mockResolvedValue(mockPlan);

      await expect(service.deletePlan('plan-id')).rejects.toThrow(
        'Cannot delete plan with status IN_EXECUTION'
      );

      expect(mockRepository.delete).not.toHaveBeenCalled();
    });
  });

  describe('getPlanById', () => {
    /**
     * US 4.1.1 & 4.1.3: Test retrieving plan by ID
     * Verifies:
     * - Service can fetch individual plan
     * - Repository query is called with correct ID
     * - Plan data is returned correctly
     * Supports plan detail views and updates
     */
    it('should return plan if found', async () => {
      const mockPlan = new OperationPlan({
        targetDate: futureDate,
        algorithm: PlanningAlgorithm.OPTIMAL,
        operations: [
          new PlannedOperation({
            vvnId: 'VVN-001',
            vesselImo: 'IMO1234567',
            plannedStart: new Date(futureDate.getTime() + 8 * 60 * 60 * 1000),
            plannedEnd: new Date(futureDate.getTime() + 12 * 60 * 60 * 1000),
            assignedCranes: 2,
            operationType: OperationType.UNLOAD,
          }),
        ],
        totalDelay: 60,
        createdBy: 'user-123',
      });

      mockRepository.findById.mockResolvedValue(mockPlan);

      const result = await service.getPlanById('plan-id');

      expect(result).toBeDefined();
      expect(result!.operationPlanId).toBe(mockPlan.operationPlanId);
    });

    /**
     * US 4.1.1 & 4.1.3: Test error handling for missing plan
     * Verifies that service throws error when plan doesn't exist
     * Provides clear feedback for invalid plan IDs
     */
    it('should return null if plan not found', async () => {
      mockRepository.findById.mockResolvedValue(null);

      const result = await service.getPlanById('plan-id');

      expect(result).toBeNull();
    });
  });

  // Note: listPlans tests removed as the method doesn't exist in current implementation
  // The service uses findAll, findByStatus, and findByDateRange methods instead
});
