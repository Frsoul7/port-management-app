/**
 * VVE and Operation Plan Seeder
 * Seeds initial data for development/testing environments
 * US 4.1.9: ExecutedOperations now derive from PlannedOperations
 */

import { logger } from '@shared/utils/logger';
import { VesselVisitExecutionRepository } from '../../repositories/VesselVisitExecutionRepository';
import { OperationPlanRepository } from '../../repositories/OperationPlanRepository';
import { VesselVisitExecutionService } from '@application/services/VesselVisitExecutionService';
import { OperationPlan } from '@domain/entities/OperationPlan';
import { PlannedOperation } from '@domain/value-objects/PlannedOperation';
import { PlanningAlgorithm, OperationType, VesselVisitExecutionStatus } from '@shared/types';

/**
 * Seed VVE from Operation Plan for development/testing
 * Creates a sample operation plan and initializes a VVE from it
 */
export async function seedVveFromOperationPlanIfNeeded(): Promise<void> {
  try {
    const vveRepository = new VesselVisitExecutionRepository();
    const operationPlanRepository = new OperationPlanRepository();

    // Check if VVE already exists
    const existingVves = await vveRepository.findByStatus(VesselVisitExecutionStatus.IN_PROGRESS);
    if (existingVves.length > 0 && existingVves[0]!.operations.length > 0) {
      logger.debug(`VVE ${existingVves[0]!.vveId} already exists with ${existingVves[0]!.operations.length} operations`);
      return;
    }

    // Check if Operation Plan exists for today
    const today = new Date();
    today.setHours(0, 0, 0, 0);
    let operationPlan = await operationPlanRepository.findByTargetDate(today);

    if (!operationPlan) {
      logger.info('No operation plan found for today. Creating seed Operation Plan...');

      // Create PlannedOperations for a mock vessel
      const arrivalTime = new Date();
      arrivalTime.setHours(8, 0, 0, 0);

      const plannedOperations = [
        new PlannedOperation({
          vvnId: 'VVN-MOCK-001',
          vesselImo: '9876543',
          operationType: OperationType.UNLOAD,
          plannedStart: new Date(arrivalTime.getTime() + 2 * 60 * 60 * 1000), // 10:00
          plannedEnd: new Date(arrivalTime.getTime() + 4 * 60 * 60 * 1000), // 12:00
          assignedCranes: 2,
          assignedDock: 'DOCK-A',
          assignedStaff: ['STAFF001', 'STAFF002'],
        }),
        new PlannedOperation({
          vvnId: 'VVN-MOCK-001',
          vesselImo: '9876543',
          operationType: OperationType.LOAD,
          plannedStart: new Date(arrivalTime.getTime() + 4 * 60 * 60 * 1000), // 12:00
          plannedEnd: new Date(arrivalTime.getTime() + 6 * 60 * 60 * 1000), // 14:00
          assignedCranes: 2,
          assignedDock: 'DOCK-A',
          assignedStaff: ['STAFF003', 'STAFF004'],
        }),
        new PlannedOperation({
          vvnId: 'VVN-MOCK-001',
          vesselImo: '9876543',
          operationType: OperationType.MAINTENANCE,
          plannedStart: new Date(arrivalTime.getTime() + 6 * 60 * 60 * 1000), // 14:00
          plannedEnd: new Date(arrivalTime.getTime() + 7 * 60 * 60 * 1000), // 15:00
          assignedCranes: 1,
          assignedDock: 'DOCK-A',
          assignedStaff: ['STAFF005'],
        }),
      ];

      // Create Operation Plan
      operationPlan = new OperationPlan({
        targetDate: today,
        algorithm: PlanningAlgorithm.OPTIMAL,
        createdBy: 'SEED_SYSTEM',
        totalDelay: 0,
        operations: plannedOperations,
      });

      // Approve the plan
      operationPlan.approve();

      // Save the plan
      operationPlan = await operationPlanRepository.save(operationPlan);
      logger.info(`✅ Created seed Operation Plan ${operationPlan.operationPlanId} with 3 planned operations`);
    }

    // Initialize VVE from Operation Plan
    const vveService = new VesselVisitExecutionService(vveRepository, operationPlanRepository);

    const arrivalTime = new Date();
    const vve = await vveService.initializeVveFromPlan({
      operationPlanId: operationPlan.operationPlanId,
      vvnId: 'VVN-MOCK-001',
      arrivalTime: arrivalTime.toISOString(),
      createdBy: 'SEED_SYSTEM',
    });

    logger.info(`✅ Initialized VVE ${vve.vveId} from Operation Plan with ${vve.operations.length} executed operations`);
    logger.info('   US 4.1.9: ExecutedOperations derived from PlannedOperations');
    logger.info('   All operations have status=STARTED and are linked to planned operations');
  } catch (error) {
    logger.warn('Could not seed VVE from Operation Plan:', error);
    // Don't fail the app startup if seeding fails
  }
}
