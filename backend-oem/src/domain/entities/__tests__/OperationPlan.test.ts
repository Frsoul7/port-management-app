import { OperationPlan } from '../OperationPlan';
import { PlannedOperation } from '@domain/value-objects/PlannedOperation';
import { OperationPlanStatus, PlanningAlgorithm, OperationType } from '@shared/types';

describe('OperationPlan Entity', () => {
  const tomorrow = new Date();
  tomorrow.setDate(tomorrow.getDate() + 1);
  tomorrow.setHours(0, 0, 0, 0);

  const createValidOperation = (): PlannedOperation => {
    const startTime = new Date(tomorrow);
    startTime.setHours(8, 0, 0, 0);
    const endTime = new Date(tomorrow);
    endTime.setHours(12, 0, 0, 0);

    return new PlannedOperation({
      vvnId: 'VVN-001',
      vesselImo: 'IMO1234567',
      plannedStart: startTime,
      plannedEnd: endTime,
      assignedCranes: 2,
      operationType: OperationType.BOTH,
    });
  };

  describe('Creation', () => {
    it('should create a valid operation plan', () => {
      const operation = createValidOperation();
      const plan = new OperationPlan({
        targetDate: tomorrow,
        algorithm: PlanningAlgorithm.OPTIMAL,
        createdBy: 'test-user',
        totalDelay: 0,
        operations: [operation],
      });

      expect(plan.operationPlanId).toBeDefined();
      expect(plan.targetDate).toEqual(tomorrow);
      expect(plan.algorithm).toBe(PlanningAlgorithm.OPTIMAL);
      expect(plan.status).toBe(OperationPlanStatus.GENERATED);
      expect(plan.operations).toHaveLength(1);
    });

    it('should reject plan with empty operations', () => {
      expect(() => {
        new OperationPlan({
          targetDate: tomorrow,
          algorithm: PlanningAlgorithm.OPTIMAL,
          createdBy: 'test-user',
          totalDelay: 0,
          operations: [],
        });
      }).toThrow('Operation plan must have at least one operation');
    });

    it('should reject plan for past dates', () => {
      const yesterday = new Date();
      yesterday.setDate(yesterday.getDate() - 1);

      const operation = createValidOperation();

      expect(() => {
        new OperationPlan({
          targetDate: yesterday,
          algorithm: PlanningAlgorithm.OPTIMAL,
          createdBy: 'test-user',
          totalDelay: 0,
          operations: [operation],
        });
      }).toThrow('Cannot create operation plan for past dates');
    });

    it('should reject plan with negative delay', () => {
      const operation = createValidOperation();

      expect(() => {
        new OperationPlan({
          targetDate: tomorrow,
          algorithm: PlanningAlgorithm.OPTIMAL,
          createdBy: 'test-user',
          totalDelay: -5,
          operations: [operation],
        });
      }).toThrow('Total delay cannot be negative');
    });
  });

  describe('State Transitions', () => {
    it('should allow GENERATED -> APPROVED transition', () => {
      const operation = createValidOperation();
      const plan = new OperationPlan({
        targetDate: tomorrow,
        algorithm: PlanningAlgorithm.OPTIMAL,
        createdBy: 'test-user',
        totalDelay: 0,
        operations: [operation],
      });

      expect(plan.status).toBe(OperationPlanStatus.GENERATED);
      plan.approve();
      expect(plan.status).toBe(OperationPlanStatus.APPROVED);
    });

    it('should allow APPROVED -> IN_EXECUTION transition', () => {
      const operation = createValidOperation();
      const plan = new OperationPlan({
        targetDate: tomorrow,
        algorithm: PlanningAlgorithm.OPTIMAL,
        createdBy: 'test-user',
        totalDelay: 0,
        operations: [operation],
        status: OperationPlanStatus.APPROVED,
      });

      plan.startExecution();
      expect(plan.status).toBe(OperationPlanStatus.IN_EXECUTION);
    });

    it('should allow IN_EXECUTION -> COMPLETED transition', () => {
      const operation = createValidOperation();
      const plan = new OperationPlan({
        targetDate: tomorrow,
        algorithm: PlanningAlgorithm.OPTIMAL,
        createdBy: 'test-user',
        totalDelay: 0,
        operations: [operation],
        status: OperationPlanStatus.IN_EXECUTION,
      });

      plan.complete();
      expect(plan.status).toBe(OperationPlanStatus.COMPLETED);
    });

    it('should reject invalid state transitions', () => {
      const operation = createValidOperation();
      const plan = new OperationPlan({
        targetDate: tomorrow,
        algorithm: PlanningAlgorithm.OPTIMAL,
        createdBy: 'test-user',
        totalDelay: 0,
        operations: [operation],
        status: OperationPlanStatus.COMPLETED,
      });

      expect(() => plan.approve()).toThrow();
      expect(() => plan.startExecution()).toThrow();
    });
  });

  describe('Update Operations', () => {
    it('should update dock assignment for operation', () => {
      const operation = createValidOperation();
      const plan = new OperationPlan({
        targetDate: tomorrow,
        algorithm: PlanningAlgorithm.OPTIMAL,
        createdBy: 'test-user',
        totalDelay: 0,
        operations: [operation],
      });

      plan.updateDockAssignment('VVN-001', 'DOCK-A');
      const ops = plan.getOperationsSortedByStartTime();
      expect(ops[0]?.assignedDock).toBe('DOCK-A');
    });

    it('should reject updates to completed plans', () => {
      const operation = createValidOperation();
      const plan = new OperationPlan({
        targetDate: tomorrow,
        algorithm: PlanningAlgorithm.OPTIMAL,
        createdBy: 'test-user',
        totalDelay: 0,
        operations: [operation],
        status: OperationPlanStatus.COMPLETED,
      });

      expect(() => {
        plan.updateDockAssignment('VVN-001', 'DOCK-A');
      }).toThrow('Cannot update plan with status COMPLETED');
    });
  });

  describe('Business Logic', () => {
    it('should calculate total planned duration', () => {
      const operation = createValidOperation();
      const plan = new OperationPlan({
        targetDate: tomorrow,
        algorithm: PlanningAlgorithm.OPTIMAL,
        createdBy: 'test-user',
        totalDelay: 0,
        operations: [operation],
      });

      const duration = plan.getTotalPlannedDuration();
      expect(duration).toBe(4); // 4 hours (8:00 to 12:00)
    });

    it('should check if all operations have docks assigned', () => {
      const operation = createValidOperation();
      const plan = new OperationPlan({
        targetDate: tomorrow,
        algorithm: PlanningAlgorithm.OPTIMAL,
        createdBy: 'test-user',
        totalDelay: 0,
        operations: [operation],
      });

      expect(plan.allOperationsHaveDocks()).toBe(false);

      plan.updateDockAssignment('VVN-001', 'DOCK-A');
      expect(plan.allOperationsHaveDocks()).toBe(true);
    });

    it('should return operations sorted by start time', () => {
      const op1Start = new Date(tomorrow);
      op1Start.setHours(10, 0, 0, 0);
      const op1End = new Date(tomorrow);
      op1End.setHours(12, 0, 0, 0);

      const op2Start = new Date(tomorrow);
      op2Start.setHours(8, 0, 0, 0);
      const op2End = new Date(tomorrow);
      op2End.setHours(10, 0, 0, 0);

      const operation1 = new PlannedOperation({
        vvnId: 'VVN-001',
        vesselImo: 'IMO1234567',
        plannedStart: op1Start,
        plannedEnd: op1End,
        assignedCranes: 1,
        operationType: OperationType.LOAD,
      });

      const operation2 = new PlannedOperation({
        vvnId: 'VVN-002',
        vesselImo: 'IMO7654321',
        plannedStart: op2Start,
        plannedEnd: op2End,
        assignedCranes: 1,
        operationType: OperationType.UNLOAD,
      });

      const plan = new OperationPlan({
        targetDate: tomorrow,
        algorithm: PlanningAlgorithm.OPTIMAL,
        createdBy: 'test-user',
        totalDelay: 0,
        operations: [operation1, operation2],
      });

      const sorted = plan.getOperationsSortedByStartTime();
      expect(sorted[0]?.vvnId).toBe('VVN-002'); // Earlier start time
      expect(sorted[1]?.vvnId).toBe('VVN-001');
    });
  });

  describe('Serialization', () => {
    it('should serialize to JSON correctly', () => {
      const operation = createValidOperation();
      const plan = new OperationPlan({
        targetDate: tomorrow,
        algorithm: PlanningAlgorithm.OPTIMAL,
        createdBy: 'test-user',
        totalDelay: 0,
        operations: [operation],
      });

      const json = plan.toJSON();

      expect(json.operationPlanId).toBe(plan.operationPlanId);
      expect(json.status).toBe(OperationPlanStatus.GENERATED);
      expect(json.operations).toHaveLength(1);
      expect(typeof json.targetDate).toBe('string'); // ISO string
    });
  });
});
