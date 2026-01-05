import { OperationPlan } from '@domain/entities/OperationPlan';
import { PlannedOperation } from '@domain/value-objects/PlannedOperation';
import { OperationType, PlanningAlgorithm, OperationPlanStatus } from '@shared/types';

describe('US 4.1.4 - OperationPlan.updateOperation()', () => {
  const createMockOperation = (vvnId: string): PlannedOperation => {
    return new PlannedOperation({
      vvnId,
      vesselImo: `IMO-${vvnId}`,
      plannedStart: new Date('2026-01-10T08:00:00Z'),
      plannedEnd: new Date('2026-01-10T12:00:00Z'),
      assignedCranes: 2,
      operationType: OperationType.UNLOAD,
      assignedDock: 'D1',
      assignedStaff: ['STAFF-001', 'STAFF-002'],
    });
  };

  describe('successful updates', () => {
    it('should update operation with new crane count', () => {
      const targetDate = new Date('2026-01-10');
      const operations = [createMockOperation('VVN-001'), createMockOperation('VVN-002')];

      const plan = new OperationPlan({
        targetDate,
        algorithm: PlanningAlgorithm.OPTIMAL,
        operations,
        totalDelay: 150,
        createdBy: 'user-123',
      });

      plan.updateOperation(
        'VVN-001',
        { assignedCranes: 3 },
        'user-456',
        'Jane Smith',
        'Increasing crane capacity'
      );

      const updatedOp = plan.operations.find(op => op.vvnId === 'VVN-001');
      expect(updatedOp?.assignedCranes).toBe(3);
    });

    it('should update operation with new time window', () => {
      const targetDate = new Date('2026-01-10');
      const operations = [createMockOperation('VVN-001')];

      const plan = new OperationPlan({
        targetDate,
        algorithm: PlanningAlgorithm.OPTIMAL,
        operations,
        totalDelay: 0,
        createdBy: 'user-123',
      });

      const newStart = new Date('2025-01-10T09:00:00Z');
      const newEnd = new Date('2025-01-10T13:00:00Z');

      plan.updateOperation(
        'VVN-001',
        {
          plannedStart: newStart,
          plannedEnd: newEnd,
        },
        'user-456',
        'Jane Smith',
        'Adjusting schedule'
      );

      const updatedOp = plan.operations.find(op => op.vvnId === 'VVN-001');
      expect(updatedOp?.plannedStart).toEqual(newStart);
      expect(updatedOp?.plannedEnd).toEqual(newEnd);
    });

    it('should update operation with new dock assignment', () => {
      const targetDate = new Date('2026-01-10');
      const operations = [createMockOperation('VVN-001')];

      const plan = new OperationPlan({
        targetDate,
        algorithm: PlanningAlgorithm.OPTIMAL,
        operations,
        totalDelay: 0,
        createdBy: 'user-123',
      });

      plan.updateOperation(
        'VVN-001',
        { assignedDock: 'D2' },
        'user-456',
        'Jane Smith',
        'Moving to different dock'
      );

      const updatedOp = plan.operations.find(op => op.vvnId === 'VVN-001');
      expect(updatedOp?.assignedDock).toBe('D2');
    });

    it('should update operation with new staff assignment', () => {
      const targetDate = new Date('2026-01-10');
      const operations = [createMockOperation('VVN-001')];

      const plan = new OperationPlan({
        targetDate,
        algorithm: PlanningAlgorithm.OPTIMAL,
        operations,
        totalDelay: 0,
        createdBy: 'user-123',
      });

      const newStaff = ['STAFF-003', 'STAFF-004', 'STAFF-005'];

      plan.updateOperation(
        'VVN-001',
        { assignedStaff: newStaff },
        'user-456',
        'Jane Smith',
        'Changing staff assignments'
      );

      const updatedOp = plan.operations.find(op => op.vvnId === 'VVN-001');
      expect(updatedOp?.assignedStaff).toEqual(newStaff);
    });

    it('should update multiple fields at once', () => {
      const targetDate = new Date('2026-01-10');
      const operations = [createMockOperation('VVN-001')];

      const plan = new OperationPlan({
        targetDate,
        algorithm: PlanningAlgorithm.OPTIMAL,
        operations,
        totalDelay: 0,
        createdBy: 'user-123',
      });

      plan.updateOperation(
        'VVN-001',
        {
          assignedCranes: 4,
          assignedDock: 'D3',
          assignedStaff: ['STAFF-010'],
        },
        'user-456',
        'Jane Smith',
        'Major reconfiguration'
      );

      const updatedOp = plan.operations.find(op => op.vvnId === 'VVN-001');
      expect(updatedOp?.assignedCranes).toBe(4);
      expect(updatedOp?.assignedDock).toBe('D3');
      expect(updatedOp?.assignedStaff).toEqual(['STAFF-010']);
    });
  });

  describe('audit log tracking', () => {
    it('should create audit log entry when updating operation', () => {
      const targetDate = new Date('2026-01-10');
      const operations = [createMockOperation('VVN-001')];

      const plan = new OperationPlan({
        targetDate,
        algorithm: PlanningAlgorithm.OPTIMAL,
        operations,
        totalDelay: 0,
        createdBy: 'user-123',
      });

      const initialAuditLogLength = plan.auditLog.length;

      plan.updateOperation(
        'VVN-001',
        { assignedCranes: 3 },
        'user-456',
        'Jane Smith',
        'Increasing capacity'
      );

      expect(plan.auditLog.length).toBe(initialAuditLogLength + 1);
      const lastEntry = plan.auditLog[plan.auditLog.length - 1];
      expect(lastEntry).toBeDefined();
      expect(lastEntry!.action).toBe('UPDATED');
    });

    it('should record correct user information in audit log', () => {
      const targetDate = new Date('2026-01-10');
      const operations = [createMockOperation('VVN-001')];

      const plan = new OperationPlan({
        targetDate,
        algorithm: PlanningAlgorithm.OPTIMAL,
        operations,
        totalDelay: 0,
        createdBy: 'user-123',
      });

      plan.updateOperation(
        'VVN-001',
        { assignedCranes: 3 },
        'user-456',
        'Jane Smith',
        'Test update'
      );

      const lastEntry = plan.auditLog[plan.auditLog.length - 1];
      expect(lastEntry).toBeDefined();
      expect(lastEntry!.userId).toBe('user-456');
      expect(lastEntry!.userName).toBe('Jane Smith');
    });

    it('should record VVN ID in audit log', () => {
      const targetDate = new Date('2026-01-10');
      const operations = [createMockOperation('VVN-001')];

      const plan = new OperationPlan({
        targetDate,
        algorithm: PlanningAlgorithm.OPTIMAL,
        operations,
        totalDelay: 0,
        createdBy: 'user-123',
      });

      plan.updateOperation(
        'VVN-001',
        { assignedCranes: 3 },
        'user-456',
        'Jane Smith',
        'Test update'
      );

      const lastEntry = plan.auditLog[plan.auditLog.length - 1];
      expect(lastEntry).toBeDefined();
      expect(lastEntry!.vvnId).toBe('VVN-001');
    });

    it('should record reason in audit log', () => {
      const targetDate = new Date('2026-01-10');
      const operations = [createMockOperation('VVN-001')];

      const plan = new OperationPlan({
        targetDate,
        algorithm: PlanningAlgorithm.OPTIMAL,
        operations,
        totalDelay: 0,
        createdBy: 'user-123',
      });

      const reason = 'Increasing capacity due to vessel size';

      plan.updateOperation(
        'VVN-001',
        { assignedCranes: 3 },
        'user-456',
        'Jane Smith',
        reason
      );

      const lastEntry = plan.auditLog[plan.auditLog.length - 1];
      expect(lastEntry).toBeDefined();
      expect(lastEntry!.reason).toBe(reason);
    });

    it('should record field-level changes in audit log', () => {
      const targetDate = new Date('2026-01-10');
      const operations = [createMockOperation('VVN-001')];

      const plan = new OperationPlan({
        targetDate,
        algorithm: PlanningAlgorithm.OPTIMAL,
        operations,
        totalDelay: 0,
        createdBy: 'user-123',
      });

      plan.updateOperation(
        'VVN-001',
        { assignedCranes: 3 },
        'user-456',
        'Jane Smith',
        'Test update'
      );

      const lastEntry = plan.auditLog[plan.auditLog.length - 1];
      expect(lastEntry).toBeDefined();
      expect(lastEntry!.changes).toContainEqual({
        field: 'assignedCranes',
        oldValue: 2,
        newValue: 3,
      });
    });

    it('should record multiple changes in single update', () => {
      const targetDate = new Date('2026-01-10');
      const operations = [createMockOperation('VVN-001')];

      const plan = new OperationPlan({
        targetDate,
        algorithm: PlanningAlgorithm.OPTIMAL,
        operations,
        totalDelay: 0,
        createdBy: 'user-123',
      });

      plan.updateOperation(
        'VVN-001',
        {
          assignedCranes: 3,
          assignedDock: 'D2',
        },
        'user-456',
        'Jane Smith',
        'Multiple updates'
      );

      const lastEntry = plan.auditLog[plan.auditLog.length - 1];
      expect(lastEntry).toBeDefined();
      expect(lastEntry!.changes).toHaveLength(2);
      expect(lastEntry!.changes.some(c => c.field === 'assignedCranes')).toBe(true);
      expect(lastEntry!.changes.some(c => c.field === 'assignedDock')).toBe(true);
    });

    it('should record timestamp in audit log', () => {
      const targetDate = new Date('2026-01-10');
      const operations = [createMockOperation('VVN-001')];

      const plan = new OperationPlan({
        targetDate,
        algorithm: PlanningAlgorithm.OPTIMAL,
        operations,
        totalDelay: 0,
        createdBy: 'user-123',
      });

      const beforeUpdate = new Date();
      plan.updateOperation(
        'VVN-001',
        { assignedCranes: 3 },
        'user-456',
        'Jane Smith',
        'Test update'
      );
      const afterUpdate = new Date();

      const lastEntry = plan.auditLog[plan.auditLog.length - 1];
      expect(lastEntry).toBeDefined();
      expect(lastEntry!.timestamp).toBeInstanceOf(Date);
      expect(lastEntry!.timestamp.getTime()).toBeGreaterThanOrEqual(beforeUpdate.getTime());
      expect(lastEntry!.timestamp.getTime()).toBeLessThanOrEqual(afterUpdate.getTime());
    });
  });

  describe('validation and error handling', () => {
    it('should throw error if operation not found', () => {
      const targetDate = new Date('2026-01-10');
      const operations = [createMockOperation('VVN-001')];

      const plan = new OperationPlan({
        targetDate,
        algorithm: PlanningAlgorithm.OPTIMAL,
        operations,
        totalDelay: 0,
        createdBy: 'user-123',
      });

      expect(() => {
        plan.updateOperation(
          'VVN-999',
          { assignedCranes: 3 },
          'user-456',
          'Jane Smith',
          'Test update'
        );
      }).toThrow('Operation with VVN ID VVN-999 not found');
    });

    it('should throw error if plan is not editable (IN_EXECUTION)', () => {
      const targetDate = new Date('2026-01-10');
      const operations = [createMockOperation('VVN-001')];

      const plan = new OperationPlan({
        targetDate,
        algorithm: PlanningAlgorithm.OPTIMAL,
        operations,
        totalDelay: 0,
        createdBy: 'user-123',
        status: OperationPlanStatus.IN_EXECUTION,
      });

      expect(() => {
        plan.updateOperation(
          'VVN-001',
          { assignedCranes: 3 },
          'user-456',
          'Jane Smith',
          'Test update'
        );
      }).toThrow('Cannot update plan with status IN_EXECUTION');
    });

    it('should throw error if plan is COMPLETED', () => {
      const targetDate = new Date('2026-01-10');
      const operations = [createMockOperation('VVN-001')];

      const plan = new OperationPlan({
        targetDate,
        algorithm: PlanningAlgorithm.OPTIMAL,
        operations,
        totalDelay: 0,
        createdBy: 'user-123',
        status: OperationPlanStatus.COMPLETED,
      });

      expect(() => {
        plan.updateOperation(
          'VVN-001',
          { assignedCranes: 3 },
          'user-456',
          'Jane Smith',
          'Test update'
        );
      }).toThrow('Cannot update plan with status COMPLETED');
    });

    it('should allow updates to GENERATED plan', () => {
      const targetDate = new Date('2026-01-10');
      const operations = [createMockOperation('VVN-001')];

      const plan = new OperationPlan({
        targetDate,
        algorithm: PlanningAlgorithm.OPTIMAL,
        operations,
        totalDelay: 0,
        createdBy: 'user-123',
        status: OperationPlanStatus.GENERATED,
      });

      expect(() => {
        plan.updateOperation(
          'VVN-001',
          { assignedCranes: 3 },
          'user-456',
          'Jane Smith',
          'Test update'
        );
      }).not.toThrow();
    });

    it('should allow updates to APPROVED plan', () => {
      const targetDate = new Date('2026-01-10');
      const operations = [createMockOperation('VVN-001')];

      const plan = new OperationPlan({
        targetDate,
        algorithm: PlanningAlgorithm.OPTIMAL,
        operations,
        totalDelay: 0,
        createdBy: 'user-123',
        status: OperationPlanStatus.APPROVED,
      });

      expect(() => {
        plan.updateOperation(
          'VVN-001',
          { assignedCranes: 3 },
          'user-456',
          'Jane Smith',
          'Test update'
        );
      }).not.toThrow();
    });
  });

  describe('audit log limits', () => {
    it('should limit audit log to 100 entries', () => {
      const targetDate = new Date('2026-01-10');
      const operations = [createMockOperation('VVN-001')];

      const plan = new OperationPlan({
        targetDate,
        algorithm: PlanningAlgorithm.OPTIMAL,
        operations,
        totalDelay: 0,
        createdBy: 'user-123',
      });

      // Perform 105 updates
      for (let i = 0; i < 105; i++) {
        plan.updateOperation(
          'VVN-001',
          { assignedCranes: 2 + (i % 3) },
          'user-456',
          'Jane Smith',
          `Update ${i}`
        );
      }

      expect(plan.auditLog.length).toBe(100);
    });

    it('should keep most recent entries when limit reached', () => {
      const targetDate = new Date('2026-01-10');
      const operations = [createMockOperation('VVN-001')];

      const plan = new OperationPlan({
        targetDate,
        algorithm: PlanningAlgorithm.OPTIMAL,
        operations,
        totalDelay: 0,
        createdBy: 'user-123',
      });

      // Perform 105 updates
      for (let i = 0; i < 105; i++) {
        plan.updateOperation(
          'VVN-001',
          { assignedCranes: 2 + (i % 3) },
          'user-456',
          'Jane Smith',
          `Update ${i}`
        );
      }

      const lastEntry = plan.auditLog[plan.auditLog.length - 1];
      expect(lastEntry).toBeDefined();
      expect(lastEntry!.reason).toBe('Update 104');
    });
  });
});
