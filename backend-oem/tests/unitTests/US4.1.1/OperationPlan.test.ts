import { OperationPlan } from '@domain/entities/OperationPlan';
import { PlannedOperation } from '@domain/value-objects/PlannedOperation';
import { OperationPlanStatus, PlanningAlgorithm, OperationType } from '@shared/types';

/**
 * Unit Tests for OperationPlan Domain Entity
 * US 4.1.1: OEM Module Setup - Core domain logic
 * 
 * Test Coverage:
 * - Entity creation and validation
 * - Business rule enforcement
 * - State transitions (lifecycle)
 * - Operation management
 * - Audit logging (US 4.1.4)
 * - Aggregate root invariants
 */
describe('US 4.1.1 - OperationPlan Domain Entity', () => {
  // Use future date for all tests to avoid date validation errors
  const futureDate = new Date();
  futureDate.setDate(futureDate.getDate() + 7);
  futureDate.setHours(0, 0, 0, 0);

  /**
   * Create a sample planned operation for testing
   */
  const createSampleOperation = (vvnId: string = 'VVN-001', vesselImo: string = 'IMO1234567'): PlannedOperation => {
    return new PlannedOperation({
      vvnId,
      vesselImo,
      operationType: OperationType.UNLOAD,
      plannedStart: new Date(futureDate.getTime() + 8 * 60 * 60 * 1000), // 8:00 AM
      plannedEnd: new Date(futureDate.getTime() + 12 * 60 * 60 * 1000), // 12:00 PM
      assignedCranes: 2,
    });
  };

  /**
   * Create a sample operation plan for testing
   */
  const createSamplePlan = (overrides?: Partial<{
    targetDate: Date;
    algorithm: PlanningAlgorithm;
    createdBy: string;
    operations: PlannedOperation[];
    totalDelay: number;
    status: OperationPlanStatus;
  }>): OperationPlan => {
    return new OperationPlan({
      targetDate: overrides?.targetDate || futureDate,
      algorithm: overrides?.algorithm || PlanningAlgorithm.OPTIMAL,
      createdBy: overrides?.createdBy || 'user-001',
      operations: overrides?.operations || [createSampleOperation()],
      totalDelay: overrides?.totalDelay ?? 0,
      status: overrides?.status,
    });
  };

  describe('Constructor and Validation', () => {
    /**
     * Test: Entity can be created with valid minimum data
     * Validates that all required fields are properly initialized
     */
    it('should create operation plan with valid data', () => {
      const operation = createSampleOperation();
      
      const plan = new OperationPlan({
        targetDate: futureDate,
        algorithm: PlanningAlgorithm.OPTIMAL,
        createdBy: 'user-001',
        operations: [operation],
        totalDelay: 30,
      });

      expect(plan.operationPlanId).toBeDefined();
      expect(plan.targetDate).toEqual(futureDate);
      expect(plan.algorithm).toBe(PlanningAlgorithm.OPTIMAL);
      expect(plan.createdBy).toBe('user-001');
      expect(plan.status).toBe(OperationPlanStatus.GENERATED);
      expect(plan.totalDelay).toBe(30);
      expect(plan.operations).toHaveLength(1);
      expect(plan.createdAt).toBeInstanceOf(Date);
    });

    /**
     * Test: Enforces required field validation
     * Business rule: All plans must have target date
     */
    it('should throw error if target date is missing', () => {
      expect(() => {
        new OperationPlan({
          targetDate: null as any,
          algorithm: PlanningAlgorithm.OPTIMAL,
          createdBy: 'user-001',
          operations: [createSampleOperation()],
          totalDelay: 0,
        });
      }).toThrow('Target date is required');
    });

    /**
     * Test: Enforces algorithm selection
     * Business rule: Plans must specify which algorithm was used
     */
    it('should throw error if algorithm is missing', () => {
      expect(() => {
        new OperationPlan({
          targetDate: futureDate,
          algorithm: null as any,
          createdBy: 'user-001',
          operations: [createSampleOperation()],
          totalDelay: 0,
        });
      }).toThrow('Algorithm is required');
    });

    /**
     * Test: Enforces user accountability
     * Business rule: All plans must track who created them
     */
    it('should throw error if createdBy is missing', () => {
      expect(() => {
        new OperationPlan({
          targetDate: futureDate,
          algorithm: PlanningAlgorithm.OPTIMAL,
          createdBy: '',
          operations: [createSampleOperation()],
          totalDelay: 0,
        });
      }).toThrow('CreatedBy (user ID) is required');
    });

    /**
     * Test: Validates delay cannot be negative
     * Business rule: Delays represent time losses, cannot be negative
     */
    it('should throw error if total delay is negative', () => {
      expect(() => {
        new OperationPlan({
          targetDate: futureDate,
          algorithm: PlanningAlgorithm.OPTIMAL,
          createdBy: 'user-001',
          operations: [createSampleOperation()],
          totalDelay: -10,
        });
      }).toThrow('Total delay cannot be negative');
    });

    /**
     * Test: Enforces at least one operation
     * Business rule: Empty plans are not meaningful
     */
    it('should throw error if operations array is empty', () => {
      expect(() => {
        new OperationPlan({
          targetDate: futureDate,
          algorithm: PlanningAlgorithm.OPTIMAL,
          createdBy: 'user-001',
          operations: [],
          totalDelay: 0,
        });
      }).toThrow('Operation plan must have at least one operation');
    });

    /**
     * Test: Prevents creating plans for past dates
     * Business rule: Cannot plan operations in the past
     */
    it('should throw error if target date is in the past', () => {
      const pastDate = new Date();
      pastDate.setDate(pastDate.getDate() - 1);
      pastDate.setHours(0, 0, 0, 0);

      expect(() => {
        new OperationPlan({
          targetDate: pastDate,
          algorithm: PlanningAlgorithm.OPTIMAL,
          createdBy: 'user-001',
          operations: [createSampleOperation()],
          totalDelay: 0,
        });
      }).toThrow('Cannot create operation plan for past dates');
    });

    /**
     * Test: Allows loading historical plans from database
     * Business rule: Past date validation only applies to new plans
     */
    it('should allow past date when loading existing plan from database', () => {
      const pastDate = new Date();
      pastDate.setDate(pastDate.getDate() - 1);
      pastDate.setHours(0, 0, 0, 0);

      // Should not throw when operationPlanId is provided (indicates existing plan)
      const plan = new OperationPlan({
        operationPlanId: 'existing-plan-001',
        targetDate: pastDate,
        algorithm: PlanningAlgorithm.OPTIMAL,
        createdBy: 'user-001',
        operations: [createSampleOperation()],
        totalDelay: 0,
      });

      expect(plan.operationPlanId).toBe('existing-plan-001');
    });

    /**
     * Test: Creates immutable copy of operations array
     * Aggregate root invariant: External code cannot modify internal state
     */
    it('should create defensive copy of operations array', () => {
      const operations = [createSampleOperation('VVN-001'), createSampleOperation('VVN-002')];
      const plan = createSamplePlan({ operations });

      // Modify original array
      operations.push(createSampleOperation('VVN-003'));

      // Plan should still have 2 operations
      expect(plan.operations).toHaveLength(2);
    });

    /**
     * Test: Initializes audit log for new plans
     * US 4.1.4: Track plan creation
     */
    it('should initialize audit log with creation entry for new plans', () => {
      const plan = createSamplePlan();

      expect(plan.auditLog).toHaveLength(1);
      expect(plan.auditLog[0]!.action).toBe('CREATED');
      expect(plan.auditLog[0]!.userId).toBe('user-001');
    });
  });

  describe('Lifecycle State Transitions', () => {
    /**
     * Test: GENERATED → APPROVED transition
     * Business rule: New plans start as GENERATED and can be approved
     */
    it('should transition from GENERATED to APPROVED', () => {
      const plan = createSamplePlan();
      
      expect(plan.status).toBe(OperationPlanStatus.GENERATED);
      
      plan.approve();
      
      expect(plan.status).toBe(OperationPlanStatus.APPROVED);
    });

    /**
     * Test: Cannot approve non-GENERATED plans
     * Business rule: Only generated plans can be approved
     */
    it('should throw error when trying to approve non-GENERATED plan', () => {
      const plan = createSamplePlan();
      plan.approve();

      expect(() => plan.approve()).toThrow('Cannot approve plan with status APPROVED');
    });

    /**
     * Test: APPROVED → IN_EXECUTION transition
     * Business rule: Only approved plans can start execution
     */
    it('should transition from APPROVED to IN_EXECUTION', () => {
      const plan = createSamplePlan();
      plan.approve();
      
      plan.startExecution();
      
      expect(plan.status).toBe(OperationPlanStatus.IN_EXECUTION);
    });

    /**
     * Test: Cannot start execution of non-APPROVED plans
     * Business rule: Execution requires prior approval
     */
    it('should throw error when trying to start execution of non-APPROVED plan', () => {
      const plan = createSamplePlan();

      expect(() => plan.startExecution()).toThrow(
        'Cannot start execution of plan with status GENERATED'
      );
    });

    /**
     * Test: IN_EXECUTION → COMPLETED transition
     * Business rule: Only executing plans can be completed
     */
    it('should transition from IN_EXECUTION to COMPLETED', () => {
      const plan = createSamplePlan();
      plan.approve();
      plan.startExecution();
      
      plan.complete();
      
      expect(plan.status).toBe(OperationPlanStatus.COMPLETED);
    });

    /**
     * Test: Cannot complete non-executing plans
     * Business rule: Only plans in execution can be completed
     */
    it('should throw error when trying to complete non-IN_EXECUTION plan', () => {
      const plan = createSamplePlan();

      expect(() => plan.complete()).toThrow(
        'Cannot complete plan with status GENERATED'
      );
    });

    /**
     * Test: GENERATED/APPROVED → OUTDATED transition
     * Business rule: Plans can be marked outdated if superseded
     */
    it('should mark GENERATED plan as OUTDATED', () => {
      const plan = createSamplePlan();
      
      plan.markAsOutdated();
      
      expect(plan.status).toBe(OperationPlanStatus.OUTDATED);
    });

    /**
     * Test: Cannot mark executing/completed plans as outdated
     * Business rule: Active or completed plans maintain their status
     */
    it('should throw error when trying to mark IN_EXECUTION plan as outdated', () => {
      const plan = createSamplePlan();
      plan.approve();
      plan.startExecution();

      expect(() => plan.markAsOutdated()).toThrow(
        'Cannot mark IN_EXECUTION plan as outdated'
      );
    });

    /**
     * Test: Cannot mark completed plans as outdated
     * Business rule: Completed plans are historical records
     */
    it('should throw error when trying to mark COMPLETED plan as outdated', () => {
      const plan = createSamplePlan();
      plan.approve();
      plan.startExecution();
      plan.complete();

      expect(() => plan.markAsOutdated()).toThrow(
        'Cannot mark COMPLETED plan as outdated'
      );
    });
  });

  describe('Operation Management', () => {
    /**
     * Test: Can retrieve operations as read-only array
     * Aggregate root invariant: Cannot modify operations from outside
     */
    it('should return read-only copy of operations', () => {
      const plan = createSamplePlan({
        operations: [createSampleOperation('VVN-001'), createSampleOperation('VVN-002')],
      });

      const operations = plan.operations;

      expect(operations).toHaveLength(2);
      
      // Attempting to modify should not affect internal state
      (operations as any).push(createSampleOperation('VVN-003'));
      
      expect(plan.operations).toHaveLength(2);
    });

    /**
     * Test: Can find operation by VVN ID
     * Business rule: Operations are identified by their vessel visit notification
     */
    it('should find operation by VVN ID', () => {
      const plan = createSamplePlan({
        operations: [createSampleOperation('VVN-001'), createSampleOperation('VVN-002')],
      });

      const operation = plan.getOperationByVvnId('VVN-002');

      expect(operation).toBeDefined();
      expect(operation!.vvnId).toBe('VVN-002');
    });

    /**
     * Test: Returns undefined for non-existent VVN ID
     * Business rule: Fail gracefully when operation not found
     */
    it('should return undefined for non-existent VVN ID', () => {
      const plan = createSamplePlan();

      const operation = plan.getOperationByVvnId('VVN-999');

      expect(operation).toBeUndefined();
    });

    /**
     * Test: Can count total operations
     * Business rule: Plans track how many vessel operations they contain
     */
    it('should return correct operation count', () => {
      const plan = createSamplePlan({
        operations: [
          createSampleOperation('VVN-001'),
          createSampleOperation('VVN-002'),
          createSampleOperation('VVN-003'),
        ],
      });

      expect(plan.getOperationCount()).toBe(3);
    });

    /**
     * Test: Calculates total planned duration
     * Business rule: Duration = time from earliest start to latest end
     */
    it('should calculate total planned duration correctly', () => {
      const op1 = new PlannedOperation({
        vvnId: 'VVN-001',
        vesselImo: 'IMO1234567',
        operationType: OperationType.UNLOAD,
        plannedStart: new Date(futureDate.getTime() + 8 * 60 * 60 * 1000), // 8:00 AM
        plannedEnd: new Date(futureDate.getTime() + 12 * 60 * 60 * 1000), // 12:00 PM
        assignedCranes: 2,
      });

      const op2 = new PlannedOperation({
        vvnId: 'VVN-002',
        vesselImo: 'IMO7654321',
        operationType: OperationType.LOAD,
        plannedStart: new Date(futureDate.getTime() + 14 * 60 * 60 * 1000), // 2:00 PM
        plannedEnd: new Date(futureDate.getTime() + 18 * 60 * 60 * 1000), // 6:00 PM
        assignedCranes: 1,
      });

      const plan = createSamplePlan({ operations: [op1, op2] });

      // Duration: 8:00 AM to 6:00 PM = 10 hours
      expect(plan.getTotalPlannedDuration()).toBe(10);
    });

    /**
     * Test: Returns operations sorted by start time
     * Business rule: Operations should be ordered chronologically
     */
    it('should return operations sorted by planned start time', () => {
      const op1 = createSampleOperation('VVN-001');
      const op2 = new PlannedOperation({
        vvnId: 'VVN-002',
        vesselImo: 'IMO7654321',
        operationType: OperationType.LOAD,
        plannedStart: new Date(futureDate.getTime() + 6 * 60 * 60 * 1000), // 6:00 AM (earlier)
        plannedEnd: new Date(futureDate.getTime() + 10 * 60 * 60 * 1000),
        assignedCranes: 1,
      });

      const plan = createSamplePlan({ operations: [op1, op2] }); // op1 starts later

      const sorted = plan.getOperationsSortedByStartTime();

      expect(sorted[0]!.vvnId).toBe('VVN-002'); // Earlier start
      expect(sorted[1]!.vvnId).toBe('VVN-001'); // Later start
    });
  });

  describe('Operation Updates (US 4.1.4)', () => {
    /**
     * Test: Can update dock assignment for GENERATED plan
     * Business rule: GENERATED and APPROVED plans are editable
     */
    it('should update dock assignment for GENERATED plan', () => {
      const plan = createSamplePlan();

      plan.updateDockAssignment('VVN-001', 'DOCK-A1');

      const operation = plan.getOperationByVvnId('VVN-001');
      expect(operation!.assignedDock).toBe('DOCK-A1');
    });

    /**
     * Test: Can update dock assignment for APPROVED plan
     * Business rule: Approved plans can still be adjusted
     */
    it('should update dock assignment for APPROVED plan', () => {
      const plan = createSamplePlan();
      plan.approve();

      plan.updateDockAssignment('VVN-001', 'DOCK-B2');

      const operation = plan.getOperationByVvnId('VVN-001');
      expect(operation!.assignedDock).toBe('DOCK-B2');
    });

    /**
     * Test: Cannot update dock for executing plans
     * Business rule: Plans in execution are locked
     */
    it('should throw error when updating dock for IN_EXECUTION plan', () => {
      const plan = createSamplePlan();
      plan.approve();
      plan.startExecution();

      expect(() => plan.updateDockAssignment('VVN-001', 'DOCK-A1')).toThrow(
        'Cannot update plan with status IN_EXECUTION'
      );
    });

    /**
     * Test: Cannot update non-existent operation
     * Business rule: Updates must target existing operations
     */
    it('should throw error when updating non-existent operation', () => {
      const plan = createSamplePlan();

      expect(() => plan.updateDockAssignment('VVN-999', 'DOCK-A1')).toThrow(
        'Operation with VVN ID VVN-999 not found in plan'
      );
    });

    /**
     * Test: Can update staff assignment
     * Business rule: Staff can be assigned to operations
     */
    it('should update staff assignment for operation', () => {
      const plan = createSamplePlan();

      plan.updateStaffAssignment('VVN-001', ['staff-001', 'staff-002']);

      const operation = plan.getOperationByVvnId('VVN-001');
      expect(operation!.assignedStaff).toEqual(['staff-001', 'staff-002']);
    });

    /**
     * Test: Updates with audit logging
     * US 4.1.4: Track all changes with reason
     */
    it('should update operation with audit logging', () => {
      const plan = createSamplePlan();
      const initialAuditCount = plan.auditLog.length;

      plan.updateOperation(
        'VVN-001',
        {
          assignedCranes: 3,
          assignedDock: 'DOCK-A1',
        },
        'user-002',
        'John Doe',
        'Optimizing crane allocation'
      );

      const operation = plan.getOperationByVvnId('VVN-001');
      expect(operation!.assignedCranes).toBe(3);
      expect(operation!.assignedDock).toBe('DOCK-A1');

      // Check audit log
      expect(plan.auditLog).toHaveLength(initialAuditCount + 1);
      const auditEntry = plan.auditLog[plan.auditLog.length - 1]!;
      expect(auditEntry.action).toBe('UPDATED');
      expect(auditEntry.userId).toBe('user-002');
      expect(auditEntry.vvnId).toBe('VVN-001');
      expect(auditEntry.reason).toBe('Optimizing crane allocation');
      expect(auditEntry.changes).toHaveLength(2);
    });

    /**
     * Test: Requires reason for updates
     * Business rule: All changes must be justified
     */
    it('should throw error if reason for change is not provided', () => {
      const plan = createSamplePlan();

      expect(() =>
        plan.updateOperation('VVN-001', { assignedCranes: 3 }, 'user-002', 'John Doe', '')
      ).toThrow('Reason for change is required');
    });

    /**
     * Test: Validates crane count
     * Business rule: At least one crane must be assigned
     */
    it('should throw error if trying to assign less than 1 crane', () => {
      const plan = createSamplePlan();

      expect(() =>
        plan.updateOperation(
          'VVN-001',
          { assignedCranes: 0 },
          'user-002',
          'John Doe',
          'Testing'
        )
      ).toThrow('At least one crane must be assigned');
    });
  });

  describe('Status Checks and Helpers', () => {
    /**
     * Test: GENERATED and APPROVED plans are editable
     * Business rule: Only certain statuses allow modifications
     */
    it('should return true for isEditable when plan is GENERATED or APPROVED', () => {
      const plan1 = createSamplePlan();
      expect(plan1.isEditable()).toBe(true);

      plan1.approve();
      expect(plan1.isEditable()).toBe(true);
    });

    /**
     * Test: Executing and completed plans are not editable
     * Business rule: Active/completed plans are locked
     */
    it('should return false for isEditable when plan is IN_EXECUTION or COMPLETED', () => {
      const plan = createSamplePlan();
      plan.approve();
      plan.startExecution();

      expect(plan.isEditable()).toBe(false);

      plan.complete();
      expect(plan.isEditable()).toBe(false);
    });

    /**
     * Test: Completed and outdated plans are read-only
     * Business rule: Historical plans cannot be modified
     */
    it('should return true for isReadOnly when plan is COMPLETED or OUTDATED', () => {
      const plan1 = createSamplePlan();
      plan1.approve();
      plan1.startExecution();
      plan1.complete();

      expect(plan1.isReadOnly()).toBe(true);

      const plan2 = createSamplePlan();
      plan2.markAsOutdated();

      expect(plan2.isReadOnly()).toBe(true);
    });

    /**
     * Test: Checks if all operations have dock assignments
     * Business rule: Complete plans should have all resources assigned
     */
    it('should check if all operations have dock assignments', () => {
      const op1 = new PlannedOperation({
        vvnId: 'VVN-001',
        vesselImo: 'IMO1234567',
        operationType: OperationType.UNLOAD,
        plannedStart: new Date(futureDate.getTime() + 8 * 60 * 60 * 1000),
        plannedEnd: new Date(futureDate.getTime() + 12 * 60 * 60 * 1000),
        assignedCranes: 2,
        assignedDock: 'DOCK-A1',
      });

      const op2 = createSampleOperation('VVN-002'); // No dock assigned

      const plan1 = createSamplePlan({ operations: [op1, op2] });
      expect(plan1.allOperationsHaveDocks()).toBe(false);

      const plan2 = createSamplePlan({ operations: [op1] });
      expect(plan2.allOperationsHaveDocks()).toBe(true);
    });

    /**
     * Test: Checks if all operations have staff assignments
     * Business rule: Complete plans should have all resources assigned
     */
    it('should check if all operations have staff assignments', () => {
      const op1 = new PlannedOperation({
        vvnId: 'VVN-001',
        vesselImo: 'IMO1234567',
        operationType: OperationType.UNLOAD,
        plannedStart: new Date(futureDate.getTime() + 8 * 60 * 60 * 1000),
        plannedEnd: new Date(futureDate.getTime() + 12 * 60 * 60 * 1000),
        assignedCranes: 2,
        assignedStaff: ['staff-001'],
      });

      const op2 = createSampleOperation('VVN-002'); // No staff assigned

      const plan1 = createSamplePlan({ operations: [op1, op2] });
      expect(plan1.allOperationsHaveStaff()).toBe(false);

      const plan2 = createSamplePlan({ operations: [op1] });
      expect(plan2.allOperationsHaveStaff()).toBe(true);
    });
  });

  describe('Serialization', () => {
    /**
     * Test: Can convert to JSON for persistence
     * Business rule: Entity must be serializable for database storage
     */
    it('should convert to JSON correctly', () => {
      const plan = createSamplePlan();
      const json = plan.toJSON();

      expect(json).toHaveProperty('operationPlanId');
      expect(json).toHaveProperty('targetDate');
      expect(json).toHaveProperty('algorithm');
      expect(json).toHaveProperty('createdBy');
      expect(json).toHaveProperty('createdAt');
      expect(json).toHaveProperty('status');
      expect(json).toHaveProperty('totalDelay');
      expect(json).toHaveProperty('operations');
      expect(Array.isArray(json.operations)).toBe(true);
    });

    /**
     * Test: JSON contains ISO date strings
     * Business rule: Dates must be serialized consistently
     */
    it('should serialize dates as ISO strings', () => {
      const plan = createSamplePlan();
      const json = plan.toJSON();

      expect(typeof json.targetDate).toBe('string');
      expect(typeof json.createdAt).toBe('string');
      expect(json.targetDate).toContain('T'); // ISO format includes T
    });
  });

  describe('Audit Log Management (US 4.1.4)', () => {
    /**
     * Test: Returns immutable copy of audit log
     * Business rule: External code cannot tamper with audit trail
     */
    it('should return read-only copy of audit log', () => {
      const plan = createSamplePlan();
      const auditLog = plan.auditLog;

      const initialLength = auditLog.length;

      // Attempt to modify
      (auditLog as any).push({
        timestamp: new Date(),
        userId: 'hacker',
        userName: 'Hacker',
        action: 'DELETED',
        changes: [],
      });

      // Should not affect internal state
      expect(plan.auditLog).toHaveLength(initialLength);
    });

    /**
     * Test: Limits audit log size
     * Business rule: Prevent unbounded growth of audit trail
     */
    it('should limit audit log to 100 entries', () => {
      const plan = createSamplePlan();

      // Make 110 updates
      for (let i = 0; i < 110; i++) {
        plan.updateOperation(
          'VVN-001',
          { assignedCranes: (i % 3) + 1 },
          'user-001',
          'Test User',
          `Update ${i}`
        );
      }

      // Should keep only last 100 entries (plus initial creation entry)
      expect(plan.auditLog.length).toBeLessThanOrEqual(101);
    });
  });
});
