import { ConflictDetectionService } from '@domain/services/ConflictDetectionService';
import { OperationPlan } from '@domain/entities/OperationPlan';
import { PlannedOperation } from '@domain/value-objects/PlannedOperation';
import { OperationType, PlanningAlgorithm } from '@shared/types';

/**
 * US 4.1.4 - ConflictDetectionService Tests
 *
 * Tests for domain service that validates manual plan adjustments
 * Detects resource conflicts when operations are updated:
 * - Crane conflicts (ERROR severity): Same crane assigned to overlapping operations
 * - Dock conflicts (ERROR severity): Same dock assigned to overlapping operations
 * - Staff conflicts (WARNING severity): Same staff member assigned to overlapping operations
 *
 * Conflict detection prevents invalid manual adjustments and ensures:
 * - Physical resource constraints are respected
 * - Time overlap calculations are accurate
 * - Conflict severity is appropriate (ERROR vs WARNING)
 * - Cross-plan conflicts are detected (same date, different plans)
 */
describe('US 4.1.4 - ConflictDetectionService', () => {
  let service: ConflictDetectionService;

  beforeEach(() => {
    service = new ConflictDetectionService();
  });

  const createMockPlan = (
    date: Date,
    operations: PlannedOperation[]
  ): OperationPlan => {
    return new OperationPlan({
      targetDate: date,
      algorithm: PlanningAlgorithm.OPTIMAL,
      createdBy: 'user-123',
      totalDelay: 0,
      operations,
    });
  };

  const createMockOperation = (
    vvnId: string,
    startHour: number,
    endHour: number,
    cranes: number = 2,
    dock?: string,
    staff?: string[]
  ): PlannedOperation => {
    const date = '2026-01-10';
    return new PlannedOperation({
      vvnId,
      vesselImo: `IMO-${vvnId}`,
      plannedStart: new Date(`${date}T${startHour.toString().padStart(2, '0')}:00:00Z`),
      plannedEnd: new Date(`${date}T${endHour.toString().padStart(2, '0')}:00:00Z`),
      assignedCranes: cranes,
      operationType: OperationType.UNLOAD,
      assignedDock: dock,
      assignedStaff: staff,
    });
  };

  describe('detectConflicts - No Conflicts', () => {
    /**
     * US 4.1.4: Test no conflicts for non-overlapping operations
     * Verifies that operations with different time windows do not generate conflicts
     * Expected: hasConflicts = false, conflicts array empty
     */
    it('should return no conflicts when operations do not overlap', () => {
      const op1 = createMockOperation('VVN-001', 8, 12, 2);
      const op2 = createMockOperation('VVN-002', 12, 16, 3); // Different cranes, no overlap
      const targetPlan = createMockPlan(new Date('2026-01-10'), [op1, op2]);

      const updatedOp = op1.withCranes(2);

      const result = service.detectConflicts(
        targetPlan,
        'VVN-001',
        updatedOp,
        [targetPlan]
      );

      expect(result.hasConflicts).toBe(false);
      expect(result.hasErrors).toBe(false);
      expect(result.hasWarnings).toBe(false);
      expect(result.conflicts).toHaveLength(0);
      expect(result.summary).toContain('No conflicts detected');
    });

    /**
     * US 4.1.4: Test that operation is not compared with itself
     * Verifies that the updated operation is excluded from conflict detection
     * Prevents false positives when checking conflicts for an operation
     */
    it('should not detect conflicts with the operation being updated', () => {
      const op1 = createMockOperation('VVN-001', 8, 12, 2, 'D1');
      const op2 = createMockOperation('VVN-002', 14, 16, 3, 'D2');
      const targetPlan = createMockPlan(new Date('2026-01-10'), [op1, op2]);

      const updatedOp = op1.withCranes(2);

      const result = service.detectConflicts(
        targetPlan,
        'VVN-001',
        updatedOp,
        [targetPlan]
      );

      expect(result.hasConflicts).toBe(false);
    });

    /**
     * US 4.1.4: Test no conflicts for operations using different cranes
     * Verifies that overlapping time windows with different crane assignments
     * do not generate conflicts (cranes are independent resources)
     */
    it('should not detect conflicts when different cranes are used', () => {
      const op1 = createMockOperation('VVN-001', 8, 12, 2);
      const op2 = createMockOperation('VVN-002', 10, 14, 3); // Different crane number
      const targetPlan = createMockPlan(new Date('2026-01-10'), [op1, op2]);

      const updatedOp = op1.withCranes(2);

      const result = service.detectConflicts(
        targetPlan,
        'VVN-001',
        updatedOp,
        [targetPlan]
      );

      expect(result.hasConflicts).toBe(false);
    });
  });

  describe('detectConflicts - Crane Conflicts', () => {
    /**
     * US 4.1.4: Test crane conflict detection
     * Verifies:
     * - Same crane assigned to overlapping time windows generates ERROR
     * - Conflict type = 'CRANE', severity = 'ERROR'
     * - Conflicting VVN and resource ID are captured
     * Prevents double-booking of cranes
     */
    it('should detect ERROR when same crane number assigned to overlapping operations', () => {
      const op1 = createMockOperation('VVN-001', 8, 12, 2);
      const op2 = createMockOperation('VVN-002', 10, 14, 3);
      const targetPlan = createMockPlan(new Date('2026-01-10'), [op1, op2]);

      // Update op1 to use same crane number as op2
      const updatedOp = op1.withCranes(3);

      const result = service.detectConflicts(
        targetPlan,
        'VVN-001',
        updatedOp,
        [targetPlan]
      );

      expect(result.hasConflicts).toBe(true);
      expect(result.hasErrors).toBe(true);
      expect(result.hasWarnings).toBe(false);
      expect(result.conflicts).toHaveLength(1);

      const conflict = result.conflicts[0];
      expect(conflict).toBeDefined();
      expect(conflict!.type).toBe('CRANE');
      expect(conflict!.severity).toBe('ERROR');
      expect(conflict!.conflictingVvnId).toBe('VVN-002');
      expect(conflict!.resourceId).toBe('CRANE-3');
    });

    /**
     * US 4.1.4: Test crane conflict message generation
     * Verifies that conflict messages include:
     * - Crane number, conflicting VVN ID, vessel IMO
     * Supports clear error reporting for operators
     */
    it('should provide detailed crane conflict message', () => {
      const op1 = createMockOperation('VVN-001', 8, 12, 2);
      const op2 = createMockOperation('VVN-002', 10, 14, 2); // Same crane
      const targetPlan = createMockPlan(new Date('2026-01-10'), [op1, op2]);

      const updatedOp = op1.withCranes(2);

      const result = service.detectConflicts(
        targetPlan,
        'VVN-001',
        updatedOp,
        [targetPlan]
      );

      expect(result.hasConflicts).toBe(true);

      const conflict = result.conflicts[0];
      expect(conflict).toBeDefined();
      expect(conflict!.message).toContain('Crane #2');
      expect(conflict!.message).toContain('VVN-002');
      expect(conflict!.message).toContain('IMO-VVN-002');
    });

    /**
     * US 4.1.4: Test time overlap calculation accuracy
     * Verifies that overlap duration (in minutes) is calculated correctly
     * Includes start time, end time, and duration of the overlap
     */
    it('should calculate correct overlap duration', () => {
      const op1 = createMockOperation('VVN-001', 8, 14, 2); // 6 hours
      const op2 = createMockOperation('VVN-002', 10, 12, 2); // 2 hours, same crane
      const targetPlan = createMockPlan(new Date('2026-01-10'), [op1, op2]);

      const updatedOp = op1.withCranes(2);

      const result = service.detectConflicts(
        targetPlan,
        'VVN-001',
        updatedOp,
        [targetPlan]
      );

      expect(result.hasConflicts).toBe(true);

      const conflict = result.conflicts[0];
      expect(conflict).toBeDefined();
      expect(conflict!.timeOverlap.durationMinutes).toBe(120); // 2 hours
      expect(conflict!.timeOverlap.start).toEqual(new Date('2026-01-10T10:00:00Z'));
      expect(conflict!.timeOverlap.end).toEqual(new Date('2026-01-10T12:00:00Z'));
    });
  });

  describe('detectConflicts - Dock Conflicts', () => {
    /**
     * US 4.1.4: Test dock conflict detection
     * Verifies same dock assigned to overlapping operations generates ERROR
     * Prevents double-booking of docking locations
     */
    it('should detect ERROR when same dock assigned to overlapping operations', () => {
      const op1 = createMockOperation('VVN-001', 8, 12, 2, 'D1');
      const op2 = createMockOperation('VVN-002', 10, 14, 3, 'D2');
      const targetPlan = createMockPlan(new Date('2026-01-10'), [op1, op2]);

      // Update op1 to use same dock as op2
      const updatedOp = op1.withUpdates({ assignedDock: 'D2' });

      const result = service.detectConflicts(
        targetPlan,
        'VVN-001',
        updatedOp,
        [targetPlan]
      );

      expect(result.hasConflicts).toBe(true);
      expect(result.hasErrors).toBe(true);
      expect(result.conflicts).toHaveLength(1);

      const conflict = result.conflicts[0];
      expect(conflict).toBeDefined();
      expect(conflict!.type).toBe('DOCK');
      expect(conflict!.severity).toBe('ERROR');
      expect(conflict!.resourceId).toBe('D2');
    });

    /**
     * US 4.1.4: Test no dock conflict for sequential operations
     * Verifies operations ending/starting at same time don't conflict
     */
    it('should not detect dock conflict when operations do not overlap', () => {
      const op1 = createMockOperation('VVN-001', 8, 10, 2, 'D1');
      const op2 = createMockOperation('VVN-002', 10, 12, 3, 'D1'); // No time overlap
      const targetPlan = createMockPlan(new Date('2026-01-10'), [op1, op2]);

      const updatedOp = op1.withCranes(2);

      const result = service.detectConflicts(
        targetPlan,
        'VVN-001',
        updatedOp,
        [targetPlan]
      );

      expect(result.hasConflicts).toBe(false);
    });

    /**
     * US 4.1.4: Test dock conflict when dock assignment is optional
     * Verifies no conflict when one operation has no dock assigned
     */
    it('should not detect dock conflict when one operation has no dock assigned', () => {
      const op1 = createMockOperation('VVN-001', 8, 12, 2, 'D1');
      const op2 = createMockOperation('VVN-002', 10, 14, 3); // No dock
      const targetPlan = createMockPlan(new Date('2026-01-10'), [op1, op2]);

      const updatedOp = op1.withCranes(2);

      const result = service.detectConflicts(
        targetPlan,
        'VVN-001',
        updatedOp,
        [targetPlan]
      );

      const dockConflicts = result.conflicts.filter(c => c.type === 'DOCK');
      expect(dockConflicts).toHaveLength(0);
    });

    /**
     * US 4.1.4: Test dock conflict message details
     * Verifies that dock conflict messages include dock ID and conflicting VVN
     * Supports clear error reporting for operators identifying dock conflicts
     */
    it('should provide detailed dock conflict message', () => {
      const op1 = createMockOperation('VVN-001', 8, 12, 2, 'D1');
      const op2 = createMockOperation('VVN-002', 10, 14, 3, 'D1');
      const targetPlan = createMockPlan(new Date('2026-01-10'), [op1, op2]);

      const updatedOp = op1.withCranes(2);

      const result = service.detectConflicts(
        targetPlan,
        'VVN-001',
        updatedOp,
        [targetPlan]
      );

      const dockConflict = result.conflicts.find(c => c.type === 'DOCK');
      expect(dockConflict).toBeDefined();
      expect(dockConflict!.message).toContain('Dock D1');
      expect(dockConflict!.message).toContain('VVN-002');
    });
  });

  describe('detectConflicts - Staff Conflicts', () => {
    /**
     * US 4.1.4: Test staff conflict detection with WARNING severity
     * Verifies:
     * - Same staff assigned to overlapping operations generates WARNING (not ERROR)
     * - Conflict type = 'STAFF', severity = 'WARNING'
     * - Staff can be over-assigned (soft constraint) unlike cranes/docks
     */
    it('should detect WARNING when same staff assigned to overlapping operations', () => {
      const op1 = createMockOperation('VVN-001', 8, 12, 2, 'D1', ['STAFF-001', 'STAFF-002']);
      const op2 = createMockOperation('VVN-002', 10, 14, 3, 'D2', ['STAFF-003']);
      const targetPlan = createMockPlan(new Date('2026-01-10'), [op1, op2]);

      // Update op1 to include staff from op2
      const updatedOp = op1.withUpdates({ assignedStaff: ['STAFF-001', 'STAFF-003'] });

      const result = service.detectConflicts(
        targetPlan,
        'VVN-001',
        updatedOp,
        [targetPlan]
      );

      expect(result.hasConflicts).toBe(true);
      expect(result.hasWarnings).toBe(true);
      expect(result.hasErrors).toBe(false);
      expect(result.conflicts.some(c => c.type === 'STAFF')).toBe(true);
      const staffConflict = result.conflicts.find(c => c.type === 'STAFF');
      expect(staffConflict?.severity).toBe('WARNING');
      expect(staffConflict?.resourceId).toContain('STAFF-003');
    });

    /**
     * US 4.1.4: Test no staff conflict for different staff members
     * Verifies that operations with completely different staff assignments
     * do not generate conflicts even with overlapping time windows
     */
    it('should not detect staff conflict when no staff overlap', () => {
      const op1 = createMockOperation('VVN-001', 8, 12, 2, 'D1', ['STAFF-001']);
      const op2 = createMockOperation('VVN-002', 10, 14, 3, 'D2', ['STAFF-002']);
      const targetPlan = createMockPlan(new Date('2026-01-10'), [op1, op2]);

      const updatedOp = op1.withCranes(2);

      const result = service.detectConflicts(
        targetPlan,
        'VVN-001',
        updatedOp,
        [targetPlan]
      );

      const staffConflicts = result.conflicts.filter(c => c.type === 'STAFF');
      expect(staffConflicts).toHaveLength(0);
    });

    /**
     * US 4.1.4: Test detection of multiple simultaneous staff conflicts
     * Verifies that when multiple staff members are assigned to both operations,
     * each overlapping staff member generates a separate conflict warning
     * Example: STAFF-002 and STAFF-003 both assigned to overlapping operations
     */
    it('should handle multiple overlapping staff members', () => {
      const op1 = createMockOperation('VVN-001', 8, 12, 2, 'D1', ['STAFF-001', 'STAFF-002', 'STAFF-003']);
      const op2 = createMockOperation('VVN-002', 10, 14, 3, 'D2', ['STAFF-002', 'STAFF-003', 'STAFF-004']);
      const targetPlan = createMockPlan(new Date('2026-01-10'), [op1, op2]);

      const updatedOp = op1.withCranes(2);

      const result = service.detectConflicts(
        targetPlan,
        'VVN-001',
        updatedOp,
        [targetPlan]
      );

      const staffConflicts = result.conflicts.filter(c => c.type === 'STAFF');
      expect(staffConflicts.length).toBeGreaterThan(0);
      const staffConflict = staffConflicts[0];
      expect(staffConflict).toBeDefined();
      expect(staffConflict!.resourceId).toMatch(/STAFF-002|STAFF-003/);
    });

    /**
     * US 4.1.4: Test no staff conflict when staff assignment is optional
     * Verifies that operations without staff assignments cannot generate
     * staff conflicts (null/undefined staff arrays are handled correctly)
     */
    it('should not detect staff conflict when one operation has no staff', () => {
      const op1 = createMockOperation('VVN-001', 8, 12, 2, 'D1', ['STAFF-001']);
      const op2 = createMockOperation('VVN-002', 10, 14, 3, 'D2'); // No staff
      const targetPlan = createMockPlan(new Date('2026-01-10'), [op1, op2]);

      const updatedOp = op1.withCranes(2);

      const result = service.detectConflicts(
        targetPlan,
        'VVN-001',
        updatedOp,
        [targetPlan]
      );

      const staffConflicts = result.conflicts.filter(c => c.type === 'STAFF');
      expect(staffConflicts).toHaveLength(0);
    });
  });

  describe('detectConflicts - Multiple Conflicts', () => {
    /**
     * US 4.1.4: Test simultaneous detection of multiple conflict types
     * Verifies that service can detect crane, dock, and staff conflicts
     * in a single validation pass when operation has multiple resource overlaps
     * Real scenario: Operator assigns operation to existing crane, dock, and staff
     */
    it('should detect multiple types of conflicts simultaneously', () => {
      const op1 = createMockOperation('VVN-001', 8, 12, 3, 'D1', ['STAFF-001']);
      const op2 = createMockOperation('VVN-002', 10, 14, 3, 'D2', ['STAFF-002']);
      const targetPlan = createMockPlan(new Date('2026-01-10'), [op1, op2]);

      // Update to create both crane, dock, and staff conflicts
      const updatedOp = op1.withUpdates({
        assignedCranes: 3, // Same as op2
        assignedDock: 'D2', // Same as op2
        assignedStaff: ['STAFF-001', 'STAFF-002'], // Overlaps with op2
      });

      const result = service.detectConflicts(
        targetPlan,
        'VVN-001',
        updatedOp,
        [targetPlan]
      );

      expect(result.hasConflicts).toBe(true);
      expect(result.conflicts.length).toBe(3); // Crane + Dock + Staff

      const conflictTypes = result.conflicts.map(c => c.type);
      expect(conflictTypes).toContain('CRANE');
      expect(conflictTypes).toContain('DOCK');
      expect(conflictTypes).toContain('STAFF');
    });

    /**
     * US 4.1.4: Test correct severity classification for mixed conflicts
     * Verifies:
     * - Crane and Dock conflicts are ERROR severity (hard constraints)
     * - Staff conflicts are WARNING severity (soft constraints)
     * - hasErrors and hasWarnings flags are set correctly
     * Operators can save plans with warnings but not with errors
     */
    it('should distinguish between errors and warnings', () => {
      const op1 = createMockOperation('VVN-001', 8, 12, 3, 'D1', ['STAFF-001']);
      const op2 = createMockOperation('VVN-002', 10, 14, 3, 'D1', ['STAFF-001']);
      const targetPlan = createMockPlan(new Date('2026-01-10'), [op1, op2]);

      const updatedOp = op1.withCranes(3); // Crane + Dock error + Staff warning

      const result = service.detectConflicts(
        targetPlan,
        'VVN-001',
        updatedOp,
        [targetPlan]
      );

      expect(result.hasConflicts).toBe(true);
      expect(result.hasErrors).toBe(true); // Crane and Dock conflicts
      expect(result.hasWarnings).toBe(true); // Staff conflict

      const errors = result.conflicts.filter(c => c.severity === 'ERROR');
      const warnings = result.conflicts.filter(c => c.severity === 'WARNING');

      expect(errors.length).toBe(2); // Crane + Dock
      expect(warnings.length).toBe(1); // Staff
    });
  });

  describe('detectConflicts - Multiple Plans', () => {
    /**
     * US 4.1.4: Test cross-plan conflict detection
     * Verifies conflicts are detected across different operation plans
     * for the same target date (e.g., multiple plans generated with
     * different algorithms before selecting which one to use)
     * Ensures global resource consistency across all plans
     */
    it('should check conflicts across multiple plans for the same date', () => {
      const op1 = createMockOperation('VVN-001', 8, 12, 2, 'D1');
      const plan1 = createMockPlan(new Date('2026-01-10'), [op1]);

      const op2 = createMockOperation('VVN-002', 10, 14, 3, 'D2');
      const plan2 = createMockPlan(new Date('2026-01-10'), [op2]);

      // Update to use same crane as op2 in plan2
      const updatedOp = op1.withCranes(3);

      const result = service.detectConflicts(
        plan1,
        'VVN-001',
        updatedOp,
        [plan1, plan2]
      );

      expect(result.hasConflicts).toBe(true);

      const conflict = result.conflicts[0];
      expect(conflict).toBeDefined();
      expect(conflict!.conflictingVvnId).toBe('VVN-002');
    });

    /**
     * US 4.1.4: Test intra-plan conflict detection
     * Verifies conflicts are detected between operations within the same plan
     * Most common scenario: Updating one operation creates conflict with another
     * operation already in the plan
     */
    it('should check conflicts within the same plan (other operations)', () => {
      const op1 = createMockOperation('VVN-001', 8, 12, 2, 'D1');
      const op2 = createMockOperation('VVN-002', 10, 14, 2, 'D2');
      const plan = createMockPlan(new Date('2026-01-10'), [op1, op2]);

      const updatedOp = op1.withCranes(2); // Same crane as op1 currently has

      const result = service.detectConflicts(
        plan,
        'VVN-001',
        updatedOp,
        [plan]
      );

      expect(result.hasConflicts).toBe(true);

      const conflict = result.conflicts[0];
      expect(conflict).toBeDefined();
      expect(conflict!.conflictingVvnId).toBe('VVN-002');
    });
  });

  describe('summary generation', () => {
    /**
     * US 4.1.4: Test summary message for successful validation
     * Verifies that when no conflicts exist, summary clearly states
     * "No conflicts detected" for operator confidence
     */
    it('should generate appropriate summary for no conflicts', () => {
      const op1 = createMockOperation('VVN-001', 8, 10, 2);
      const op2 = createMockOperation('VVN-002', 10, 12, 3);
      const targetPlan = createMockPlan(new Date('2026-01-10'), [op1, op2]);

      const updatedOp = op1.withCranes(2);

      const result = service.detectConflicts(
        targetPlan,
        'VVN-001',
        updatedOp,
        [targetPlan]
      );

      expect(result.summary).toBe('No conflicts detected');
    });

    /**
     * US 4.1.4: Test summary with mixed error and warning counts
     * Verifies summary includes total conflict count and breakdown by severity
     * Example: "Found 3 conflict(s): 2 error(s), 1 warning(s)"
     * Helps operators understand severity of validation failures
     */
    it('should generate summary with conflict counts', () => {
      const op1 = createMockOperation('VVN-001', 8, 12, 3, 'D1', ['STAFF-001']);
      const op2 = createMockOperation('VVN-002', 10, 14, 3, 'D1', ['STAFF-001']);
      const targetPlan = createMockPlan(new Date('2026-01-10'), [op1, op2]);

      const updatedOp = op1.withCranes(3);

      const result = service.detectConflicts(
        targetPlan,
        'VVN-001',
        updatedOp,
        [targetPlan]
      );

      expect(result.summary).toContain('Found 3 conflict(s)');
      expect(result.summary).toContain('2 error(s)');
      expect(result.summary).toContain('1 warning(s)');
    });

    /**
     * US 4.1.4: Test summary for error-only scenarios
     * Verifies summary mentions only errors when no warnings exist
     * Example: "Found 2 conflict(s): 2 error(s)"
     * (no mention of "0 warnings")
     */
    it('should generate summary with only errors', () => {
      const op1 = createMockOperation('VVN-001', 8, 12, 3, 'D1');
      const op2 = createMockOperation('VVN-002', 10, 14, 3, 'D1');
      const targetPlan = createMockPlan(new Date('2026-01-10'), [op1, op2]);

      const updatedOp = op1.withCranes(3);

      const result = service.detectConflicts(
        targetPlan,
        'VVN-001',
        updatedOp,
        [targetPlan]
      );

      expect(result.summary).toContain('2 error(s)');
      expect(result.summary).not.toContain('warning');
    });

    /**
     * US 4.1.4: Test summary for warning-only scenarios
     * Verifies summary mentions only warnings when no errors exist
     * Example: "Found 1 conflict(s): 1 warning(s)"
     * Indicates plan can be saved despite conflicts (soft constraints only)
     */
    it('should generate summary with only warnings', () => {
      const op1 = createMockOperation('VVN-001', 8, 12, 2, undefined, ['STAFF-001']);
      const op2 = createMockOperation('VVN-002', 10, 14, 3, undefined, ['STAFF-001']);
      const targetPlan = createMockPlan(new Date('2026-01-10'), [op1, op2]);

      const updatedOp = op1.withCranes(2);

      const result = service.detectConflicts(
        targetPlan,
        'VVN-001',
        updatedOp,
        [targetPlan]
      );

      expect(result.summary).toContain('1 warning(s)');
      expect(result.summary).not.toContain('error');
    });
  });
});
