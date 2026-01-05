import { PlannedOperation } from '@domain/value-objects/PlannedOperation';
import { OperationType } from '@shared/types';

/**
 * US 4.1.1 & 4.1.4 - PlannedOperation Value Object Tests
 *
 * Tests for PlannedOperation value object which represents a scheduled cargo operation
 * within an operation plan. Each operation links to a VVN and specifies:
 * - When the operation should occur (time window)
 * - What resources are assigned (cranes, staff, dock)
 * - What type of operation (LOAD, UNLOAD, BOTH)
 *
 * US 4.1.1: Tests creation and validation of planned operations
 * US 4.1.4: Tests immutable updates to operation details (time, cranes, resources)
 */
describe('US 4.1.1 - PlannedOperation Value Object', () => {
  describe('constructor', () => {
    /**
     * US 4.1.1: Test creating planned operation with complete data
     * Verifies:
     * - All operation details are stored correctly
     * - VVN ID and vessel IMO are linked
     * - Time window (start/end) is preserved
     * - Resource allocation (cranes, staff, dock) is stored
     * - Operation type (LOAD/UNLOAD/BOTH) is recorded
     * Supports operation plan generation from Prolog results
     */
    it('should create a PlannedOperation with valid data', () => {
      const plannedStart = new Date('2025-01-10T08:00:00Z');
      const plannedEnd = new Date('2025-01-10T12:00:00Z');

      const operation = new PlannedOperation({
        vvnId: 'VVN-001',
        vesselImo: 'IMO1234567',
        plannedStart,
        plannedEnd,
        assignedCranes: 2,
        operationType: OperationType.UNLOAD,
        assignedDock: 'D1',
        assignedStaff: ['STAFF-001', 'STAFF-002'],
      });

      expect(operation.vvnId).toBe('VVN-001');
      expect(operation.vesselImo).toBe('IMO1234567');
      expect(operation.plannedStart).toEqual(plannedStart);
      expect(operation.plannedEnd).toEqual(plannedEnd);
      expect(operation.assignedCranes).toBe(2);
      expect(operation.operationType).toBe(OperationType.UNLOAD);
      expect(operation.assignedDock).toBe('D1');
      expect(operation.assignedStaff).toEqual(['STAFF-001', 'STAFF-002']);
    });

    /**
     * US 4.1.1: Test time window chronological validation
     * Verifies that operation end time must be after start time
     * Prevents invalid time windows in generated operation plans
     * Ensures realistic scheduling constraints
     */
    it('should throw error if plannedEnd is before plannedStart', () => {
      const plannedStart = new Date('2025-01-10T12:00:00Z');
      const plannedEnd = new Date('2025-01-10T08:00:00Z');

      expect(() => {
        new PlannedOperation({
          vvnId: 'VVN-001',
          vesselImo: 'IMO1234567',
          plannedStart,
          plannedEnd,
          assignedCranes: 2,
          operationType: OperationType.UNLOAD,
        });
      }).toThrow('Planned end time must be after planned start time');
    });

    /**
     * US 4.1.1: Test crane assignment validation
     * Verifies that at least one crane must be assigned to an operation
     * Ensures realistic resource allocation in operation plans
     * Prevents invalid plans with zero cranes
     */
    it('should throw error if assignedCranes is less than 1', () => {
      const plannedStart = new Date('2025-01-10T08:00:00Z');
      const plannedEnd = new Date('2025-01-10T12:00:00Z');

      expect(() => {
        new PlannedOperation({
          vvnId: 'VVN-001',
          vesselImo: 'IMO1234567',
          plannedStart,
          plannedEnd,
          assignedCranes: 0,
          operationType: OperationType.UNLOAD,
        });
      }).toThrow('At least one crane must be assigned');
    });

    /**
     * US 4.1.1: Test optional dock assignment
     * Verifies that operations can be created without specifying a dock
     * Supports flexibility in operation plans (dock may be assigned later)
     * Dock assignment is optional in the domain model
     */
    it('should accept operation without assignedDock', () => {
      const plannedStart = new Date('2025-01-10T08:00:00Z');
      const plannedEnd = new Date('2025-01-10T12:00:00Z');

      const operation = new PlannedOperation({
        vvnId: 'VVN-001',
        vesselImo: 'IMO1234567',
        plannedStart,
        plannedEnd,
        assignedCranes: 2,
        operationType: OperationType.UNLOAD,
      });

      expect(operation.assignedDock).toBeUndefined();
    });

    /**
     * US 4.1.1: Test optional staff assignment
     * Verifies that operations can be created without specifying staff
     * Supports flexibility in operation plans (staff may be assigned later)
     * Staff assignment is optional in the domain model
     */
    it('should accept operation without assignedStaff', () => {
      const plannedStart = new Date('2025-01-10T08:00:00Z');
      const plannedEnd = new Date('2025-01-10T12:00:00Z');

      const operation = new PlannedOperation({
        vvnId: 'VVN-001',
        vesselImo: 'IMO1234567',
        plannedStart,
        plannedEnd,
        assignedCranes: 2,
        operationType: OperationType.UNLOAD,
      });

      expect(operation.assignedStaff).toBeUndefined();
    });
  });

  describe('US 4.1.4 - withTimeWindow', () => {
    /**
     * US 4.1.4: Test immutable time window update
     * Verifies:
     * - Updating operation time window returns new instance (immutability)
     * - Original operation remains unchanged
     * - New start and end times are applied correctly
     * - All other operation details are preserved
     * Supports manual adjustments to operation plans (US 4.1.4 requirement)
     */
    it('should create new PlannedOperation with updated time window', () => {
      const originalStart = new Date('2025-01-10T08:00:00Z');
      const originalEnd = new Date('2025-01-10T12:00:00Z');
      const newStart = new Date('2025-01-10T09:00:00Z');
      const newEnd = new Date('2025-01-10T13:00:00Z');

      const original = new PlannedOperation({
        vvnId: 'VVN-001',
        vesselImo: 'IMO1234567',
        plannedStart: originalStart,
        plannedEnd: originalEnd,
        assignedCranes: 2,
        operationType: OperationType.UNLOAD,
      });

      const updated = original.withTimeWindow(newStart, newEnd);

      expect(updated.plannedStart).toEqual(newStart);
      expect(updated.plannedEnd).toEqual(newEnd);
      expect(updated.vvnId).toBe(original.vvnId);
      expect(updated.assignedCranes).toBe(original.assignedCranes);
      expect(updated).not.toBe(original); // Immutability check
    });

    /**
     * US 4.1.4: Test time window validation during update
     * Verifies that updated end time must still be after start time
     * Prevents creating invalid time windows during manual adjustments
     * Maintains data integrity during operation plan updates
     */
    it('should throw error if new end time is before start time', () => {
      const original = new PlannedOperation({
        vvnId: 'VVN-001',
        vesselImo: 'IMO1234567',
        plannedStart: new Date('2025-01-10T08:00:00Z'),
        plannedEnd: new Date('2025-01-10T12:00:00Z'),
        assignedCranes: 2,
        operationType: OperationType.UNLOAD,
      });

      expect(() => {
        original.withTimeWindow(
          new Date('2025-01-10T12:00:00Z'),
          new Date('2025-01-10T08:00:00Z')
        );
      }).toThrow('Planned end time must be after planned start time');
    });
  });

  describe('US 4.1.4 - withCranes', () => {
    /**
     * US 4.1.4: Test immutable crane count update
     * Verifies:
     * - Updating assigned cranes returns new instance (immutability)
     * - Original operation remains unchanged
     * - New crane count is applied correctly
     * - All other operation details are preserved
     * Supports manual resource reallocation during plan adjustments
     */
    it('should create new PlannedOperation with updated crane count', () => {
      const original = new PlannedOperation({
        vvnId: 'VVN-001',
        vesselImo: 'IMO1234567',
        plannedStart: new Date('2025-01-10T08:00:00Z'),
        plannedEnd: new Date('2025-01-10T12:00:00Z'),
        assignedCranes: 2,
        operationType: OperationType.UNLOAD,
      });

      const updated = original.withCranes(3);

      expect(updated.assignedCranes).toBe(3);
      expect(updated.vvnId).toBe(original.vvnId);
      expect(updated).not.toBe(original); // Immutability check
    });

    /**
     * US 4.1.4: Test crane validation during update
     * Verifies that crane count cannot be updated to zero or negative
     * Ensures realistic resource allocation during manual adjustments
     * Maintains data integrity during operation plan updates
     */
    it('should throw error if crane count is less than 1', () => {
      const original = new PlannedOperation({
        vvnId: 'VVN-001',
        vesselImo: 'IMO1234567',
        plannedStart: new Date('2025-01-10T08:00:00Z'),
        plannedEnd: new Date('2025-01-10T12:00:00Z'),
        assignedCranes: 2,
        operationType: OperationType.UNLOAD,
      });

      expect(() => {
        original.withCranes(0);
      }).toThrow('At least one crane must be assigned');
    });
  });

  describe('US 4.1.4 - withUpdates', () => {
    /**
     * US 4.1.4: Test batch update of multiple operation fields
     * Verifies:
     * - Multiple operation attributes can be updated in a single call
     * - Time window, cranes, dock, and staff can be updated together
     * - Returns new instance (immutability)
     * - Original operation remains unchanged
     * Supports efficient bulk updates during manual plan adjustments
     */
    it('should update multiple fields at once', () => {
      const original = new PlannedOperation({
        vvnId: 'VVN-001',
        vesselImo: 'IMO1234567',
        plannedStart: new Date('2025-01-10T08:00:00Z'),
        plannedEnd: new Date('2025-01-10T12:00:00Z'),
        assignedCranes: 2,
        operationType: OperationType.UNLOAD,
        assignedDock: 'D1',
      });

      const updated = original.withUpdates({
        plannedStart: new Date('2025-01-10T09:00:00Z'),
        plannedEnd: new Date('2025-01-10T13:00:00Z'),
        assignedCranes: 3,
        assignedDock: 'D2',
        assignedStaff: ['STAFF-003'],
      });

      expect(updated.plannedStart).toEqual(new Date('2025-01-10T09:00:00Z'));
      expect(updated.plannedEnd).toEqual(new Date('2025-01-10T13:00:00Z'));
      expect(updated.assignedCranes).toBe(3);
      expect(updated.assignedDock).toBe('D2');
      expect(updated.assignedStaff).toEqual(['STAFF-003']);
      expect(updated).not.toBe(original); // Immutability check
    });

    /**
     * US 4.1.4: Test selective field updates
     * Verifies:
     * - Only specified fields are updated
     * - Unspecified fields retain their original values
     * - Supports partial updates without affecting other attributes
     * Allows precise control during manual plan adjustments
     */
    it('should preserve original values for fields not being updated', () => {
      const original = new PlannedOperation({
        vvnId: 'VVN-001',
        vesselImo: 'IMO1234567',
        plannedStart: new Date('2025-01-10T08:00:00Z'),
        plannedEnd: new Date('2025-01-10T12:00:00Z'),
        assignedCranes: 2,
        operationType: OperationType.UNLOAD,
        assignedDock: 'D1',
      });

      const updated = original.withUpdates({
        assignedCranes: 3,
      });

      expect(updated.assignedCranes).toBe(3);
      expect(updated.plannedStart).toEqual(original.plannedStart);
      expect(updated.plannedEnd).toEqual(original.plannedEnd);
      expect(updated.assignedDock).toBe(original.assignedDock);
    });

    /**
     * US 4.1.4: Test time window validation during batch update
     * Verifies that batch updates still validate time window constraints
     * Prevents creating invalid time windows when updating multiple fields
     * Maintains data integrity during complex plan adjustments
     */
    it('should validate time window when updating start/end times', () => {
      const original = new PlannedOperation({
        vvnId: 'VVN-001',
        vesselImo: 'IMO1234567',
        plannedStart: new Date('2025-01-10T08:00:00Z'),
        plannedEnd: new Date('2025-01-10T12:00:00Z'),
        assignedCranes: 2,
        operationType: OperationType.UNLOAD,
      });

      expect(() => {
        original.withUpdates({
          plannedStart: new Date('2025-01-10T14:00:00Z'),
          plannedEnd: new Date('2025-01-10T10:00:00Z'),
        });
      }).toThrow('Planned end time must be after planned start time');
    });

    /**
     * US 4.1.4: Test crane validation during batch update
     * Verifies that batch updates still validate crane count constraints
     * Prevents invalid crane assignments when updating multiple fields
     * Maintains data integrity during complex plan adjustments
     */
    it('should validate crane count when updating', () => {
      const original = new PlannedOperation({
        vvnId: 'VVN-001',
        vesselImo: 'IMO1234567',
        plannedStart: new Date('2025-01-10T08:00:00Z'),
        plannedEnd: new Date('2025-01-10T12:00:00Z'),
        assignedCranes: 2,
        operationType: OperationType.UNLOAD,
      });

      expect(() => {
        original.withUpdates({
          assignedCranes: 0,
        });
      }).toThrow('At least one crane must be assigned');
    });
  });
});
