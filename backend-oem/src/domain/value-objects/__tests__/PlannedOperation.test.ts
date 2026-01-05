import { PlannedOperation } from '../PlannedOperation';
import { OperationType } from '@shared/types';

describe('PlannedOperation Value Object', () => {
  const createValidProps = () => {
    const start = new Date('2025-12-10T08:00:00Z');
    const end = new Date('2025-12-10T12:00:00Z');

    return {
      vvnId: 'VVN-001',
      vesselImo: 'IMO1234567',
      plannedStart: start,
      plannedEnd: end,
      assignedCranes: 2,
      operationType: OperationType.BOTH,
    };
  };

  describe('Creation', () => {
    it('should create a valid planned operation', () => {
      const props = createValidProps();
      const operation = new PlannedOperation(props);

      expect(operation.vvnId).toBe('VVN-001');
      expect(operation.vesselImo).toBe('IMO1234567');
      expect(operation.assignedCranes).toBe(2);
      expect(operation.operationType).toBe(OperationType.BOTH);
    });

    it('should reject operation without VVN ID', () => {
      const props = createValidProps();
      props.vvnId = '';

      expect(() => new PlannedOperation(props)).toThrow('VVN ID is required');
    });

    it('should reject operation with invalid time range', () => {
      const props = createValidProps();
      props.plannedEnd = new Date('2025-12-10T06:00:00Z'); // Before start

      expect(() => new PlannedOperation(props)).toThrow(
        'Planned end time must be after planned start time'
      );
    });

    it('should reject operation with zero cranes', () => {
      const props = createValidProps();
      props.assignedCranes = 0;

      expect(() => new PlannedOperation(props)).toThrow('At least one crane must be assigned');
    });

    it('should reject operation with negative cranes', () => {
      const props = createValidProps();
      props.assignedCranes = -1;

      expect(() => new PlannedOperation(props)).toThrow('At least one crane must be assigned');
    });
  });

  describe('Duration Calculation', () => {
    it('should calculate operation duration in hours', () => {
      const props = createValidProps();
      const operation = new PlannedOperation(props);

      expect(operation.getDurationHours()).toBe(4); // 08:00 to 12:00
    });

    it('should calculate fractional hours correctly', () => {
      const props = createValidProps();
      props.plannedStart = new Date('2025-12-10T08:00:00Z');
      props.plannedEnd = new Date('2025-12-10T10:30:00Z');

      const operation = new PlannedOperation(props);

      expect(operation.getDurationHours()).toBe(2.5);
    });
  });

  describe('Immutability', () => {
    it('should return new instance when updating dock', () => {
      const props = createValidProps();
      const original = new PlannedOperation(props);

      const updated = original.withDock('DOCK-A');

      expect(original.assignedDock).toBeUndefined();
      expect(updated.assignedDock).toBe('DOCK-A');
      expect(original).not.toBe(updated);
    });

    it('should return new instance when updating staff', () => {
      const props = createValidProps();
      const original = new PlannedOperation(props);

      const updated = original.withStaff(['STAFF-1', 'STAFF-2']);

      expect(original.assignedStaff).toBeUndefined();
      expect(updated.assignedStaff).toEqual(['STAFF-1', 'STAFF-2']);
      expect(original).not.toBe(updated);
    });
  });

  describe('Equality', () => {
    it('should be equal when all fields match', () => {
      const props = createValidProps();
      const op1 = new PlannedOperation(props);
      const op2 = new PlannedOperation(props);

      expect(op1.equals(op2)).toBe(true);
    });

    it('should not be equal when VVN IDs differ', () => {
      const props1 = createValidProps();
      const props2 = { ...createValidProps(), vvnId: 'VVN-002' };

      const op1 = new PlannedOperation(props1);
      const op2 = new PlannedOperation(props2);

      expect(op1.equals(op2)).toBe(false);
    });

    it('should not be equal when times differ', () => {
      const props1 = createValidProps();
      const props2 = createValidProps();
      props2.plannedStart = new Date('2025-12-10T09:00:00Z');

      const op1 = new PlannedOperation(props1);
      const op2 = new PlannedOperation(props2);

      expect(op1.equals(op2)).toBe(false);
    });

    it('should not be equal when crane count differs', () => {
      const props1 = createValidProps();
      const props2 = { ...createValidProps(), assignedCranes: 3 };

      const op1 = new PlannedOperation(props1);
      const op2 = new PlannedOperation(props2);

      expect(op1.equals(op2)).toBe(false);
    });
  });

  describe('Serialization', () => {
    it('should serialize to JSON correctly', () => {
      const props = createValidProps();
      const operation = new PlannedOperation(props);

      const json = operation.toJSON();

      expect(json.vvnId).toBe('VVN-001');
      expect(json.vesselImo).toBe('IMO1234567');
      expect(typeof json.plannedStart).toBe('string'); // ISO string
      expect(typeof json.plannedEnd).toBe('string'); // ISO string
      expect(json.assignedCranes).toBe(2);
    });

    it('should include optional fields in JSON when present', () => {
      const props = createValidProps();
      const operation = new PlannedOperation(props).withDock('DOCK-A').withStaff(['STAFF-1']);

      const json = operation.toJSON();

      expect(json.assignedDock).toBe('DOCK-A');
      expect(json.assignedStaff).toEqual(['STAFF-1']);
    });
  });
});
