import { ExecutedOperation } from '../ExecutedOperation';
import { OperationType, ExecutedOperationStatus } from '@shared/types';

describe('ExecutedOperation Value Object', () => {
  const createValidProps = (): {
    operationType: OperationType;
    startTime: Date;
    containersProcessed: number;
    cranesUsed: number;
    staffAssigned: string[];
    status: ExecutedOperationStatus;
    endTime?: Date;
  } => ({
    operationType: OperationType.UNLOAD,
    startTime: new Date('2025-12-10T10:00:00Z'),
    containersProcessed: 0,
    cranesUsed: 2,
    staffAssigned: ['STAFF-1', 'STAFF-2'],
    status: ExecutedOperationStatus.STARTED,
  });

  describe('Creation', () => {
    it('should create a valid executed operation', () => {
      const props = createValidProps();
      const operation = new ExecutedOperation(props);

      expect(operation.operationType).toBe(OperationType.UNLOAD);
      expect(operation.cranesUsed).toBe(2);
      expect(operation.status).toBe(ExecutedOperationStatus.STARTED);
      expect(operation.containersProcessed).toBe(0);
    });

    it('should reject operation with zero cranes', () => {
      const props = createValidProps();
      props.cranesUsed = 0;

      expect(() => new ExecutedOperation(props)).toThrow('At least one crane must be used');
    });

    it('should reject operation with negative containers', () => {
      const props = createValidProps();
      props.containersProcessed = -10;

      expect(() => new ExecutedOperation(props)).toThrow('Containers processed cannot be negative');
    });

    it('should reject completed operation without end time', () => {
      const props = createValidProps();
      props.status = ExecutedOperationStatus.COMPLETED;

      expect(() => new ExecutedOperation(props)).toThrow(
        'Completed operations must have an end time'
      );
    });

    it('should reject completed operation with zero containers', () => {
      const props = createValidProps();
      props.status = ExecutedOperationStatus.COMPLETED;
      props.endTime = new Date('2025-12-10T14:00:00Z');
      props.containersProcessed = 0;

      expect(() => new ExecutedOperation(props)).toThrow(
        'Completed operations must have processed at least one container'
      );
    });

    it('should allow STARTED operation with zero containers', () => {
      const props = createValidProps();
      props.containersProcessed = 0;
      props.status = ExecutedOperationStatus.STARTED;

      const operation = new ExecutedOperation(props);
      expect(operation.containersProcessed).toBe(0);
    });
  });

  describe('Duration Calculation', () => {
    it('should calculate actual duration for completed operations', () => {
      const props = createValidProps();
      props.startTime = new Date('2025-12-10T10:00:00Z');
      props.endTime = new Date('2025-12-10T14:00:00Z');

      const operation = new ExecutedOperation(props);

      expect(operation.getActualDurationHours()).toBe(4);
    });

    it('should return null for ongoing operations without end time', () => {
      const props = createValidProps();

      const operation = new ExecutedOperation(props);

      expect(operation.getActualDurationHours()).toBeNull();
    });
  });

  describe('Immutability - Completion', () => {
    it('should return new instance when completing operation', () => {
      const props = createValidProps();
      const ongoing = new ExecutedOperation(props);

      const endTime = new Date('2025-12-10T14:00:00Z');
      const completed = ongoing.complete(endTime, 100);

      expect(ongoing.status).toBe(ExecutedOperationStatus.STARTED);
      expect(ongoing.containersProcessed).toBe(0);
      expect(ongoing.endTime).toBeUndefined();

      expect(completed.status).toBe(ExecutedOperationStatus.COMPLETED);
      expect(completed.containersProcessed).toBe(100);
      expect(completed.endTime).toEqual(endTime);

      expect(ongoing).not.toBe(completed);
    });

    it('should reject completion with invalid end time', () => {
      const props = createValidProps();
      const operation = new ExecutedOperation(props);

      const invalidEndTime = new Date('2025-12-10T09:00:00Z'); // Before start

      expect(() => {
        operation.complete(invalidEndTime, 50);
      }).toThrow('End time must be after start time');
    });

    it('should reject completion with zero containers', () => {
      const props = createValidProps();
      const operation = new ExecutedOperation(props);

      expect(() => {
        operation.complete(new Date('2025-12-10T14:00:00Z'), 0);
      }).toThrow('Completed operations must have processed at least one container');
    });
  });

  describe('Immutability - Mark as Delayed', () => {
    it('should return new instance when marking as delayed', () => {
      const props = createValidProps();
      const normal = new ExecutedOperation(props);

      const delayed = normal.markAsDelayed();

      expect(normal.status).toBe(ExecutedOperationStatus.STARTED);
      expect(delayed.status).toBe(ExecutedOperationStatus.DELAYED);
      expect(normal).not.toBe(delayed);
    });
  });

  describe('Performance Metrics', () => {
    it('should calculate containers per hour', () => {
      const props = createValidProps();
      props.startTime = new Date('2025-12-10T10:00:00Z');
      props.endTime = new Date('2025-12-10T14:00:00Z');
      props.containersProcessed = 80;
      props.status = ExecutedOperationStatus.COMPLETED;

      const operation = new ExecutedOperation(props);

      expect(operation.getContainersPerHour()).toBe(20); // 80 containers / 4 hours
    });

    it('should return null for ongoing operations', () => {
      const props = createValidProps();

      const operation = new ExecutedOperation(props);

      expect(operation.getContainersPerHour()).toBeNull();
    });
  });

  describe('Status Checks', () => {
    it('should identify completed operations', () => {
      const completed = new ExecutedOperation({
        ...createValidProps(),
        endTime: new Date('2025-12-10T14:00:00Z'),
        containersProcessed: 50,
        status: ExecutedOperationStatus.COMPLETED,
      });

      expect(completed.isCompleted()).toBe(true);
    });

    it('should identify ongoing operations', () => {
      const ongoing = new ExecutedOperation(createValidProps());

      expect(ongoing.isCompleted()).toBe(false);
    });

    it('should identify delayed operations', () => {
      const delayed = new ExecutedOperation({
        ...createValidProps(),
        status: ExecutedOperationStatus.DELAYED,
      });

      expect(delayed.isDelayed()).toBe(true);
    });
  });

  describe('Serialization', () => {
    it('should serialize to JSON correctly', () => {
      const props = createValidProps();
      const operation = new ExecutedOperation(props);

      const json = operation.toJSON();

      expect(json.operationType).toBe(OperationType.UNLOAD);
      expect(typeof json.startTime).toBe('string'); // ISO string
      expect(json.containersProcessed).toBe(0);
      expect(json.cranesUsed).toBe(2);
      expect(json.status).toBe(ExecutedOperationStatus.STARTED);
    });

    it('should include end time when present', () => {
      const props = createValidProps();
      props.endTime = new Date('2025-12-10T14:00:00Z');

      const operation = new ExecutedOperation(props);
      const json = operation.toJSON();

      expect(typeof json.endTime).toBe('string');
    });
  });
});
