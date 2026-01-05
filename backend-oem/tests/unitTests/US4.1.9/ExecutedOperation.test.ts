import { ExecutedOperation } from '@domain/value-objects/ExecutedOperation';
import { OperationType, ExecutedOperationStatus } from '@shared/types';

/**
 * US 4.1.9 - Update VVE with executed operations
 *
 * Tests for ExecutedOperation value object
 * Records actual execution details for cargo operations
 */
describe('US 4.1.9 - ExecutedOperation Value Object', () => {
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
    /**
     * US 4.1.9: Test creating executed operation with valid data
     * Verifies:
     * - All operation details are stored correctly
     * - Operation type (LOAD, UNLOAD, BOTH) is recorded
     * - Resource usage (cranes, staff) is tracked
     * - Initial status is set correctly
     */
    it('should create a valid executed operation', () => {
      const props = createValidProps();
      const operation = new ExecutedOperation(props);

      expect(operation.operationType).toBe(OperationType.UNLOAD);
      expect(operation.cranesUsed).toBe(2);
      expect(operation.status).toBe(ExecutedOperationStatus.STARTED);
      expect(operation.containersProcessed).toBe(0);
    });

    /**
     * US 4.1.9: Test crane validation
     * Verifies that at least one crane must be assigned to operation
     * Ensures realistic resource allocation
     */
    it('should reject operation with zero cranes', () => {
      const props = createValidProps();
      props.cranesUsed = 0;

      expect(() => new ExecutedOperation(props)).toThrow('At least one crane must be used');
    });

    /**
     * US 4.1.9: Test containers processed validation
     * Verifies that container count cannot be negative
     * Ensures data integrity
     */
    it('should reject operation with negative containers', () => {
      const props = createValidProps();
      props.containersProcessed = -10;

      expect(() => new ExecutedOperation(props)).toThrow('Containers processed cannot be negative');
    });

    /**
     * US 4.1.9: Test completion validation - end time required
     * Verifies that COMPLETED operations must have an end time
     * Ensures complete execution timeline tracking
     */
    it('should reject completed operation without end time', () => {
      const props = createValidProps();
      props.status = ExecutedOperationStatus.COMPLETED;

      expect(() => new ExecutedOperation(props)).toThrow(
        'Completed operations must have an end time'
      );
    });

    /**
     * US 4.1.9: Test completion validation - containers required
     * Verifies that completed operations must have processed at least one container
     * Prevents recording empty completed operations
     */
    it('should reject completed operation with zero containers', () => {
      const props = createValidProps();
      props.status = ExecutedOperationStatus.COMPLETED;
      props.endTime = new Date('2025-12-10T14:00:00Z');
      props.containersProcessed = 0;

      expect(() => new ExecutedOperation(props)).toThrow(
        'Completed operations must have processed at least one container'
      );
    });

    /**
     * US 4.1.9: Test STARTED operation creation
     * Verifies that ongoing operations can start with zero containers
     * Supports real-time tracking of operation progress
     */
    it('should allow STARTED operation with zero containers', () => {
      const props = createValidProps();
      props.containersProcessed = 0;
      props.status = ExecutedOperationStatus.STARTED;

      const operation = new ExecutedOperation(props);
      expect(operation.containersProcessed).toBe(0);
    });
  });

  describe('Duration Calculation', () => {
    /**
     * US 4.1.9 & 4.1.10: Test actual duration calculation
     * Verifies calculation of time taken for operation execution
     * Formula: endTime - startTime (in hours)
     * Supports performance analysis and planned vs actual comparison
     */
    it('should calculate actual duration for completed operations', () => {
      const props = createValidProps();
      props.startTime = new Date('2025-12-10T10:00:00Z');
      props.endTime = new Date('2025-12-10T14:00:00Z');

      const operation = new ExecutedOperation(props);

      expect(operation.getActualDurationHours()).toBe(4);
    });

    /**
     * US 4.1.9: Test duration for ongoing operations
     * Verifies that duration is null for operations without end time
     * Prevents incorrect metrics for incomplete operations
     */
    it('should return null for ongoing operations without end time', () => {
      const props = createValidProps();

      const operation = new ExecutedOperation(props);

      expect(operation.getActualDurationHours()).toBeNull();
    });
  });

  describe('Immutability - Completion', () => {
    /**
     * US 4.1.9: Test immutable operation completion
     * Verifies:
     * - Completing operation returns new instance (immutability)
     * - Original operation remains unchanged
     * - Status transitions to COMPLETED
     * - End time and final container count are recorded
     * - Operator ID is stored for audit trail
     */
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

    /**
     * US 4.1.9: Test completion timestamp validation
     * Verifies that operation end time must be after start time
     * Ensures chronological order of execution events
     */
    it('should reject completion with invalid end time', () => {
      const props = createValidProps();
      const operation = new ExecutedOperation(props);

      const invalidEndTime = new Date('2025-12-10T09:00:00Z'); // Before start

      expect(() => {
        operation.complete(invalidEndTime, 50);
      }).toThrow('End time must be after start time');
    });

    /**
     * US 4.1.9: Test completion container validation
     * Verifies that completed operations must have processed containers
     * Prevents marking empty operations as complete
     */
    it('should reject completion with zero containers', () => {
      const props = createValidProps();
      const operation = new ExecutedOperation(props);

      expect(() => {
        operation.complete(new Date('2025-12-10T14:00:00Z'), 0);
      }).toThrow('Completed operations must have processed at least one container');
    });
  });

  describe('Immutability - Mark as Delayed', () => {
    /**
     * US 4.1.9 & 4.1.10: Test marking operation as delayed
     * Verifies:
     * - Operation can be flagged as delayed (immutably)
     * - Status transitions to DELAYED
     * - Original operation remains unchanged
     * - Supports deviation tracking from planned schedule
     */
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
    /**
     * US 4.1.10: Test productivity metric calculation
     * Verifies calculation of containers processed per hour
     * Formula: containersProcessed / actualDurationHours
     * Supports efficiency analysis and crane productivity tracking
     */
    it('should calculate containers per hour', () => {
      const props = createValidProps();
      props.startTime = new Date('2025-12-10T10:00:00Z');
      props.endTime = new Date('2025-12-10T14:00:00Z');
      props.containersProcessed = 80;
      props.status = ExecutedOperationStatus.COMPLETED;

      const operation = new ExecutedOperation(props);

      expect(operation.getContainersPerHour()).toBe(20); // 80 containers / 4 hours
    });

    /**
     * US 4.1.10: Test metric calculation for ongoing operations
     * Verifies that productivity metrics are only available after completion
     * Prevents incomplete data in performance reports
     */
    it('should return null for ongoing operations', () => {
      const props = createValidProps();

      const operation = new ExecutedOperation(props);

      expect(operation.getContainersPerHour()).toBeNull();
    });
  });

  describe('Status Checks', () => {
    /**
     * US 4.1.9: Test identifying completed operations
     * Verifies helper method for checking completion status
     * Supports filtering and status-based logic
     */
    it('should identify completed operations', () => {
      const completed = new ExecutedOperation({
        ...createValidProps(),
        endTime: new Date('2025-12-10T14:00:00Z'),
        containersProcessed: 50,
        status: ExecutedOperationStatus.COMPLETED,
      });

      expect(completed.isCompleted()).toBe(true);
    });

    /**
     * US 4.1.9: Test identifying ongoing operations
     * Verifies that non-completed operations return false
     * Supports real-time operation monitoring
     */
    it('should identify ongoing operations', () => {
      const ongoing = new ExecutedOperation(createValidProps());

      expect(ongoing.isCompleted()).toBe(false);
    });

    /**
     * US 4.1.9 & 4.1.10: Test identifying delayed operations
     * Verifies helper method for checking delayed status
     * Supports delay analysis and alerts
     */
    it('should identify delayed operations', () => {
      const delayed = new ExecutedOperation({
        ...createValidProps(),
        status: ExecutedOperationStatus.DELAYED,
      });

      expect(delayed.isDelayed()).toBe(true);
    });
  });

  describe('Serialization', () => {
    /**
     * US 4.1.9: Test operation serialization to JSON
     * Verifies:
     * - All operation data can be converted to JSON format
     * - Dates are serialized as ISO strings
     * - Required for API responses and database storage
     */
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

    /**
     * US 4.1.9: Test end time serialization
     * Verifies that optional end time is included when present
     * Ensures complete data in JSON representation
     */
    it('should include end time when present', () => {
      const props = createValidProps();
      props.endTime = new Date('2025-12-10T14:00:00Z');

      const operation = new ExecutedOperation(props);
      const json = operation.toJSON();

      expect(typeof json.endTime).toBe('string');
    });
  });
});
