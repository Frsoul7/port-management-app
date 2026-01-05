import { VesselVisitExecution } from '@domain/entities/VesselVisitExecution';
import { ExecutedOperation } from '@domain/value-objects/ExecutedOperation';
import { VesselVisitExecutionStatus, OperationType, ExecutedOperationStatus } from '@shared/types';

/**
 * US 4.1.7 - Create Vessel Visit Execution (VVE)
 * US 4.1.8 - Update VVE with berthing information
 * US 4.1.9 - Update VVE with executed operations
 * US 4.1.11 - Mark VVE as completed
 *
 * Tests for VesselVisitExecution entity lifecycle and business logic
 */
describe('US 4.1.7-4.1.11 - VesselVisitExecution Entity', () => {
  describe('Creation', () => {
    /**
     * US 4.1.7: Test VVE creation with valid VVN reference
     * Verifies:
     * - Automatic VVE ID generation
     * - Initial status = PLANNED
     * - VVN ID is stored correctly
     * - Empty operations and incidents arrays initialized
     */
    it('should create a valid VVE in PLANNED status', () => {
      const vve = new VesselVisitExecution({
        vvnId: 'VVN-001',
      });

      expect(vve.vveId).toBeDefined();
      expect(vve.vvnId).toBe('VVN-001');
      expect(vve.status).toBe(VesselVisitExecutionStatus.PLANNED);
      expect(vve.operations).toHaveLength(0);
      expect(vve.incidents).toHaveLength(0);
    });

    /**
     * US 4.1.7: Test VVE creation validation
     * Verifies that VVN ID is required for VVE creation
     */
    it('should reject VVE without VVN ID', () => {
      expect(() => {
        new VesselVisitExecution({
          vvnId: '',
        });
      }).toThrow('VVN ID is required');
    });

    /**
     * US 4.1.7 & 4.1.8: Test timestamp sequence validation
     * Verifies that berth time must occur after port arrival time
     * Ensures data integrity for execution timeline
     */
    it('should validate timestamp sequence', () => {
      const arrivalTime = new Date('2025-12-10T08:00:00Z');
      const berthTime = new Date('2025-12-10T07:00:00Z'); // Before arrival - invalid

      expect(() => {
        new VesselVisitExecution({
          vvnId: 'VVN-001',
          actualPortArrivalTime: arrivalTime,
          actualBerthTime: berthTime,
        });
      }).toThrow('Berth time must be after port arrival time');
    });
  });

  describe('Lifecycle Management', () => {
    /**
     * US 4.1.7: Test recording port arrival
     * Verifies:
     * - Actual arrival time is recorded
     * - Status transitions from PLANNED to IN_PROGRESS
     * - VVE becomes active for operation recording
     */
    it('should record port arrival and transition to IN_PROGRESS', () => {
      const vve = new VesselVisitExecution({
        vvnId: 'VVN-001',
      });

      const arrivalTime = new Date('2025-12-10T08:00:00Z');
      vve.recordPortArrival(arrivalTime);

      expect(vve.actualPortArrivalTime).toEqual(arrivalTime);
      expect(vve.status).toBe(VesselVisitExecutionStatus.IN_PROGRESS);
    });

    /**
     * US 4.1.8: Test berthing information recording
     * Verifies:
     * - Actual berth time is recorded
     * - Assigned dock is stored
     * - Allows tracking of dock usage vs plan
     */
    it('should record berthing with dock assignment', () => {
      const vve = new VesselVisitExecution({
        vvnId: 'VVN-001',
        actualPortArrivalTime: new Date('2025-12-10T08:00:00Z'),
        status: VesselVisitExecutionStatus.IN_PROGRESS,
      });

      const berthTime = new Date('2025-12-10T09:00:00Z');
      vve.recordBerthing(berthTime, 'DOCK-A');

      expect(vve.actualBerthTime).toEqual(berthTime);
      expect(vve.assignedDock).toBe('DOCK-A');
    });

    /**
     * US 4.1.8: Test berthing validation - port arrival required
     * Verifies business rule that vessel must arrive at port before berthing
     */
    it('should reject berthing without port arrival', () => {
      const vve = new VesselVisitExecution({
        vvnId: 'VVN-001',
        status: VesselVisitExecutionStatus.IN_PROGRESS,
      });

      expect(() => {
        vve.recordBerthing(new Date(), 'DOCK-A');
      }).toThrow('Cannot record berthing without port arrival time');
    });

    /**
     * US 4.1.8: Test berthing timestamp validation
     * Verifies that berth time cannot be before port arrival time
     * Ensures chronological order of execution events
     */
    it('should reject berthing with invalid timestamp', () => {
      const arrivalTime = new Date('2025-12-10T08:00:00Z');
      const vve = new VesselVisitExecution({
        vvnId: 'VVN-001',
        actualPortArrivalTime: arrivalTime,
        status: VesselVisitExecutionStatus.IN_PROGRESS,
      });

      const berthTime = new Date('2025-12-10T07:00:00Z'); // Before arrival

      expect(() => {
        vve.recordBerthing(berthTime, 'DOCK-A');
      }).toThrow('Berth time must be after port arrival time');
    });
  });

  describe('Operation Management', () => {
    /**
     * US 4.1.9: Test adding executed operation to VVE
     * Verifies:
     * - Operations can be added when VVE is IN_PROGRESS
     * - Operation details are stored correctly
     * - Operation type, resources, and staff are tracked
     */
    it('should add executed operation when IN_PROGRESS', () => {
      const vve = new VesselVisitExecution({
        vvnId: 'VVN-001',
        status: VesselVisitExecutionStatus.IN_PROGRESS,
      });

      const operation = new ExecutedOperation({
        operationType: OperationType.UNLOAD,
        startTime: new Date('2025-12-10T10:00:00Z'),
        containersProcessed: 0,
        cranesUsed: 2,
        staffAssigned: ['STAFF-1', 'STAFF-2'],
        status: ExecutedOperationStatus.STARTED,
      });

      vve.addExecutedOperation(operation);

      expect(vve.operations).toHaveLength(1);
      expect(vve.operations[0]?.operationType).toBe(OperationType.UNLOAD);
    });

    /**
     * US 4.1.9: Test operation addition validation
     * Verifies that operations can only be added when VVE is IN_PROGRESS
     * Prevents recording operations for planned or completed visits
     */
    it('should reject adding operations when not IN_PROGRESS', () => {
      const vve = new VesselVisitExecution({
        vvnId: 'VVN-001',
        status: VesselVisitExecutionStatus.PLANNED,
      });

      const operation = new ExecutedOperation({
        operationType: OperationType.UNLOAD,
        startTime: new Date(),
        containersProcessed: 0,
        cranesUsed: 1,
        staffAssigned: [],
        status: ExecutedOperationStatus.STARTED,
      });

      expect(() => {
        vve.addExecutedOperation(operation);
      }).toThrow('Cannot add operation for VVE with status PLANNED');
    });
  });

  describe('Completion', () => {
    /**
     * US 4.1.11: Test VVE completion
     * Verifies:
     * - VVE can be marked as completed with all required data
     * - Port departure time is recorded
     * - User who completed the VVE is tracked
     * - Completion timestamp is automatically set
     * - Status transitions to COMPLETED
     */
    it('should complete VVE when all operations are done', () => {
      const vve = new VesselVisitExecution({
        vvnId: 'VVN-001',
        actualPortArrivalTime: new Date('2025-12-10T08:00:00Z'),
        actualBerthTime: new Date('2025-12-10T09:00:00Z'),
        actualUnberthTime: new Date('2025-12-10T15:00:00Z'),
        status: VesselVisitExecutionStatus.IN_PROGRESS,
      });

      const completedOp = new ExecutedOperation({
        operationType: OperationType.BOTH,
        startTime: new Date('2025-12-10T10:00:00Z'),
        endTime: new Date('2025-12-10T14:00:00Z'),
        containersProcessed: 100,
        cranesUsed: 2,
        staffAssigned: [],
        status: ExecutedOperationStatus.COMPLETED,
      });

      vve.addExecutedOperation(completedOp);

      const departureTime = new Date('2025-12-10T16:00:00Z');
      vve.markAsCompleted(departureTime, 'port-operator');

      expect(vve.status).toBe(VesselVisitExecutionStatus.COMPLETED);
      expect(vve.actualPortDepartureTime).toEqual(departureTime);
      expect(vve.completedBy).toBe('port-operator');
      expect(vve.completedAt).toBeDefined();
    });

    /**
     * US 4.1.11: Test completion validation - unberth time required
     * Verifies that VVE cannot be completed without recording unberth time
     * Ensures complete timeline tracking
     */
    it('should reject completion without unberth time', () => {
      const vve = new VesselVisitExecution({
        vvnId: 'VVN-001',
        status: VesselVisitExecutionStatus.IN_PROGRESS,
      });

      expect(() => {
        vve.markAsCompleted(new Date(), 'port-operator');
      }).toThrow('Cannot complete VVE without unberth time');
    });

    /**
     * US 4.1.11: Test completion validation - all operations must be finished
     * Verifies that VVE cannot be completed if any operations are still in progress
     * Ensures data integrity and accurate execution tracking
     */
    it('should reject completion with incomplete operations', () => {
      const vve = new VesselVisitExecution({
        vvnId: 'VVN-001',
        actualUnberthTime: new Date('2025-12-10T15:00:00Z'),
        status: VesselVisitExecutionStatus.IN_PROGRESS,
      });

      const incompleteOp = new ExecutedOperation({
        operationType: OperationType.UNLOAD,
        startTime: new Date('2025-12-10T10:00:00Z'),
        containersProcessed: 0,
        cranesUsed: 1,
        staffAssigned: [],
        status: ExecutedOperationStatus.STARTED,
      });

      vve.addExecutedOperation(incompleteOp);

      expect(() => {
        vve.markAsCompleted(new Date('2025-12-10T16:00:00Z'), 'port-operator');
      }).toThrow('Cannot complete VVE with incomplete operations');
    });
  });

  describe('Incident Management', () => {
    /**
     * US 4.1.13: Test linking incident to VVE
     * Verifies:
     * - Incidents can be associated with VVE
     * - Incident ID is stored in VVE's incident list
     * - VVE correctly reports having incidents
     */
    it('should link incident to VVE', () => {
      const vve = new VesselVisitExecution({
        vvnId: 'VVN-001',
      });

      vve.linkIncident('INCIDENT-001');

      expect(vve.incidents).toContain('INCIDENT-001');
      expect(vve.hasIncidents()).toBe(true);
    });

    /**
     * US 4.1.13: Test duplicate incident prevention
     * Verifies that the same incident cannot be linked twice to a VVE
     * Ensures data integrity
     */
    it('should reject duplicate incident links', () => {
      const vve = new VesselVisitExecution({
        vvnId: 'VVN-001',
      });

      vve.linkIncident('INCIDENT-001');

      expect(() => {
        vve.linkIncident('INCIDENT-001');
      }).toThrow('Incident INCIDENT-001 already linked to this VVE');
    });

    /**
     * US 4.1.13: Test unlinking incident from VVE
     * Verifies that incidents can be removed from VVE
     * Supports incident resolution workflow
     */
    it('should unlink incident from VVE', () => {
      const vve = new VesselVisitExecution({
        vvnId: 'VVN-001',
      });

      vve.linkIncident('INCIDENT-001');
      expect(vve.incidents).toHaveLength(1);

      vve.unlinkIncident('INCIDENT-001');
      expect(vve.incidents).toHaveLength(0);
    });
  });

  describe('Disruption Management', () => {
    /**
     * US 4.1.13: Test marking VVE as disrupted
     * Verifies:
     * - VVE status can be changed to DISRUPTED
     * - Supports tracking of operational disruptions
     */
    it('should mark VVE as disrupted', () => {
      const vve = new VesselVisitExecution({
        vvnId: 'VVN-001',
        status: VesselVisitExecutionStatus.IN_PROGRESS,
      });

      vve.markAsDisrupted();

      expect(vve.status).toBe(VesselVisitExecutionStatus.DISRUPTED);
    });

    /**
     * US 4.1.13: Test disruption validation
     * Verifies that completed VVEs cannot be marked as disrupted
     * Ensures state consistency
     */
    it('should reject disruption of completed VVE', () => {
      const vve = new VesselVisitExecution({
        vvnId: 'VVN-001',
        status: VesselVisitExecutionStatus.COMPLETED,
      });

      expect(() => {
        vve.markAsDisrupted();
      }).toThrow('Cannot mark completed VVE as disrupted');
    });

    /**
     * US 4.1.13: Test resuming VVE after disruption
     * Verifies:
     * - VVE can be resumed after disruption is resolved
     * - Status transitions from DISRUPTED to IN_PROGRESS
     */
    it('should resume VVE after disruption', () => {
      const vve = new VesselVisitExecution({
        vvnId: 'VVN-001',
        status: VesselVisitExecutionStatus.DISRUPTED,
      });

      vve.resumeAfterDisruption();

      expect(vve.status).toBe(VesselVisitExecutionStatus.IN_PROGRESS);
    });
  });

  describe('Metrics Calculation', () => {
    /**
     * US 4.1.10: Test turnaround time calculation
     * Verifies calculation of total time from port arrival to departure
     * Formula: departureTime - arrivalTime (in hours)
     */
    it('should calculate turnaround time', () => {
      const vve = new VesselVisitExecution({
        vvnId: 'VVN-001',
        actualPortArrivalTime: new Date('2025-12-10T08:00:00Z'),
        actualPortDepartureTime: new Date('2025-12-10T16:00:00Z'),
      });

      const turnaroundTime = vve.getTurnaroundTimeHours();

      expect(turnaroundTime).toBe(8); // 8 hours
    });

    /**
     * US 4.1.10: Test berth occupancy time calculation
     * Verifies calculation of time vessel occupied berth
     * Formula: unberthTime - berthTime (in hours)
     */
    it('should calculate berth occupancy time', () => {
      const vve = new VesselVisitExecution({
        vvnId: 'VVN-001',
        actualBerthTime: new Date('2025-12-10T09:00:00Z'),
        actualUnberthTime: new Date('2025-12-10T15:00:00Z'),
      });

      const occupancy = vve.getBerthOccupancyHours();

      expect(occupancy).toBe(6); // 6 hours
    });

    /**
     * US 4.1.10: Test waiting time for berthing calculation
     * Verifies calculation of time vessel waited for berth
     * Formula: berthTime - arrivalTime (in hours)
     */
    it('should calculate waiting time for berthing', () => {
      const vve = new VesselVisitExecution({
        vvnId: 'VVN-001',
        actualPortArrivalTime: new Date('2025-12-10T08:00:00Z'),
        actualBerthTime: new Date('2025-12-10T09:30:00Z'),
      });

      const waitingTime = vve.getWaitingTimeHours();

      expect(waitingTime).toBe(1.5); // 1.5 hours
    });

    /**
     * US 4.1.10: Test total containers processed calculation
     * Verifies aggregation of containers across all operations
     * Supports productivity analysis
     */
    it('should calculate total containers processed', () => {
      const vve = new VesselVisitExecution({
        vvnId: 'VVN-001',
        status: VesselVisitExecutionStatus.IN_PROGRESS,
      });

      const op1 = new ExecutedOperation({
        operationType: OperationType.UNLOAD,
        startTime: new Date('2025-12-10T10:00:00Z'),
        endTime: new Date('2025-12-10T12:00:00Z'),
        containersProcessed: 50,
        cranesUsed: 1,
        staffAssigned: [],
        status: ExecutedOperationStatus.COMPLETED,
      });

      const op2 = new ExecutedOperation({
        operationType: OperationType.LOAD,
        startTime: new Date('2025-12-10T12:00:00Z'),
        endTime: new Date('2025-12-10T14:00:00Z'),
        containersProcessed: 75,
        cranesUsed: 1,
        staffAssigned: [],
        status: ExecutedOperationStatus.COMPLETED,
      });

      vve.addExecutedOperation(op1);
      vve.addExecutedOperation(op2);

      expect(vve.getTotalContainersProcessed()).toBe(125);
    });
  });

  describe('Business Rules', () => {
    /**
     * US 4.1.11: Test VVE editability rules
     * Verifies:
     * - IN_PROGRESS VVEs are editable
     * - COMPLETED VVEs are read-only (except for admin corrections)
     * - Enforces data integrity
     */
    it('should identify editable VVEs', () => {
      const editableVve = new VesselVisitExecution({
        vvnId: 'VVN-001',
        status: VesselVisitExecutionStatus.IN_PROGRESS,
      });

      const completedVve = new VesselVisitExecution({
        vvnId: 'VVN-002',
        status: VesselVisitExecutionStatus.COMPLETED,
      });

      expect(editableVve.isEditable()).toBe(true);
      expect(completedVve.isEditable()).toBe(false);
    });
  });
});
