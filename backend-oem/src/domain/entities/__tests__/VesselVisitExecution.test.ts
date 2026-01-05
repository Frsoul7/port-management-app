import { VesselVisitExecution } from '../VesselVisitExecution';
import { ExecutedOperation } from '@domain/value-objects/ExecutedOperation';
import { VesselVisitExecutionStatus, OperationType, ExecutedOperationStatus } from '@shared/types';

describe('VesselVisitExecution Entity', () => {
  describe('Creation', () => {
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

    it('should reject VVE without VVN ID', () => {
      expect(() => {
        new VesselVisitExecution({
          vvnId: '',
        });
      }).toThrow('VVN ID is required');
    });

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
    it('should record port arrival and transition to IN_PROGRESS', () => {
      const vve = new VesselVisitExecution({
        vvnId: 'VVN-001',
      });

      const arrivalTime = new Date('2025-12-10T08:00:00Z');
      vve.recordPortArrival(arrivalTime);

      expect(vve.actualPortArrivalTime).toEqual(arrivalTime);
      expect(vve.status).toBe(VesselVisitExecutionStatus.IN_PROGRESS);
    });

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

    it('should reject berthing without port arrival', () => {
      const vve = new VesselVisitExecution({
        vvnId: 'VVN-001',
        status: VesselVisitExecutionStatus.IN_PROGRESS,
      });

      expect(() => {
        vve.recordBerthing(new Date(), 'DOCK-A');
      }).toThrow('Cannot record berthing without port arrival time');
    });

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

    it('should reject completion without unberth time', () => {
      const vve = new VesselVisitExecution({
        vvnId: 'VVN-001',
        status: VesselVisitExecutionStatus.IN_PROGRESS,
      });

      expect(() => {
        vve.markAsCompleted(new Date(), 'port-operator');
      }).toThrow('Cannot complete VVE without unberth time');
    });

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
    it('should link incident to VVE', () => {
      const vve = new VesselVisitExecution({
        vvnId: 'VVN-001',
      });

      vve.linkIncident('INCIDENT-001');

      expect(vve.incidents).toContain('INCIDENT-001');
      expect(vve.hasIncidents()).toBe(true);
    });

    it('should reject duplicate incident links', () => {
      const vve = new VesselVisitExecution({
        vvnId: 'VVN-001',
      });

      vve.linkIncident('INCIDENT-001');

      expect(() => {
        vve.linkIncident('INCIDENT-001');
      }).toThrow('Incident INCIDENT-001 already linked to this VVE');
    });

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
    it('should mark VVE as disrupted', () => {
      const vve = new VesselVisitExecution({
        vvnId: 'VVN-001',
        status: VesselVisitExecutionStatus.IN_PROGRESS,
      });

      vve.markAsDisrupted();

      expect(vve.status).toBe(VesselVisitExecutionStatus.DISRUPTED);
    });

    it('should reject disruption of completed VVE', () => {
      const vve = new VesselVisitExecution({
        vvnId: 'VVN-001',
        status: VesselVisitExecutionStatus.COMPLETED,
      });

      expect(() => {
        vve.markAsDisrupted();
      }).toThrow('Cannot mark completed VVE as disrupted');
    });

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
    it('should calculate turnaround time', () => {
      const vve = new VesselVisitExecution({
        vvnId: 'VVN-001',
        actualPortArrivalTime: new Date('2025-12-10T08:00:00Z'),
        actualPortDepartureTime: new Date('2025-12-10T16:00:00Z'),
      });

      const turnaroundTime = vve.getTurnaroundTimeHours();

      expect(turnaroundTime).toBe(8); // 8 hours
    });

    it('should calculate berth occupancy time', () => {
      const vve = new VesselVisitExecution({
        vvnId: 'VVN-001',
        actualBerthTime: new Date('2025-12-10T09:00:00Z'),
        actualUnberthTime: new Date('2025-12-10T15:00:00Z'),
      });

      const occupancy = vve.getBerthOccupancyHours();

      expect(occupancy).toBe(6); // 6 hours
    });

    it('should calculate waiting time for berthing', () => {
      const vve = new VesselVisitExecution({
        vvnId: 'VVN-001',
        actualPortArrivalTime: new Date('2025-12-10T08:00:00Z'),
        actualBerthTime: new Date('2025-12-10T09:30:00Z'),
      });

      const waitingTime = vve.getWaitingTimeHours();

      expect(waitingTime).toBe(1.5); // 1.5 hours
    });

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
