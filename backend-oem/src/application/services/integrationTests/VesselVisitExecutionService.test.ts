import { VesselVisitExecutionService } from '../VesselVisitExecutionService';
import { IVesselVisitExecutionRepository } from '@domain/repositories/IVesselVisitExecutionRepository';
import { VesselVisitExecution } from '@domain/entities/VesselVisitExecution';
import { ExecutedOperation } from '@domain/value-objects/ExecutedOperation';
import { ExecutedOperationStatus, OperationType } from '@shared/types';
import {
  CreateVesselVisitExecutionDto,
  RecordPortArrivalDto,
  RecordBerthingDto,
  RecordUnberthingDto,
  AddExecutedOperationDto,
  CompleteVveDto,
  LinkIncidentDto,
} from '@application/dtos';

// Mock logger to avoid console output during tests
jest.mock('@shared/utils/logger', () => ({
  logger: {
    info: jest.fn(),
    warn: jest.fn(),
    error: jest.fn(),
    debug: jest.fn(),
  },
}));

describe('VesselVisitExecutionService', () => {
  let service: VesselVisitExecutionService;
  let mockRepository: jest.Mocked<IVesselVisitExecutionRepository>;

  const mockVvnId = 'VVN-001';
  const mockVveId = 'VVE-123';
  const mockUserId = 'user-456';

  beforeEach(() => {
    mockRepository = {
      save: jest.fn(),
      findById: jest.fn(),
      findByVvnId: jest.fn(),
      findAll: jest.fn(),
      findByStatus: jest.fn(),
      update: jest.fn(),
      delete: jest.fn(),
    } as any;

    service = new VesselVisitExecutionService(mockRepository);
  });

  describe('createVve', () => {
    it('should create a new VesselVisitExecution', async () => {
      const dto: CreateVesselVisitExecutionDto = {
        vvnId: mockVvnId,
      };

      const mockVve = new VesselVisitExecution({ vvnId: mockVvnId });
      mockRepository.save.mockResolvedValue(mockVve);

      const result = await service.createVve(dto);

      expect(result).toBeDefined();
      expect(result.vvnId).toBe(mockVvnId);
      expect(result.status).toBe('PLANNED');
      expect(mockRepository.save).toHaveBeenCalled();
    });
  });

  describe('getVveById', () => {
    it('should return VVE DTO when VVE exists', async () => {
      const mockVve = new VesselVisitExecution({ vvnId: mockVvnId });
      mockRepository.findById.mockResolvedValue(mockVve);

      const result = await service.getVveById(mockVveId);

      expect(result).toBeDefined();
      expect(result?.vvnId).toBe(mockVvnId);
      expect(mockRepository.findById).toHaveBeenCalledWith(mockVveId);
    });

    it('should return null when VVE does not exist', async () => {
      mockRepository.findById.mockResolvedValue(null);

      const result = await service.getVveById('non-existent');

      expect(result).toBeNull();
    });
  });

  describe('getVveByVvnId', () => {
    it('should return VVE DTO when VVE exists for VVN', async () => {
      const mockVve = new VesselVisitExecution({ vvnId: mockVvnId });
      mockRepository.findByVvnId.mockResolvedValue(mockVve);

      const result = await service.getVveByVvnId(mockVvnId);

      expect(result).toBeDefined();
      expect(result?.vvnId).toBe(mockVvnId);
      expect(mockRepository.findByVvnId).toHaveBeenCalledWith(mockVvnId);
    });

    it('should return null when no VVE exists for VVN', async () => {
      mockRepository.findByVvnId.mockResolvedValue(null);

      const result = await service.getVveByVvnId('non-existent-vvn');

      expect(result).toBeNull();
    });
  });

  describe('getAllVves', () => {
    it('should return all VVEs as DTOs', async () => {
      const mockVves = [
        new VesselVisitExecution({ vvnId: 'VVN-001' }),
        new VesselVisitExecution({ vvnId: 'VVN-002' }),
      ];

      mockRepository.findAll.mockResolvedValue(mockVves);

      const result = await service.getAllVves();

      expect(result).toHaveLength(2);
      expect(result[0]!.vvnId).toBe('VVN-001');
      expect(result[1]!.vvnId).toBe('VVN-002');
    });

    it('should return empty array when no VVEs exist', async () => {
      mockRepository.findAll.mockResolvedValue([]);

      const result = await service.getAllVves();

      expect(result).toEqual([]);
    });
  });

  describe('getVvesByStatus', () => {
    it('should return VVEs filtered by status', async () => {
      const mockVves = [
        new VesselVisitExecution({ vvnId: 'VVN-001' }),
        new VesselVisitExecution({ vvnId: 'VVN-002' }),
      ];

      mockRepository.findByStatus.mockResolvedValue(mockVves);

      const result = await service.getVvesByStatus('PENDING');

      expect(result).toHaveLength(2);
      expect(mockRepository.findByStatus).toHaveBeenCalledWith('PENDING', undefined);
    });
  });

  describe('recordPortArrival', () => {
    it('should record port arrival time', async () => {
      const mockVve = new VesselVisitExecution({ vvnId: mockVvnId });
      const arrivalTime = new Date('2025-12-10T08:00:00Z');

      const dto: RecordPortArrivalDto = {
        arrivalTime: arrivalTime.toISOString(),
      };

      mockRepository.findById.mockResolvedValue(mockVve);
      mockRepository.update.mockResolvedValue(mockVve);

      const result = await service.recordPortArrival(mockVveId, dto);

      expect(result).toBeDefined();
      expect(result.status).toBe('IN_PROGRESS');
      expect(mockRepository.update).toHaveBeenCalled();
    });

    it('should throw error when VVE not found', async () => {
      mockRepository.findById.mockResolvedValue(null);

      const dto: RecordPortArrivalDto = {
        arrivalTime: new Date().toISOString(),
      };

      await expect(service.recordPortArrival('non-existent', dto)).rejects.toThrow(
        'VVE non-existent not found'
      );
    });
  });

  describe('recordBerthing', () => {
    it('should record berthing time and dock', async () => {
      const mockVve = new VesselVisitExecution({ vvnId: mockVvnId });
      mockVve.recordPortArrival(new Date('2025-12-10T08:00:00Z'));

      const berthTime = new Date('2025-12-10T09:00:00Z');
      const dto: RecordBerthingDto = {
        berthTime: berthTime.toISOString(),
        dockId: 'DOCK-A',
      };

      mockRepository.findById.mockResolvedValue(mockVve);
      mockRepository.update.mockResolvedValue(mockVve);

      const result = await service.recordBerthing(mockVveId, dto);

      expect(result).toBeDefined();
      expect(result.status).toBe('IN_PROGRESS');
      expect(result.assignedDock).toBe('DOCK-A');
      expect(mockRepository.update).toHaveBeenCalled();
    });

    it('should throw error when VVE not found', async () => {
      mockRepository.findById.mockResolvedValue(null);

      const dto: RecordBerthingDto = {
        berthTime: new Date().toISOString(),
        dockId: 'DOCK-A',
      };

      await expect(service.recordBerthing('non-existent', dto)).rejects.toThrow(
        'VVE non-existent not found'
      );
    });
  });

  describe('recordUnberthing', () => {
    it('should record unberthing time', async () => {
      const mockVve = new VesselVisitExecution({ vvnId: mockVvnId });
      mockVve.recordPortArrival(new Date('2025-12-10T08:00:00Z'));
      mockVve.recordBerthing(new Date('2025-12-10T09:00:00Z'), 'DOCK-A');

      const unberthTime = new Date('2025-12-10T15:00:00Z');
      const dto: RecordUnberthingDto = {
        unberthTime: unberthTime.toISOString(),
      };

      mockRepository.findById.mockResolvedValue(mockVve);
      mockRepository.update.mockResolvedValue(mockVve);

      const result = await service.recordUnberthing(mockVveId, dto);

      expect(result).toBeDefined();
      expect(result.status).toBe('IN_PROGRESS');
      expect(mockRepository.update).toHaveBeenCalled();
    });

    it('should throw error when VVE not found', async () => {
      mockRepository.findById.mockResolvedValue(null);

      const dto: RecordUnberthingDto = {
        unberthTime: new Date().toISOString(),
      };

      await expect(service.recordUnberthing('non-existent', dto)).rejects.toThrow(
        'VVE non-existent not found'
      );
    });
  });

  describe('addExecutedOperation', () => {
    it('should add a new executed operation', async () => {
      const mockVve = new VesselVisitExecution({ vvnId: mockVvnId });
      mockVve.recordPortArrival(new Date('2025-12-10T08:00:00Z'));
      mockVve.recordBerthing(new Date('2025-12-10T09:00:00Z'), 'DOCK-A');

      const dto: AddExecutedOperationDto = {
        operationType: OperationType.UNLOAD,
        startTime: new Date('2025-12-10T10:00:00Z').toISOString(),
        cranesUsed: 2,
        staffAssigned: ['STAFF-1', 'STAFF-2'],
      };

      mockRepository.findById.mockResolvedValue(mockVve);
      mockRepository.update.mockResolvedValue(mockVve);

      const result = await service.addExecutedOperation(mockVveId, dto);

      expect(result).toBeDefined();
      expect(result.operations).toHaveLength(1);
      expect(result.operations[0]!.operationType).toBe(OperationType.UNLOAD);
      expect(result.operations[0]!.status).toBe(ExecutedOperationStatus.STARTED);
      expect(mockRepository.update).toHaveBeenCalled();
    });

    it('should throw error when VVE not found', async () => {
      mockRepository.findById.mockResolvedValue(null);

      const dto: AddExecutedOperationDto = {
        operationType: OperationType.UNLOAD,
        startTime: new Date().toISOString(),
        cranesUsed: 1,
        staffAssigned: [],
      };

      await expect(service.addExecutedOperation('non-existent', dto)).rejects.toThrow(
        'VVE non-existent not found'
      );
    });
  });

  describe('completeOperation', () => {
    it('should throw error indicating method not yet implemented', async () => {
      const mockVve = new VesselVisitExecution({ vvnId: mockVvnId });
      mockVve.recordPortArrival(new Date('2025-12-10T08:00:00Z')); // Set to IN_PROGRESS

      const operation = new ExecutedOperation({
        operationType: OperationType.UNLOAD,
        startTime: new Date(),
        containersProcessed: 0,
        cranesUsed: 1,
        staffAssigned: [],
        status: ExecutedOperationStatus.STARTED,
      });
      mockVve.addExecutedOperation(operation);

      mockRepository.findById.mockResolvedValue(mockVve);

      await expect(
        service.completeOperation(mockVveId, {
          operationIndex: 0,
          endTime: new Date().toISOString(),
          containersProcessed: 50,
        })
      ).rejects.toThrow('End time must be after start time');
    });
  });

  describe('completeVve', () => {
    it('should complete VVE with departure time', async () => {
      const mockVve = new VesselVisitExecution({ vvnId: mockVvnId });
      mockVve.recordPortArrival(new Date('2025-12-10T08:00:00Z'));
      mockVve.recordBerthing(new Date('2025-12-10T09:00:00Z'), 'DOCK-A');
      mockVve.recordUnberthing(new Date('2025-12-10T15:00:00Z'));

      const dto: CompleteVveDto = {
        departureTime: new Date('2025-12-10T16:00:00Z').toISOString(),
        completedBy: mockUserId,
      };

      mockRepository.findById.mockResolvedValue(mockVve);
      mockRepository.update.mockResolvedValue(mockVve);

      const result = await service.completeVve(mockVveId, dto);

      expect(result).toBeDefined();
      expect(result.status).toBe('COMPLETED');
      expect(result.completedBy).toBe(mockUserId);
      expect(mockRepository.update).toHaveBeenCalled();
    });

    it('should throw error when VVE not found', async () => {
      mockRepository.findById.mockResolvedValue(null);

      const dto: CompleteVveDto = {
        departureTime: new Date().toISOString(),
        completedBy: mockUserId,
      };

      await expect(service.completeVve('non-existent', dto)).rejects.toThrow(
        'VVE non-existent not found'
      );
    });
  });

  describe('markAsDisrupted', () => {
    it('should mark VVE as disrupted', async () => {
      const mockVve = new VesselVisitExecution({ vvnId: mockVvnId });
      mockVve.recordPortArrival(new Date('2025-12-10T08:00:00Z'));
      mockVve.recordBerthing(new Date('2025-12-10T09:00:00Z'), 'DOCK-A');

      mockRepository.findById.mockResolvedValue(mockVve);
      mockRepository.update.mockResolvedValue(mockVve);

      const result = await service.markAsDisrupted(mockVveId);

      expect(result).toBeDefined();
      expect(result.status).toBe('DISRUPTED');
      expect(mockRepository.update).toHaveBeenCalled();
    });

    it('should throw error when VVE not found', async () => {
      mockRepository.findById.mockResolvedValue(null);

      await expect(service.markAsDisrupted('non-existent')).rejects.toThrow(
        'VVE non-existent not found'
      );
    });
  });

  describe('resumeAfterDisruption', () => {
    it('should resume VVE after disruption', async () => {
      const mockVve = new VesselVisitExecution({ vvnId: mockVvnId });
      mockVve.recordPortArrival(new Date('2025-12-10T08:00:00Z'));
      mockVve.recordBerthing(new Date('2025-12-10T09:00:00Z'), 'DOCK-A');
      mockVve.markAsDisrupted();

      mockRepository.findById.mockResolvedValue(mockVve);
      mockRepository.update.mockResolvedValue(mockVve);

      const result = await service.resumeAfterDisruption(mockVveId);

      expect(result).toBeDefined();
      expect(result.status).toBe('IN_PROGRESS');
      expect(mockRepository.update).toHaveBeenCalled();
    });

    it('should throw error when VVE not found', async () => {
      mockRepository.findById.mockResolvedValue(null);

      await expect(service.resumeAfterDisruption('non-existent')).rejects.toThrow(
        'VVE non-existent not found'
      );
    });
  });

  describe('linkIncident', () => {
    it('should link an incident to VVE', async () => {
      const mockVve = new VesselVisitExecution({ vvnId: mockVvnId });
      const incidentId = 'INCIDENT-001';

      const dto: LinkIncidentDto = {
        incidentId,
      };

      mockRepository.findById.mockResolvedValue(mockVve);
      mockRepository.update.mockResolvedValue(mockVve);

      const result = await service.linkIncident(mockVveId, dto);

      expect(result).toBeDefined();
      expect(result.incidents).toContain(incidentId);
      expect(mockRepository.update).toHaveBeenCalled();
    });

    it('should throw error when VVE not found', async () => {
      mockRepository.findById.mockResolvedValue(null);

      const dto: LinkIncidentDto = {
        incidentId: 'INCIDENT-001',
      };

      await expect(service.linkIncident('non-existent', dto)).rejects.toThrow(
        'VVE non-existent not found'
      );
    });
  });

  describe('unlinkIncident', () => {
    it('should unlink an incident from VVE', async () => {
      const mockVve = new VesselVisitExecution({ vvnId: mockVvnId });
      const incidentId = 'INCIDENT-001';
      mockVve.linkIncident(incidentId);

      const dto: LinkIncidentDto = {
        incidentId,
      };

      mockRepository.findById.mockResolvedValue(mockVve);
      mockRepository.update.mockResolvedValue(mockVve);

      const result = await service.unlinkIncident(mockVveId, dto);

      expect(result).toBeDefined();
      expect(result.incidents).not.toContain(incidentId);
      expect(mockRepository.update).toHaveBeenCalled();
    });

    it('should throw error when VVE not found', async () => {
      mockRepository.findById.mockResolvedValue(null);

      const dto: LinkIncidentDto = {
        incidentId: 'INCIDENT-001',
      };

      await expect(service.unlinkIncident('non-existent', dto)).rejects.toThrow(
        'VVE non-existent not found'
      );
    });
  });

  describe('getVveMetrics', () => {
    it('should calculate and return VVE metrics', async () => {
      const mockVve = new VesselVisitExecution({ vveId: mockVveId, vvnId: mockVvnId });
      mockVve.recordPortArrival(new Date('2025-12-10T08:00:00Z'));
      mockVve.recordBerthing(new Date('2025-12-10T09:00:00Z'), 'DOCK-A');

      const operation = new ExecutedOperation({
        operationType: OperationType.UNLOAD,
        startTime: new Date('2025-12-10T10:00:00Z'),
        containersProcessed: 0,
        cranesUsed: 2,
        staffAssigned: ['STAFF-1'],
        status: ExecutedOperationStatus.STARTED,
      });
      const completedOperation = operation.complete(new Date('2025-12-10T14:00:00Z'), 100);
      mockVve.addExecutedOperation(completedOperation);

      mockVve.recordUnberthing(new Date('2025-12-10T15:00:00Z'));

      mockRepository.findById.mockResolvedValue(mockVve);

      const result = await service.getMetrics(mockVveId);

      expect(result).toBeDefined();
      expect(result.vveId).toBe(mockVveId);
      expect(result.totalContainersProcessed).toBe(100);
      expect(result.berthOccupancyTime).toBe(6); // 9am to 3pm
      expect(result.waitingTimeForBerthing).toBe(1); // 8am to 9am
      expect(result.operationsCount).toBe(1);
      expect(result.incidentsCount).toBe(0);
    });

    it('should throw error when VVE not found', async () => {
      mockRepository.findById.mockResolvedValue(null);

      await expect(service.getMetrics('non-existent')).rejects.toThrow(
        'Vessel Visit Execution non-existent not found'
      );
    });
  });
});
