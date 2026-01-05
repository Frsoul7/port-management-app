import { IVesselVisitExecutionRepository } from '@domain/repositories/IVesselVisitExecutionRepository';
import { IOperationPlanRepository } from '@domain/repositories/IOperationPlanRepository';
import { VesselVisitExecution } from '@domain/entities/VesselVisitExecution';
import { ExecutedOperation } from '@domain/value-objects/ExecutedOperation';
import { ExecutedOperationStatus, QueryOptions, VesselVisitExecutionStatus, PaginatedResult } from '@shared/types';
import {
  CreateVesselVisitExecutionDto,
  VesselVisitExecutionDto,
  ExecutedOperationDto,
  RecordPortArrivalDto,
  RecordBerthingDto,
  RecordUnberthingDto,
  AddExecutedOperationDto,
  CompleteOperationDto,
  UpdateExecutedOperationDto,
  CompleteVveDto,
  LinkIncidentDto,
  VveMetricsDto,
  InitializeVveFromPlanDto,
  ListVesselVisitExecutionsQuery,
} from '@application/dtos';
import { VveIdGenerator } from '@domain/services/VveIdGenerator';
import { logger } from '@shared/utils/logger';

/**
 * Application Service for VesselVisitExecution aggregate
 */
export class VesselVisitExecutionService {
  constructor(
    private readonly vveRepository: IVesselVisitExecutionRepository,
    private readonly operationPlanRepository?: IOperationPlanRepository
  ) {}

  async createVve(dto: CreateVesselVisitExecutionDto): Promise<VesselVisitExecutionDto> {
    const vve = new VesselVisitExecution({ vvnId: dto.vvnId });
    const saved = await this.vveRepository.save(vve);
    logger.info(`VVE ${saved.vveId} created for VVN ${dto.vvnId}`);
    return this.toDto(saved);
  }

  /**
   * US 4.1.7: Initialize VVE from approved operation plan
   * Creates VVE when vessel arrives at port
   */
  async initializeVveFromPlan(dto: InitializeVveFromPlanDto): Promise<VesselVisitExecutionDto> {
    if (!this.operationPlanRepository) {
      throw new Error('Operation plan repository not configured');
    }

    // Fetch operation plan to get VVN ID
    const operationPlan = await this.operationPlanRepository.findById(dto.operationPlanId);
    if (!operationPlan) {
      throw new Error(`Operation plan ${dto.operationPlanId} not found`);
    }

    // Verify plan is approved
    if (operationPlan.status !== 'APPROVED') {
      throw new Error(`Operation plan must be APPROVED. Current status: ${operationPlan.status}`);
    }

    // Find all operations for this VVN in the plan
    const plannedOperationsForVvn = operationPlan.operations.filter(
      (op) => op.vvnId === dto.vvnId
    );

    if (plannedOperationsForVvn.length === 0) {
      throw new Error(`No planned operations found for VVN ${dto.vvnId} in plan ${dto.operationPlanId}`);
    }

    // US 4.1.7: Check if VVE already exists for this VVN (prevent duplicates)
    const existingVve = await this.vveRepository.findByVvnId(dto.vvnId);
    if (existingVve) {
      throw new Error(`VVE already exists for vessel visit ${dto.vvnId}. VVE ID: ${existingVve.vveId}`);
    }

    // US 4.1.7: Generate sequential VVE ID (VVE-000001, VVE-000002, etc.)
    const nextSequentialNumber = await this.vveRepository.getNextSequentialNumber();
    const vveId = VveIdGenerator.generate(nextSequentialNumber);

    // US 4.1.9: Create ExecutedOperations derived from PlannedOperations
    const executedOperations = plannedOperationsForVvn.map((plannedOp, index) => {
      // Generate unique identifier for the planned operation
      const plannedOperationId = `${dto.operationPlanId}-${dto.vvnId}-${plannedOp.operationType}-${index}`;

      return new ExecutedOperation({
        operationType: plannedOp.operationType,
        startTime: plannedOp.plannedStart, // Initially set to planned time
        containersProcessed: 0, // Not yet processed
        cranesUsed: plannedOp.assignedCranes,
        staffAssigned: plannedOp.assignedStaff || [],
        status: ExecutedOperationStatus.STARTED, // Initially in STARTED status
        plannedOperationId, // US 4.1.9: Link to planned operation
      });
    });

    // Create VVE with IN_PROGRESS status (US 4.1.7 requirement)
    const vve = new VesselVisitExecution({
      vveId, // US 4.1.7: Sequential business identifier
      vvnId: dto.vvnId,
      operationPlanId: dto.operationPlanId,
      actualPortArrivalTime: new Date(dto.arrivalTime),
      operations: executedOperations, // US 4.1.9: Pre-populate with operations derived from plan
      status: VesselVisitExecutionStatus.IN_PROGRESS, // US 4.1.7: marked as "In Progress"
    });

    const saved = await this.vveRepository.save(vve);

    // US 4.1.7: Update operation plan status to IN_EXECUTION
    operationPlan.startExecution();
    await this.operationPlanRepository.update(operationPlan);

    logger.info(`VVE ${saved.vveId} initialized for VVN ${dto.vvnId} from plan ${dto.operationPlanId}`, {
      arrivalTime: dto.arrivalTime,
      createdBy: dto.createdBy,
      derivedOperations: executedOperations.length,
    });

    return this.toDto(saved);
  }

  async getVveById(vveId: string): Promise<VesselVisitExecutionDto | null> {
    const vve = await this.vveRepository.findById(vveId);
    return vve ? this.toDto(vve) : null;
  }

  async getVveByVvnId(vvnId: string): Promise<VesselVisitExecutionDto | null> {
    const vve = await this.vveRepository.findByVvnId(vvnId);
    return vve ? this.toDto(vve) : null;
  }

  async getAllVves(options?: QueryOptions): Promise<VesselVisitExecutionDto[]> {
    const vves = await this.vveRepository.findAll(options);
    return vves.map((vve) => this.toDto(vve));
  }

  async getVvesByStatus(
    status: string,
    options?: QueryOptions
  ): Promise<VesselVisitExecutionDto[]> {
    const vves = await this.vveRepository.findByStatus(status as any, options);
    return vves.map((vve) => this.toDto(vve));
  }

  /**
   * Alias for getVveById (used by controller)
   */
  async getById(vveId: string): Promise<VesselVisitExecutionDto | null> {
    return this.getVveById(vveId);
  }

  /**
   * Alias for getVveByVvnId (used by controller)
   */
  async getByVvnId(vvnId: string): Promise<VesselVisitExecutionDto | null> {
    return this.getVveByVvnId(vvnId);
  }

  /**
   * Get execution metrics for a VVE
   * Business logic: Delegates to domain entity for metric calculations
   */
  async getMetrics(vveId: string): Promise<VveMetricsDto> {
    const vve = await this.vveRepository.findById(vveId);

    if (!vve) {
      throw new Error(`Vessel Visit Execution ${vveId} not found`);
    }

    logger.debug('Calculating VVE metrics', { vveId });

    // Domain entity performs all calculations
    const metrics = vve.calculateMetrics();

    return {
      vveId: metrics.vveId,
      totalTurnaroundTime: metrics.totalTurnaroundTime,
      berthOccupancyTime: metrics.berthOccupancyTime,
      waitingTimeForBerthing: metrics.waitingTimeForBerthing,
      totalOperationsTime: metrics.totalOperationsTime,
      operationsCount: metrics.operationsCount,
      incidentsCount: metrics.incidentsCount,
      totalContainersProcessed: metrics.totalContainersProcessed,
      status: metrics.status,
    };
  }

  /**
   * Find VVEs by filters with pagination
   */
  async findByFilters(
    filter: any,
    skip: number,
    limit: number,
    sortBy: string,
    sortOrder: string
  ): Promise<VesselVisitExecutionDto[]> {
    const options: QueryOptions = {
      page: Math.floor(skip / limit) + 1,
      limit,
      sortBy,
      sortOrder: sortOrder === 'asc' ? 'asc' : 'desc',
    };

    let vves: VesselVisitExecution[];

    if (filter.vvnId) {
      const vve = await this.vveRepository.findByVvnId(filter.vvnId);
      vves = vve ? [vve] : [];
    } else if (filter.status) {
      vves = await this.vveRepository.findByStatus(filter.status, options);
    } else {
      vves = await this.vveRepository.findAll(options);
    }

    return vves.map((vve) => this.toDto(vve));
  }

  /**
   * Count VVEs by filters
   */
  async countByFilters(filter: any): Promise<number> {
    // Simplified implementation - in production, add efficient count to repository
    const allVves = await this.findByFilters(filter, 0, 10000, 'createdAt', 'desc');
    return allVves.length;
  }

  /**
   * List vessel visit executions with filters and pagination
   * Handles filter construction and pagination calculations in service layer
   */
  async listVesselVisitExecutions(
    query: ListVesselVisitExecutionsQuery
  ): Promise<PaginatedResult<VesselVisitExecutionDto>> {
    // Build filter object
    const filter = this.buildFilterFromQuery(query);

    // Pagination
    const page = query.page || 1;
    const limit = query.limit || 10;
    const skip = (page - 1) * limit;
    const sortBy = query.sortBy || 'createdAt';
    const sortOrder = query.sortOrder || 'desc';

    // Get VVEs and total count
    const [executions, total] = await Promise.all([
      this.findByFilters(filter, skip, limit, sortBy, sortOrder),
      this.countByFilters(filter),
    ]);

    return {
      data: executions,
      pagination: {
        page,
        limit,
        total,
        totalPages: Math.ceil(total / limit),
      },
    };
  }

  /**
   * Build filter object from query parameters
   * Centralizes filter construction logic in service layer
   */
  private buildFilterFromQuery(query: ListVesselVisitExecutionsQuery): any {
    const filter: any = {};

    if (query.vvnId) {
      filter.vvnId = query.vvnId;
    }

    if (query.status) {
      filter.status = query.status;
    }

    if (query.fromDate || query.toDate) {
      filter.actualPortArrivalTime = {};
      if (query.fromDate) {
        filter.actualPortArrivalTime.$gte = new Date(query.fromDate);
      }
      if (query.toDate) {
        filter.actualPortArrivalTime.$lte = new Date(query.toDate);
      }
    }

    return filter;
  }

  async recordPortArrival(
    vveId: string,
    dto: RecordPortArrivalDto
  ): Promise<VesselVisitExecutionDto> {
    const vve = await this.getVveOrThrow(vveId);
    vve.recordPortArrival(new Date(dto.arrivalTime));
    const updated = await this.vveRepository.update(vve);
    logger.info(`Port arrival recorded for VVE ${vveId}`);
    return this.toDto(updated);
  }

  async recordBerthing(vveId: string, dto: RecordBerthingDto): Promise<VesselVisitExecutionDto> {
    const vve = await this.getVveOrThrow(vveId);
    vve.recordBerthing(new Date(dto.berthTime), dto.dockId);
    const updated = await this.vveRepository.update(vve);
    logger.info(`Berthing recorded for VVE ${vveId} at dock ${dto.dockId}`);
    return this.toDto(updated);
  }

  async recordUnberthing(
    vveId: string,
    dto: RecordUnberthingDto
  ): Promise<VesselVisitExecutionDto> {
    const vve = await this.getVveOrThrow(vveId);
    vve.recordUnberthing(new Date(dto.unberthTime));
    const updated = await this.vveRepository.update(vve);
    logger.info(`Unberthing recorded for VVE ${vveId}`);
    return this.toDto(updated);
  }

  async addExecutedOperation(
    vveId: string,
    dto: AddExecutedOperationDto
  ): Promise<VesselVisitExecutionDto> {
    const vve = await this.getVveOrThrow(vveId);

    const operation = new ExecutedOperation({
      operationType: dto.operationType,
      startTime: new Date(dto.startTime),
      containersProcessed: 0,
      cranesUsed: dto.cranesUsed,
      staffAssigned: dto.staffAssigned,
      status: ExecutedOperationStatus.STARTED,
    });

    vve.addExecutedOperation(operation);
    const updated = await this.vveRepository.update(vve);
    logger.info(`Executed operation added to VVE ${vveId}`);
    return this.toDto(updated);
  }

  async completeOperation(
    vveId: string,
    dto: CompleteOperationDto
  ): Promise<VesselVisitExecutionDto> {
    const vve = await this.getVveOrThrow(vveId);

    if (dto.operationIndex < 0 || dto.operationIndex >= vve.operations.length) {
      throw new Error(`Invalid operation index ${dto.operationIndex}`);
    }

    const operation = vve.operations[dto.operationIndex];
    if (!operation) {
      throw new Error(`Operation at index ${dto.operationIndex} not found`);
    }

    // Use ExecutedOperation.complete() to create updated instance
    const updatedOperation = operation.complete(
      new Date(dto.endTime),
      dto.containersProcessed
    );

    // Update in VVE using existing domain method
    vve.updateExecutedOperation(dto.operationIndex, updatedOperation);

    const updated = await this.vveRepository.update(vve);
    logger.info(`Operation ${dto.operationIndex} completed for VVE ${vveId}`);
    return this.toDto(updated);
  }

  /**
   * US 4.1.9: Update executed operation (status, times, resources)
   */
  async updateExecutedOperation(
    vveId: string,
    dto: UpdateExecutedOperationDto
  ): Promise<VesselVisitExecutionDto> {
    const vve = await this.getVveOrThrow(vveId);

    if (dto.operationIndex < 0 || dto.operationIndex >= vve.operations.length) {
      throw new Error(`Invalid operation index ${dto.operationIndex}`);
    }

    const currentOperation = vve.operations[dto.operationIndex];
    if (!currentOperation) {
      throw new Error(`Operation at index ${dto.operationIndex} not found`);
    }

    // Create updated operation based on requested changes
    let updatedOperation: ExecutedOperation;

    if (dto.status === ExecutedOperationStatus.COMPLETED) {
      // Complete the operation
      if (!dto.endTime || dto.containersProcessed === undefined) {
        throw new Error('endTime and containersProcessed required for COMPLETED status');
      }
      updatedOperation = currentOperation.complete(
        new Date(dto.endTime),
        dto.containersProcessed
      );
    } else if (dto.status === ExecutedOperationStatus.DELAYED) {
      // Mark as delayed
      updatedOperation = currentOperation.markAsDelayed();
    } else {
      // Just update times/resources without changing status
      updatedOperation = new ExecutedOperation({
        operationType: currentOperation.operationType,
        startTime: dto.startTime ? new Date(dto.startTime) : currentOperation.startTime,
        endTime: dto.endTime ? new Date(dto.endTime) : currentOperation.endTime,
        containersProcessed: dto.containersProcessed ?? currentOperation.containersProcessed,
        cranesUsed: dto.cranesUsed ?? currentOperation.cranesUsed,
        staffAssigned: dto.staffAssigned ?? currentOperation.staffAssigned,
        status: currentOperation.status,
      });
    }

    vve.updateExecutedOperation(dto.operationIndex, updatedOperation);

    const updated = await this.vveRepository.update(vve);
    logger.info(`Operation ${dto.operationIndex} updated for VVE ${vveId}`, {
      status: dto.status,
      updatedBy: dto.updatedBy,
    });

    return this.toDto(updated);
  }

  async completeVve(vveId: string, dto: CompleteVveDto): Promise<VesselVisitExecutionDto> {
    const vve = await this.getVveOrThrow(vveId);
    vve.markAsCompleted(new Date(dto.departureTime), dto.completedBy);
    const updated = await this.vveRepository.update(vve);
    logger.info(`VVE ${vveId} completed`);
    return this.toDto(updated);
  }

  async markAsDisrupted(vveId: string): Promise<VesselVisitExecutionDto> {
    const vve = await this.getVveOrThrow(vveId);
    vve.markAsDisrupted();
    const updated = await this.vveRepository.update(vve);
    logger.info(`VVE ${vveId} marked as disrupted`);
    return this.toDto(updated);
  }

  async resumeAfterDisruption(vveId: string): Promise<VesselVisitExecutionDto> {
    const vve = await this.getVveOrThrow(vveId);
    vve.resumeAfterDisruption();
    const updated = await this.vveRepository.update(vve);
    logger.info(`VVE ${vveId} resumed after disruption`);
    return this.toDto(updated);
  }

  async linkIncident(vveId: string, dto: LinkIncidentDto): Promise<VesselVisitExecutionDto> {
    const vve = await this.getVveOrThrow(vveId);
    vve.linkIncident(dto.incidentId);
    const updated = await this.vveRepository.update(vve);
    logger.info(`Incident ${dto.incidentId} linked to VVE ${vveId}`);
    return this.toDto(updated);
  }

  async unlinkIncident(vveId: string, dto: LinkIncidentDto): Promise<VesselVisitExecutionDto> {
    const vve = await this.getVveOrThrow(vveId);
    vve.unlinkIncident(dto.incidentId);
    const updated = await this.vveRepository.update(vve);
    logger.info(`Incident ${dto.incidentId} unlinked from VVE ${vveId}`);
    return this.toDto(updated);
  }

  private async getVveOrThrow(vveId: string): Promise<VesselVisitExecution> {
    const vve = await this.vveRepository.findById(vveId);
    if (!vve) {
      throw new Error(`VVE ${vveId} not found`);
    }
    return vve;
  }

  private toDto(vve: VesselVisitExecution): VesselVisitExecutionDto {
    return {
      vveId: vve.vveId,
      vvnId: vve.vvnId,
      status: vve.status,
      actualPortArrivalTime: vve.actualPortArrivalTime?.toISOString(),
      actualBerthTime: vve.actualBerthTime?.toISOString(),
      actualUnberthTime: vve.actualUnberthTime?.toISOString(),
      actualPortDepartureTime: vve.actualPortDepartureTime?.toISOString(),
      assignedDock: vve.assignedDock,
      operations: vve.operations.filter((op) => op !== null && op !== undefined).map((op) => this.operationToDto(op)),
      incidents: [...vve.incidents], // Create a copy
      completedAt: vve.completedAt?.toISOString(),
      completedBy: vve.completedBy,
      createdAt: new Date().toISOString(), // VVE doesn't track createdAt currently
    };
  }

  private operationToDto(operation: ExecutedOperation): ExecutedOperationDto {
    return {
      operationType: operation.operationType,
      startTime: operation.startTime.toISOString(),
      endTime: operation.endTime?.toISOString(),
      containersProcessed: operation.containersProcessed,
      cranesUsed: operation.cranesUsed,
      staffAssigned: operation.staffAssigned,
      status: operation.status,
      plannedOperationId: operation.plannedOperationId, // US 4.1.9
    };
  }
}
