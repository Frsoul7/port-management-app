import { IOperationPlanRepository } from '@domain/repositories/IOperationPlanRepository';
import { OperationPlan } from '@domain/entities/OperationPlan';
import { PlannedOperation } from '@domain/value-objects/PlannedOperation';
import { ICoreBackendClient, IPlanningClient } from '@domain/ports';
import { VvnReference, PlanningVesselInput, QueryOptions, PlanningAlgorithm, OperationType, PaginatedResult } from '@shared/types';
import {
  CreateOperationPlanDto,
  OperationPlanDto,
  PlannedOperationDto,
  UpdateDockAssignmentDto,
  UpdateStaffAssignmentDto,
  UpdateOperationDto,
  ConflictDetectionRequestDto,
  ConflictDetectionResultDto,
  ResourceConflictDto,
  OperationPlanAuditEntryDto,
  ListOperationPlansQuery,
} from '@application/dtos';
import {
  ConflictDetectionService,
  ConflictDetectionResult,
} from '@domain/services/ConflictDetectionService';
import { logger } from '@shared/utils/logger';

/**
 * Application Service for OperationPlan aggregate
 * Orchestrates business logic for operation planning
 */
export class OperationPlanService {
  // Container processing rates (containers per hour)
  private static readonly UNLOAD_RATE = 20; // containers/hour with 1 crane
  private static readonly LOAD_RATE = 15; // containers/hour with 1 crane

  private readonly conflictDetectionService: ConflictDetectionService;

  constructor(
    private readonly operationPlanRepository: IOperationPlanRepository,
    private readonly coreBackendClient: ICoreBackendClient,
    private readonly planningClient: IPlanningClient
  ) {
    this.conflictDetectionService = new ConflictDetectionService();
  }

  /**
   * List operation plans with filters and pagination
   * Business logic: All filter construction and pagination calculation happens here
   */
  async listOperationPlans(query: ListOperationPlansQuery): Promise<PaginatedResult<OperationPlanDto>> {
    logger.debug('Listing operation plans', query);

    // Build filter object (business logic - was in controller)
    const filter = this.buildFilterFromQuery(query);

    // Calculate pagination (business logic - was in controller)
    const page = query.page || 1;
    const limit = query.limit || 10;
    const skip = (page - 1) * limit;
    const sortBy = query.sortBy || 'targetDate';
    const sortOrder = query.sortOrder || 'desc';

    // Get plans and count (orchestration - service responsibility)
    const [plans, total] = await Promise.all([
      this.findByFilters(filter, skip, limit, sortBy, sortOrder),
      this.countByFilters(filter),
    ]);

    // Return paginated result with pre-calculated metadata
    return {
      data: plans,
      pagination: {
        page,
        limit,
        total,
        totalPages: Math.ceil(total / limit), // Calculation moved from controller
      },
    };
  }

  /**
   * Build filter object from query parameters
   * Private helper - encapsulates filter construction logic
   */
  private buildFilterFromQuery(query: ListOperationPlansQuery): any {
    const filter: any = {};

    if (query.targetDate) {
      filter.targetDate = new Date(query.targetDate);
    }

    if (query.algorithm) {
      filter.algorithm = query.algorithm;
    }

    if (query.status) {
      filter.status = query.status;
    }

    if (query.vvnId) {
      filter.vvnId = query.vvnId;
    }

    if (query.vesselImo) {
      filter.vesselImo = query.vesselImo;
    }

    // Date range filter (MongoDB operators)
    if (query.fromDate || query.toDate) {
      filter.targetDate = {};
      if (query.fromDate) {
        filter.targetDate.$gte = new Date(query.fromDate);
      }
      if (query.toDate) {
        filter.targetDate.$lte = new Date(query.toDate);
      }
    }

    return filter;
  }

  /**
   * Generate a new operation plan for a specific date
   * @param dto - Plan generation parameters
   * @param createdBy - User creating the plan
   * @returns Created operation plan
   */
  async generatePlan(
    dto: CreateOperationPlanDto,
    createdBy: string
  ): Promise<OperationPlanDto> {
    logger.info(`Generating ${dto.algorithm} plan for ${dto.targetDate}`);

    const targetDate = new Date(dto.targetDate);

    // Check if a plan already exists for this date
    const existingPlan = await this.operationPlanRepository.findByTargetDate(targetDate);
    if (
      existingPlan &&
      (existingPlan.status === 'APPROVED' || existingPlan.status === 'IN_EXECUTION')
    ) {
      throw new Error(`An ${existingPlan.status} plan already exists for ${dto.targetDate}`);
    }

    // Fetch VVNs from Core Backend (or use mock data)
    const vvns = process.env.USE_MOCK_VVNS === 'true' 
      ? this.getMockVvns(targetDate)
      : await this.fetchVvnsForDate(targetDate, dto.vvnIds);

    if (vvns.length === 0) {
      throw new Error(`No approved VVNs found for ${dto.targetDate}`);
    }

    logger.info(`Found ${vvns.length} approved VVNs for planning${process.env.USE_MOCK_VVNS === 'true' ? ' (MOCK DATA)' : ''}`);

    // Transform VVNs to Prolog input format
    const prologInput = this.transformVvnsToPrologInput(vvns, targetDate);

    // Generate plan using planning algorithm (calls Prolog - currently mock)
    const prologResponse = await this.planningClient.generatePlan(
      prologInput,
      dto.algorithm,
      targetDate
    );

    logger.info(
      `Planning algorithm returned sequence with ${prologResponse.sequence.length} operations, total delay: ${prologResponse.total_delay}h`
    );

    // Convert Prolog response to PlannedOperations
    const operations = this.convertPrologResponseToOperations(
      prologResponse.sequence,
      vvns,
      targetDate
    );

    // Create OperationPlan entity
    const operationPlan = new OperationPlan({
      targetDate,
      algorithm: dto.algorithm,
      createdBy,
      totalDelay: prologResponse.total_delay,
      operations,
    });

    // Save to repository
    const savedPlan = await this.operationPlanRepository.save(operationPlan);

    logger.info(`Operation plan ${savedPlan.operationPlanId} created successfully`);

    return this.toDto(savedPlan);
  }

  /**
   * Get operation plan by ID
   */
  async getPlanById(operationPlanId: string): Promise<OperationPlanDto | null> {
    const plan = await this.operationPlanRepository.findById(operationPlanId);
    return plan ? this.toDto(plan) : null;
  }

  /**
   * Get operation plan by target date
   */
  async getPlanByDate(targetDate: Date): Promise<OperationPlanDto | null> {
    const plan = await this.operationPlanRepository.findByTargetDate(targetDate);
    return plan ? this.toDto(plan) : null;
  }

  /**
   * Alias for getPlanByDate (used by controller)
   */
  async getPlanByTargetDate(targetDate: Date): Promise<OperationPlanDto | null> {
    return this.getPlanByDate(targetDate);
  }

  /**
   * Get all operation plans with optional filters
   */
  async getAllPlans(options?: QueryOptions): Promise<OperationPlanDto[]> {
    const plans = await this.operationPlanRepository.findAll(options);
    return plans.map((plan) => this.toDto(plan));
  }

  /**
   * Get plans by status
   */
  async getPlansByStatus(status: string, options?: QueryOptions): Promise<OperationPlanDto[]> {
    const plans = await this.operationPlanRepository.findByStatus(status as any, options);
    return plans.map((plan) => this.toDto(plan));
  }

  /**
   * Get plans within a date range
   */
  async getPlansByDateRange(
    fromDate: Date,
    toDate: Date,
    options?: QueryOptions
  ): Promise<OperationPlanDto[]> {
    const plans = await this.operationPlanRepository.findByDateRange(fromDate, toDate, options);
    return plans.map((plan) => this.toDto(plan));
  }

  /**
   * Approve an operation plan
   */
  async approvePlan(operationPlanId: string): Promise<OperationPlanDto> {
    const plan = await this.operationPlanRepository.findById(operationPlanId);
    if (!plan) {
      throw new Error(`Operation plan ${operationPlanId} not found`);
    }

    plan.approve();
    const updated = await this.operationPlanRepository.update(plan);

    logger.info(`Operation plan ${operationPlanId} approved`);
    return this.toDto(updated);
  }

  /**
   * Start execution of an operation plan
   */
  async startExecution(operationPlanId: string): Promise<OperationPlanDto> {
    const plan = await this.operationPlanRepository.findById(operationPlanId);
    if (!plan) {
      throw new Error(`Operation plan ${operationPlanId} not found`);
    }

    plan.startExecution();
    const updated = await this.operationPlanRepository.update(plan);

    logger.info(`Operation plan ${operationPlanId} execution started`);
    return this.toDto(updated);
  }

  /**
   * Complete an operation plan
   */
  async completePlan(operationPlanId: string): Promise<OperationPlanDto> {
    const plan = await this.operationPlanRepository.findById(operationPlanId);
    if (!plan) {
      throw new Error(`Operation plan ${operationPlanId} not found`);
    }

    plan.complete();
    const updated = await this.operationPlanRepository.update(plan);

    logger.info(`Operation plan ${operationPlanId} completed`);
    return this.toDto(updated);
  }

  /**
   * Update dock assignment for a specific VVN in the plan
   */
  async updateDockAssignment(
    operationPlanId: string,
    dto: UpdateDockAssignmentDto
  ): Promise<OperationPlanDto> {
    const plan = await this.operationPlanRepository.findById(operationPlanId);
    if (!plan) {
      throw new Error(`Operation plan ${operationPlanId} not found`);
    }

    plan.updateDockAssignment(dto.vvnId, dto.dockId);
    const updated = await this.operationPlanRepository.update(plan);

    logger.info(`Dock ${dto.dockId} assigned to VVN ${dto.vvnId} in plan ${operationPlanId}`);
    return this.toDto(updated);
  }

  /**
   * Update staff assignment for a specific VVN in the plan
   */
  async updateStaffAssignment(
    operationPlanId: string,
    dto: UpdateStaffAssignmentDto
  ): Promise<OperationPlanDto> {
    const plan = await this.operationPlanRepository.findById(operationPlanId);
    if (!plan) {
      throw new Error(`Operation plan ${operationPlanId} not found`);
    }

    plan.updateStaffAssignment(dto.vvnId, dto.staffIds);
    const updated = await this.operationPlanRepository.update(plan);

    logger.info(`Staff assigned to VVN ${dto.vvnId} in plan ${operationPlanId}`);
    return this.toDto(updated);
  }

  /**
   * US 4.1.4: Update operation (general update with audit logging)
   * Supports updating any editable field with change tracking
   */
  async updateOperation(
    operationPlanId: string,
    dto: UpdateOperationDto,
    userId: string,
    userName: string
  ): Promise<OperationPlanDto> {
    const plan = await this.operationPlanRepository.findById(operationPlanId);
    if (!plan) {
      throw new Error(`Operation plan ${operationPlanId} not found`);
    }

    logger.info('Updating operation in plan', {
      operationPlanId,
      vvnId: dto.vvnId,
      updates: dto.updates,
      userId,
      userName,
    });

    // Apply updates with audit logging (handled by domain entity)
    plan.updateOperation(dto.vvnId, dto.updates, userId, userName, dto.reason);

    // Persist changes
    const updated = await this.operationPlanRepository.update(plan);

    logger.info(`Operation ${dto.vvnId} updated in plan ${operationPlanId}`, {
      changedFields: Object.keys(dto.updates),
    });

    return this.toDto(updated);
  }

  /**
   * US 4.1.4: Detect conflicts for proposed operation update
   * Checks if proposed changes would create resource conflicts
   */
  async detectConflicts(
    operationPlanId: string,
    dto: ConflictDetectionRequestDto
  ): Promise<ConflictDetectionResultDto> {
    const targetPlan = await this.operationPlanRepository.findById(operationPlanId);
    if (!targetPlan) {
      throw new Error(`Operation plan ${operationPlanId} not found`);
    }

    // Get current operation
    const currentOperation = targetPlan.getOperationByVvnId(dto.vvnId);
    if (!currentOperation) {
      throw new Error(`Operation with VVN ID ${dto.vvnId} not found in plan`);
    }

    // Create hypothetical updated operation
    const updatedOperation = currentOperation.withUpdates(dto.updates);

    // Get all plans for the same date (to check conflicts)
    const targetDate = targetPlan.targetDate;
    const allPlansForDate = await this.operationPlanRepository.findByDateRange(
      targetDate,
      targetDate
    );

    // Detect conflicts using domain service
    const result = this.conflictDetectionService.detectConflicts(
      targetPlan,
      dto.vvnId,
      updatedOperation,
      allPlansForDate
    );

    logger.debug('Conflict detection completed', {
      operationPlanId,
      vvnId: dto.vvnId,
      hasConflicts: result.hasConflicts,
      conflictCount: result.conflicts.length,
    });

    return this.toConflictDetectionDto(result);
  }

  /**
   * US 4.1.4: Convert conflict detection result to DTO
   */
  private toConflictDetectionDto(result: ConflictDetectionResult): ConflictDetectionResultDto {
    return {
      hasConflicts: result.hasConflicts,
      hasErrors: result.hasErrors,
      hasWarnings: result.hasWarnings,
      conflicts: result.conflicts.map((c): ResourceConflictDto => ({
        type: c.type,
        resourceId: c.resourceId,
        resourceName: c.resourceName,
        conflictingPlanId: c.conflictingPlanId,
        conflictingVvnId: c.conflictingVvnId,
        conflictingVesselName: c.conflictingVesselName,
        timeOverlap: {
          start: c.timeOverlap.start.toISOString(),
          end: c.timeOverlap.end.toISOString(),
          durationMinutes: c.timeOverlap.durationMinutes,
        },
        severity: c.severity,
        message: c.message,
      })),
      summary: result.summary,
    };
  }

  /**
   * Delete an operation plan
   */
  async deletePlan(operationPlanId: string): Promise<void> {
    const plan = await this.operationPlanRepository.findById(operationPlanId);
    if (!plan) {
      throw new Error(`Operation plan ${operationPlanId} not found`);
    }

    if (plan.status === 'IN_EXECUTION' || plan.status === 'COMPLETED') {
      throw new Error(`Cannot delete plan with status ${plan.status}`);
    }

    await this.operationPlanRepository.delete(operationPlanId);
    logger.info(`Operation plan ${operationPlanId} deleted`);
  }

  /**
   * Find plans by filters with pagination
   * US 4.1.3: Enhanced with vvnId and vesselImo filtering
   */
  async findByFilters(
    filter: any,
    skip: number,
    limit: number,
    sortBy: string,
    sortOrder: string
  ): Promise<OperationPlanDto[]> {
    const options: QueryOptions = {
      page: Math.floor(skip / limit) + 1,
      limit,
      sortBy,
      sortOrder: sortOrder === 'asc' ? 'asc' : 'desc',
    };

    let plans: OperationPlan[];

    // Priority order for filters (most specific first)
    if (filter.vvnId) {
      // Filter by VVN ID (US 4.1.3 - vessel identifier filtering)
      // Get all plans and filter by operations containing the VVN
      plans = await this.operationPlanRepository.findAll(options);
      plans = plans.filter((p) => p.operations.some((op) => op.vvnId === filter.vvnId));

      // Apply additional filters if present
      if (filter.status) {
        plans = plans.filter((p) => p.status === filter.status);
      }
      if (filter.algorithm) {
        plans = plans.filter((p) => p.algorithm === filter.algorithm);
      }
      if (filter.targetDate && !filter.targetDate.$gte && !filter.targetDate.$lte) {
        const targetDateStr = filter.targetDate.toISOString().split('T')[0];
        plans = plans.filter(
          (p) => p.targetDate.toISOString().split('T')[0] === targetDateStr
        );
      }
    } else if (filter.vesselImo) {
      // Filter by Vessel IMO (US 4.1.3 - vessel identifier filtering)
      // Note: This requires fetching VVN data from Core Backend
      // For now, get all plans and filter on the frontend
      // TODO: Optimize by storing vessel info denormalized or querying Core Backend
      logger.warn(
        'Vessel IMO filtering not fully implemented - requires VVN data from Core Backend'
      );
      plans = await this.operationPlanRepository.findAll(options);
      // This filter will be applied in the frontend for now
    } else if (filter.targetDate && !filter.targetDate.$gte && !filter.targetDate.$lte) {
      // Single date query
      plans = [await this.operationPlanRepository.findByTargetDate(filter.targetDate)].filter(
        Boolean
      ) as OperationPlan[];
    } else if (filter.targetDate && (filter.targetDate.$gte || filter.targetDate.$lte)) {
      // Date range query
      const fromDate = filter.targetDate.$gte || new Date(0);
      const toDate = filter.targetDate.$lte || new Date('2099-12-31');
      plans = await this.operationPlanRepository.findByDateRange(fromDate, toDate, options);

      // Apply additional filters
      if (filter.status) {
        plans = plans.filter((p) => p.status === filter.status);
      }
      if (filter.algorithm) {
        plans = plans.filter((p) => p.algorithm === filter.algorithm);
      }
    } else if (filter.status) {
      // Status filter
      plans = await this.operationPlanRepository.findByStatus(filter.status, options);

      // Apply algorithm filter if present
      if (filter.algorithm) {
        plans = plans.filter((p) => p.algorithm === filter.algorithm);
      }
    } else if (filter.algorithm) {
      // Algorithm filter
      plans = await this.operationPlanRepository.findAll(options);
      plans = plans.filter((p) => p.algorithm === filter.algorithm);
    } else {
      // All plans
      plans = await this.operationPlanRepository.findAll(options);
    }

    return plans.map((plan) => this.toDto(plan));
  }

  /**
   * Count plans by filters
   */
  async countByFilters(filter: any): Promise<number> {
    // This is a simplified implementation
    // In production, you'd want a more efficient count query in the repository
    const allPlans = await this.findByFilters(filter, 0, 10000, 'createdAt', 'desc');
    return allPlans.length;
  }

  /**
   * Get VVNs without operation plans for a specific date
   */
  async getVvnsWithoutPlans(_targetDate: Date): Promise<VvnReference[]> {
    // This method needs a JWT token, but controller should provide it
    // For now, we'll throw an error indicating implementation needed
    throw new Error('getVvnsWithoutPlans requires authentication token - use controller method');
  }

  /**
   * Get VVNs without operation plans (with auth token)
   */
  async getVvnsWithoutPlansWithAuth(targetDate: Date): Promise<VvnReference[]> {
    logger.info(`Checking for VVNs without operation plans on ${targetDate.toISOString()}`);

    // Get existing plan for the date
    const existingPlan = await this.operationPlanRepository.findByTargetDate(targetDate);

    // Fetch all approved VVNs for the date
    const startOfDay = new Date(targetDate);
    startOfDay.setHours(0, 0, 0, 0);
    const endOfDay = new Date(targetDate);
    endOfDay.setHours(23, 59, 59, 999);

    const allVvns = await this.coreBackendClient.getVvnsByDateRange(
      startOfDay,
      endOfDay
    );

    if (!existingPlan) {
      // No plan exists, all VVNs are missing
      logger.info(
        `No operation plan exists for ${targetDate.toISOString()}, ${allVvns.length} VVNs missing`
      );
      return allVvns;
    }

    // Filter out VVNs that are already in the plan
    const plannedVvnIds = new Set(existingPlan.operations.map((op) => op.vvnId));
    const missingVvns = allVvns.filter((vvn) => !plannedVvnIds.has(vvn.vvnId));

    logger.info(
      `Found ${missingVvns.length} VVNs without operation plans for ${targetDate.toISOString()}`
    );
    return missingVvns;
  }

  /**
   * Get resource allocation for a specific resource type and period
   */
  async getResourceAllocation(
    resourceType: string,
    resourceId: string,
    fromDate: Date,
    toDate: Date
  ): Promise<any> {
    logger.info(
      `Calculating resource allocation for ${resourceType}:${resourceId} from ${fromDate.toISOString()} to ${toDate.toISOString()}`
    );

    const plans = await this.operationPlanRepository.findByDateRange(fromDate, toDate);

    let totalAllocatedTime = 0;
    let operationCount = 0;
    const operations: any[] = [];

    for (const plan of plans) {
      for (const operation of plan.operations) {
        let isResourceUsed = false;

        switch (resourceType.toLowerCase()) {
          case 'crane':
            // Check if crane count matches (simplified - in real system would match crane IDs)
            isResourceUsed = operation.assignedCranes > 0;
            break;
          case 'dock':
            isResourceUsed = operation.assignedDock === resourceId;
            break;
          case 'staff':
            isResourceUsed = operation.assignedStaff?.includes(resourceId) || false;
            break;
          default:
            throw new Error(`Unknown resource type: ${resourceType}`);
        }

        if (isResourceUsed) {
          const duration =
            (operation.plannedEnd.getTime() - operation.plannedStart.getTime()) / (1000 * 60 * 60); // hours
          totalAllocatedTime += duration;
          operationCount++;
          operations.push({
            planId: plan.operationPlanId,
            vvnId: operation.vvnId,
            vesselImo: operation.vesselImo,
            plannedStart: operation.plannedStart,
            plannedEnd: operation.plannedEnd,
            duration,
          });
        }
      }
    }

    return {
      resourceType,
      resourceId,
      fromDate: fromDate.toISOString(),
      toDate: toDate.toISOString(),
      totalAllocatedTime,
      operationCount,
      operations,
    };
  }

  /**
   * Transition plan status (generic method)
   */
  async transitionStatus(operationPlanId: string, newStatus: string): Promise<OperationPlanDto> {
    const plan = await this.operationPlanRepository.findById(operationPlanId);
    if (!plan) {
      throw new Error(`Operation plan ${operationPlanId} not found`);
    }

    switch (newStatus) {
      case 'APPROVED':
        plan.approve();
        break;
      case 'IN_EXECUTION':
        plan.startExecution();
        break;
      case 'COMPLETED':
        plan.complete();
        break;
      default:
        throw new Error(`Invalid status transition: ${newStatus}`);
    }

    const updated = await this.operationPlanRepository.update(plan);
    logger.info(`Operation plan ${operationPlanId} transitioned to ${newStatus}`);
    return this.toDto(updated);
  }

  /**
   * Generate plan with simpler signature for controller
   */
  async generatePlanSimple(
    targetDate: Date | string,
    algorithm: PlanningAlgorithm,
    createdBy: string,
    vvnIds?: string[]
  ): Promise<OperationPlanDto> {
    const targetDateStr = (
      typeof targetDate === 'string' ? targetDate : (targetDate as Date).toISOString().split('T')[0]
    )!;

    const dto: CreateOperationPlanDto = {
      targetDate: targetDateStr,
      algorithm,
      vvnIds,
    };

    return this.generatePlan(dto, createdBy);
  }

  /**
   * Generate mock VVN data for testing
   */
  private getMockVvns(targetDate: Date): VvnReference[] {
    const eta1 = new Date(targetDate);
    eta1.setHours(8, 0, 0, 0); // 8:00 AM
    const etd1 = new Date(targetDate);
    etd1.setHours(20, 0, 0, 0); // 8:00 PM (12 hours later)

    const eta2 = new Date(targetDate);
    eta2.setHours(10, 0, 0, 0); // 10:00 AM
    const etd2 = new Date(targetDate);
    etd2.setHours(18, 0, 0, 0); // 6:00 PM (8 hours later)

    return [
      {
        vvnId: 'VVN-MOCK-001',
        vesselImo: '9876543',
        eta: eta1,
        etd: etd1,
        loadingCount: 75,
        unloadingCount: 75,
        purpose: OperationType.BOTH,
        state: 'APPROVED'
      },
      {
        vvnId: 'VVN-MOCK-002',
        vesselImo: '9876544',
        eta: eta2,
        etd: etd2,
        loadingCount: 100,
        unloadingCount: 100,
        purpose: OperationType.BOTH,
        state: 'APPROVED'
      }
    ];
  }

  /**
   * Fetch VVNs for a specific date (all approved or specific IDs)
   */
  private async fetchVvnsForDate(
    targetDate: Date,
    vvnIds?: string[]
  ): Promise<VvnReference[]> {
    if (vvnIds && vvnIds.length > 0) {
      // Fetch specific VVNs
      const vvns: VvnReference[] = [];
      for (const vvnId of vvnIds) {
        const vvn = await this.coreBackendClient.getVvnById(vvnId);
        if (vvn) {
          vvns.push(vvn);
        }
      }
      return vvns;
    } else {
      // Fetch all approved VVNs for the date
      const startOfDay = new Date(targetDate);
      startOfDay.setHours(0, 0, 0, 0);
      const endOfDay = new Date(targetDate);
      endOfDay.setHours(23, 59, 59, 999);

      return this.coreBackendClient.getVvnsByDateRange(startOfDay, endOfDay);
    }
  }

  /**
   * Transform VVNs to Prolog input format
   * Calculates processing times based on container counts
   */
  private transformVvnsToPrologInput(
    vvns: VvnReference[],
    targetDate: Date
  ): PlanningVesselInput[] {
    return vvns.map((vvn) => {
      const eta = new Date(vvn.eta);
      const etd = new Date(vvn.etd);

      // Convert times to hours since midnight on target date
      const arrivalHours = this.getHoursSinceMidnight(eta, targetDate);
      const departureHours = this.getHoursSinceMidnight(etd, targetDate);

      // Calculate processing times
      const unloadTime = this.calculateUnloadTime(vvn.unloadingCount);
      const loadTime = this.calculateLoadTime(vvn.loadingCount);

      return {
        name: vvn.vvnId,
        arrival: arrivalHours,
        departure: departureHours,
        unload: unloadTime,
        load: loadTime,
      };
    });
  }

  /**
   * Calculate hours since midnight for a given date
   */
  private getHoursSinceMidnight(dateTime: Date, referenceDate: Date): number {
    const midnight = new Date(referenceDate);
    midnight.setHours(0, 0, 0, 0);

    const diffMs = dateTime.getTime() - midnight.getTime();
    return diffMs / (1000 * 60 * 60); // Convert to hours
  }

  /**
   * Calculate unload time based on container count
   * Formula: containers / UNLOAD_RATE
   */
  private calculateUnloadTime(containerCount: number): number {
    if (containerCount === 0) return 0;
    return Math.ceil((containerCount / OperationPlanService.UNLOAD_RATE) * 10) / 10; // Round to 1 decimal
  }

  /**
   * Calculate load time based on container count
   * Formula: containers / LOAD_RATE
   */
  private calculateLoadTime(containerCount: number): number {
    if (containerCount === 0) return 0;
    return Math.ceil((containerCount / OperationPlanService.LOAD_RATE) * 10) / 10; // Round to 1 decimal
  }

  /**
   * Map Core Backend purpose to OEM Backend OperationType
   */
  private mapPurposeToOperationType(purpose: string): OperationType {
    switch (purpose) {
      case 'LOAD_UNLOAD':
        return OperationType.BOTH;
      case 'LOAD_ONLY':
        return OperationType.LOAD;
      case 'UNLOAD_ONLY':
        return OperationType.UNLOAD;
      default:
        logger.warn(`Unknown purpose: ${purpose}, defaulting to BOTH`);
        return OperationType.BOTH;
    }
  }

  /**
   * Convert Prolog response to PlannedOperation entities
   */
  private convertPrologResponseToOperations(
    sequence: any[],
    vvns: VvnReference[],
    targetDate: Date
  ): PlannedOperation[] {
    const midnight = new Date(targetDate);
    midnight.setHours(0, 0, 0, 0);

    return sequence.map((item) => {
      const vvn = vvns.find((v) => v.vvnId === item.vessel);
      if (!vvn) {
        throw new Error(`VVN ${item.vessel} not found in input list`);
      }

      // Convert hours since midnight to Date objects
      const plannedStart = new Date(midnight.getTime() + item.start * 60 * 60 * 1000);
      const plannedEnd = new Date(midnight.getTime() + item.end * 60 * 60 * 1000);

      // Determine number of cranes (from multi_cranes algorithm or default to 1)
      const assignedCranes = item.cranes || 1;

      return new PlannedOperation({
        vvnId: vvn.vvnId,
        vesselImo: vvn.vesselImo,
        plannedStart,
        plannedEnd,
        assignedCranes,
        operationType: this.mapPurposeToOperationType(vvn.purpose),
      });
    });
  }

  /**
   * Convert OperationPlan entity to DTO
   */
  private toDto(plan: OperationPlan): OperationPlanDto {
    return {
      operationPlanId: plan.operationPlanId,
      targetDate: plan.targetDate.toISOString().split('T')[0] as string, // YYYY-MM-DD
      algorithm: plan.algorithm,
      status: plan.status,
      totalDelay: plan.totalDelay,
      operations: plan.operations.map((op) => this.operationToDto(op)),
      createdAt: plan.createdAt.toISOString(),
      createdBy: plan.createdBy,
      auditLog: plan.auditLog.map((entry) => this.auditEntryToDto(entry)), // US 4.1.4
    };
  }

  /**
   * US 4.1.4: Convert audit entry to DTO
   */
  private auditEntryToDto(entry: any): OperationPlanAuditEntryDto {
    return {
      timestamp: entry.timestamp.toISOString(),
      userId: entry.userId,
      userName: entry.userName,
      action: entry.action,
      vvnId: entry.vvnId,
      changes: entry.changes,
      reason: entry.reason,
    };
  }

  /**
   * Convert PlannedOperation to DTO
   */
  private operationToDto(operation: PlannedOperation): PlannedOperationDto {
    return {
      vvnId: operation.vvnId,
      vesselImo: operation.vesselImo,
      plannedStart: operation.plannedStart.toISOString(),
      plannedEnd: operation.plannedEnd.toISOString(),
      assignedCranes: operation.assignedCranes,
      operationType: operation.operationType,
      assignedDock: operation.assignedDock,
      assignedStaff: operation.assignedStaff,
    };
  }
}
