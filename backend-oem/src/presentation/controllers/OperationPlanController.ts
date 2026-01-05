import { Request, Response, NextFunction } from 'express';
import { OperationPlanService } from '@application/services/OperationPlanService';
import { logger } from '@shared/utils/logger';
import { OperationPlanStatus } from '@shared/types';

/**
 * OperationPlanController
 * Handles HTTP requests for Operation Plan management
 *
 * User Stories:
 * - US 4.1.1: Module setup (architectural foundation)
 * - US 4.1.2: Generate operation plan for a given day
 * - US 4.1.3: Search and list operation plans
 * - US 4.1.4: Manually update operation plan
 * - US 4.1.5: Identify VVNs without operation plans
 * - US 4.1.6: Query resource allocation time
 */
export class OperationPlanController {
  constructor(private operationPlanService: OperationPlanService) {}

  /**
   * US 4.1.2: Generate operation plan
   * POST /api/operationplans/generate
   */
  async generatePlan(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { targetDate, algorithm, vvnIds } = req.body;
      const userId = (req as any).user?.sub || (req as any).user?.email; // From JWT middleware - use sub (user ID) or email as fallback

      logger.info('Generating operation plan', { targetDate, algorithm, userId });

      const plan = await this.operationPlanService.generatePlanSimple(
        targetDate!,
        algorithm!,
        userId!,
        vvnIds
      );

      res.status(201).json({
        success: true,
        message: 'Operation plan generated successfully',
        data: plan,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.3: List and filter operation plans
   * GET /api/operationplans
   * Query params: targetDate, algorithm, status, vvnId, vesselImo, fromDate, toDate, page, limit, sortBy, sortOrder
   */
  async listPlans(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const query = req.query;

      logger.debug('Listing operation plans', query);

      const result = await this.operationPlanService.listOperationPlans(query);

      res.json({
        success: true,
        data: result.data,
        pagination: result.pagination,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.3: Get operation plan by ID
   * GET /api/operationplans/:id
   */
  async getPlanById(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { id } = req.params;

      logger.debug('Getting operation plan by ID', { id });

      const plan = await this.operationPlanService.getPlanById(id!);

      res.json({
        success: true,
        data: plan,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.3: Get operation plan by target date
   * GET /api/operationplans/by-date/:date
   */
  async getPlanByDate(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { date } = req.params;

      logger.debug('Getting operation plan by date', { date });

      const plan = await this.operationPlanService.getPlanByTargetDate(new Date(date!));

      res.json({
        success: true,
        data: plan,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.4: Update operation plan (dock assignment)
   * PATCH /api/operationplans/:id/dock
   */
  async updateDockAssignment(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { id } = req.params;
      const { vvnId, dockId } = req.body;

      logger.info('Updating dock assignment', { id, vvnId, dockId });

      const plan = await this.operationPlanService.updateDockAssignment(id!, { vvnId, dockId });

      res.json({
        success: true,
        message: 'Dock assignment updated successfully',
        data: plan,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.4: Update operation plan (staff assignment)
   * PATCH /api/operationplans/:id/staff
   */
  async updateStaffAssignment(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { id } = req.params;
      const { vvnId, staffIds } = req.body;

      logger.info('Updating staff assignment', { id, vvnId, staffIds });

      const plan = await this.operationPlanService.updateStaffAssignment(id!, { vvnId, staffIds });

      res.json({
        success: true,
        message: 'Staff assignment updated successfully',
        data: plan,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.4: Update operation (general update)
   * PATCH /api/v1/operation-plans/:id/operations
   */
  async updateOperation(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { id } = req.params;
      const { vvnId, updates, reason } = req.body;

      // Get user info from JWT (set by authMiddleware)
      const userId = (req as any).user?.userId || 'unknown';
      const userName = (req as any).user?.name || 'Unknown User';

      logger.info('Updating operation in plan', { id, vvnId, userId });

      const dto = {
        vvnId,
        updates: {
          plannedStart: updates.plannedStart ? new Date(updates.plannedStart) : undefined,
          plannedEnd: updates.plannedEnd ? new Date(updates.plannedEnd) : undefined,
          assignedCranes: updates.assignedCranes,
          assignedDock: updates.assignedDock,
          assignedStaff: updates.assignedStaff,
        },
        reason,
      };

      const plan = await this.operationPlanService.updateOperation(id!, dto, userId, userName);

      res.json({
        success: true,
        message: 'Operation updated successfully',
        data: plan,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.4: Detect conflicts
   * POST /api/v1/operation-plans/:id/conflicts
   */
  async detectConflicts(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { id } = req.params;
      const { vvnId, updates } = req.body;

      logger.debug('Detecting conflicts for operation update', { id, vvnId });

      const dto = {
        vvnId,
        updates: {
          plannedStart: updates.plannedStart ? new Date(updates.plannedStart) : undefined,
          plannedEnd: updates.plannedEnd ? new Date(updates.plannedEnd) : undefined,
          assignedCranes: updates.assignedCranes,
          assignedDock: updates.assignedDock,
          assignedStaff: updates.assignedStaff,
        },
      };

      const result = await this.operationPlanService.detectConflicts(id!, dto);

      res.json({
        success: true,
        data: result,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.4: Approve operation plan
   * PATCH /api/operationplans/:id/approve
   */
  async approvePlan(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { id } = req.params;

      logger.info('Approving operation plan', { id });

      const plan = await this.operationPlanService.transitionStatus(
        id!,
        OperationPlanStatus.APPROVED
      );

      res.json({
        success: true,
        message: 'Operation plan approved successfully',
        data: plan,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.5: Identify VVNs without operation plans
   * GET /api/operationplans/missing
   * Query params: date (required)
   */
  async getMissingPlans(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { date } = req.query;

      if (!date) {
        res.status(400).json({
          success: false,
          message: 'Date parameter is required',
        });
        return;
      }

      logger.debug('Getting VVNs without operation plans', { date });

      const missingVvns = await this.operationPlanService.getVvnsWithoutPlans(
        new Date(date as string)
      );

      res.json({
        success: true,
        data: missingVvns,
        count: missingVvns.length,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.6: Query resource allocation time
   * GET /api/operationplans/resource-allocation
   * Query params: resourceType, resourceId, fromDate, toDate
   */
  async getResourceAllocation(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { resourceType, resourceId, fromDate, toDate } = req.query;

      if (!resourceType || !resourceId || !fromDate || !toDate) {
        res.status(400).json({
          success: false,
          message: 'resourceType, resourceId, fromDate, and toDate are required',
        });
        return;
      }

      logger.debug('Getting resource allocation', {
        resourceType,
        resourceId,
        fromDate,
        toDate,
      });

      const allocation = await this.operationPlanService.getResourceAllocation(
        resourceType as string,
        resourceId as string,
        new Date(fromDate as string),
        new Date(toDate as string)
      );

      res.json({
        success: true,
        data: allocation,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * Delete operation plan (only if PENDING)
   * DELETE /api/operationplans/:id
   */
  async deletePlan(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { id } = req.params;

      logger.info('Deleting operation plan', { id });

      await this.operationPlanService.deletePlan(id!);

      res.json({
        success: true,
        message: 'Operation plan deleted successfully',
      });
    } catch (error) {
      next(error);
    }
  }
}
