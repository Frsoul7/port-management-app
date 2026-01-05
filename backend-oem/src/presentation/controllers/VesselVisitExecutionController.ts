import { Request, Response, NextFunction } from 'express';
import { VesselVisitExecutionService } from '@application/services/VesselVisitExecutionService';
import { logger } from '@shared/utils/logger';

/**
 * VesselVisitExecutionController
 * Handles HTTP requests for Vessel Visit Execution (VVE) management
 *
 * User Stories:
 * - US 4.1.7: Create VVE when vessel arrives
 * - US 4.1.8: Update VVE with berth time and dock
 * - US 4.1.9: Update VVE with executed operations
 * - US 4.1.10: Search and list VVEs
 * - US 4.1.11: Mark VVE as completed
 */
export class VesselVisitExecutionController {
  constructor(private vveService: VesselVisitExecutionService) {}

  /**
   * US 4.1.7: Initialize VVE from operation plan
   * US 4.1.9: Now derives ExecutedOperations from PlannedOperations
   * POST /api/vessel-visit-executions/initialize
   */
  async initializeFromPlan(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { operationPlanId, vvnId, arrivalTime } = req.body;
      const user = (req as any).user; // JWT payload from auth middleware

      if (!user || !user.sub) {
        res.status(401).json({
          success: false,
          message: 'User authentication required',
        });
        return;
      }

      logger.info('Initializing VVE from operation plan', {
        operationPlanId,
        vvnId,
        arrivalTime,
        createdBy: user.sub,
      });

      const vve = await this.vveService.initializeVveFromPlan({
        operationPlanId,
        vvnId, // US 4.1.9: VVN ID to filter planned operations
        arrivalTime,
        createdBy: user.sub,
      });

      res.status(201).json({
        success: true,
        message: 'Vessel visit execution initialized successfully',
        data: vve,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.7: Record port arrival
   * POST /api/vessel-visit-executions/:id/arrival
   */
  async recordPortArrival(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { id } = req.params;
      const { arrivalTime } = req.body;

      logger.info('Recording port arrival', { id, arrivalTime });

      const vve = await this.vveService.recordPortArrival(id!, { arrivalTime });

      res.json({
        success: true,
        message: 'Port arrival recorded successfully',
        data: vve,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.8: Record berthing with dock assignment
   * POST /api/vessel-visit-executions/:id/berth
   */
  async recordBerthing(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { id } = req.params;
      const { berthTime, dockId } = req.body;

      logger.info('Recording berthing', { id, berthTime, dockId });

      const vve = await this.vveService.recordBerthing(id!, { berthTime, dockId });

      res.json({
        success: true,
        message: 'Berthing recorded successfully',
        data: vve,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.9: Add executed operation
   * POST /api/vessel-visit-executions/:id/operations
   */
  async addExecutedOperation(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { id } = req.params;
      const { operationType, startTime, cranesUsed, staffAssigned } = req.body;

      logger.info('Adding executed operation', { id, operationType, startTime });

      const vve = await this.vveService.addExecutedOperation(id!, {
        operationType,
        startTime,
        cranesUsed,
        staffAssigned: staffAssigned || [],
      });

      res.status(201).json({
        success: true,
        message: 'Executed operation added successfully',
        data: vve,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.9: Update executed operation
   * PATCH /api/vessel-visit-executions/:id/operations/:operationIndex
   */
  async updateExecutedOperation(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { id, operationIndex } = req.params;
      const { startTime, endTime, containersProcessed, cranesUsed, staffAssigned, status } = req.body;
      const user = (req as any).user; // JWT payload

      if (!user || !user.sub) {
        res.status(401).json({
          success: false,
          message: 'User authentication required',
        });
        return;
      }

      logger.info('Updating executed operation', {
        vveId: id,
        operationIndex,
        status,
        updatedBy: user.sub,
      });

      const vve = await this.vveService.updateExecutedOperation(id!, {
        operationIndex: parseInt(operationIndex!),
        startTime,
        endTime,
        containersProcessed,
        cranesUsed,
        staffAssigned,
        status,
        updatedBy: user.sub,
      });

      res.json({
        success: true,
        message: 'Executed operation updated successfully',
        data: vve,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.9: Record unberthing
   * POST /api/vessel-visit-executions/:id/unberth
   */
  async recordUnberthing(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { id } = req.params;
      const { unberthTime } = req.body;

      logger.info('Recording unberthing', { id, unberthTime });

      const vve = await this.vveService.recordUnberthing(id!, { unberthTime });

      res.json({
        success: true,
        message: 'Unberthing recorded successfully',
        data: vve,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.11: Record port departure and mark as completed
   * POST /api/vessel-visit-executions/:id/complete
   */
  async recordCompletion(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { id } = req.params;
      const { departureTime } = req.body;
      const userId = (req as any).user?.id; // From JWT middleware

      logger.info('Recording port departure and completion', { id, departureTime, userId });

      const vve = await this.vveService.completeVve(id!, {
        departureTime,
        completedBy: userId,
      });

      res.json({
        success: true,
        message: 'Vessel visit execution completed successfully',
        data: vve,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.10: List and filter VVEs
   * GET /api/vessel-visit-executions
   * Query params: vvnId, status, fromDate, toDate, page, limit, sortBy, sortOrder
   */
  async listExecutions(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const query = req.query;
      logger.debug('Listing vessel visit executions', query);

      const result = await this.vveService.listVesselVisitExecutions(query);

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
   * US 4.1.10: Get VVE by ID
   * GET /api/vessel-visit-executions/:id
   */
  async getExecutionById(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { id } = req.params;

      logger.debug('Getting vessel visit execution by ID', { id });

      const vve = await this.vveService.getById(id!);

      res.json({
        success: true,
        data: vve,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.10: Get VVE by VVN ID
   * GET /api/vessel-visit-executions/by-vvn/:vvnId
   */
  async getExecutionByVvnId(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { vvnId } = req.params;

      logger.debug('Getting vessel visit execution by VVN ID', { vvnId });

      const vve = await this.vveService.getByVvnId(vvnId!);

      res.json({
        success: true,
        data: vve,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * Link incident to VVE
   * POST /api/vessel-visit-executions/:id/incidents
   */
  async linkIncident(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { id } = req.params;
      const { incidentId } = req.body;

      logger.info('Linking incident to VVE', { id, incidentId });

      const vve = await this.vveService.linkIncident(id!, { incidentId });

      res.json({
        success: true,
        message: 'Incident linked successfully',
        data: vve,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * Unlink incident from VVE
   * DELETE /api/vessel-visit-executions/:id/incidents/:incidentId
   */
  async unlinkIncident(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { id, incidentId } = req.params;

      logger.info('Unlinking incident from VVE', { id, incidentId });

      const vve = await this.vveService.unlinkIncident(id!, { incidentId: incidentId! });

      res.json({
        success: true,
        message: 'Incident unlinked successfully',
        data: vve,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * Get VVE execution metrics
   * GET /api/vessel-visit-executions/:id/metrics
   */
  async getMetrics(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { id } = req.params;
      logger.debug('Getting VVE metrics', { id });

      const metrics = await this.vveService.getMetrics(id!);

      res.json({
        success: true,
        data: metrics,
      });
    } catch (error) {
      next(error);
    }
  }

}
