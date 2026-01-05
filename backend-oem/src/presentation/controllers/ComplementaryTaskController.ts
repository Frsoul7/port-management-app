import { Request, Response, NextFunction } from 'express';
import { ComplementaryTaskService } from '@application/services/ComplementaryTaskService';
import { ComplementaryTaskStatus } from '@shared/types';
import { logger } from '@shared/utils/logger';

/**
 * ComplementaryTaskController
 * Handles HTTP requests for Complementary Task management
 *
 * User Story:
 * - US 4.1.15: Record and manage complementary tasks during vessel visits
 */
export class ComplementaryTaskController {
  constructor(private complementaryTaskService: ComplementaryTaskService) {}

  /**
   * US 4.1.15: Create new complementary task
   * POST /api/v1/complementary-tasks
   */
  async createTask(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const dto = req.body;

      logger.info('Creating complementary task', { title: dto.title });

      const task = await this.complementaryTaskService.createComplementaryTask(dto);

      res.status(201).json({
        success: true,
        message: 'Complementary task created successfully',
        data: task,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.15: Get complementary task by ID
   * GET /api/v1/complementary-tasks/:id
   */
  async getTaskById(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { id } = req.params;

      logger.info('Fetching complementary task by ID', { id });

      const task = await this.complementaryTaskService.getComplementaryTaskById(id!);

      res.json({
        success: true,
        data: task,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.15: Update complementary task
   * PATCH /api/v1/complementary-tasks/:id
   */
  async updateTask(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { id } = req.params;
      const updates = req.body;

      logger.info('Updating complementary task', { id });

      const task = await this.complementaryTaskService.updateComplementaryTask(id!, updates);

      res.json({
        success: true,
        message: 'Complementary task updated successfully',
        data: task,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.15: Delete complementary task
   * DELETE /api/v1/complementary-tasks/:id
   */
  async deleteTask(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { id } = req.params;

      logger.info('Deleting complementary task', { id });

      await this.complementaryTaskService.deleteComplementaryTask(id!);

      res.json({
        success: true,
        message: 'Complementary task deleted successfully',
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.15: List complementary tasks with filters
   * GET /api/v1/complementary-tasks
   * Query params: status, vveId, taskCategoryId, assignedTo, fromDate, toDate, sortBy, sortOrder, page, limit
   */
  async listTasks(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const query = req.query;

      logger.debug('Listing complementary tasks', query);

      const result = await this.complementaryTaskService.listComplementaryTasksPaginated(query);

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
   * US 4.1.15: List tasks by VVE
   * GET /api/v1/complementary-tasks/vve/:vveId
   */
  async listTasksByVve(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { vveId } = req.params;
      const { sortBy, sortOrder } = req.query;

      logger.info('Listing tasks by VVE', { vveId });

      const options = {
        sortBy: sortBy as string,
        sortOrder: sortOrder as 'asc' | 'desc',
      };

      const tasks = await this.complementaryTaskService.listTasksByVveId(vveId!, options);

      res.json({
        success: true,
        data: tasks,
        count: tasks.length,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.15: List tasks by status
   * GET /api/v1/complementary-tasks/status/:status
   */
  async listTasksByStatus(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { status } = req.params;
      const { sortBy, sortOrder } = req.query;

      logger.info('Listing tasks by status', { status });

      const options = {
        sortBy: sortBy as string,
        sortOrder: sortOrder as 'asc' | 'desc',
      };

      const tasks = await this.complementaryTaskService.listTasksByStatus(
        status as ComplementaryTaskStatus,
        options
      );

      res.json({
        success: true,
        data: tasks,
        count: tasks.length,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.15: Get active tasks impacting operations
   * GET /api/v1/complementary-tasks/active-impacting
   */
  async getActiveImpactingTasks(_req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      logger.info('Fetching active tasks impacting operations');

      const tasks = await this.complementaryTaskService.getActiveTasksImpactingOperations();

      res.json({
        success: true,
        data: tasks,
        count: tasks.length,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.15: Assign task to team/user
   * PATCH /api/v1/complementary-tasks/:id/assign
   */
  async assignTask(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { id } = req.params;
      const dto = req.body;

      logger.info('Assigning task', { id, assignedTo: dto.assignedTo });

      const task = await this.complementaryTaskService.assignTask(id!, dto);

      res.json({
        success: true,
        message: 'Task assigned successfully',
        data: task,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.15: Start task
   * PATCH /api/v1/complementary-tasks/:id/start
   */
  async startTask(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { id } = req.params;
      const dto = req.body;

      logger.info('Starting task', { id });

      const task = await this.complementaryTaskService.startTask(id!, dto);

      res.json({
        success: true,
        message: 'Task started successfully',
        data: task,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.15: Complete task
   * PATCH /api/v1/complementary-tasks/:id/complete
   */
  async completeTask(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { id } = req.params;
      const dto = req.body;

      logger.info('Completing task', { id, completedBy: dto.completedBy });

      const task = await this.complementaryTaskService.completeTask(id!, dto);

      res.json({
        success: true,
        message: 'Task completed successfully',
        data: task,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.15: Cancel task
   * PATCH /api/v1/complementary-tasks/:id/cancel
   */
  async cancelTask(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { id } = req.params;
      const dto = req.body;

      logger.info('Cancelling task', { id, cancelledBy: dto.cancelledBy });

      const task = await this.complementaryTaskService.cancelTask(id!, dto);

      res.json({
        success: true,
        message: 'Task cancelled successfully',
        data: task,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.15: Add note to task
   * POST /api/v1/complementary-tasks/:id/notes
   */
  async addNote(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { id } = req.params;
      const dto = req.body;

      logger.info('Adding note to task', { id, author: dto.author });

      const task = await this.complementaryTaskService.addTaskNote(id!, dto);

      res.json({
        success: true,
        message: 'Note added successfully',
        data: task,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.15: Get task statistics
   * GET /api/v1/complementary-tasks/statistics
   * Query params: fromDate, toDate (ISO 8601 date strings)
   */
  async getStatistics(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { fromDate, toDate } = req.query;

      if (!fromDate || !toDate) {
        res.status(400).json({
          success: false,
          message: 'fromDate and toDate query parameters are required',
        });
        return;
      }

      logger.info('Fetching task statistics', { fromDate, toDate });

      const stats = await this.complementaryTaskService.getTaskStatistics(
        new Date(fromDate as string),
        new Date(toDate as string)
      );

      res.json({
        success: true,
        data: stats,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.15: Get task count
   * GET /api/v1/complementary-tasks/count
   * Query params: status (optional)
   */
  async getTaskCount(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { status } = req.query;

      logger.info('Getting task count', { status });

      const count = await this.complementaryTaskService.getTaskCount(
        status as ComplementaryTaskStatus | undefined
      );

      res.json({
        success: true,
        data: { count },
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.15: Get overdue task count
   * GET /api/v1/complementary-tasks/overdue-count
   */
  async getOverdueCount(_req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      logger.info('Getting overdue task count');

      const count = await this.complementaryTaskService.getOverdueTaskCount();

      res.json({
        success: true,
        data: { count },
      });
    } catch (error) {
      next(error);
    }
  }
}
