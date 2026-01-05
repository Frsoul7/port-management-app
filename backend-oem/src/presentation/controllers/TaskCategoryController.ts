import { Request, Response, NextFunction } from 'express';
import { TaskCategoryService } from '@application/services/TaskCategoryService';
import { logger } from '@shared/utils/logger';

/**
 * TaskCategoryController
 * Handles HTTP requests for Task Category catalog management
 *
 * User Story:
 * - US 4.1.14: Manage complementary task category catalog
 */
export class TaskCategoryController {
  constructor(private taskCategoryService: TaskCategoryService) {}

  /**
   * US 4.1.14: Create new task category
   * POST /api/v1/task-categories
   */
  async createTaskCategory(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const dto = req.body;

      logger.info('Creating task category', { categoryName: dto.categoryName });

      const taskCategory = await this.taskCategoryService.createTaskCategory(dto);

      res.status(201).json({
        success: true,
        message: 'Task category created successfully',
        data: taskCategory,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.14: Get task category by ID
   * GET /api/v1/task-categories/:id
   */
  async getTaskCategoryById(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { id } = req.params;

      logger.info('Fetching task category by ID', { id });

      const taskCategory = await this.taskCategoryService.getTaskCategoryById(id!);

      res.json({
        success: true,
        data: taskCategory,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.14: Get task category by code
   * GET /api/v1/task-categories/code/:code
   */
  async getTaskCategoryByCode(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { code } = req.params;

      logger.info('Fetching task category by code', { code });

      const taskCategory = await this.taskCategoryService.getTaskCategoryByCode(code!);

      res.json({
        success: true,
        data: taskCategory,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.14: List all task categories
   * GET /api/v1/task-categories
   * Query params: activeOnly, sortBy, sortOrder
   */
  async listTaskCategories(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { activeOnly, sortBy, sortOrder } = req.query;

      logger.debug('Listing task categories', { activeOnly });

      const filters = { activeOnly: activeOnly === 'true' };
      const options = {
        sortBy: sortBy as string,
        sortOrder: sortOrder as 'asc' | 'desc',
      };

      const taskCategories = await this.taskCategoryService.listTaskCategories(filters, options);

      res.json({
        success: true,
        data: taskCategories,
        count: taskCategories.length,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.14: Update task category
   * PUT /api/v1/task-categories/:id
   */
  async updateTaskCategory(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { id } = req.params;
      const dto = req.body;

      logger.info('Updating task category', { id });

      const taskCategory = await this.taskCategoryService.updateTaskCategory(id!, dto);

      res.json({
        success: true,
        message: 'Task category updated successfully',
        data: taskCategory,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.14: Deactivate task category
   * POST /api/v1/task-categories/:id/deactivate
   */
  async deactivateTaskCategory(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { id } = req.params;

      logger.info('Deactivating task category', { id });

      const taskCategory = await this.taskCategoryService.deactivateTaskCategory(id!);

      res.json({
        success: true,
        message: 'Task category deactivated successfully',
        data: taskCategory,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.14: Reactivate task category
   * POST /api/v1/task-categories/:id/reactivate
   */
  async reactivateTaskCategory(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { id } = req.params;

      logger.info('Reactivating task category', { id });

      const taskCategory = await this.taskCategoryService.reactivateTaskCategory(id!);

      res.json({
        success: true,
        message: 'Task category reactivated successfully',
        data: taskCategory,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.14: Delete task category permanently
   * DELETE /api/v1/task-categories/:id
   */
  async deleteTaskCategory(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { id } = req.params;

      logger.info('Deleting task category', { id });

      await this.taskCategoryService.deleteTaskCategory(id!);

      res.json({
        success: true,
        message: 'Task category deleted successfully',
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.14: Get count of task categories
   * GET /api/v1/task-categories/count
   * Query params: activeOnly
   */
  async getTaskCategoryCount(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { activeOnly } = req.query;

      logger.info('Getting task category count', { activeOnly });

      const count = await this.taskCategoryService.getTaskCategoryCount(
        activeOnly === 'true'
      );

      res.json({
        success: true,
        data: { count },
      });
    } catch (error) {
      next(error);
    }
  }
}
