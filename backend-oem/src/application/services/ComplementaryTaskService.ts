import { IComplementaryTaskRepository } from '@domain/repositories/IComplementaryTaskRepository';
import { ITaskCategoryRepository } from '@domain/repositories/ITaskCategoryRepository';
import { ComplementaryTask } from '@domain/entities/ComplementaryTask';
import { ComplementaryTaskStatus, QueryOptions } from '@shared/types';
import { logger } from '@shared/utils/logger';
import {
  CreateComplementaryTaskDto,
  ComplementaryTaskDto,
  AssignTaskDto,
  StartTaskDto,
  CompleteTaskDto,
  CancelTaskDto,
  AddTaskNoteDto,
  TaskStatisticsDto,
  TaskNoteDto,
  ListComplementaryTasksQuery,
} from '@application/dtos';
import { PaginatedResult } from '@shared/types';

/**
 * Application Service for ComplementaryTask aggregate
 * US 4.1.15: Handles complementary task management during vessel visits
 */
export class ComplementaryTaskService {
  constructor(
    private readonly taskRepository: IComplementaryTaskRepository,
    private readonly taskCategoryRepository: ITaskCategoryRepository
  ) {}

  /**
   * Create a new complementary task
   * US 4.1.15: Record non-cargo activities during vessel visits
   */
  async createComplementaryTask(dto: CreateComplementaryTaskDto): Promise<ComplementaryTaskDto> {
    // Validate task category exists and is active
    const category = await this.taskCategoryRepository.findById(dto.taskCategoryId);
    if (!category) {
      throw new Error(`Task category ${dto.taskCategoryId} not found`);
    }
    if (!category.isActive) {
      throw new Error(`Task category ${dto.taskCategoryId} is inactive`);
    }

    const task = new ComplementaryTask({
      taskCategoryId: dto.taskCategoryId,
      vveId: dto.vveId,
      title: dto.title,
      description: dto.description,
      dueDate: new Date(dto.dueDate),
      estimatedDurationHours: dto.estimatedDurationHours,
      assignedTo: dto.assignedTo,
      createdBy: dto.createdBy,
    });

    const saved = await this.taskRepository.save(task);
    logger.info(`Complementary task ${saved.taskId} created: ${saved.title}`);
    return this.toDto(saved);
  }

  /**
   * Get complementary task by ID
   */
  async getComplementaryTaskById(taskId: string): Promise<ComplementaryTaskDto> {
    const task = await this.taskRepository.findById(taskId);
    if (!task) {
      throw new Error(`Complementary task ${taskId} not found`);
    }
    return this.toDto(task);
  }

  /**
   * Update complementary task
   * US 4.1.15: Modify task details (only for PLANNED tasks)
   */
  async updateComplementaryTask(
    taskId: string,
    updates: {
      title?: string;
      description?: string;
      dueDate?: string;
      estimatedDurationHours?: number;
    }
  ): Promise<ComplementaryTaskDto> {
    const task = await this.taskRepository.findById(taskId);
    if (!task) {
      throw new Error(`Complementary task ${taskId} not found`);
    }

    if (task.status !== ComplementaryTaskStatus.PLANNED) {
      throw new Error('Can only update PLANNED tasks');
    }

    task.update({
      title: updates.title,
      description: updates.description,
    });

    const updated = await this.taskRepository.update(task);
    logger.info(`Complementary task ${updated.taskId} updated`);
    return this.toDto(updated);
  }

  /**
   * Delete complementary task
   * US 4.1.15: Remove task (only if not started)
   */
  async deleteComplementaryTask(taskId: string): Promise<void> {
    const task = await this.taskRepository.findById(taskId);
    if (!task) {
      throw new Error(`Complementary task ${taskId} not found`);
    }

    if (task.status !== ComplementaryTaskStatus.PLANNED) {
      throw new Error('Can only delete PLANNED tasks');
    }

    await this.taskRepository.delete(taskId);
    logger.info(`Complementary task ${taskId} deleted`);
  }

  /**
   * List all complementary tasks
   * US 4.1.15: Filter by status, VVE, date range
   */
  async listComplementaryTasks(
    filters?: {
      status?: ComplementaryTaskStatus;
      vveId?: string;
      taskCategoryId?: string;
      assignedTo?: string;
      fromDate?: string;
      toDate?: string;
    },
    options?: QueryOptions
  ): Promise<ComplementaryTaskDto[]> {
    let tasks: ComplementaryTask[] = [];

    if (filters?.status) {
      tasks = await this.taskRepository.findByStatus(filters.status, options);
    } else if (filters?.vveId) {
      tasks = await this.taskRepository.findByVveId(filters.vveId, options);
    } else if (filters?.taskCategoryId) {
      tasks = await this.taskRepository.findByCategory(filters.taskCategoryId, options);
    } else if (filters?.assignedTo) {
      tasks = await this.taskRepository.findByAssignee(filters.assignedTo, options);
    } else if (filters?.fromDate && filters?.toDate) {
      tasks = await this.taskRepository.findByDueDateRange(
        new Date(filters.fromDate),
        new Date(filters.toDate),
        options
      );
    } else {
      tasks = await this.taskRepository.findAll(options);
    }

    return tasks.map((task) => this.toDto(task));
  }

  /**
   * List tasks by VVE
   * US 4.1.15: Get all tasks for a vessel visit execution
   */
  async listTasksByVveId(vveId: string, options?: QueryOptions): Promise<ComplementaryTaskDto[]> {
    const tasks = await this.taskRepository.findByVveId(vveId, options);
    return tasks.map((task) => this.toDto(task));
  }

  /**
   * List tasks by status
   */
  async listTasksByStatus(
    status: ComplementaryTaskStatus,
    options?: QueryOptions
  ): Promise<ComplementaryTaskDto[]> {
    const tasks = await this.taskRepository.findByStatus(status, options);
    return tasks.map((task) => this.toDto(task));
  }

  /**
   * List tasks by date range
   */
  async listTasksByDateRange(
    fromDate: Date,
    toDate: Date,
    options?: QueryOptions
  ): Promise<ComplementaryTaskDto[]> {
    const tasks = await this.taskRepository.findByDueDateRange(fromDate, toDate, options);
    return tasks.map((task) => this.toDto(task));
  }

  /**
   * Get active tasks that are impacting operations
   * US 4.1.15: Identify tasks that suspend cargo operations
   */
  async getActiveTasksImpactingOperations(): Promise<ComplementaryTaskDto[]> {
    const activeTasks = await this.taskRepository.findActive();

    // Filter tasks with categories that have expectedImpact indicating operation suspension
    const impactingTasks: ComplementaryTask[] = [];
    for (const task of activeTasks) {
      const category = await this.taskCategoryRepository.findById(task.taskCategoryId);
      if (category?.expectedImpact?.toLowerCase().includes('suspend')) {
        impactingTasks.push(task);
      }
    }

    return impactingTasks.map((task) => this.toDto(task));
  }

  /**
   * Assign task to team or user
   * US 4.1.15: Set responsible team/service
   */
  async assignTask(taskId: string, dto: AssignTaskDto): Promise<ComplementaryTaskDto> {
    const task = await this.taskRepository.findById(taskId);
    if (!task) {
      throw new Error(`Complementary task ${taskId} not found`);
    }

    task.assignTo(dto.assignedTo);
    const updated = await this.taskRepository.update(task);
    logger.info(`Task ${taskId} assigned to ${dto.assignedTo}`);
    return this.toDto(updated);
  }

  /**
   * Start task
   * US 4.1.15: Begin task execution (PLANNED → IN_PROGRESS)
   */
  async startTask(taskId: string, _dto: StartTaskDto): Promise<ComplementaryTaskDto> {
    const task = await this.taskRepository.findById(taskId);
    if (!task) {
      throw new Error(`Complementary task ${taskId} not found`);
    }

    task.start();
    const updated = await this.taskRepository.update(task);
    logger.info(`Task ${taskId} started`);
    return this.toDto(updated);
  }

  /**
   * Complete task
   * US 4.1.15: Mark task as completed (IN_PROGRESS → COMPLETED)
   */
  async completeTask(taskId: string, dto: CompleteTaskDto): Promise<ComplementaryTaskDto> {
    const task = await this.taskRepository.findById(taskId);
    if (!task) {
      throw new Error(`Complementary task ${taskId} not found`);
    }

    task.complete(dto.completedBy);
    const updated = await this.taskRepository.update(task);
    logger.info(`Task ${taskId} completed by ${dto.completedBy}`);
    return this.toDto(updated);
  }

  /**
   * Cancel task
   * US 4.1.15: Cancel task with reason
   */
  async cancelTask(taskId: string, dto: CancelTaskDto): Promise<ComplementaryTaskDto> {
    const task = await this.taskRepository.findById(taskId);
    if (!task) {
      throw new Error(`Complementary task ${taskId} not found`);
    }

    task.cancel(dto.cancelledBy, dto.cancellationReason);
    const updated = await this.taskRepository.update(task);
    logger.info(`Task ${taskId} cancelled by ${dto.cancelledBy}: ${dto.cancellationReason}`);
    return this.toDto(updated);
  }

  /**
   * Add note to task
   * US 4.1.15: Record observations during task execution
   */
  async addTaskNote(taskId: string, dto: AddTaskNoteDto): Promise<ComplementaryTaskDto> {
    const task = await this.taskRepository.findById(taskId);
    if (!task) {
      throw new Error(`Complementary task ${taskId} not found`);
    }

    task.addNote(dto.author, dto.content);
    const updated = await this.taskRepository.update(task);
    logger.info(`Note added to task ${taskId} by ${dto.author}`);
    return this.toDto(updated);
  }

  /**
   * Get task statistics
   * US 4.1.15: Analyze task performance over time period
   */
  async getTaskStatistics(fromDate: Date, toDate: Date): Promise<TaskStatisticsDto> {
    const stats = await this.taskRepository.getStatistics(fromDate, toDate);
    return {
      total: stats.total,
      byStatus: stats.byStatus,
      averageCompletionTimeHours: stats.averageCompletionTimeHours,
      onTimeCompletionRate: stats.onTimeCompletionRate,
    };
  }

  /**
   * Get count of tasks by status
   */
  async getTaskCount(status?: ComplementaryTaskStatus): Promise<number> {
    return await this.taskRepository.count(status);
  }

  /**
   * Get count of overdue tasks
   */
  async getOverdueTaskCount(): Promise<number> {
    return await this.taskRepository.countOverdue();
  }

  /**
   * Convert domain entity to DTO
   */
  private toDto(task: ComplementaryTask): ComplementaryTaskDto {
    return {
      taskId: task.taskId,
      taskCategoryId: task.taskCategoryId,
      vveId: task.vveId || '',
      title: task.title,
      description: task.description,
      status: task.status,
      dueDate: task.dueDate?.toISOString() || '',
      assignedTo: task.assignedTo,
      estimatedDurationHours: task.estimatedDurationHours,
      startedAt: task.startedAt?.toISOString(),
      completedAt: task.completedAt?.toISOString(),
      completedBy: task.completedBy,
      cancelledAt: task.cancelledAt?.toISOString(),
      cancelledBy: task.cancelledBy,
      cancellationReason: task.cancellationReason,
      notes: task.notes.map(
        (note): TaskNoteDto => ({
          timestamp: note.timestamp.toISOString(),
          author: note.author,
          content: note.content,
        })
      ),
      createdAt: task.createdAt.toISOString(),
      createdBy: task.createdBy,
    };
  }

  /**
   * List complementary tasks with pagination and filtering
   * Unified method for paginated task listing
   */
  async listComplementaryTasksPaginated(
    query: ListComplementaryTasksQuery
  ): Promise<PaginatedResult<ComplementaryTaskDto>> {
    try {
      logger.info('Listing complementary tasks with pagination', { query });

      // Build filters from query
      const filters = this.buildFiltersFromQuery(query);

      // Pagination parameters
      const page = query.page || 1;
      const limit = query.limit || 10;
      const skip = (page - 1) * limit;
      const sortBy = query.sortBy || 'dueDate';
      const sortOrder = query.sortOrder || 'asc';

      // Build query options
      const options: QueryOptions = {
        limit,
        offset: skip,
      };

      // Get tasks based on filter
      let tasks: ComplementaryTask[];
      let total: number;

      if (filters.status) {
        tasks = await this.taskRepository.findByStatus(filters.status, options);
        total = await this.countByStatus(filters.status);
      } else if (filters.vveId) {
        tasks = await this.taskRepository.findByVveId(filters.vveId, options);
        total = await this.countByVveId(filters.vveId);
      } else if (filters.taskCategoryId) {
        tasks = await this.taskRepository.findByCategory(filters.taskCategoryId, options);
        total = await this.countByCategory(filters.taskCategoryId);
      } else if (filters.assignedTo) {
        tasks = await this.taskRepository.findByAssignee(filters.assignedTo, options);
        total = await this.countByAssignee(filters.assignedTo);
      } else if (filters.fromDate && filters.toDate) {
        tasks = await this.taskRepository.findByDueDateRange(filters.fromDate, filters.toDate, options);
        total = await this.countByDateRange(filters.fromDate, filters.toDate);
      } else {
        tasks = await this.taskRepository.findAll(options);
        total = await this.countAll();
      }

      // Apply sorting
      tasks = this.sortTasks(tasks, sortBy, sortOrder);

      const taskDtos = tasks.map((task) => this.toDto(task));

      return {
        data: taskDtos,
        pagination: {
          page,
          limit,
          total,
          totalPages: Math.ceil(total / limit),
        },
      };
    } catch (error) {
      logger.error('Error listing complementary tasks with pagination', { error, query });
      throw error;
    }
  }

  /**
   * Build filters object from query parameters
   */
  private buildFiltersFromQuery(query: ListComplementaryTasksQuery): {
    status?: ComplementaryTaskStatus;
    vveId?: string;
    taskCategoryId?: string;
    assignedTo?: string;
    fromDate?: Date;
    toDate?: Date;
  } {
    const filters: any = {};

    if (query.status) filters.status = query.status as ComplementaryTaskStatus;
    if (query.vveId) filters.vveId = query.vveId;
    if (query.taskCategoryId) filters.taskCategoryId = query.taskCategoryId;
    if (query.assignedTo) filters.assignedTo = query.assignedTo;
    if (query.fromDate && query.toDate) {
      filters.fromDate = new Date(query.fromDate);
      filters.toDate = new Date(query.toDate);
    }

    return filters;
  }

  /**
   * Sort tasks based on field and order
   */
  private sortTasks(tasks: ComplementaryTask[], sortBy: string, sortOrder: string): ComplementaryTask[] {
    return tasks.sort((a, b) => {
      let aValue: any;
      let bValue: any;

      switch (sortBy) {
        case 'dueDate':
          aValue = a.dueDate?.getTime() ?? 0;
          bValue = b.dueDate?.getTime() ?? 0;
          break;
        case 'status':
          aValue = a.status;
          bValue = b.status;
          break;
        case 'title':
          aValue = a.title;
          bValue = b.title;
          break;
        default:
          aValue = a.dueDate?.getTime() ?? 0;
          bValue = b.dueDate?.getTime() ?? 0;
      }

      if (sortOrder === 'asc') {
        return aValue > bValue ? 1 : -1;
      } else {
        return aValue < bValue ? 1 : -1;
      }
    });
  }

  // Count methods for different filters
  private async countByStatus(status: ComplementaryTaskStatus): Promise<number> {
    const tasks = await this.taskRepository.findByStatus(status);
    return tasks.length;
  }

  private async countByVveId(vveId: string): Promise<number> {
    const tasks = await this.taskRepository.findByVveId(vveId);
    return tasks.length;
  }

  private async countByCategory(taskCategoryId: string): Promise<number> {
    const tasks = await this.taskRepository.findByCategory(taskCategoryId);
    return tasks.length;
  }

  private async countByAssignee(assignedTo: string): Promise<number> {
    const tasks = await this.taskRepository.findByAssignee(assignedTo);
    return tasks.length;
  }

  private async countByDateRange(fromDate: Date, toDate: Date): Promise<number> {
    const tasks = await this.taskRepository.findByDueDateRange(fromDate, toDate);
    return tasks.length;
  }

  private async countAll(): Promise<number> {
    const tasks = await this.taskRepository.findAll();
    return tasks.length;
  }
}
