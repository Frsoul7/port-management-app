import { ITaskCategoryRepository } from '@domain/repositories/ITaskCategoryRepository';
import { TaskCategory } from '@domain/entities/TaskCategory';
import { QueryOptions } from '@shared/types';
import { logger } from '@shared/utils/logger';
import {
  CreateTaskCategoryDto,
  UpdateTaskCategoryDto,
  TaskCategoryDto,
} from '@application/dtos';

/**
 * Application Service for TaskCategory aggregate
 * US 4.1.14: Handles complementary task category management
 */
export class TaskCategoryService {
  constructor(private readonly taskCategoryRepository: ITaskCategoryRepository) {}

  /**
   * Create a new task category
   * US 4.1.14: Auto-generate sequential category code (CTC001, CTC002, etc.)
   */
  async createTaskCategory(dto: CreateTaskCategoryDto): Promise<TaskCategoryDto> {
    // Validate name uniqueness
    const existingByName = await this.taskCategoryRepository.findByName(dto.categoryName);
    if (existingByName) {
      throw new Error(`Task category with name ${dto.categoryName} already exists`);
    }

    // Generate sequential category code
    const nextNumber = await this.taskCategoryRepository.getNextSequentialNumber();
    const categoryCode = `CTC${nextNumber.toString().padStart(3, '0')}`;

    // Double-check code uniqueness (should never happen, but safety check)
    const existingByCode = await this.taskCategoryRepository.findByCode(categoryCode);
    if (existingByCode) {
      throw new Error(`Task category with code ${categoryCode} already exists`);
    }

    const taskCategory = new TaskCategory({
      categoryCode,
      categoryName: dto.categoryName,
      description: dto.description,
      defaultDurationHours: dto.defaultDurationHours,
      expectedImpact: dto.expectedImpact,
    });

    const saved = await this.taskCategoryRepository.save(taskCategory);
    logger.info(`Task category ${saved.categoryCode} created: ${saved.categoryName}`);
    return this.toDto(saved);
  }

  /**
   * Get task category by ID
   */
  async getTaskCategoryById(taskCategoryId: string): Promise<TaskCategoryDto> {
    const taskCategory = await this.taskCategoryRepository.findById(taskCategoryId);
    if (!taskCategory) {
      throw new Error(`Task category ${taskCategoryId} not found`);
    }
    return this.toDto(taskCategory);
  }

  /**
   * Get task category by code
   * US 4.1.14: Lookup by unique code
   */
  async getTaskCategoryByCode(categoryCode: string): Promise<TaskCategoryDto> {
    const taskCategory = await this.taskCategoryRepository.findByCode(categoryCode);
    if (!taskCategory) {
      throw new Error(`Task category with code ${categoryCode} not found`);
    }
    return this.toDto(taskCategory);
  }

  /**
   * List all task categories
   */
  async listAllTaskCategories(options?: QueryOptions): Promise<TaskCategoryDto[]> {
    const taskCategories = await this.taskCategoryRepository.findAll(options);
    return taskCategories.map((tc) => this.toDto(tc));
  }

  /**
   * List active task categories only
   * US 4.1.14: Only active categories can be used for new tasks
   */
  async listActiveTaskCategories(options?: QueryOptions): Promise<TaskCategoryDto[]> {
    const taskCategories = await this.taskCategoryRepository.findActive(options);
    return taskCategories.map((tc) => this.toDto(tc));
  }

  /**
   * List task categories with filters
   * US 4.1.14: Unified method for listing with optional filtering
   * @param filters - Optional filters (activeOnly)
   * @param options - Query options (sorting)
   */
  async listTaskCategories(
    filters?: { activeOnly?: boolean },
    options?: QueryOptions
  ): Promise<TaskCategoryDto[]> {
    let taskCategories: TaskCategory[];

    if (filters?.activeOnly) {
      taskCategories = await this.taskCategoryRepository.findActive(options);
    } else {
      taskCategories = await this.taskCategoryRepository.findAll(options);
    }

    return taskCategories.map((tc) => this.toDto(tc));
  }

  /**
   * Update task category
   * US 4.1.14: Modify category properties (code cannot be changed)
   */
  async updateTaskCategory(
    taskCategoryId: string,
    dto: UpdateTaskCategoryDto
  ): Promise<TaskCategoryDto> {
    const taskCategory = await this.taskCategoryRepository.findById(taskCategoryId);
    if (!taskCategory) {
      throw new Error(`Task category ${taskCategoryId} not found`);
    }

    // Validate name uniqueness if changed
    if (dto.categoryName) {
      const existingByName = await this.taskCategoryRepository.existsByName(
        dto.categoryName,
        taskCategoryId
      );
      if (existingByName) {
        throw new Error(`Task category with name ${dto.categoryName} already exists`);
      }
    }

    taskCategory.update({
      categoryName: dto.categoryName,
      description: dto.description,
      defaultDurationHours: dto.defaultDurationHours,
      expectedImpact: dto.expectedImpact,
    });

    const updated = await this.taskCategoryRepository.update(taskCategory);
    logger.info(`Task category ${updated.categoryCode} updated`);
    return this.toDto(updated);
  }

  /**
   * Deactivate task category
   * US 4.1.14: Soft delete by deactivating
   */
  async deactivateTaskCategory(taskCategoryId: string): Promise<TaskCategoryDto> {
    const taskCategory = await this.taskCategoryRepository.findById(taskCategoryId);
    if (!taskCategory) {
      throw new Error(`Task category ${taskCategoryId} not found`);
    }

    // Check if used in complementary tasks
    const isUsed = await this.taskCategoryRepository.isUsedInTasks(taskCategoryId);
    if (isUsed) {
      throw new Error(
        'Cannot deactivate task category that is used in existing complementary tasks'
      );
    }

    taskCategory.deactivate();
    const updated = await this.taskCategoryRepository.update(taskCategory);
    logger.info(`Task category ${updated.categoryCode} deactivated`);
    return this.toDto(updated);
  }

  /**
   * Reactivate task category
   */
  async reactivateTaskCategory(taskCategoryId: string): Promise<TaskCategoryDto> {
    const taskCategory = await this.taskCategoryRepository.findById(taskCategoryId);
    if (!taskCategory) {
      throw new Error(`Task category ${taskCategoryId} not found`);
    }

    taskCategory.reactivate();
    const updated = await this.taskCategoryRepository.update(taskCategory);
    logger.info(`Task category ${updated.categoryCode} reactivated`);
    return this.toDto(updated);
  }

  /**
   * Delete task category permanently
   * Should only be used for categories never used in tasks
   */
  async deleteTaskCategory(taskCategoryId: string): Promise<void> {
    const taskCategory = await this.taskCategoryRepository.findById(taskCategoryId);
    if (!taskCategory) {
      throw new Error(`Task category ${taskCategoryId} not found`);
    }

    // Check if used in complementary tasks
    const isUsed = await this.taskCategoryRepository.isUsedInTasks(taskCategoryId);
    if (isUsed) {
      throw new Error(
        'Cannot delete task category that is used in existing complementary tasks'
      );
    }

    await this.taskCategoryRepository.delete(taskCategoryId);
    logger.info(`Task category ${taskCategory.categoryCode} deleted`);
  }

  /**
   * Get count of task categories
   * Optional: only count active categories
   */
  async getTaskCategoryCount(activeOnly: boolean = false): Promise<number> {
    return this.taskCategoryRepository.count(activeOnly);
  }

  /**
   * Convert domain entity to DTO
   */
  private toDto(taskCategory: TaskCategory): TaskCategoryDto {
    return {
      taskCategoryId: taskCategory.taskCategoryId,
      categoryCode: taskCategory.categoryCode,
      categoryName: taskCategory.categoryName,
      description: taskCategory.description,
      defaultDurationHours: taskCategory.defaultDurationHours,
      expectedImpact: taskCategory.expectedImpact,
      isActive: taskCategory.isActive,
      createdAt: taskCategory.createdAt.toISOString(),
      updatedAt: taskCategory.updatedAt.toISOString(),
    };
  }
}
