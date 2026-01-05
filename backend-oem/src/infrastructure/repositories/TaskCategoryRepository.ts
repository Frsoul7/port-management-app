import { ITaskCategoryRepository } from '@domain/repositories/ITaskCategoryRepository';
import { TaskCategory } from '@domain/entities/TaskCategory';
import {
  TaskCategoryModel,
  ITaskCategoryDocument,
} from '@infrastructure/database/models/TaskCategoryModel';
import { ComplementaryTaskModel } from '@infrastructure/database/models/ComplementaryTaskModel';
import { QueryOptions } from '@shared/types';

/**
 * Repository implementation for TaskCategory
 * US 4.1.14: Enhanced with code-based operations
 */
export class TaskCategoryRepository implements ITaskCategoryRepository {
  private toDomain(doc: ITaskCategoryDocument): TaskCategory {
    return new TaskCategory({
      taskCategoryId: doc.taskCategoryId,
      categoryCode: doc.categoryCode, // US 4.1.14
      categoryName: doc.categoryName,
      description: doc.description,
      defaultDurationHours: doc.defaultDurationHours, // US 4.1.14
      expectedImpact: doc.expectedImpact, // US 4.1.14
      isActive: doc.isActive,
      createdAt: doc.createdAt,
      updatedAt: doc.updatedAt,
    });
  }

  private toDocument(taskCategory: TaskCategory): Partial<ITaskCategoryDocument> {
    return {
      taskCategoryId: taskCategory.taskCategoryId,
      categoryCode: taskCategory.categoryCode, // US 4.1.14
      categoryName: taskCategory.categoryName,
      description: taskCategory.description,
      defaultDurationHours: taskCategory.defaultDurationHours, // US 4.1.14
      expectedImpact: taskCategory.expectedImpact, // US 4.1.14
      isActive: taskCategory.isActive,
      createdAt: taskCategory.createdAt,
      updatedAt: taskCategory.updatedAt,
    };
  }

  async findById(taskCategoryId: string): Promise<TaskCategory | null> {
    const doc = await TaskCategoryModel.findOne({ taskCategoryId });
    return doc ? this.toDomain(doc) : null;
  }

  async findByName(categoryName: string): Promise<TaskCategory | null> {
    const doc = await TaskCategoryModel.findOne({ categoryName });
    return doc ? this.toDomain(doc) : null;
  }

  /**
   * US 4.1.14: Find task category by code
   */
  async findByCode(categoryCode: string): Promise<TaskCategory | null> {
    const doc = await TaskCategoryModel.findOne({ categoryCode });
    return doc ? this.toDomain(doc) : null;
  }

  async findAll(options?: QueryOptions): Promise<TaskCategory[]> {
    let query = TaskCategoryModel.find();

    if (options?.sortBy) {
      const sortOrder = options.sortOrder === 'desc' ? -1 : 1;
      query = query.sort({ [options.sortBy]: sortOrder });
    } else {
      query = query.sort({ categoryName: 1 });
    }

    if (options?.page && options?.limit) {
      const skip = (options.page - 1) * options.limit;
      query = query.skip(skip).limit(options.limit);
    }

    const docs = await query.exec();
    return docs.map((doc) => this.toDomain(doc));
  }

  async findActive(options?: QueryOptions): Promise<TaskCategory[]> {
    let query = TaskCategoryModel.find({ isActive: true });

    if (options?.sortBy) {
      const sortOrder = options.sortOrder === 'desc' ? -1 : 1;
      query = query.sort({ [options.sortBy]: sortOrder });
    } else {
      query = query.sort({ categoryName: 1 });
    }

    if (options?.page && options?.limit) {
      const skip = (options.page - 1) * options.limit;
      query = query.skip(skip).limit(options.limit);
    }

    const docs = await query.exec();
    return docs.map((doc) => this.toDomain(doc));
  }

  async save(taskCategory: TaskCategory): Promise<TaskCategory> {
    const docData = this.toDocument(taskCategory);
    const doc = new TaskCategoryModel(docData);
    await doc.save();
    return this.toDomain(doc);
  }

  async update(taskCategory: TaskCategory): Promise<TaskCategory> {
    const docData = this.toDocument(taskCategory);
    const doc = await TaskCategoryModel.findOneAndUpdate(
      { taskCategoryId: taskCategory.taskCategoryId },
      docData,
      { new: true, runValidators: true }
    );

    if (!doc) {
      throw new Error(`TaskCategory ${taskCategory.taskCategoryId} not found`);
    }

    return this.toDomain(doc);
  }

  async delete(taskCategoryId: string): Promise<void> {
    await TaskCategoryModel.deleteOne({ taskCategoryId });
  }

  async count(activeOnly?: boolean): Promise<number> {
    if (activeOnly) {
      return TaskCategoryModel.countDocuments({ isActive: true });
    }
    return TaskCategoryModel.countDocuments();
  }

  /**
   * US 4.1.14: Check if category code already exists
   */
  async existsByCode(categoryCode: string, excludeId?: string): Promise<boolean> {
    const query: Record<string, unknown> = { categoryCode };
    if (excludeId) {
      query.taskCategoryId = { $ne: excludeId };
    }

    const count = await TaskCategoryModel.countDocuments(query);
    return count > 0;
  }

  async existsByName(categoryName: string, excludeId?: string): Promise<boolean> {
    const query: Record<string, unknown> = { categoryName };
    if (excludeId) {
      query.taskCategoryId = { $ne: excludeId };
    }

    const count = await TaskCategoryModel.countDocuments(query);
    return count > 0;
  }

  /**
   * US 4.1.14: Get next sequential number for code generation
   * Finds highest existing code and increments by 1
   * Example: If highest is CTC005, returns 6
   */
  async getNextSequentialNumber(): Promise<number> {
    const lastCategory = await TaskCategoryModel.findOne()
      .sort({ categoryCode: -1 })
      .limit(1)
      .exec();

    if (!lastCategory || !lastCategory.categoryCode) {
      return 1; // First category
    }

    // Extract number from code (e.g., "CTC005" -> 5)
    const match = lastCategory.categoryCode.match(/^CTC(\d{3})$/);
    if (!match) {
      return 1; // Fallback if format is unexpected
    }

    return parseInt(match[1]!, 10) + 1;
  }

  async isUsedInTasks(taskCategoryId: string): Promise<boolean> {
    const count = await ComplementaryTaskModel.countDocuments({ taskCategoryId });
    return count > 0;
  }
}
