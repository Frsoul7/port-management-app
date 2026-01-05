import { IComplementaryTaskRepository } from '@domain/repositories/IComplementaryTaskRepository';
import { ComplementaryTask } from '@domain/entities/ComplementaryTask';
import {
  ComplementaryTaskModel,
  IComplementaryTaskDocument,
} from '@infrastructure/database/models/ComplementaryTaskModel';
import { ComplementaryTaskStatus, QueryOptions } from '@shared/types';

/**
 * Repository implementation for ComplementaryTask
 */
export class ComplementaryTaskRepository implements IComplementaryTaskRepository {
  private toDomain(doc: IComplementaryTaskDocument): ComplementaryTask {
    return new ComplementaryTask({
      taskId: doc.taskId,
      taskCategoryId: doc.taskCategoryId,
      vveId: doc.vveId,
      title: doc.title,
      description: doc.description,
      status: doc.status,
      assignedTo: doc.assignedTo,
      dueDate: doc.dueDate,
      createdAt: doc.createdAt,
      createdBy: doc.createdBy,
      startedAt: doc.startedAt,
      completedAt: doc.completedAt,
      completedBy: doc.completedBy,
      cancelledAt: doc.cancelledAt,
      cancelledBy: doc.cancelledBy,
      cancellationReason: doc.cancellationReason,
      estimatedDurationHours: doc.estimatedDurationHours,
      notes: doc.notes.map((note) => ({
        timestamp: note.timestamp,
        author: note.author,
        content: note.content,
      })),
    });
  }

  private toDocument(task: ComplementaryTask): Partial<IComplementaryTaskDocument> {
    return {
      taskId: task.taskId,
      taskCategoryId: task.taskCategoryId,
      vveId: task.vveId,
      title: task.title,
      description: task.description,
      status: task.status,
      assignedTo: task.assignedTo,
      dueDate: task.dueDate,
      createdAt: task.createdAt,
      createdBy: task.createdBy,
      startedAt: task.startedAt,
      completedAt: task.completedAt,
      completedBy: task.completedBy,
      cancelledAt: task.cancelledAt,
      cancelledBy: task.cancelledBy,
      cancellationReason: task.cancellationReason,
      estimatedDurationHours: task.estimatedDurationHours,
      notes: task.notes.map((note) => ({
        timestamp: note.timestamp,
        author: note.author,
        content: note.content,
      })),
    };
  }

  async findById(taskId: string): Promise<ComplementaryTask | null> {
    const doc = await ComplementaryTaskModel.findOne({ taskId });
    return doc ? this.toDomain(doc) : null;
  }

  async findAll(options?: QueryOptions): Promise<ComplementaryTask[]> {
    let query = ComplementaryTaskModel.find();

    if (options?.sortBy) {
      const sortOrder = options.sortOrder === 'desc' ? -1 : 1;
      query = query.sort({ [options.sortBy]: sortOrder });
    } else {
      query = query.sort({ createdAt: -1 });
    }

    if (options?.page && options?.limit) {
      const skip = (options.page - 1) * options.limit;
      query = query.skip(skip).limit(options.limit);
    }

    const docs = await query.exec();
    return docs.map((doc) => this.toDomain(doc));
  }

  async findByStatus(
    status: ComplementaryTaskStatus,
    options?: QueryOptions
  ): Promise<ComplementaryTask[]> {
    let query = ComplementaryTaskModel.find({ status });

    if (options?.sortBy) {
      const sortOrder = options.sortOrder === 'desc' ? -1 : 1;
      query = query.sort({ [options.sortBy]: sortOrder });
    } else {
      query = query.sort({ dueDate: 1 });
    }

    if (options?.page && options?.limit) {
      const skip = (options.page - 1) * options.limit;
      query = query.skip(skip).limit(options.limit);
    }

    const docs = await query.exec();
    return docs.map((doc) => this.toDomain(doc));
  }

  async findByCategory(
    taskCategoryId: string,
    options?: QueryOptions
  ): Promise<ComplementaryTask[]> {
    let query = ComplementaryTaskModel.find({ taskCategoryId });

    if (options?.sortBy) {
      const sortOrder = options.sortOrder === 'desc' ? -1 : 1;
      query = query.sort({ [options.sortBy]: sortOrder });
    } else {
      query = query.sort({ createdAt: -1 });
    }

    if (options?.page && options?.limit) {
      const skip = (options.page - 1) * options.limit;
      query = query.skip(skip).limit(options.limit);
    }

    const docs = await query.exec();
    return docs.map((doc) => this.toDomain(doc));
  }

  async findByVveId(vveId: string, options?: QueryOptions): Promise<ComplementaryTask[]> {
    let query = ComplementaryTaskModel.find({ vveId });

    if (options?.sortBy) {
      const sortOrder = options.sortOrder === 'desc' ? -1 : 1;
      query = query.sort({ [options.sortBy]: sortOrder });
    } else {
      query = query.sort({ createdAt: -1 });
    }

    if (options?.page && options?.limit) {
      const skip = (options.page - 1) * options.limit;
      query = query.skip(skip).limit(options.limit);
    }

    const docs = await query.exec();
    return docs.map((doc) => this.toDomain(doc));
  }

  async findByAssignee(assignedTo: string, options?: QueryOptions): Promise<ComplementaryTask[]> {
    let query = ComplementaryTaskModel.find({ assignedTo });

    if (options?.sortBy) {
      const sortOrder = options.sortOrder === 'desc' ? -1 : 1;
      query = query.sort({ [options.sortBy]: sortOrder });
    } else {
      query = query.sort({ dueDate: 1 });
    }

    if (options?.page && options?.limit) {
      const skip = (options.page - 1) * options.limit;
      query = query.skip(skip).limit(options.limit);
    }

    const docs = await query.exec();
    return docs.map((doc) => this.toDomain(doc));
  }

  async findByCreator(createdBy: string, options?: QueryOptions): Promise<ComplementaryTask[]> {
    let query = ComplementaryTaskModel.find({ createdBy });

    if (options?.sortBy) {
      const sortOrder = options.sortOrder === 'desc' ? -1 : 1;
      query = query.sort({ [options.sortBy]: sortOrder });
    } else {
      query = query.sort({ createdAt: -1 });
    }

    if (options?.page && options?.limit) {
      const skip = (options.page - 1) * options.limit;
      query = query.skip(skip).limit(options.limit);
    }

    const docs = await query.exec();
    return docs.map((doc) => this.toDomain(doc));
  }

  async findOverdue(options?: QueryOptions): Promise<ComplementaryTask[]> {
    const now = new Date();
    let query = ComplementaryTaskModel.find({
      dueDate: { $lt: now },
      status: {
        $in: [ComplementaryTaskStatus.PLANNED, ComplementaryTaskStatus.IN_PROGRESS],
      },
    });

    if (options?.sortBy) {
      const sortOrder = options.sortOrder === 'desc' ? -1 : 1;
      query = query.sort({ [options.sortBy]: sortOrder });
    } else {
      query = query.sort({ dueDate: 1 });
    }

    if (options?.page && options?.limit) {
      const skip = (options.page - 1) * options.limit;
      query = query.skip(skip).limit(options.limit);
    }

    const docs = await query.exec();
    return docs.map((doc) => this.toDomain(doc));
  }

  async findByDueDateRange(
    fromDate: Date,
    toDate: Date,
    options?: QueryOptions
  ): Promise<ComplementaryTask[]> {
    let query = ComplementaryTaskModel.find({
      dueDate: { $gte: fromDate, $lte: toDate },
    });

    if (options?.sortBy) {
      const sortOrder = options.sortOrder === 'desc' ? -1 : 1;
      query = query.sort({ [options.sortBy]: sortOrder });
    } else {
      query = query.sort({ dueDate: 1 });
    }

    if (options?.page && options?.limit) {
      const skip = (options.page - 1) * options.limit;
      query = query.skip(skip).limit(options.limit);
    }

    const docs = await query.exec();
    return docs.map((doc) => this.toDomain(doc));
  }

  async findByCreatedDateRange(
    fromDate: Date,
    toDate: Date,
    options?: QueryOptions
  ): Promise<ComplementaryTask[]> {
    let query = ComplementaryTaskModel.find({
      createdAt: { $gte: fromDate, $lte: toDate },
    });

    if (options?.sortBy) {
      const sortOrder = options.sortOrder === 'desc' ? -1 : 1;
      query = query.sort({ [options.sortBy]: sortOrder });
    } else {
      query = query.sort({ createdAt: -1 });
    }

    if (options?.page && options?.limit) {
      const skip = (options.page - 1) * options.limit;
      query = query.skip(skip).limit(options.limit);
    }

    const docs = await query.exec();
    return docs.map((doc) => this.toDomain(doc));
  }

  async findUnassigned(options?: QueryOptions): Promise<ComplementaryTask[]> {
    let query = ComplementaryTaskModel.find({
      assignedTo: { $exists: false },
      status: {
        $in: [ComplementaryTaskStatus.PLANNED, ComplementaryTaskStatus.IN_PROGRESS],
      },
    });

    if (options?.sortBy) {
      const sortOrder = options.sortOrder === 'desc' ? -1 : 1;
      query = query.sort({ [options.sortBy]: sortOrder });
    } else {
      query = query.sort({ createdAt: -1 });
    }

    if (options?.page && options?.limit) {
      const skip = (options.page - 1) * options.limit;
      query = query.skip(skip).limit(options.limit);
    }

    const docs = await query.exec();
    return docs.map((doc) => this.toDomain(doc));
  }

  async findActive(options?: QueryOptions): Promise<ComplementaryTask[]> {
    let query = ComplementaryTaskModel.find({
      status: {
        $in: [ComplementaryTaskStatus.PLANNED, ComplementaryTaskStatus.IN_PROGRESS],
      },
    });

    if (options?.sortBy) {
      const sortOrder = options.sortOrder === 'desc' ? -1 : 1;
      query = query.sort({ [options.sortBy]: sortOrder });
    } else {
      query = query.sort({ dueDate: 1 });
    }

    if (options?.page && options?.limit) {
      const skip = (options.page - 1) * options.limit;
      query = query.skip(skip).limit(options.limit);
    }

    const docs = await query.exec();
    return docs.map((doc) => this.toDomain(doc));
  }

  async save(task: ComplementaryTask): Promise<ComplementaryTask> {
    const docData = this.toDocument(task);
    const doc = new ComplementaryTaskModel(docData);
    await doc.save();
    return this.toDomain(doc);
  }

  async update(task: ComplementaryTask): Promise<ComplementaryTask> {
    const docData = this.toDocument(task);
    const doc = await ComplementaryTaskModel.findOneAndUpdate({ taskId: task.taskId }, docData, {
      new: true,
      runValidators: true,
    });

    if (!doc) {
      throw new Error(`ComplementaryTask ${task.taskId} not found`);
    }

    return this.toDomain(doc);
  }

  async delete(taskId: string): Promise<void> {
    await ComplementaryTaskModel.deleteOne({ taskId });
  }

  async count(status?: ComplementaryTaskStatus): Promise<number> {
    if (status) {
      return ComplementaryTaskModel.countDocuments({ status });
    }
    return ComplementaryTaskModel.countDocuments();
  }

  async countOverdue(): Promise<number> {
    const now = new Date();
    return ComplementaryTaskModel.countDocuments({
      dueDate: { $lt: now },
      status: {
        $in: [ComplementaryTaskStatus.PLANNED, ComplementaryTaskStatus.IN_PROGRESS],
      },
    });
  }

  async getStatistics(
    fromDate: Date,
    toDate: Date
  ): Promise<{
    total: number;
    byStatus: Record<ComplementaryTaskStatus, number>;
    averageCompletionTimeHours: number | null;
    onTimeCompletionRate: number;
  }> {
    const match = {
      createdAt: { $gte: fromDate, $lte: toDate },
    };

    // Get total count
    const total = await ComplementaryTaskModel.countDocuments(match);

    // Get counts by status
    const byStatusResult = await ComplementaryTaskModel.aggregate([
      { $match: match },
      {
        $group: {
          _id: '$status',
          count: { $sum: 1 },
        },
      },
    ]);

    const byStatus = Object.values(ComplementaryTaskStatus).reduce(
      (acc, status) => {
        acc[status] = 0;
        return acc;
      },
      {} as Record<ComplementaryTaskStatus, number>
    );

    byStatusResult.forEach((item) => {
      byStatus[item._id as ComplementaryTaskStatus] = item.count;
    });

    // Get average completion time
    const completionTimeResult = await ComplementaryTaskModel.aggregate([
      {
        $match: {
          ...match,
          status: ComplementaryTaskStatus.COMPLETED,
          startedAt: { $exists: true },
          completedAt: { $exists: true },
        },
      },
      {
        $project: {
          completionTime: {
            $divide: [
              { $subtract: ['$completedAt', '$startedAt'] },
              1000 * 60 * 60, // Convert ms to hours
            ],
          },
        },
      },
      {
        $group: {
          _id: null,
          avgCompletionTime: { $avg: '$completionTime' },
        },
      },
    ]);

    const averageCompletionTimeHours =
      completionTimeResult.length > 0 && completionTimeResult[0]?.avgCompletionTime
        ? completionTimeResult[0].avgCompletionTime
        : null;

    // Get on-time completion rate
    const completedWithDueDate = await ComplementaryTaskModel.countDocuments({
      ...match,
      status: ComplementaryTaskStatus.COMPLETED,
      dueDate: { $exists: true },
      completedAt: { $exists: true },
    });

    const completedOnTime = await ComplementaryTaskModel.countDocuments({
      ...match,
      status: ComplementaryTaskStatus.COMPLETED,
      dueDate: { $exists: true },
      completedAt: { $exists: true },
      $expr: { $lte: ['$completedAt', '$dueDate'] },
    });

    const onTimeCompletionRate =
      completedWithDueDate > 0 ? (completedOnTime / completedWithDueDate) * 100 : 0;

    return {
      total,
      byStatus,
      averageCompletionTimeHours,
      onTimeCompletionRate,
    };
  }

  async getAverageDurationByCategory(taskCategoryId: string): Promise<number | null> {
    const result = await ComplementaryTaskModel.aggregate([
      {
        $match: {
          taskCategoryId,
          status: ComplementaryTaskStatus.COMPLETED,
          startedAt: { $exists: true },
          completedAt: { $exists: true },
        },
      },
      {
        $project: {
          duration: {
            $divide: [
              { $subtract: ['$completedAt', '$startedAt'] },
              1000 * 60 * 60, // Convert ms to hours
            ],
          },
        },
      },
      {
        $group: {
          _id: null,
          avgDuration: { $avg: '$duration' },
        },
      },
    ]);

    return result.length > 0 && result[0]?.avgDuration ? result[0].avgDuration : null;
  }
}
