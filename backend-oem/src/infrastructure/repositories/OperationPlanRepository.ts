import { IOperationPlanRepository } from '@domain/repositories/IOperationPlanRepository';
import { OperationPlan } from '@domain/entities/OperationPlan';
import { PlannedOperation } from '@domain/value-objects/PlannedOperation';
import {
  OperationPlanModel,
  IOperationPlanDocument,
} from '@infrastructure/database/models/OperationPlanModel';
import { OperationPlanStatus, PlanningAlgorithm, QueryOptions } from '@shared/types';

/**
 * Repository implementation for OperationPlan
 * Maps between domain entities and Mongoose documents
 */
export class OperationPlanRepository implements IOperationPlanRepository {
  /**
   * Convert Mongoose document to domain entity
   */
  private toDomain(doc: IOperationPlanDocument): OperationPlan {
    const operations = doc.operations.map(
      (op) =>
        new PlannedOperation({
          vvnId: op.vvnId,
          vesselImo: op.vesselImo,
          plannedStart: new Date(op.plannedStart),
          plannedEnd: new Date(op.plannedEnd),
          assignedCranes: op.assignedCranes,
          assignedDock: op.assignedDock,
          assignedStaff: op.assignedStaff,
          operationType: op.operationType,
        })
    );

    return new OperationPlan({
      operationPlanId: doc.operationPlanId,
      targetDate: new Date(doc.targetDate),
      algorithm: doc.algorithm,
      createdBy: doc.createdBy,
      createdAt: new Date(doc.createdAt),
      status: doc.status,
      totalDelay: doc.totalDelay,
      operations,
    });
  }

  /**
   * Convert domain entity to Mongoose document data
   */
  private toDocument(plan: OperationPlan): Partial<IOperationPlanDocument> {
    return {
      operationPlanId: plan.operationPlanId,
      targetDate: plan.targetDate,
      algorithm: plan.algorithm,
      createdBy: plan.createdBy,
      createdAt: plan.createdAt,
      status: plan.status,
      totalDelay: plan.totalDelay,
      operations: plan.operations.map((op) => ({
        vvnId: op.vvnId,
        vesselImo: op.vesselImo,
        plannedStart: op.plannedStart,
        plannedEnd: op.plannedEnd,
        assignedCranes: op.assignedCranes,
        assignedDock: op.assignedDock,
        assignedStaff: op.assignedStaff,
        operationType: op.operationType,
      })),
    };
  }

  async findById(operationPlanId: string): Promise<OperationPlan | null> {
    const doc = await OperationPlanModel.findOne({ operationPlanId });
    return doc ? this.toDomain(doc) : null;
  }

  async findAll(options?: QueryOptions): Promise<OperationPlan[]> {
    let query = OperationPlanModel.find();

    // Apply sorting
    if (options?.sortBy) {
      const sortOrder = options.sortOrder === 'desc' ? -1 : 1;
      query = query.sort({ [options.sortBy]: sortOrder });
    } else {
      query = query.sort({ createdAt: -1 }); // Default sort
    }

    // Apply pagination
    if (options?.page && options?.limit) {
      const skip = (options.page - 1) * options.limit;
      query = query.skip(skip).limit(options.limit);
    }

    const docs = await query.exec();
    return docs.map((doc) => this.toDomain(doc));
  }

  async findByStatus(
    status: OperationPlanStatus,
    options?: QueryOptions
  ): Promise<OperationPlan[]> {
    let query = OperationPlanModel.find({ status });

    if (options?.sortBy) {
      const sortOrder = options.sortOrder === 'desc' ? -1 : 1;
      query = query.sort({ [options.sortBy]: sortOrder });
    } else {
      query = query.sort({ targetDate: 1 });
    }

    if (options?.page && options?.limit) {
      const skip = (options.page - 1) * options.limit;
      query = query.skip(skip).limit(options.limit);
    }

    const docs = await query.exec();
    return docs.map((doc) => this.toDomain(doc));
  }

  async findByTargetDate(targetDate: Date): Promise<OperationPlan | null> {
    // Normalize date to start of day
    const startOfDay = new Date(targetDate);
    startOfDay.setHours(0, 0, 0, 0);
    const endOfDay = new Date(targetDate);
    endOfDay.setHours(23, 59, 59, 999);

    const doc = await OperationPlanModel.findOne({
      targetDate: { $gte: startOfDay, $lte: endOfDay },
    }).sort({ createdAt: -1 });

    return doc ? this.toDomain(doc) : null;
  }

  async findByDateRange(
    fromDate: Date,
    toDate: Date,
    options?: QueryOptions
  ): Promise<OperationPlan[]> {
    const startOfDay = new Date(fromDate);
    startOfDay.setHours(0, 0, 0, 0);
    const endOfDay = new Date(toDate);
    endOfDay.setHours(23, 59, 59, 999);

    let query = OperationPlanModel.find({
      targetDate: { $gte: startOfDay, $lte: endOfDay },
    });

    if (options?.sortBy) {
      const sortOrder = options.sortOrder === 'desc' ? -1 : 1;
      query = query.sort({ [options.sortBy]: sortOrder });
    } else {
      query = query.sort({ targetDate: 1 });
    }

    if (options?.page && options?.limit) {
      const skip = (options.page - 1) * options.limit;
      query = query.skip(skip).limit(options.limit);
    }

    const docs = await query.exec();
    return docs.map((doc) => this.toDomain(doc));
  }

  async findByAlgorithm(
    algorithm: PlanningAlgorithm,
    options?: QueryOptions
  ): Promise<OperationPlan[]> {
    let query = OperationPlanModel.find({ algorithm });

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

  async findByVvnId(vvnId: string): Promise<OperationPlan | null> {
    const doc = await OperationPlanModel.findOne({
      'operations.vvnId': vvnId,
    }).sort({ createdAt: -1 });

    return doc ? this.toDomain(doc) : null;
  }

  async save(operationPlan: OperationPlan): Promise<OperationPlan> {
    const docData = this.toDocument(operationPlan);
    const doc = new OperationPlanModel(docData);
    await doc.save();
    return this.toDomain(doc);
  }

  async update(operationPlan: OperationPlan): Promise<OperationPlan> {
    const docData = this.toDocument(operationPlan);
    const doc = await OperationPlanModel.findOneAndUpdate(
      { operationPlanId: operationPlan.operationPlanId },
      docData,
      { new: true, runValidators: true }
    );

    if (!doc) {
      throw new Error(`OperationPlan ${operationPlan.operationPlanId} not found`);
    }

    return this.toDomain(doc);
  }

  async delete(operationPlanId: string): Promise<void> {
    await OperationPlanModel.deleteOne({ operationPlanId });
  }

  async count(status?: OperationPlanStatus): Promise<number> {
    if (status) {
      return OperationPlanModel.countDocuments({ status });
    }
    return OperationPlanModel.countDocuments();
  }

  async existsForDate(targetDate: Date): Promise<boolean> {
    const startOfDay = new Date(targetDate);
    startOfDay.setHours(0, 0, 0, 0);
    const endOfDay = new Date(targetDate);
    endOfDay.setHours(23, 59, 59, 999);

    const count = await OperationPlanModel.countDocuments({
      targetDate: { $gte: startOfDay, $lte: endOfDay },
    });

    return count > 0;
  }

  async markOutdatedPlansForDate(targetDate: Date): Promise<void> {
    const startOfDay = new Date(targetDate);
    startOfDay.setHours(0, 0, 0, 0);
    const endOfDay = new Date(targetDate);
    endOfDay.setHours(23, 59, 59, 999);

    await OperationPlanModel.updateMany(
      {
        targetDate: { $gte: startOfDay, $lte: endOfDay },
        status: {
          $in: [OperationPlanStatus.GENERATED, OperationPlanStatus.APPROVED],
        },
      },
      { $set: { status: OperationPlanStatus.OUTDATED } }
    );
  }
}
