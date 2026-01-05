import { IVesselVisitExecutionRepository } from '@domain/repositories/IVesselVisitExecutionRepository';
import { VesselVisitExecution } from '@domain/entities/VesselVisitExecution';
import { ExecutedOperation } from '@domain/value-objects/ExecutedOperation';
import {
  VesselVisitExecutionModel,
  IVesselVisitExecutionDocument,
} from '@infrastructure/database/models/VesselVisitExecutionModel';
import { VesselVisitExecutionStatus, QueryOptions } from '@shared/types';

/**
 * Repository implementation for VesselVisitExecution
 */
export class VesselVisitExecutionRepository implements IVesselVisitExecutionRepository {
  private toDomain(doc: IVesselVisitExecutionDocument): VesselVisitExecution {
    // Filter out null/undefined operations before mapping
    const operations = doc.operations
      .filter((op) => op !== null && op !== undefined)
      .map(
        (op) =>
          new ExecutedOperation({
            operationType: op.operationType,
            startTime: op.startTime,
            endTime: op.endTime,
            containersProcessed: op.containersProcessed,
            cranesUsed: op.cranesUsed,
            staffAssigned: op.staffAssigned,
            status: op.status,
            plannedOperationId: op.plannedOperationId, // US 4.1.9
          })
      );

    return new VesselVisitExecution({
      vveId: doc.vveId,
      vvnId: doc.vvnId,
      operationPlanId: doc.operationPlanId,
      actualPortArrivalTime: doc.actualPortArrivalTime,
      actualBerthTime: doc.actualBerthTime,
      actualUnberthTime: doc.actualUnberthTime,
      actualPortDepartureTime: doc.actualPortDepartureTime,
      assignedDock: doc.assignedDock,
      operations,
      status: doc.status,
      completedAt: doc.completedAt,
      completedBy: doc.completedBy,
      incidents: doc.incidents,
    });
  }

  private toDocument(vve: VesselVisitExecution): Partial<IVesselVisitExecutionDocument> {
    return {
      vveId: vve.vveId,
      vvnId: vve.vvnId,
      operationPlanId: vve.operationPlanId,
      actualPortArrivalTime: vve.actualPortArrivalTime,
      actualBerthTime: vve.actualBerthTime,
      actualUnberthTime: vve.actualUnberthTime,
      actualPortDepartureTime: vve.actualPortDepartureTime,
      assignedDock: vve.assignedDock,
      operations: vve.operations.map((op) => ({
        operationType: op.operationType,
        startTime: op.startTime,
        endTime: op.endTime,
        containersProcessed: op.containersProcessed,
        cranesUsed: op.cranesUsed,
        staffAssigned: op.staffAssigned,
        status: op.status,
        plannedOperationId: op.plannedOperationId, // US 4.1.9
      })),
      status: vve.status,
      completedAt: vve.completedAt,
      completedBy: vve.completedBy,
      incidents: [...vve.incidents],
    };
  }

  async findById(vveId: string): Promise<VesselVisitExecution | null> {
    const doc = await VesselVisitExecutionModel.findOne({ vveId });
    return doc ? this.toDomain(doc) : null;
  }

  async findByVvnId(vvnId: string): Promise<VesselVisitExecution | null> {
    const doc = await VesselVisitExecutionModel.findOne({ vvnId });
    return doc ? this.toDomain(doc) : null;
  }

  async findAll(options?: QueryOptions): Promise<VesselVisitExecution[]> {
    let query = VesselVisitExecutionModel.find();

    if (options?.sortBy) {
      const sortOrder = options.sortOrder === 'desc' ? -1 : 1;
      query = query.sort({ [options.sortBy]: sortOrder });
    } else {
      query = query.sort({ actualPortArrivalTime: -1 });
    }

    if (options?.page && options?.limit) {
      const skip = (options.page - 1) * options.limit;
      query = query.skip(skip).limit(options.limit);
    }

    const docs = await query.exec();
    return docs.map((doc) => this.toDomain(doc));
  }

  async findByStatus(
    status: VesselVisitExecutionStatus,
    options?: QueryOptions
  ): Promise<VesselVisitExecution[]> {
    let query = VesselVisitExecutionModel.find({ status });

    if (options?.sortBy) {
      const sortOrder = options.sortOrder === 'desc' ? -1 : 1;
      query = query.sort({ [options.sortBy]: sortOrder });
    }

    if (options?.page && options?.limit) {
      const skip = (options.page - 1) * options.limit;
      query = query.skip(skip).limit(options.limit);
    }

    const docs = await query.exec();
    return docs.map((doc) => this.toDomain(doc));
  }

  async findByOperationPlanId(
    operationPlanId: string,
    options?: QueryOptions
  ): Promise<VesselVisitExecution[]> {
    let query = VesselVisitExecutionModel.find({ operationPlanId });

    if (options?.sortBy) {
      const sortOrder = options.sortOrder === 'desc' ? -1 : 1;
      query = query.sort({ [options.sortBy]: sortOrder });
    }

    if (options?.page && options?.limit) {
      const skip = (options.page - 1) * options.limit;
      query = query.skip(skip).limit(options.limit);
    }

    const docs = await query.exec();
    return docs.map((doc) => this.toDomain(doc));
  }

  async findByDateRange(
    fromDate: Date,
    toDate: Date,
    options?: QueryOptions
  ): Promise<VesselVisitExecution[]> {
    let query = VesselVisitExecutionModel.find({
      actualPortArrivalTime: { $gte: fromDate, $lte: toDate },
    });

    if (options?.sortBy) {
      const sortOrder = options.sortOrder === 'desc' ? -1 : 1;
      query = query.sort({ [options.sortBy]: sortOrder });
    }

    if (options?.page && options?.limit) {
      const skip = (options.page - 1) * options.limit;
      query = query.skip(skip).limit(options.limit);
    }

    const docs = await query.exec();
    return docs.map((doc) => this.toDomain(doc));
  }

  async findByDock(dockCode: string, options?: QueryOptions): Promise<VesselVisitExecution[]> {
    let query = VesselVisitExecutionModel.find({ assignedDock: dockCode });

    if (options?.sortBy) {
      const sortOrder = options.sortOrder === 'desc' ? -1 : 1;
      query = query.sort({ [options.sortBy]: sortOrder });
    }

    if (options?.page && options?.limit) {
      const skip = (options.page - 1) * options.limit;
      query = query.skip(skip).limit(options.limit);
    }

    const docs = await query.exec();
    return docs.map((doc) => this.toDomain(doc));
  }

  async findWithIncidents(options?: QueryOptions): Promise<VesselVisitExecution[]> {
    let query = VesselVisitExecutionModel.find({
      incidents: { $exists: true, $ne: [] },
    });

    if (options?.sortBy) {
      const sortOrder = options.sortOrder === 'desc' ? -1 : 1;
      query = query.sort({ [options.sortBy]: sortOrder });
    }

    if (options?.page && options?.limit) {
      const skip = (options.page - 1) * options.limit;
      query = query.skip(skip).limit(options.limit);
    }

    const docs = await query.exec();
    return docs.map((doc) => this.toDomain(doc));
  }

  async findInProgress(options?: QueryOptions): Promise<VesselVisitExecution[]> {
    return this.findByStatus(VesselVisitExecutionStatus.IN_PROGRESS, options);
  }

  async save(vve: VesselVisitExecution): Promise<VesselVisitExecution> {
    const docData = this.toDocument(vve);
    const doc = new VesselVisitExecutionModel(docData);
    await doc.save();
    return this.toDomain(doc);
  }

  async update(vve: VesselVisitExecution): Promise<VesselVisitExecution> {
    const docData = this.toDocument(vve);
    const doc = await VesselVisitExecutionModel.findOneAndUpdate({ vveId: vve.vveId }, docData, {
      new: true,
      runValidators: true,
    });

    if (!doc) {
      throw new Error(`VesselVisitExecution ${vve.vveId} not found`);
    }

    return this.toDomain(doc);
  }

  async delete(vveId: string): Promise<void> {
    await VesselVisitExecutionModel.deleteOne({ vveId });
  }

  async count(status?: VesselVisitExecutionStatus): Promise<number> {
    if (status) {
      return VesselVisitExecutionModel.countDocuments({ status });
    }
    return VesselVisitExecutionModel.countDocuments();
  }

  async existsForVvn(vvnId: string): Promise<boolean> {
    const count = await VesselVisitExecutionModel.countDocuments({ vvnId });
    return count > 0;
  }

  async getAverageTurnaroundTime(fromDate?: Date, toDate?: Date): Promise<number | null> {
    const match: Record<string, unknown> = {
      status: VesselVisitExecutionStatus.COMPLETED,
      actualPortArrivalTime: { $exists: true },
      actualPortDepartureTime: { $exists: true },
    };

    if (fromDate && toDate) {
      match.completedAt = { $gte: fromDate, $lte: toDate };
    }

    const result = await VesselVisitExecutionModel.aggregate([
      { $match: match },
      {
        $project: {
          turnaroundTime: {
            $divide: [
              { $subtract: ['$actualPortDepartureTime', '$actualPortArrivalTime'] },
              1000 * 60 * 60, // Convert ms to hours
            ],
          },
        },
      },
      {
        $group: {
          _id: null,
          avgTurnaround: { $avg: '$turnaroundTime' },
        },
      },
    ]);

    return result.length > 0 && result[0]?.avgTurnaround ? result[0].avgTurnaround : null;
  }

  async getAverageBerthOccupancy(fromDate?: Date, toDate?: Date): Promise<number | null> {
    const match: Record<string, unknown> = {
      status: VesselVisitExecutionStatus.COMPLETED,
      actualBerthTime: { $exists: true },
      actualUnberthTime: { $exists: true },
    };

    if (fromDate && toDate) {
      match.completedAt = { $gte: fromDate, $lte: toDate };
    }

    const result = await VesselVisitExecutionModel.aggregate([
      { $match: match },
      {
        $project: {
          berthOccupancy: {
            $divide: [
              { $subtract: ['$actualUnberthTime', '$actualBerthTime'] },
              1000 * 60 * 60, // Convert ms to hours
            ],
          },
        },
      },
      {
        $group: {
          _id: null,
          avgOccupancy: { $avg: '$berthOccupancy' },
        },
      },
    ]);

    return result.length > 0 && result[0]?.avgOccupancy ? result[0].avgOccupancy : null;
  }

  /**
   * US 4.1.7: Get next sequential number for VVE ID generation
   * Finds the highest VVE ID and returns the next sequential number
   * @returns Next sequential number (starts at 1 if no VVEs exist)
   */
  async getNextSequentialNumber(): Promise<number> {
    // Find the latest VVE sorted by vveId in descending order
    const latestVve = await VesselVisitExecutionModel.findOne()
      .sort({ vveId: -1 })
      .select('vveId')
      .exec();

    if (!latestVve || !latestVve.vveId) {
      return 1; // Start at 1 if no VVEs exist
    }

    // Extract the sequential number from VVE-XXXXXX format
    const match = latestVve.vveId.match(/^VVE-(\d{6})$/);
    if (match) {
      return parseInt(match[1]!, 10) + 1;
    }

    // Fallback: if format doesn't match, start at 1
    return 1;
  }
}
