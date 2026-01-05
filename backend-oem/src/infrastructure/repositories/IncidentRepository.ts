import { IIncidentRepository } from '@domain/repositories/IIncidentRepository';
import { Incident } from '@domain/entities/Incident';
import { IncidentModel, IIncidentDocument } from '@infrastructure/database/models/IncidentModel';
import { IncidentStatus, IncidentSeverity, QueryOptions } from '@shared/types';

/**
 * Repository implementation for Incident
 */
export class IncidentRepository implements IIncidentRepository {
  private toDomain(doc: IIncidentDocument): Incident {
    return new Incident({
      incidentId: doc.incidentId,
      incidentTypeId: doc.incidentTypeId,
      vveId: doc.vveId,
      title: doc.title,
      description: doc.description,
      severity: doc.severity,
      status: doc.status,
      reportedAt: doc.reportedAt,
      reportedBy: doc.reportedBy,
      investigatedAt: doc.investigatedAt,
      investigatedBy: doc.investigatedBy,
      resolvedAt: doc.resolvedAt,
      resolvedBy: doc.resolvedBy,
      closedAt: doc.closedAt,
      closedBy: doc.closedBy,
      externalEntitiesInvolved: doc.externalEntitiesInvolved,
      notes: doc.notes.map((note) => ({
        timestamp: note.timestamp,
        author: note.author,
        content: note.content,
      })),
      resolutionSummary: doc.resolutionSummary,
      impactDescription: doc.impactDescription,
    });
  }

  private toDocument(incident: Incident): Partial<IIncidentDocument> {
    return {
      incidentId: incident.incidentId,
      incidentTypeId: incident.incidentTypeId,
      vveId: incident.vveId,
      title: incident.title,
      description: incident.description,
      severity: incident.severity,
      status: incident.status,
      reportedAt: incident.reportedAt,
      reportedBy: incident.reportedBy,
      investigatedAt: incident.investigatedAt,
      investigatedBy: incident.investigatedBy,
      resolvedAt: incident.resolvedAt,
      resolvedBy: incident.resolvedBy,
      closedAt: incident.closedAt,
      closedBy: incident.closedBy,
      externalEntitiesInvolved: [...incident.externalEntitiesInvolved],
      notes: incident.notes.map((note) => ({
        timestamp: note.timestamp,
        author: note.author,
        content: note.content,
      })),
      resolutionSummary: incident.resolutionSummary,
      impactDescription: incident.impactDescription,
    };
  }

  async findById(incidentId: string): Promise<Incident | null> {
    const doc = await IncidentModel.findOne({ incidentId });
    return doc ? this.toDomain(doc) : null;
  }

  async findAll(options?: QueryOptions): Promise<Incident[]> {
    let query = IncidentModel.find();

    if (options?.sortBy) {
      const sortOrder = options.sortOrder === 'desc' ? -1 : 1;
      query = query.sort({ [options.sortBy]: sortOrder });
    } else {
      query = query.sort({ reportedAt: -1 });
    }

    if (options?.page && options?.limit) {
      const skip = (options.page - 1) * options.limit;
      query = query.skip(skip).limit(options.limit);
    }

    const docs = await query.exec();
    return docs.map((doc) => this.toDomain(doc));
  }

  async findByStatus(status: IncidentStatus, options?: QueryOptions): Promise<Incident[]> {
    let query = IncidentModel.find({ status });

    if (options?.sortBy) {
      const sortOrder = options.sortOrder === 'desc' ? -1 : 1;
      query = query.sort({ [options.sortBy]: sortOrder });
    } else {
      query = query.sort({ reportedAt: -1 });
    }

    if (options?.page && options?.limit) {
      const skip = (options.page - 1) * options.limit;
      query = query.skip(skip).limit(options.limit);
    }

    const docs = await query.exec();
    return docs.map((doc) => this.toDomain(doc));
  }

  async findBySeverity(severity: IncidentSeverity, options?: QueryOptions): Promise<Incident[]> {
    let query = IncidentModel.find({ severity });

    if (options?.sortBy) {
      const sortOrder = options.sortOrder === 'desc' ? -1 : 1;
      query = query.sort({ [options.sortBy]: sortOrder });
    } else {
      query = query.sort({ reportedAt: -1 });
    }

    if (options?.page && options?.limit) {
      const skip = (options.page - 1) * options.limit;
      query = query.skip(skip).limit(options.limit);
    }

    const docs = await query.exec();
    return docs.map((doc) => this.toDomain(doc));
  }

  async findByIncidentType(incidentTypeId: string, options?: QueryOptions): Promise<Incident[]> {
    let query = IncidentModel.find({ incidentTypeId });

    if (options?.sortBy) {
      const sortOrder = options.sortOrder === 'desc' ? -1 : 1;
      query = query.sort({ [options.sortBy]: sortOrder });
    } else {
      query = query.sort({ reportedAt: -1 });
    }

    if (options?.page && options?.limit) {
      const skip = (options.page - 1) * options.limit;
      query = query.skip(skip).limit(options.limit);
    }

    const docs = await query.exec();
    return docs.map((doc) => this.toDomain(doc));
  }

  async findByVveId(vveId: string, options?: QueryOptions): Promise<Incident[]> {
    let query = IncidentModel.find({ vveId });

    if (options?.sortBy) {
      const sortOrder = options.sortOrder === 'desc' ? -1 : 1;
      query = query.sort({ [options.sortBy]: sortOrder });
    } else {
      query = query.sort({ reportedAt: -1 });
    }

    if (options?.page && options?.limit) {
      const skip = (options.page - 1) * options.limit;
      query = query.skip(skip).limit(options.limit);
    }

    const docs = await query.exec();
    return docs.map((doc) => this.toDomain(doc));
  }

  async findByReporter(reportedBy: string, options?: QueryOptions): Promise<Incident[]> {
    let query = IncidentModel.find({ reportedBy });

    if (options?.sortBy) {
      const sortOrder = options.sortOrder === 'desc' ? -1 : 1;
      query = query.sort({ [options.sortBy]: sortOrder });
    } else {
      query = query.sort({ reportedAt: -1 });
    }

    if (options?.page && options?.limit) {
      const skip = (options.page - 1) * options.limit;
      query = query.skip(skip).limit(options.limit);
    }

    const docs = await query.exec();
    return docs.map((doc) => this.toDomain(doc));
  }

  async findByDateRange(fromDate: Date, toDate: Date, options?: QueryOptions): Promise<Incident[]> {
    let query = IncidentModel.find({
      reportedAt: { $gte: fromDate, $lte: toDate },
    });

    if (options?.sortBy) {
      const sortOrder = options.sortOrder === 'desc' ? -1 : 1;
      query = query.sort({ [options.sortBy]: sortOrder });
    } else {
      query = query.sort({ reportedAt: -1 });
    }

    if (options?.page && options?.limit) {
      const skip = (options.page - 1) * options.limit;
      query = query.skip(skip).limit(options.limit);
    }

    const docs = await query.exec();
    return docs.map((doc) => this.toDomain(doc));
  }

  async findOpen(options?: QueryOptions): Promise<Incident[]> {
    let query = IncidentModel.find({
      status: { $in: [IncidentStatus.REPORTED, IncidentStatus.UNDER_INVESTIGATION] },
    });

    if (options?.sortBy) {
      const sortOrder = options.sortOrder === 'desc' ? -1 : 1;
      query = query.sort({ [options.sortBy]: sortOrder });
    } else {
      query = query.sort({ reportedAt: -1 });
    }

    if (options?.page && options?.limit) {
      const skip = (options.page - 1) * options.limit;
      query = query.skip(skip).limit(options.limit);
    }

    const docs = await query.exec();
    return docs.map((doc) => this.toDomain(doc));
  }

  async findCritical(options?: QueryOptions): Promise<Incident[]> {
    return this.findBySeverity(IncidentSeverity.CRITICAL, options);
  }

  async findWithExternalEntities(options?: QueryOptions): Promise<Incident[]> {
    let query = IncidentModel.find({
      externalEntitiesInvolved: { $exists: true, $ne: [] },
    });

    if (options?.sortBy) {
      const sortOrder = options.sortOrder === 'desc' ? -1 : 1;
      query = query.sort({ [options.sortBy]: sortOrder });
    } else {
      query = query.sort({ reportedAt: -1 });
    }

    if (options?.page && options?.limit) {
      const skip = (options.page - 1) * options.limit;
      query = query.skip(skip).limit(options.limit);
    }

    const docs = await query.exec();
    return docs.map((doc) => this.toDomain(doc));
  }

  async findByExternalEntity(entityName: string, options?: QueryOptions): Promise<Incident[]> {
    let query = IncidentModel.find({
      externalEntitiesInvolved: entityName,
    });

    if (options?.sortBy) {
      const sortOrder = options.sortOrder === 'desc' ? -1 : 1;
      query = query.sort({ [options.sortBy]: sortOrder });
    } else {
      query = query.sort({ reportedAt: -1 });
    }

    if (options?.page && options?.limit) {
      const skip = (options.page - 1) * options.limit;
      query = query.skip(skip).limit(options.limit);
    }

    const docs = await query.exec();
    return docs.map((doc) => this.toDomain(doc));
  }

  async save(incident: Incident): Promise<Incident> {
    const docData = this.toDocument(incident);
    const doc = new IncidentModel(docData);
    await doc.save();
    return this.toDomain(doc);
  }

  async update(incident: Incident): Promise<Incident> {
    const docData = this.toDocument(incident);
    const doc = await IncidentModel.findOneAndUpdate({ incidentId: incident.incidentId }, docData, {
      new: true,
      runValidators: true,
    });

    if (!doc) {
      throw new Error(`Incident ${incident.incidentId} not found`);
    }

    return this.toDomain(doc);
  }

  async delete(incidentId: string): Promise<void> {
    await IncidentModel.deleteOne({ incidentId });
  }

  async count(status?: IncidentStatus, severity?: IncidentSeverity): Promise<number> {
    const query: Record<string, unknown> = {};
    if (status) query.status = status;
    if (severity) query.severity = severity;

    return IncidentModel.countDocuments(query);
  }

  async getAverageResolutionTime(fromDate?: Date, toDate?: Date): Promise<number | null> {
    const match: Record<string, unknown> = {
      status: { $in: [IncidentStatus.RESOLVED, IncidentStatus.CLOSED] },
      reportedAt: { $exists: true },
      resolvedAt: { $exists: true },
    };

    if (fromDate && toDate) {
      match.resolvedAt = { $gte: fromDate, $lte: toDate };
    }

    const result = await IncidentModel.aggregate([
      { $match: match },
      {
        $project: {
          resolutionTime: {
            $divide: [
              { $subtract: ['$resolvedAt', '$reportedAt'] },
              1000 * 60 * 60, // Convert ms to hours
            ],
          },
        },
      },
      {
        $group: {
          _id: null,
          avgResolutionTime: { $avg: '$resolutionTime' },
        },
      },
    ]);

    return result.length > 0 && result[0]?.avgResolutionTime ? result[0].avgResolutionTime : null;
  }

  async getStatistics(
    fromDate: Date,
    toDate: Date
  ): Promise<{
    total: number;
    byStatus: Record<IncidentStatus, number>;
    bySeverity: Record<IncidentSeverity, number>;
    averageResolutionTimeHours: number | null;
  }> {
    const match = {
      reportedAt: { $gte: fromDate, $lte: toDate },
    };

    // Get total count
    const total = await IncidentModel.countDocuments(match);

    // Get counts by status
    const byStatusResult = await IncidentModel.aggregate([
      { $match: match },
      {
        $group: {
          _id: '$status',
          count: { $sum: 1 },
        },
      },
    ]);

    const byStatus = Object.values(IncidentStatus).reduce(
      (acc, status) => {
        acc[status] = 0;
        return acc;
      },
      {} as Record<IncidentStatus, number>
    );

    byStatusResult.forEach((item) => {
      byStatus[item._id as IncidentStatus] = item.count;
    });

    // Get counts by severity
    const bySeverityResult = await IncidentModel.aggregate([
      { $match: match },
      {
        $group: {
          _id: '$severity',
          count: { $sum: 1 },
        },
      },
    ]);

    const bySeverity = Object.values(IncidentSeverity).reduce(
      (acc, severity) => {
        acc[severity] = 0;
        return acc;
      },
      {} as Record<IncidentSeverity, number>
    );

    bySeverityResult.forEach((item) => {
      bySeverity[item._id as IncidentSeverity] = item.count;
    });

    // Get average resolution time
    const averageResolutionTimeHours = await this.getAverageResolutionTime(fromDate, toDate);

    return {
      total,
      byStatus,
      bySeverity,
      averageResolutionTimeHours,
    };
  }
}
