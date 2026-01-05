import { IIncidentTypeRepository } from '@domain/repositories/IIncidentTypeRepository';
import { IncidentType } from '@domain/entities/IncidentType';
import {
  IncidentTypeModel,
  IIncidentTypeDocument,
} from '@infrastructure/database/models/IncidentTypeModel';
import { IncidentModel } from '@infrastructure/database/models/IncidentModel';
import { IncidentSeverity, QueryOptions } from '@shared/types';

/**
 * Repository implementation for IncidentType
 */
export class IncidentTypeRepository implements IIncidentTypeRepository {
  private toDomain(doc: IIncidentTypeDocument): IncidentType {
    return new IncidentType({
      incidentTypeId: doc.incidentTypeId,
      code: doc.code,
      typeName: doc.typeName,
      description: doc.description,
      defaultSeverity: doc.defaultSeverity,
      categoryCode: doc.categoryCode,
      parentTypeId: doc.parentTypeId,
      requiresExternalEntities: doc.requiresExternalEntities,
      estimatedResolutionTimeHours: doc.estimatedResolutionTimeHours,
      isActive: doc.isActive,
      createdAt: doc.createdAt,
      updatedAt: doc.updatedAt,
    });
  }

  private toDocument(incidentType: IncidentType): Partial<IIncidentTypeDocument> {
    return {
      incidentTypeId: incidentType.incidentTypeId,
      code: incidentType.code,
      typeName: incidentType.typeName,
      description: incidentType.description,
      defaultSeverity: incidentType.defaultSeverity,
      categoryCode: incidentType.categoryCode,
      parentTypeId: incidentType.parentTypeId,
      requiresExternalEntities: incidentType.requiresExternalEntities,
      estimatedResolutionTimeHours: incidentType.estimatedResolutionTimeHours,
      isActive: incidentType.isActive,
      createdAt: incidentType.createdAt,
      updatedAt: incidentType.updatedAt,
    };
  }

  async findById(incidentTypeId: string): Promise<IncidentType | null> {
    const doc = await IncidentTypeModel.findOne({ incidentTypeId });
    return doc ? this.toDomain(doc) : null;
  }

  async findByName(typeName: string): Promise<IncidentType | null> {
    const doc = await IncidentTypeModel.findOne({ typeName });
    return doc ? this.toDomain(doc) : null;
  }

  async findAll(options?: QueryOptions): Promise<IncidentType[]> {
    let query = IncidentTypeModel.find();

    if (options?.sortBy) {
      const sortOrder = options.sortOrder === 'desc' ? -1 : 1;
      query = query.sort({ [options.sortBy]: sortOrder });
    } else {
      query = query.sort({ typeName: 1 });
    }

    if (options?.page && options?.limit) {
      const skip = (options.page - 1) * options.limit;
      query = query.skip(skip).limit(options.limit);
    }

    const docs = await query.exec();
    return docs.map((doc) => this.toDomain(doc));
  }

  async findActive(options?: QueryOptions): Promise<IncidentType[]> {
    let query = IncidentTypeModel.find({ isActive: true });

    if (options?.sortBy) {
      const sortOrder = options.sortOrder === 'desc' ? -1 : 1;
      query = query.sort({ [options.sortBy]: sortOrder });
    } else {
      query = query.sort({ typeName: 1 });
    }

    if (options?.page && options?.limit) {
      const skip = (options.page - 1) * options.limit;
      query = query.skip(skip).limit(options.limit);
    }

    const docs = await query.exec();
    return docs.map((doc) => this.toDomain(doc));
  }

  async findBySeverity(
    severity: IncidentSeverity,
    options?: QueryOptions
  ): Promise<IncidentType[]> {
    let query = IncidentTypeModel.find({ defaultSeverity: severity });

    if (options?.sortBy) {
      const sortOrder = options.sortOrder === 'desc' ? -1 : 1;
      query = query.sort({ [options.sortBy]: sortOrder });
    } else {
      query = query.sort({ typeName: 1 });
    }

    if (options?.page && options?.limit) {
      const skip = (options.page - 1) * options.limit;
      query = query.skip(skip).limit(options.limit);
    }

    const docs = await query.exec();
    return docs.map((doc) => this.toDomain(doc));
  }

  async findRequiringExternalEntities(options?: QueryOptions): Promise<IncidentType[]> {
    let query = IncidentTypeModel.find({ requiresExternalEntities: true });

    if (options?.sortBy) {
      const sortOrder = options.sortOrder === 'desc' ? -1 : 1;
      query = query.sort({ [options.sortBy]: sortOrder });
    } else {
      query = query.sort({ typeName: 1 });
    }

    if (options?.page && options?.limit) {
      const skip = (options.page - 1) * options.limit;
      query = query.skip(skip).limit(options.limit);
    }

    const docs = await query.exec();
    return docs.map((doc) => this.toDomain(doc));
  }

  async save(incidentType: IncidentType): Promise<IncidentType> {
    const docData = this.toDocument(incidentType);
    const doc = new IncidentTypeModel(docData);
    await doc.save();
    return this.toDomain(doc);
  }

  async update(incidentType: IncidentType): Promise<IncidentType> {
    const docData = this.toDocument(incidentType);
    const doc = await IncidentTypeModel.findOneAndUpdate(
      { incidentTypeId: incidentType.incidentTypeId },
      docData,
      { new: true, runValidators: true }
    );

    if (!doc) {
      throw new Error(`IncidentType ${incidentType.incidentTypeId} not found`);
    }

    return this.toDomain(doc);
  }

  async delete(incidentTypeId: string): Promise<void> {
    await IncidentTypeModel.deleteOne({ incidentTypeId });
  }

  async count(activeOnly?: boolean): Promise<number> {
    if (activeOnly) {
      return IncidentTypeModel.countDocuments({ isActive: true });
    }
    return IncidentTypeModel.countDocuments();
  }

  async existsByName(typeName: string, excludeId?: string): Promise<boolean> {
    const query: Record<string, unknown> = { typeName };
    if (excludeId) {
      query.incidentTypeId = { $ne: excludeId };
    }

    const count = await IncidentTypeModel.countDocuments(query);
    return count > 0;
  }

  async isUsedInIncidents(incidentTypeId: string): Promise<boolean> {
    const count = await IncidentModel.countDocuments({ incidentTypeId });
    return count > 0;
  }

  async findByCode(code: string): Promise<IncidentType | null> {
    const doc = await IncidentTypeModel.findOne({ code });
    return doc ? this.toDomain(doc) : null;
  }

  async findByCategory(categoryCode: string, options?: QueryOptions): Promise<IncidentType[]> {
    let query = IncidentTypeModel.find({ categoryCode: categoryCode.toUpperCase() });

    if (options?.sortBy) {
      const sortOrder = options.sortOrder === 'desc' ? -1 : 1;
      query = query.sort({ [options.sortBy]: sortOrder });
    } else {
      query = query.sort({ typeName: 1 });
    }

    if (options?.page && options?.limit) {
      const skip = (options.page - 1) * options.limit;
      query = query.skip(skip).limit(options.limit);
    }

    const docs = await query.exec();
    return docs.map((doc) => this.toDomain(doc));
  }

  async findRootTypes(options?: QueryOptions): Promise<IncidentType[]> {
    let query = IncidentTypeModel.find({ parentTypeId: null });

    if (options?.sortBy) {
      const sortOrder = options.sortOrder === 'desc' ? -1 : 1;
      query = query.sort({ [options.sortBy]: sortOrder });
    } else {
      query = query.sort({ categoryCode: 1, typeName: 1 });
    }

    const docs = await query.exec();
    return docs.map((doc) => this.toDomain(doc));
  }

  async findChildrenOf(parentTypeId: string, options?: QueryOptions): Promise<IncidentType[]> {
    let query = IncidentTypeModel.find({ parentTypeId });

    if (options?.sortBy) {
      const sortOrder = options.sortOrder === 'desc' ? -1 : 1;
      query = query.sort({ [options.sortBy]: sortOrder });
    } else {
      query = query.sort({ typeName: 1 });
    }

    const docs = await query.exec();
    return docs.map((doc) => this.toDomain(doc));
  }

  async existsByCode(code: string, excludeId?: string): Promise<boolean> {
    const query: Record<string, unknown> = { code };
    if (excludeId) {
      query.incidentTypeId = { $ne: excludeId };
    }

    const count = await IncidentTypeModel.countDocuments(query);
    return count > 0;
  }
}
