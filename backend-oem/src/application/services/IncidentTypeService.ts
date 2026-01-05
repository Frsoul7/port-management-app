import { IIncidentTypeRepository } from '@domain/repositories/IIncidentTypeRepository';
import { IncidentType } from '@domain/entities/IncidentType';
import { QueryOptions } from '@shared/types';
import { logger } from '@shared/utils/logger';
import {
  CreateIncidentTypeDto,
  UpdateIncidentTypeDto,
  IncidentTypeDto,
} from '@application/dtos';

/**
 * Application Service for IncidentType aggregate
 * Handles hierarchical incident type management
 */
export class IncidentTypeService {
  constructor(private readonly incidentTypeRepository: IIncidentTypeRepository) {}

  /**
   * Create a new incident type
   * US 4.1.12: Manage hierarchical incident type catalog
   */
  async createIncidentType(dto: CreateIncidentTypeDto): Promise<IncidentTypeDto> {
    // Validate uniqueness
    const existingByCode = await this.incidentTypeRepository.findByCode(dto.code);
    if (existingByCode) {
      throw new Error(`Incident type with code ${dto.code} already exists`);
    }

    const existingByName = await this.incidentTypeRepository.findByName(dto.typeName);
    if (existingByName) {
      throw new Error(`Incident type with name ${dto.typeName} already exists`);
    }

    // Validate parent exists if provided
    if (dto.parentTypeId) {
      const parent = await this.incidentTypeRepository.findById(dto.parentTypeId);
      if (!parent) {
        throw new Error(`Parent incident type ${dto.parentTypeId} not found`);
      }
    }

    const incidentType = new IncidentType({
      code: dto.code,
      typeName: dto.typeName,
      description: dto.description,
      defaultSeverity: dto.defaultSeverity as any,
      categoryCode: dto.categoryCode,
      parentTypeId: dto.parentTypeId || null,
      requiresExternalEntities: dto.requiresExternalEntities,
      estimatedResolutionTimeHours: dto.estimatedResolutionTimeHours,
    });

    const saved = await this.incidentTypeRepository.save(incidentType);
    logger.info(`Incident type ${saved.code} created: ${saved.typeName}`);
    return this.toDto(saved);
  }

  /**
   * Get incident type by ID
   */
  async getIncidentTypeById(incidentTypeId: string): Promise<IncidentTypeDto> {
    const incidentType = await this.incidentTypeRepository.findById(incidentTypeId);
    if (!incidentType) {
      throw new Error(`Incident type ${incidentTypeId} not found`);
    }
    return this.toDto(incidentType);
  }

  /**
   * Get incident type by code
   */
  async getIncidentTypeByCode(code: string): Promise<IncidentTypeDto> {
    const incidentType = await this.incidentTypeRepository.findByCode(code);
    if (!incidentType) {
      throw new Error(`Incident type with code ${code} not found`);
    }
    return this.toDto(incidentType);
  }

  /**
   * List all incident types
   */
  async listAllIncidentTypes(options?: QueryOptions): Promise<IncidentTypeDto[]> {
    const incidentTypes = await this.incidentTypeRepository.findAll(options);
    return incidentTypes.map((it) => this.toDto(it));
  }

  /**
   * List active incident types only
   */
  async listActiveIncidentTypes(options?: QueryOptions): Promise<IncidentTypeDto[]> {
    const incidentTypes = await this.incidentTypeRepository.findActive(options);
    return incidentTypes.map((it) => this.toDto(it));
  }

  /**
   * List incident types by category
   * US 4.1.12: Filter by ENV, OPS, or SAF
   */
  async listIncidentTypesByCategory(
    categoryCode: string,
    options?: QueryOptions
  ): Promise<IncidentTypeDto[]> {
    const incidentTypes = await this.incidentTypeRepository.findByCategory(categoryCode, options);
    return incidentTypes.map((it) => this.toDto(it));
  }

  /**
   * List incident types with optional filters
   * Unified method that handles activeOnly and category filters
   */
  async listIncidentTypes(
    filters?: {
      activeOnly?: boolean;
      category?: string;
    },
    options?: QueryOptions
  ): Promise<IncidentTypeDto[]> {
    let incidentTypes: IncidentType[];

    if (filters?.category) {
      incidentTypes = await this.incidentTypeRepository.findByCategory(filters.category, options);
    } else if (filters?.activeOnly) {
      incidentTypes = await this.incidentTypeRepository.findActive(options);
    } else {
      incidentTypes = await this.incidentTypeRepository.findAll(options);
    }

    return incidentTypes.map((it) => this.toDto(it));
  }

  /**
   * Get root incident types (no parents)
   * US 4.1.12: Display top-level categories
   */
  async getRootIncidentTypes(options?: QueryOptions): Promise<IncidentTypeDto[]> {
    const incidentTypes = await this.incidentTypeRepository.findRootTypes(options);
    return incidentTypes.map((it) => this.toDto(it));
  }

  /**
   * Get children of a specific incident type
   * US 4.1.12: Navigate hierarchical structure
   */
  async getChildIncidentTypes(
    parentTypeId: string,
    options?: QueryOptions
  ): Promise<IncidentTypeDto[]> {
    const incidentTypes = await this.incidentTypeRepository.findChildrenOf(parentTypeId, options);
    return incidentTypes.map((it) => this.toDto(it));
  }

  /**
   * Update incident type
   * US 4.1.12: Modify incident type properties
   */
  async updateIncidentType(
    incidentTypeId: string,
    dto: UpdateIncidentTypeDto
  ): Promise<IncidentTypeDto> {
    const incidentType = await this.incidentTypeRepository.findById(incidentTypeId);
    if (!incidentType) {
      throw new Error(`Incident type ${incidentTypeId} not found`);
    }

    // Validate name uniqueness if changed
    if (dto.typeName) {
      const existingByName = await this.incidentTypeRepository.existsByName(
        dto.typeName,
        incidentTypeId
      );
      if (existingByName) {
        throw new Error(`Incident type with name ${dto.typeName} already exists`);
      }
    }

    // Validate parent if changed
    if (dto.parentTypeId) {
      const parent = await this.incidentTypeRepository.findById(dto.parentTypeId);
      if (!parent) {
        throw new Error(`Parent incident type ${dto.parentTypeId} not found`);
      }

      // Prevent circular reference
      if (dto.parentTypeId === incidentTypeId) {
        throw new Error('Incident type cannot be its own parent');
      }

      // Prevent setting parent as one of its own descendants
      await this.validateNoCircularReference(incidentTypeId, dto.parentTypeId);
    }

    incidentType.update({
      typeName: dto.typeName,
      description: dto.description,
      defaultSeverity: dto.defaultSeverity as any,
      categoryCode: dto.categoryCode,
      parentTypeId: dto.parentTypeId,
      requiresExternalEntities: dto.requiresExternalEntities,
      estimatedResolutionTimeHours: dto.estimatedResolutionTimeHours,
    });

    const updated = await this.incidentTypeRepository.update(incidentType);
    logger.info(`Incident type ${updated.code} updated`);
    return this.toDto(updated);
  }

  /**
   * Deactivate incident type
   * US 4.1.12: Soft delete by deactivating
   */
  async deactivateIncidentType(incidentTypeId: string): Promise<IncidentTypeDto> {
    const incidentType = await this.incidentTypeRepository.findById(incidentTypeId);
    if (!incidentType) {
      throw new Error(`Incident type ${incidentTypeId} not found`);
    }

    // Check if used in incidents
    const isUsed = await this.incidentTypeRepository.isUsedInIncidents(incidentTypeId);
    if (isUsed) {
      throw new Error('Cannot deactivate incident type that is used in existing incidents');
    }

    incidentType.deactivate();
    const updated = await this.incidentTypeRepository.update(incidentType);
    logger.info(`Incident type ${updated.code} deactivated`);
    return this.toDto(updated);
  }

  /**
   * Reactivate incident type
   */
  async reactivateIncidentType(incidentTypeId: string): Promise<IncidentTypeDto> {
    const incidentType = await this.incidentTypeRepository.findById(incidentTypeId);
    if (!incidentType) {
      throw new Error(`Incident type ${incidentTypeId} not found`);
    }

    incidentType.reactivate();
    const updated = await this.incidentTypeRepository.update(incidentType);
    logger.info(`Incident type ${updated.code} reactivated`);
    return this.toDto(updated);
  }

  /**
   * Delete incident type permanently
   * Should only be used for types never used in incidents
   */
  async deleteIncidentType(incidentTypeId: string): Promise<void> {
    const incidentType = await this.incidentTypeRepository.findById(incidentTypeId);
    if (!incidentType) {
      throw new Error(`Incident type ${incidentTypeId} not found`);
    }

    // Check if used in incidents
    const isUsed = await this.incidentTypeRepository.isUsedInIncidents(incidentTypeId);
    if (isUsed) {
      throw new Error('Cannot delete incident type that is used in existing incidents');
    }

    // Check if has children
    const children = await this.incidentTypeRepository.findChildrenOf(incidentTypeId);
    if (children.length > 0) {
      throw new Error('Cannot delete a parent incident type when it has child incident types. Please delete all child incident types first, then delete the parent.');
    }

    await this.incidentTypeRepository.delete(incidentTypeId);
    logger.info(`Incident type ${incidentType.code} deleted`);
  }

  /**
   * Validate no circular reference in hierarchy
   * Prevents A -> B -> C -> A loops
   */
  private async validateNoCircularReference(
    typeId: string,
    proposedParentId: string
  ): Promise<void> {
    const visited = new Set<string>();
    let currentId: string | null = proposedParentId;

    while (currentId) {
      if (visited.has(currentId)) {
        throw new Error('Circular reference detected in incident type hierarchy');
      }

      if (currentId === typeId) {
        throw new Error('Cannot set parent as one of its own descendants');
      }

      visited.add(currentId);

      const current = await this.incidentTypeRepository.findById(currentId);
      if (!current) break;

      currentId = current.parentTypeId;
    }
  }

  /**
   * Convert domain entity to DTO
   */
  private toDto(incidentType: IncidentType): IncidentTypeDto {
    return {
      incidentTypeId: incidentType.incidentTypeId,
      code: incidentType.code,
      typeName: incidentType.typeName,
      description: incidentType.description,
      defaultSeverity: incidentType.defaultSeverity,
      categoryCode: incidentType.categoryCode,
      categoryName: incidentType.getCategoryDisplayName(),
      parentTypeId: incidentType.parentTypeId,
      requiresExternalEntities: incidentType.requiresExternalEntities,
      estimatedResolutionTimeHours: incidentType.estimatedResolutionTimeHours,
      isActive: incidentType.isActive,
      isRootType: incidentType.isRootType(),
      createdAt: incidentType.createdAt.toISOString(),
      updatedAt: incidentType.updatedAt.toISOString(),
    };
  }
}
