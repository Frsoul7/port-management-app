import { IncidentSeverity } from '@shared/types';
import { generateUUID } from '@shared/utils';

/**
 * IncidentType Aggregate Root
 * Defines categories of incidents that can occur during vessel operations
 * Examples: Crane Breakdown, Weather Delay, Hazardous Material Spill, Equipment Failure
 *
 * Business Rules:
 * - Type name must be unique and non-empty
 * - Estimated resolution time must be positive
 * - Only active types can be used for new incidents
 * - Cannot update deactivated types (must reactivate first)
 */
export class IncidentType {
  readonly incidentTypeId: string;
  private _code: string; // Unique code (e.g., "T-INC001")
  private _typeName: string;
  private _description: string;
  private _defaultSeverity: IncidentSeverity;
  private _categoryCode: string; // Category: "ENV", "OPS", "SAF"
  private _parentTypeId: string | null; // Optional parent type ID for hierarchy
  private _requiresExternalEntities: boolean; // e.g., Coast Guard, Fire Department
  private _estimatedResolutionTimeHours: number;
  private _isActive: boolean;
  private _createdAt: Date;
  private _updatedAt: Date;

  constructor(props: {
    incidentTypeId?: string;
    code: string;
    typeName: string;
    description: string;
    defaultSeverity: IncidentSeverity;
    categoryCode: string;
    parentTypeId?: string | null;
    requiresExternalEntities?: boolean;
    estimatedResolutionTimeHours: number;
    isActive?: boolean;
    createdAt?: Date;
    updatedAt?: Date;
  }) {
    // Validation
    if (!props.code || props.code.trim().length === 0) {
      throw new Error('Incident type code is required');
    }

    if (!/^T-INC\d{3}$/.test(props.code.trim())) {
      throw new Error('Incident type code must follow format: T-INC### (e.g., T-INC001)');
    }

    if (!props.typeName || props.typeName.trim().length === 0) {
      throw new Error('Incident type name is required');
    }

    if (props.typeName.trim().length > 100) {
      throw new Error('Incident type name must not exceed 100 characters');
    }

    if (!props.description || props.description.trim().length === 0) {
      throw new Error('Incident type description is required');
    }

    if (props.description.trim().length > 500) {
      throw new Error('Description must not exceed 500 characters');
    }

    if (!props.categoryCode || props.categoryCode.trim().length === 0) {
      throw new Error('Category code is required');
    }

    const validCategories = ['ENV', 'OPS', 'SAF'];
    if (!validCategories.includes(props.categoryCode.trim().toUpperCase())) {
      throw new Error('Category code must be one of: ENV, OPS, SAF');
    }

    if (props.estimatedResolutionTimeHours <= 0) {
      throw new Error('Estimated resolution time must be positive');
    }

    if (props.estimatedResolutionTimeHours > 720) {
      // 30 days max
      throw new Error('Estimated resolution time cannot exceed 720 hours (30 days)');
    }

    // Assign properties
    this.incidentTypeId = props.incidentTypeId || generateUUID();
    this._code = props.code.trim();
    this._typeName = props.typeName.trim();
    this._description = props.description.trim();
    this._defaultSeverity = props.defaultSeverity;
    this._categoryCode = props.categoryCode.trim().toUpperCase();
    this._parentTypeId = props.parentTypeId || null;
    this._requiresExternalEntities = props.requiresExternalEntities ?? false;
    this._estimatedResolutionTimeHours = props.estimatedResolutionTimeHours;
    this._isActive = props.isActive ?? true;
    this._createdAt = props.createdAt || new Date();
    this._updatedAt = props.updatedAt || new Date();
  }

  // Getters
  get code(): string {
    return this._code;
  }

  get typeName(): string {
    return this._typeName;
  }

  get description(): string {
    return this._description;
  }

  get defaultSeverity(): IncidentSeverity {
    return this._defaultSeverity;
  }

  get categoryCode(): string {
    return this._categoryCode;
  }

  get parentTypeId(): string | null {
    return this._parentTypeId;
  }

  get requiresExternalEntities(): boolean {
    return this._requiresExternalEntities;
  }

  get estimatedResolutionTimeHours(): number {
    return this._estimatedResolutionTimeHours;
  }

  get isActive(): boolean {
    return this._isActive;
  }

  get createdAt(): Date {
    return this._createdAt;
  }

  get updatedAt(): Date {
    return this._updatedAt;
  }

  /**
   * Update incident type details
   * Business rule: Can only update active types
   */
  update(props: {
    typeName?: string;
    description?: string;
    defaultSeverity?: IncidentSeverity;
    categoryCode?: string;
    parentTypeId?: string | null;
    requiresExternalEntities?: boolean;
    estimatedResolutionTimeHours?: number;
  }): void {
    if (!this._isActive) {
      throw new Error('Cannot update deactivated incident type. Reactivate it first.');
    }

    // Validate new values
    if (props.typeName !== undefined) {
      const trimmedName = props.typeName.trim();
      if (trimmedName.length === 0) {
        throw new Error('Incident type name cannot be empty');
      }
      if (trimmedName.length > 100) {
        throw new Error('Incident type name must not exceed 100 characters');
      }
      this._typeName = trimmedName;
    }

    if (props.description !== undefined) {
      const trimmedDesc = props.description.trim();
      if (trimmedDesc.length === 0) {
        throw new Error('Description cannot be empty');
      }
      if (trimmedDesc.length > 500) {
        throw new Error('Description must not exceed 500 characters');
      }
      this._description = trimmedDesc;
    }

    if (props.defaultSeverity !== undefined) {
      this._defaultSeverity = props.defaultSeverity;
    }

    if (props.categoryCode !== undefined) {
      const validCategories = ['ENV', 'OPS', 'SAF'];
      const trimmedCategory = props.categoryCode.trim().toUpperCase();
      if (!validCategories.includes(trimmedCategory)) {
        throw new Error('Category code must be one of: ENV, OPS, SAF');
      }
      this._categoryCode = trimmedCategory;
    }

    if (props.parentTypeId !== undefined) {
      this._parentTypeId = props.parentTypeId;
    }

    if (props.requiresExternalEntities !== undefined) {
      this._requiresExternalEntities = props.requiresExternalEntities;
    }

    if (props.estimatedResolutionTimeHours !== undefined) {
      if (props.estimatedResolutionTimeHours <= 0) {
        throw new Error('Estimated resolution time must be positive');
      }
      if (props.estimatedResolutionTimeHours > 720) {
        throw new Error('Estimated resolution time cannot exceed 720 hours (30 days)');
      }
      this._estimatedResolutionTimeHours = props.estimatedResolutionTimeHours;
    }

    this._updatedAt = new Date();
  }

  /**
   * Deactivate incident type
   * Business rule: Deactivated types cannot be used for new incidents
   */
  deactivate(): void {
    if (!this._isActive) {
      throw new Error('Incident type is already deactivated');
    }

    this._isActive = false;
    this._updatedAt = new Date();
  }

  /**
   * Reactivate incident type
   */
  reactivate(): void {
    if (this._isActive) {
      throw new Error('Incident type is already active');
    }

    this._isActive = true;
    this._updatedAt = new Date();
  }

  /**
   * Check if this incident type is suitable for a given severity level
   * Returns true if the actual severity matches or exceeds the default severity
   */
  isSuitableForSeverity(actualSeverity: IncidentSeverity): boolean {
    const severityOrder = {
      [IncidentSeverity.LOW]: 1,
      [IncidentSeverity.MEDIUM]: 2,
      [IncidentSeverity.HIGH]: 3,
      [IncidentSeverity.CRITICAL]: 4,
    };

    return severityOrder[actualSeverity] >= severityOrder[this._defaultSeverity];
  }

  /**
   * Check if external entities are required for incidents of this type
   */
  needsExternalSupport(): boolean {
    return this._requiresExternalEntities;
  }

  /**
   * Get estimated resolution time in minutes
   */
  getEstimatedResolutionTimeMinutes(): number {
    return this._estimatedResolutionTimeHours * 60;
  }

  /**
   * Check if this type is considered high-priority
   * High priority = HIGH or CRITICAL severity
   */
  isHighPriority(): boolean {
    return (
      this._defaultSeverity === IncidentSeverity.HIGH ||
      this._defaultSeverity === IncidentSeverity.CRITICAL
    );
  }

  /**
   * Check if this is a root type (no parent)
   */
  isRootType(): boolean {
    return this._parentTypeId === null;
  }

  /**
   * Check if this type has a parent
   */
  hasParent(): boolean {
    return this._parentTypeId !== null;
  }

  /**
   * Get category display name
   */
  getCategoryDisplayName(): string {
    const categoryNames: Record<string, string> = {
      ENV: 'Environmental',
      OPS: 'Operational',
      SAF: 'Safety',
    };
    return categoryNames[this._categoryCode] || this._categoryCode;
  }

  /**
   * Convert to plain object for serialization
   */
  toJSON() {
    return {
      incidentTypeId: this.incidentTypeId,
      code: this._code,
      typeName: this._typeName,
      description: this._description,
      defaultSeverity: this._defaultSeverity,
      categoryCode: this._categoryCode,
      parentTypeId: this._parentTypeId,
      requiresExternalEntities: this._requiresExternalEntities,
      estimatedResolutionTimeHours: this._estimatedResolutionTimeHours,
      isActive: this._isActive,
      createdAt: this._createdAt.toISOString(),
      updatedAt: this._updatedAt.toISOString(),
    };
  }
}
