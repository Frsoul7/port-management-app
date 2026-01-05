import { generateUUID } from '@shared/utils';

/**
 * TaskCategory Aggregate Root
 * US 4.1.14: Defines categories of complementary tasks that can be assigned during vessel operations
 * Examples: Inspection, Maintenance, Cleaning, Documentation, Safety Check
 *
 * Business Rules:
 * - Category code and name must be unique and non-empty
 * - Code follows pattern CTC001, CTC002, etc.
 * - Only active categories can be used for new tasks
 * - Cannot update deactivated categories (must reactivate first)
 * - Description should provide clear context about task category purpose
 * - Default duration (if provided) must be positive
 */
export class TaskCategory {
  readonly taskCategoryId: string;
  readonly categoryCode: string; // US 4.1.14: Unique code (e.g., CTC001)
  private _categoryName: string;
  private _description: string;
  private _defaultDurationHours?: number; // US 4.1.14: Optional default duration
  private _expectedImpact?: string; // US 4.1.14: Optional impact description
  private _isActive: boolean;
  private _createdAt: Date;
  private _updatedAt: Date;

  constructor(props: {
    taskCategoryId?: string;
    categoryCode: string; // US 4.1.14: Required unique code
    categoryName: string;
    description: string;
    defaultDurationHours?: number; // US 4.1.14
    expectedImpact?: string; // US 4.1.14
    isActive?: boolean;
    createdAt?: Date;
    updatedAt?: Date;
  }) {
    // Validation - Code
    if (!props.categoryCode || props.categoryCode.trim().length === 0) {
      throw new Error('Task category code is required');
    }

    if (!/^CTC\d{3}$/.test(props.categoryCode.trim())) {
      throw new Error('Category code must follow pattern CTC001, CTC002, etc.');
    }

    // Validation - Name
    if (!props.categoryName || props.categoryName.trim().length === 0) {
      throw new Error('Task category name is required');
    }

    if (props.categoryName.trim().length > 100) {
      throw new Error('Category name must not exceed 100 characters');
    }

    // Validation - Description
    if (!props.description || props.description.trim().length === 0) {
      throw new Error('Task category description is required');
    }

    if (props.description.trim().length > 500) {
      throw new Error('Description must not exceed 500 characters');
    }

    // Validation - US 4.1.14: Default Duration
    if (props.defaultDurationHours !== undefined && props.defaultDurationHours <= 0) {
      throw new Error('Default duration must be greater than 0');
    }

    // Validation - US 4.1.14: Expected Impact
    if (props.expectedImpact && props.expectedImpact.trim().length > 200) {
      throw new Error('Expected impact must not exceed 200 characters');
    }

    // Assign properties
    this.taskCategoryId = props.taskCategoryId || generateUUID();
    this.categoryCode = props.categoryCode.trim();
    this._categoryName = props.categoryName.trim();
    this._description = props.description.trim();
    this._defaultDurationHours = props.defaultDurationHours;
    this._expectedImpact = props.expectedImpact?.trim();
    this._isActive = props.isActive ?? true;
    this._createdAt = props.createdAt || new Date();
    this._updatedAt = props.updatedAt || new Date();
  }

  // Getters
  get categoryName(): string {
    return this._categoryName;
  }

  get description(): string {
    return this._description;
  }

  get defaultDurationHours(): number | undefined {
    return this._defaultDurationHours;
  }

  get expectedImpact(): string | undefined {
    return this._expectedImpact;
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
   * Update task category details
   * US 4.1.14: Enhanced to support new optional fields
   * Business rule: Can only update active categories
   */
  update(props: {
    categoryName?: string;
    description?: string;
    defaultDurationHours?: number;
    expectedImpact?: string;
  }): void {
    if (!this._isActive) {
      throw new Error('Cannot update deactivated task category. Reactivate it first.');
    }

    // Validate new values
    if (props.categoryName !== undefined) {
      const trimmedName = props.categoryName.trim();
      if (trimmedName.length === 0) {
        throw new Error('Category name cannot be empty');
      }
      if (trimmedName.length > 100) {
        throw new Error('Category name must not exceed 100 characters');
      }
      this._categoryName = trimmedName;
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

    // US 4.1.14: Update default duration
    if (props.defaultDurationHours !== undefined) {
      if (props.defaultDurationHours <= 0) {
        throw new Error('Default duration must be greater than 0');
      }
      this._defaultDurationHours = props.defaultDurationHours;
    }

    // US 4.1.14: Update expected impact
    if (props.expectedImpact !== undefined) {
      const trimmedImpact = props.expectedImpact.trim();
      if (trimmedImpact.length > 200) {
        throw new Error('Expected impact must not exceed 200 characters');
      }
      this._expectedImpact = trimmedImpact.length > 0 ? trimmedImpact : undefined;
    }

    this._updatedAt = new Date();
  }

  /**
   * Deactivate task category
   * Business rule: Deactivated categories cannot be used for new tasks
   */
  deactivate(): void {
    if (!this._isActive) {
      throw new Error('Task category is already deactivated');
    }

    this._isActive = false;
    this._updatedAt = new Date();
  }

  /**
   * Reactivate task category
   */
  reactivate(): void {
    if (this._isActive) {
      throw new Error('Task category is already active');
    }

    this._isActive = true;
    this._updatedAt = new Date();
  }

  /**
   * Convert to plain object for serialization
   * US 4.1.14: Includes new optional fields
   */
  toJSON() {
    return {
      taskCategoryId: this.taskCategoryId,
      categoryCode: this.categoryCode, // US 4.1.14
      categoryName: this._categoryName,
      description: this._description,
      defaultDurationHours: this._defaultDurationHours, // US 4.1.14
      expectedImpact: this._expectedImpact, // US 4.1.14
      isActive: this._isActive,
      createdAt: this._createdAt.toISOString(),
      updatedAt: this._updatedAt.toISOString(),
    };
  }
}
