import { ComplementaryTaskStatus } from '@shared/types';
import { generateUUID } from '@shared/utils';

/**
 * ComplementaryTask Aggregate Root
 * Represents additional tasks that need to be performed during vessel operations
 * Examples: Safety inspection, equipment maintenance, documentation updates, cleaning
 *
 * Lifecycle:
 * 1. PLANNED → Task created and assigned
 * 2. IN_PROGRESS → Work has started
 * 3. COMPLETED → Task finished successfully
 * 4. CANCELLED → Task was cancelled (no longer needed)
 *
 * Business Rules:
 * - Can only start PLANNED tasks
 * - Can only complete IN_PROGRESS tasks
 * - Cannot modify COMPLETED or CANCELLED tasks
 * - Due date should be in the future when created
 * - Actual duration is calculated when task is completed
 */
export class ComplementaryTask {
  readonly taskId: string;
  readonly taskCategoryId: string; // Reference to TaskCategory
  readonly vveId?: string; // Optional: Link to specific VVE
  private _title: string;
  private _description: string;
  private _status: ComplementaryTaskStatus;
  private _assignedTo?: string; // User ID or team name
  private _dueDate?: Date;
  private _createdAt: Date;
  private _createdBy: string;
  private _startedAt?: Date;
  private _completedAt?: Date;
  private _completedBy?: string;
  private _cancelledAt?: Date;
  private _cancelledBy?: string;
  private _cancellationReason?: string;
  private _estimatedDurationHours?: number;
  private _notes: { timestamp: Date; author: string; content: string }[];

  constructor(props: {
    taskId?: string;
    taskCategoryId: string;
    vveId?: string;
    title: string;
    description: string;
    status?: ComplementaryTaskStatus;
    assignedTo?: string;
    dueDate?: Date;
    createdAt?: Date;
    createdBy: string;
    startedAt?: Date;
    completedAt?: Date;
    completedBy?: string;
    cancelledAt?: Date;
    cancelledBy?: string;
    cancellationReason?: string;
    estimatedDurationHours?: number;
    notes?: { timestamp: Date; author: string; content: string }[];
  }) {
    // Validation
    if (!props.taskCategoryId || props.taskCategoryId.trim().length === 0) {
      throw new Error('Task category ID is required');
    }

    if (!props.title || props.title.trim().length === 0) {
      throw new Error('Task title is required');
    }

    if (props.title.trim().length > 200) {
      throw new Error('Task title must not exceed 200 characters');
    }

    if (!props.description || props.description.trim().length === 0) {
      throw new Error('Task description is required');
    }

    if (props.description.trim().length > 1000) {
      throw new Error('Description must not exceed 1000 characters');
    }

    if (!props.createdBy || props.createdBy.trim().length === 0) {
      throw new Error('Creator information is required');
    }

    if (props.estimatedDurationHours !== undefined && props.estimatedDurationHours <= 0) {
      throw new Error('Estimated duration must be positive');
    }

    if (props.estimatedDurationHours !== undefined && props.estimatedDurationHours > 168) {
      // 1 week max
      throw new Error('Estimated duration cannot exceed 168 hours (1 week)');
    }

    // Business rule: Due date should be in the future (when creating new tasks)
    if (props.dueDate && !props.taskId) {
      const now = new Date();
      if (props.dueDate <= now) {
        throw new Error('Due date must be in the future');
      }
    }

    // Assign properties
    this.taskId = props.taskId || generateUUID();
    this.taskCategoryId = props.taskCategoryId;
    this.vveId = props.vveId;
    this._title = props.title.trim();
    this._description = props.description.trim();
    this._status = props.status || ComplementaryTaskStatus.PLANNED;
    this._assignedTo = props.assignedTo?.trim();
    this._dueDate = props.dueDate;
    this._createdAt = props.createdAt || new Date();
    this._createdBy = props.createdBy.trim();
    this._startedAt = props.startedAt;
    this._completedAt = props.completedAt;
    this._completedBy = props.completedBy;
    this._cancelledAt = props.cancelledAt;
    this._cancelledBy = props.cancelledBy;
    this._cancellationReason = props.cancellationReason;
    this._estimatedDurationHours = props.estimatedDurationHours;
    this._notes = props.notes ? [...props.notes] : [];
  }

  // Getters
  get title(): string {
    return this._title;
  }

  get description(): string {
    return this._description;
  }

  get status(): ComplementaryTaskStatus {
    return this._status;
  }

  get assignedTo(): string | undefined {
    return this._assignedTo;
  }

  get dueDate(): Date | undefined {
    return this._dueDate;
  }

  get createdAt(): Date {
    return this._createdAt;
  }

  get createdBy(): string {
    return this._createdBy;
  }

  get startedAt(): Date | undefined {
    return this._startedAt;
  }

  get completedAt(): Date | undefined {
    return this._completedAt;
  }

  get completedBy(): string | undefined {
    return this._completedBy;
  }

  get cancelledAt(): Date | undefined {
    return this._cancelledAt;
  }

  get cancelledBy(): string | undefined {
    return this._cancelledBy;
  }

  get cancellationReason(): string | undefined {
    return this._cancellationReason;
  }

  get estimatedDurationHours(): number | undefined {
    return this._estimatedDurationHours;
  }

  get notes(): readonly { timestamp: Date; author: string; content: string }[] {
    return [...this._notes];
  }

  /**
   * Start task
   * Business rule: Can only start PLANNED tasks
   */
  start(): void {
    if (this._status !== ComplementaryTaskStatus.PLANNED) {
      throw new Error(`Cannot start task with status ${this._status}`);
    }

    this._status = ComplementaryTaskStatus.IN_PROGRESS;
    this._startedAt = new Date();
  }

  /**
   * Complete task
   * Business rule: Can only complete IN_PROGRESS tasks
   */
  complete(completedBy: string): void {
    if (this._status !== ComplementaryTaskStatus.IN_PROGRESS) {
      throw new Error(`Cannot complete task with status ${this._status}`);
    }

    if (!completedBy || completedBy.trim().length === 0) {
      throw new Error('Completer information is required');
    }

    this._status = ComplementaryTaskStatus.COMPLETED;
    this._completedAt = new Date();
    this._completedBy = completedBy.trim();
  }

  /**
   * Cancel task
   * Business rule: Can only cancel PLANNED or IN_PROGRESS tasks
   */
  cancel(cancelledBy: string, reason: string): void {
    if (this._status === ComplementaryTaskStatus.COMPLETED) {
      throw new Error('Cannot cancel completed tasks');
    }

    if (this._status === ComplementaryTaskStatus.CANCELLED) {
      throw new Error('Task is already cancelled');
    }

    if (!cancelledBy || cancelledBy.trim().length === 0) {
      throw new Error('Canceller information is required');
    }

    if (!reason || reason.trim().length === 0) {
      throw new Error('Cancellation reason is required');
    }

    if (reason.trim().length > 500) {
      throw new Error('Cancellation reason must not exceed 500 characters');
    }

    this._status = ComplementaryTaskStatus.CANCELLED;
    this._cancelledAt = new Date();
    this._cancelledBy = cancelledBy.trim();
    this._cancellationReason = reason.trim();
  }

  /**
   * Assign task to user or team
   * Business rule: Can only assign PLANNED or IN_PROGRESS tasks
   */
  assignTo(assignee: string): void {
    if (
      this._status === ComplementaryTaskStatus.COMPLETED ||
      this._status === ComplementaryTaskStatus.CANCELLED
    ) {
      throw new Error(`Cannot assign task with status ${this._status}`);
    }

    if (!assignee || assignee.trim().length === 0) {
      throw new Error('Assignee information is required');
    }

    this._assignedTo = assignee.trim();
  }

  /**
   * Unassign task
   */
  unassign(): void {
    if (
      this._status === ComplementaryTaskStatus.COMPLETED ||
      this._status === ComplementaryTaskStatus.CANCELLED
    ) {
      throw new Error(`Cannot unassign task with status ${this._status}`);
    }

    this._assignedTo = undefined;
  }

  /**
   * Update due date
   * Business rule: Cannot update due date for completed/cancelled tasks
   */
  updateDueDate(newDueDate: Date): void {
    if (
      this._status === ComplementaryTaskStatus.COMPLETED ||
      this._status === ComplementaryTaskStatus.CANCELLED
    ) {
      throw new Error(`Cannot update due date for task with status ${this._status}`);
    }

    const now = new Date();
    if (newDueDate <= now) {
      throw new Error('Due date must be in the future');
    }

    this._dueDate = newDueDate;
  }

  /**
   * Update estimated duration
   * Business rule: Cannot update for completed/cancelled tasks
   */
  updateEstimatedDuration(hours: number): void {
    if (
      this._status === ComplementaryTaskStatus.COMPLETED ||
      this._status === ComplementaryTaskStatus.CANCELLED
    ) {
      throw new Error(`Cannot update estimated duration for task with status ${this._status}`);
    }

    if (hours <= 0) {
      throw new Error('Estimated duration must be positive');
    }

    if (hours > 168) {
      throw new Error('Estimated duration cannot exceed 168 hours (1 week)');
    }

    this._estimatedDurationHours = hours;
  }

  /**
   * Add note to task
   * Business rule: Can add notes to any task except cancelled ones
   */
  addNote(author: string, content: string): void {
    if (this._status === ComplementaryTaskStatus.CANCELLED) {
      throw new Error('Cannot add notes to cancelled tasks');
    }

    if (!author || author.trim().length === 0) {
      throw new Error('Note author is required');
    }

    if (!content || content.trim().length === 0) {
      throw new Error('Note content is required');
    }

    if (content.trim().length > 500) {
      throw new Error('Note content must not exceed 500 characters');
    }

    this._notes.push({
      timestamp: new Date(),
      author: author.trim(),
      content: content.trim(),
    });
  }

  /**
   * Update task title and description
   * Business rule: Can only update PLANNED tasks
   */
  update(props: { title?: string; description?: string }): void {
    if (this._status !== ComplementaryTaskStatus.PLANNED) {
      throw new Error(`Cannot update task with status ${this._status}`);
    }

    if (props.title !== undefined) {
      const trimmedTitle = props.title.trim();
      if (trimmedTitle.length === 0) {
        throw new Error('Title cannot be empty');
      }
      if (trimmedTitle.length > 200) {
        throw new Error('Title must not exceed 200 characters');
      }
      this._title = trimmedTitle;
    }

    if (props.description !== undefined) {
      const trimmedDesc = props.description.trim();
      if (trimmedDesc.length === 0) {
        throw new Error('Description cannot be empty');
      }
      if (trimmedDesc.length > 1000) {
        throw new Error('Description must not exceed 1000 characters');
      }
      this._description = trimmedDesc;
    }
  }

  /**
   * Check if task is overdue
   */
  isOverdue(): boolean {
    if (
      !this._dueDate ||
      this._status === ComplementaryTaskStatus.COMPLETED ||
      this._status === ComplementaryTaskStatus.CANCELLED
    ) {
      return false;
    }

    return new Date() > this._dueDate;
  }

  /**
   * Check if task is completed
   */
  isCompleted(): boolean {
    return this._status === ComplementaryTaskStatus.COMPLETED;
  }

  /**
   * Check if task is in progress
   */
  isInProgress(): boolean {
    return this._status === ComplementaryTaskStatus.IN_PROGRESS;
  }

  /**
   * Check if task is cancelled
   */
  isCancelled(): boolean {
    return this._status === ComplementaryTaskStatus.CANCELLED;
  }

  /**
   * Get actual duration in hours
   * Returns null if task is not completed
   */
  getActualDurationHours(): number | null {
    if (!this._completedAt || !this._startedAt) {
      return null;
    }

    const diffMs = this._completedAt.getTime() - this._startedAt.getTime();
    return diffMs / (1000 * 60 * 60);
  }

  /**
   * Compare actual vs estimated duration
   * Returns null if task is not completed or no estimate
   * Positive = took longer than estimated, Negative = finished early
   */
  getDurationVarianceHours(): number | null {
    const actual = this.getActualDurationHours();
    if (actual === null || this._estimatedDurationHours === undefined) {
      return null;
    }

    return actual - this._estimatedDurationHours;
  }

  /**
   * Convert to plain object for serialization
   */
  toJSON() {
    return {
      taskId: this.taskId,
      taskCategoryId: this.taskCategoryId,
      vveId: this.vveId,
      title: this._title,
      description: this._description,
      status: this._status,
      assignedTo: this._assignedTo,
      dueDate: this._dueDate?.toISOString(),
      createdAt: this._createdAt.toISOString(),
      createdBy: this._createdBy,
      startedAt: this._startedAt?.toISOString(),
      completedAt: this._completedAt?.toISOString(),
      completedBy: this._completedBy,
      cancelledAt: this._cancelledAt?.toISOString(),
      cancelledBy: this._cancelledBy,
      cancellationReason: this._cancellationReason,
      estimatedDurationHours: this._estimatedDurationHours,
      notes: this._notes.map((note) => ({
        timestamp: note.timestamp.toISOString(),
        author: note.author,
        content: note.content,
      })),
    };
  }
}
