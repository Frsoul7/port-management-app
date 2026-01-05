import { IncidentStatus, IncidentSeverity } from '@shared/types';
import { generateUUID } from '@shared/utils';

/**
 * Incident Aggregate Root
 * Represents operational incidents that affect vessel visit executions
 * Examples: Equipment failure, weather delays, safety issues, cargo damage
 *
 * Lifecycle:
 * 1. REPORTED → Initial incident report
 * 2. UNDER_INVESTIGATION → Investigation in progress
 * 3. RESOLVED → Root cause identified and fixed
 * 4. CLOSED → Incident closed with lessons learned
 *
 * Business Rules:
 * - Can only move to UNDER_INVESTIGATION from REPORTED
 * - Can only move to RESOLVED from UNDER_INVESTIGATION
 * - Can only move to CLOSED from RESOLVED
 * - Cannot reopen CLOSED incidents (create new incident instead)
 * - Severity can be different from IncidentType default severity
 * - External entities can only be involved if IncidentType requires them
 */
export class Incident {
  readonly incidentId: string;
  readonly incidentTypeId: string; // Reference to IncidentType
  readonly vveId?: string; // Optional: Link to affected VVE
  private _title: string;
  private _description: string;
  private _severity: IncidentSeverity;
  private _status: IncidentStatus;
  private _reportedAt: Date;
  private _reportedBy: string; // User ID or name
  private _investigatedAt?: Date;
  private _investigatedBy?: string;
  private _resolvedAt?: Date;
  private _resolvedBy?: string;
  private _closedAt?: Date;
  private _closedBy?: string;
  private _externalEntitiesInvolved: string[]; // e.g., ["Coast Guard", "Fire Department"]
  private _notes: { timestamp: Date; author: string; content: string }[];
  private _resolutionSummary?: string;
  private _impactDescription?: string; // How this affected operations

  constructor(props: {
    incidentId?: string;
    incidentTypeId: string;
    vveId?: string;
    title: string;
    description: string;
    severity: IncidentSeverity;
    status?: IncidentStatus;
    reportedAt?: Date;
    reportedBy: string;
    investigatedAt?: Date;
    investigatedBy?: string;
    resolvedAt?: Date;
    resolvedBy?: string;
    closedAt?: Date;
    closedBy?: string;
    externalEntitiesInvolved?: string[];
    notes?: { timestamp: Date; author: string; content: string }[];
    resolutionSummary?: string;
    impactDescription?: string;
  }) {
    // Validation
    if (!props.incidentTypeId || props.incidentTypeId.trim().length === 0) {
      throw new Error('Incident type ID is required');
    }

    if (!props.title || props.title.trim().length === 0) {
      throw new Error('Incident title is required');
    }

    if (props.title.trim().length > 200) {
      throw new Error('Incident title must not exceed 200 characters');
    }

    if (!props.description || props.description.trim().length === 0) {
      throw new Error('Incident description is required');
    }

    if (!props.reportedBy || props.reportedBy.trim().length === 0) {
      throw new Error('Reporter information is required');
    }

    // Business rule: Validate status transitions
    if (props.status && props.status !== IncidentStatus.REPORTED) {
      // If creating with non-REPORTED status, ensure all required fields are present
      if (props.status === IncidentStatus.UNDER_INVESTIGATION && !props.investigatedAt) {
        throw new Error('Investigation timestamp required for UNDER_INVESTIGATION status');
      }
      if (
        props.status === IncidentStatus.RESOLVED &&
        (!props.resolvedAt || !props.resolutionSummary)
      ) {
        throw new Error('Resolution timestamp and summary required for RESOLVED status');
      }
      if (props.status === IncidentStatus.CLOSED && !props.closedAt) {
        throw new Error('Close timestamp required for CLOSED status');
      }
    }

    // Assign properties
    this.incidentId = props.incidentId || generateUUID();
    this.incidentTypeId = props.incidentTypeId;
    this.vveId = props.vveId;
    this._title = props.title.trim();
    this._description = props.description.trim();
    this._severity = props.severity;
    this._status = props.status || IncidentStatus.REPORTED;
    this._reportedAt = props.reportedAt || new Date();
    this._reportedBy = props.reportedBy.trim();
    this._investigatedAt = props.investigatedAt;
    this._investigatedBy = props.investigatedBy;
    this._resolvedAt = props.resolvedAt;
    this._resolvedBy = props.resolvedBy;
    this._closedAt = props.closedAt;
    this._closedBy = props.closedBy;
    this._externalEntitiesInvolved = props.externalEntitiesInvolved
      ? [...props.externalEntitiesInvolved]
      : [];
    this._notes = props.notes ? [...props.notes] : [];
    this._resolutionSummary = props.resolutionSummary;
    this._impactDescription = props.impactDescription;
  }

  // Getters
  get title(): string {
    return this._title;
  }

  get description(): string {
    return this._description;
  }

  get severity(): IncidentSeverity {
    return this._severity;
  }

  get status(): IncidentStatus {
    return this._status;
  }

  get reportedAt(): Date {
    return this._reportedAt;
  }

  get reportedBy(): string {
    return this._reportedBy;
  }

  get investigatedAt(): Date | undefined {
    return this._investigatedAt;
  }

  get investigatedBy(): string | undefined {
    return this._investigatedBy;
  }

  get resolvedAt(): Date | undefined {
    return this._resolvedAt;
  }

  get resolvedBy(): string | undefined {
    return this._resolvedBy;
  }

  get closedAt(): Date | undefined {
    return this._closedAt;
  }

  get closedBy(): string | undefined {
    return this._closedBy;
  }

  get externalEntitiesInvolved(): readonly string[] {
    return [...this._externalEntitiesInvolved];
  }

  get notes(): readonly { timestamp: Date; author: string; content: string }[] {
    return [...this._notes];
  }

  get resolutionSummary(): string | undefined {
    return this._resolutionSummary;
  }

  get impactDescription(): string | undefined {
    return this._impactDescription;
  }

  /**
   * Start investigation
   * Business rule: Can only investigate REPORTED incidents
   */
  startInvestigation(investigatedBy: string): void {
    if (this._status !== IncidentStatus.REPORTED) {
      throw new Error(`Cannot start investigation for incident with status ${this._status}`);
    }

    if (!investigatedBy || investigatedBy.trim().length === 0) {
      throw new Error('Investigator information is required');
    }

    this._status = IncidentStatus.UNDER_INVESTIGATION;
    this._investigatedAt = new Date();
    this._investigatedBy = investigatedBy.trim();
  }

  /**
   * Resolve incident
   * Business rule: Can only resolve incidents UNDER_INVESTIGATION
   */
  resolve(resolvedBy: string, resolutionSummary: string): void {
    if (this._status !== IncidentStatus.UNDER_INVESTIGATION) {
      throw new Error(`Cannot resolve incident with status ${this._status}`);
    }

    if (!resolvedBy || resolvedBy.trim().length === 0) {
      throw new Error('Resolver information is required');
    }

    if (!resolutionSummary || resolutionSummary.trim().length === 0) {
      throw new Error('Resolution summary is required');
    }

    if (resolutionSummary.trim().length > 1000) {
      throw new Error('Resolution summary must not exceed 1000 characters');
    }

    this._status = IncidentStatus.RESOLVED;
    this._resolvedAt = new Date();
    this._resolvedBy = resolvedBy.trim();
    this._resolutionSummary = resolutionSummary.trim();
  }

  /**
   * Close incident
   * Business rule: Can only close RESOLVED incidents
   */
  close(closedBy: string): void {
    if (this._status !== IncidentStatus.RESOLVED) {
      throw new Error(`Cannot close incident with status ${this._status}`);
    }

    if (!closedBy || closedBy.trim().length === 0) {
      throw new Error('Closer information is required');
    }

    this._status = IncidentStatus.CLOSED;
    this._closedAt = new Date();
    this._closedBy = closedBy.trim();
  }

  /**
   * Add investigation note
   * Business rule: Cannot add notes to CLOSED incidents
   */
  addNote(author: string, content: string): void {
    if (this._status === IncidentStatus.CLOSED) {
      throw new Error('Cannot add notes to closed incidents');
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
   * Update incident severity
   * Business rule: Can only update severity before resolution
   */
  updateSeverity(newSeverity: IncidentSeverity): void {
    if (this._status === IncidentStatus.RESOLVED || this._status === IncidentStatus.CLOSED) {
      throw new Error('Cannot update severity of resolved or closed incidents');
    }

    this._severity = newSeverity;
  }

  /**
   * Add external entity involvement
   * e.g., Coast Guard, Fire Department, Police
   */
  involveExternalEntity(entityName: string): void {
    if (this._status === IncidentStatus.CLOSED) {
      throw new Error('Cannot modify closed incidents');
    }

    if (!entityName || entityName.trim().length === 0) {
      throw new Error('External entity name is required');
    }

    const trimmedName = entityName.trim();
    if (this._externalEntitiesInvolved.includes(trimmedName)) {
      throw new Error(`External entity ${trimmedName} is already involved`);
    }

    this._externalEntitiesInvolved.push(trimmedName);
  }

  /**
   * Remove external entity involvement
   */
  removeExternalEntity(entityName: string): void {
    if (this._status === IncidentStatus.CLOSED) {
      throw new Error('Cannot modify closed incidents');
    }

    const index = this._externalEntitiesInvolved.indexOf(entityName.trim());
    if (index === -1) {
      throw new Error(`External entity ${entityName} is not involved`);
    }

    this._externalEntitiesInvolved.splice(index, 1);
  }

  /**
   * Set impact description
   * Describes how this incident affected vessel operations
   */
  setImpactDescription(description: string): void {
    if (this._status === IncidentStatus.CLOSED) {
      throw new Error('Cannot modify closed incidents');
    }

    if (description && description.trim().length > 1000) {
      throw new Error('Impact description must not exceed 1000 characters');
    }

    this._impactDescription = description ? description.trim() : undefined;
  }

  /**
   * Check if incident is critical
   */
  isCritical(): boolean {
    return this._severity === IncidentSeverity.CRITICAL;
  }

  /**
   * Check if incident is resolved
   */
  isResolved(): boolean {
    return this._status === IncidentStatus.RESOLVED || this._status === IncidentStatus.CLOSED;
  }

  /**
   * Check if incident is closed
   */
  isClosed(): boolean {
    return this._status === IncidentStatus.CLOSED;
  }

  /**
   * Check if incident is still open
   */
  isOpen(): boolean {
    return (
      this._status === IncidentStatus.REPORTED ||
      this._status === IncidentStatus.UNDER_INVESTIGATION
    );
  }

  /**
   * Check if external entities are involved
   */
  hasExternalEntities(): boolean {
    return this._externalEntitiesInvolved.length > 0;
  }

  /**
   * Get time to resolution in hours
   * Returns null if not yet resolved
   */
  getTimeToResolutionHours(): number | null {
    if (!this._resolvedAt) {
      return null;
    }

    const diffMs = this._resolvedAt.getTime() - this._reportedAt.getTime();
    return diffMs / (1000 * 60 * 60);
  }

  /**
   * Convert to plain object for serialization
   */
  toJSON() {
    return {
      incidentId: this.incidentId,
      incidentTypeId: this.incidentTypeId,
      vveId: this.vveId,
      title: this._title,
      description: this._description,
      severity: this._severity,
      status: this._status,
      reportedAt: this._reportedAt.toISOString(),
      reportedBy: this._reportedBy,
      investigatedAt: this._investigatedAt?.toISOString(),
      investigatedBy: this._investigatedBy,
      resolvedAt: this._resolvedAt?.toISOString(),
      resolvedBy: this._resolvedBy,
      closedAt: this._closedAt?.toISOString(),
      closedBy: this._closedBy,
      externalEntitiesInvolved: this._externalEntitiesInvolved,
      notes: this._notes.map((note) => ({
        timestamp: note.timestamp.toISOString(),
        author: note.author,
        content: note.content,
      })),
      resolutionSummary: this._resolutionSummary,
      impactDescription: this._impactDescription,
    };
  }
}
