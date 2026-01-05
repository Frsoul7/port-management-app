import mongoose, { Schema, Document } from 'mongoose';
import { IncidentStatus, IncidentSeverity } from '@shared/types';

/**
 * Mongoose schema for Incident notes (embedded)
 */
const IncidentNoteSchema = new Schema(
  {
    timestamp: { type: Date, required: true },
    author: { type: String, required: true },
    content: { type: String, required: true, maxlength: 500 },
  },
  { _id: false }
);

/**
 * Mongoose document interface for Incident
 */
export interface IIncidentDocument extends Document {
  incidentId: string;
  incidentTypeId: string;
  vveId?: string;
  title: string;
  description: string;
  severity: IncidentSeverity;
  status: IncidentStatus;
  reportedAt: Date;
  reportedBy: string;
  investigatedAt?: Date;
  investigatedBy?: string;
  resolvedAt?: Date;
  resolvedBy?: string;
  closedAt?: Date;
  closedBy?: string;
  externalEntitiesInvolved: string[];
  notes: Array<{
    timestamp: Date;
    author: string;
    content: string;
  }>;
  resolutionSummary?: string;
  impactDescription?: string;
}

/**
 * Mongoose schema for Incident aggregate
 */
const IncidentSchema = new Schema<IIncidentDocument>(
  {
    incidentId: {
      type: String,
      required: true,
      unique: true,
      index: true,
    },
    incidentTypeId: {
      type: String,
      required: true,
      index: true,
    },
    vveId: {
      type: String,
      index: true,
    },
    title: {
      type: String,
      required: true,
      trim: true,
      maxlength: 200,
    },
    description: {
      type: String,
      required: true,
      trim: true,
    },
    severity: {
      type: String,
      enum: Object.values(IncidentSeverity),
      required: true,
      index: true,
    },
    status: {
      type: String,
      enum: Object.values(IncidentStatus),
      required: true,
      default: IncidentStatus.REPORTED,
      index: true,
    },
    reportedAt: {
      type: Date,
      required: true,
      default: Date.now,
      index: true,
    },
    reportedBy: {
      type: String,
      required: true,
      index: true,
    },
    investigatedAt: {
      type: Date,
    },
    investigatedBy: {
      type: String,
    },
    resolvedAt: {
      type: Date,
      index: true,
    },
    resolvedBy: {
      type: String,
    },
    closedAt: {
      type: Date,
    },
    closedBy: {
      type: String,
    },
    externalEntitiesInvolved: {
      type: [String],
      default: [],
      index: true,
    },
    notes: {
      type: [IncidentNoteSchema],
      default: [],
    },
    resolutionSummary: {
      type: String,
      maxlength: 1000,
    },
    impactDescription: {
      type: String,
      maxlength: 1000,
    },
  },
  {
    timestamps: false,
    collection: 'incidents',
  }
);

// Indexes for common queries
IncidentSchema.index({ status: 1, severity: 1 });
IncidentSchema.index({ incidentTypeId: 1, status: 1 });
IncidentSchema.index({ vveId: 1, status: 1 });
IncidentSchema.index({ reportedAt: -1, status: 1 });
IncidentSchema.index({ externalEntitiesInvolved: 1 }, { sparse: true });

// Index for open incidents (REPORTED or UNDER_INVESTIGATION)
IncidentSchema.index(
  { status: 1, reportedAt: -1 },
  {
    partialFilterExpression: {
      status: {
        $in: [IncidentStatus.REPORTED, IncidentStatus.UNDER_INVESTIGATION],
      },
    },
  }
);

/**
 * Mongoose model for Incident
 */
export const IncidentModel = mongoose.model<IIncidentDocument>('Incident', IncidentSchema);
