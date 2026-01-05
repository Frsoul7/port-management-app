import mongoose, { Schema, Document } from 'mongoose';
import { IncidentSeverity } from '@shared/types';

/**
 * Mongoose document interface for IncidentType
 */
export interface IIncidentTypeDocument extends Document {
  incidentTypeId: string;
  code: string;
  typeName: string;
  description: string;
  defaultSeverity: IncidentSeverity;
  categoryCode: string;
  parentTypeId: string | null;
  requiresExternalEntities: boolean;
  estimatedResolutionTimeHours: number;
  isActive: boolean;
  createdAt: Date;
  updatedAt: Date;
}

/**
 * Mongoose schema for IncidentType aggregate
 */
const IncidentTypeSchema = new Schema<IIncidentTypeDocument>(
  {
    incidentTypeId: {
      type: String,
      required: true,
      unique: true,
      index: true,
    },
    code: {
      type: String,
      required: true,
      unique: true,
      trim: true,
      match: /^T-INC\d{3}$/,
      index: true,
    },
    typeName: {
      type: String,
      required: true,
      unique: true,
      trim: true,
      maxlength: 100,
      index: true,
    },
    description: {
      type: String,
      required: true,
      trim: true,
      maxlength: 500,
    },
    defaultSeverity: {
      type: String,
      enum: Object.values(IncidentSeverity),
      required: true,
      index: true,
    },
    categoryCode: {
      type: String,
      required: true,
      enum: ['ENV', 'OPS', 'SAF'],
      index: true,
    },
    parentTypeId: {
      type: String,
      default: null,
      index: true,
    },
    requiresExternalEntities: {
      type: Boolean,
      default: false,
      index: true,
    },
    estimatedResolutionTimeHours: {
      type: Number,
      required: true,
      min: 0,
      max: 720, // 30 days
    },
    isActive: {
      type: Boolean,
      default: true,
      index: true,
    },
    createdAt: {
      type: Date,
      required: true,
      default: Date.now,
    },
    updatedAt: {
      type: Date,
      required: true,
      default: Date.now,
    },
  },
  {
    timestamps: false, // We manage timestamps manually
    collection: 'incidenttypes',
  }
);

// Indexes for common queries
IncidentTypeSchema.index({ isActive: 1, typeName: 1 });
IncidentTypeSchema.index({ defaultSeverity: 1, isActive: 1 });

/**
 * Mongoose model for IncidentType
 */
export const IncidentTypeModel = mongoose.model<IIncidentTypeDocument>(
  'IncidentType',
  IncidentTypeSchema
);
