import mongoose, { Schema, Document } from 'mongoose';
import { OperationPlanStatus, OperationType, PlanningAlgorithm } from '@shared/types';

/**
 * Mongoose schema for PlannedOperation (embedded in OperationPlan)
 */
const PlannedOperationSchema = new Schema(
  {
    vvnId: { type: String, required: true },
    vesselImo: { type: String, required: true },
    plannedStart: { type: Date, required: true },
    plannedEnd: { type: Date, required: true },
    assignedCranes: { type: Number, required: true, min: 1 },
    assignedDock: { type: String },
    assignedStaff: [{ type: String }],
    operationType: {
      type: String,
      enum: Object.values(OperationType),
      required: true,
    },
  },
  { _id: false }
); // No separate _id for embedded documents

/**
 * Mongoose document interface for OperationPlan
 */
export interface IOperationPlanDocument extends Document {
  operationPlanId: string;
  targetDate: Date;
  algorithm: PlanningAlgorithm;
  createdBy: string;
  createdAt: Date;
  status: OperationPlanStatus;
  totalDelay: number;
  operations: Array<{
    vvnId: string;
    vesselImo: string;
    plannedStart: Date;
    plannedEnd: Date;
    assignedCranes: number;
    assignedDock?: string;
    assignedStaff?: string[];
    operationType: OperationType;
  }>;
}

/**
 * Mongoose schema for OperationPlan aggregate
 */
const OperationPlanSchema = new Schema<IOperationPlanDocument>(
  {
    operationPlanId: {
      type: String,
      required: true,
      unique: true,
      index: true,
    },
    targetDate: {
      type: Date,
      required: true,
      index: true,
    },
    algorithm: {
      type: String,
      enum: Object.values(PlanningAlgorithm),
      required: true,
    },
    createdBy: {
      type: String,
      required: true,
    },
    createdAt: {
      type: Date,
      required: true,
      default: Date.now,
      index: true,
    },
    status: {
      type: String,
      enum: Object.values(OperationPlanStatus),
      required: true,
      default: OperationPlanStatus.GENERATED,
      index: true,
    },
    totalDelay: {
      type: Number,
      required: true,
      min: 0,
    },
    operations: {
      type: [PlannedOperationSchema],
      required: true,
      validate: {
        validator: (v: unknown[]) => Array.isArray(v) && v.length > 0,
        message: 'Operation plan must have at least one operation',
      },
    },
  },
  {
    timestamps: false, // We manage createdAt manually
    collection: 'operationplans',
  }
);

// Indexes for common queries
OperationPlanSchema.index({ targetDate: 1, status: 1 });
OperationPlanSchema.index({ 'operations.vvnId': 1 });
OperationPlanSchema.index({ createdBy: 1, createdAt: -1 });

// Ensure only one APPROVED or IN_EXECUTION plan per date
OperationPlanSchema.index(
  { targetDate: 1, status: 1 },
  {
    unique: true,
    partialFilterExpression: {
      status: {
        $in: [OperationPlanStatus.APPROVED, OperationPlanStatus.IN_EXECUTION],
      },
    },
  }
);

/**
 * Mongoose model for OperationPlan
 */
export const OperationPlanModel = mongoose.model<IOperationPlanDocument>(
  'OperationPlan',
  OperationPlanSchema
);
