import mongoose, { Schema, Document } from 'mongoose';
import { VesselVisitExecutionStatus, OperationType, ExecutedOperationStatus } from '@shared/types';

/**
 * Mongoose schema for ExecutedOperation (embedded in VVE)
 */
const ExecutedOperationSchema = new Schema(
  {
    operationType: {
      type: String,
      enum: Object.values(OperationType),
      required: true,
    },
    startTime: { type: Date, required: true },
    endTime: { type: Date },
    containersProcessed: { type: Number, required: true, min: 0 },
    cranesUsed: { type: Number, required: true, min: 1 },
    staffAssigned: [{ type: String }],
    status: {
      type: String,
      enum: Object.values(ExecutedOperationStatus),
      required: true,
    },
    plannedOperationId: { type: String }, // US 4.1.9: Link to planned operation
  },
  { _id: false }
);

/**
 * Mongoose document interface for VesselVisitExecution
 */
export interface IVesselVisitExecutionDocument extends Document {
  vveId: string;
  vvnId: string;
  operationPlanId?: string;
  actualPortArrivalTime?: Date;
  actualBerthTime?: Date;
  actualUnberthTime?: Date;
  actualPortDepartureTime?: Date;
  assignedDock?: string;
  operations: Array<{
    operationType: OperationType;
    startTime: Date;
    endTime?: Date;
    containersProcessed: number;
    cranesUsed: number;
    staffAssigned: string[];
    status: ExecutedOperationStatus;
    plannedOperationId?: string; // US 4.1.9
  }>;
  status: VesselVisitExecutionStatus;
  completedAt?: Date;
  completedBy?: string;
  incidents: string[];
}

/**
 * Mongoose schema for VesselVisitExecution aggregate
 */
const VesselVisitExecutionSchema = new Schema<IVesselVisitExecutionDocument>(
  {
    vveId: {
      type: String,
      required: true,
      unique: true,
      index: true,
    },
    vvnId: {
      type: String,
      required: true,
      unique: true,
      index: true,
    },
    operationPlanId: {
      type: String,
      index: true,
    },
    actualPortArrivalTime: {
      type: Date,
      index: true,
    },
    actualBerthTime: {
      type: Date,
    },
    actualUnberthTime: {
      type: Date,
    },
    actualPortDepartureTime: {
      type: Date,
    },
    assignedDock: {
      type: String,
      index: true,
    },
    operations: {
      type: [ExecutedOperationSchema],
      default: [],
    },
    status: {
      type: String,
      enum: Object.values(VesselVisitExecutionStatus),
      required: true,
      default: VesselVisitExecutionStatus.PLANNED,
      index: true,
    },
    completedAt: {
      type: Date,
    },
    completedBy: {
      type: String,
    },
    incidents: {
      type: [String],
      default: [],
      index: true,
    },
  },
  {
    timestamps: false,
    collection: 'vesselvisitexecutions',
  }
);

// Indexes for common queries
VesselVisitExecutionSchema.index({ status: 1, actualPortArrivalTime: -1 });
VesselVisitExecutionSchema.index({ assignedDock: 1, status: 1 });
VesselVisitExecutionSchema.index({ completedAt: -1 });
VesselVisitExecutionSchema.index({ incidents: 1 }, { sparse: true });

/**
 * Mongoose model for VesselVisitExecution
 */
export const VesselVisitExecutionModel = mongoose.model<IVesselVisitExecutionDocument>(
  'VesselVisitExecution',
  VesselVisitExecutionSchema
);
