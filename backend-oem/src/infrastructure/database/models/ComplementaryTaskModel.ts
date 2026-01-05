import mongoose, { Schema, Document } from 'mongoose';
import { ComplementaryTaskStatus } from '@shared/types';

/**
 * Mongoose schema for Task notes (embedded)
 */
const TaskNoteSchema = new Schema(
  {
    timestamp: { type: Date, required: true },
    author: { type: String, required: true },
    content: { type: String, required: true, maxlength: 500 },
  },
  { _id: false }
);

/**
 * Mongoose document interface for ComplementaryTask
 */
export interface IComplementaryTaskDocument extends Document {
  taskId: string;
  taskCategoryId: string;
  vveId?: string;
  title: string;
  description: string;
  status: ComplementaryTaskStatus;
  assignedTo?: string;
  dueDate?: Date;
  createdAt: Date;
  createdBy: string;
  startedAt?: Date;
  completedAt?: Date;
  completedBy?: string;
  cancelledAt?: Date;
  cancelledBy?: string;
  cancellationReason?: string;
  estimatedDurationHours?: number;
  notes: Array<{
    timestamp: Date;
    author: string;
    content: string;
  }>;
}

/**
 * Mongoose schema for ComplementaryTask aggregate
 */
const ComplementaryTaskSchema = new Schema<IComplementaryTaskDocument>(
  {
    taskId: {
      type: String,
      required: true,
      unique: true,
      index: true,
    },
    taskCategoryId: {
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
      maxlength: 1000,
    },
    status: {
      type: String,
      enum: Object.values(ComplementaryTaskStatus),
      required: true,
      default: ComplementaryTaskStatus.PLANNED,
      index: true,
    },
    assignedTo: {
      type: String,
      index: true,
    },
    dueDate: {
      type: Date,
      index: true,
    },
    createdAt: {
      type: Date,
      required: true,
      default: Date.now,
      index: true,
    },
    createdBy: {
      type: String,
      required: true,
      index: true,
    },
    startedAt: {
      type: Date,
    },
    completedAt: {
      type: Date,
      index: true,
    },
    completedBy: {
      type: String,
    },
    cancelledAt: {
      type: Date,
    },
    cancelledBy: {
      type: String,
    },
    cancellationReason: {
      type: String,
      maxlength: 500,
    },
    estimatedDurationHours: {
      type: Number,
      min: 0,
      max: 168, // 1 week
    },
    notes: {
      type: [TaskNoteSchema],
      default: [],
    },
  },
  {
    timestamps: false,
    collection: 'complementarytasks',
  }
);

// Indexes for common queries
ComplementaryTaskSchema.index({ status: 1, dueDate: 1 });
ComplementaryTaskSchema.index({ taskCategoryId: 1, status: 1 });
ComplementaryTaskSchema.index({ vveId: 1, status: 1 });
ComplementaryTaskSchema.index({ assignedTo: 1, status: 1 });
ComplementaryTaskSchema.index({ createdBy: 1, createdAt: -1 });

// Index for overdue tasks
ComplementaryTaskSchema.index(
  { dueDate: 1, status: 1 },
  {
    partialFilterExpression: {
      status: {
        $in: [ComplementaryTaskStatus.PLANNED, ComplementaryTaskStatus.IN_PROGRESS],
      },
      dueDate: { $exists: true },
    },
  }
);

// Index for unassigned tasks
ComplementaryTaskSchema.index(
  { status: 1, createdAt: -1 },
  {
    partialFilterExpression: {
      assignedTo: { $exists: false },
      status: { $in: [ComplementaryTaskStatus.PLANNED, ComplementaryTaskStatus.IN_PROGRESS] },
    },
  }
);

/**
 * Mongoose model for ComplementaryTask
 */
export const ComplementaryTaskModel = mongoose.model<IComplementaryTaskDocument>(
  'ComplementaryTask',
  ComplementaryTaskSchema
);
