import mongoose, { Schema, Document } from 'mongoose';

/**
 * Mongoose document interface for TaskCategory
 * US 4.1.14: Enhanced with code, default duration, and expected impact
 */
export interface ITaskCategoryDocument extends Document {
  taskCategoryId: string;
  categoryCode: string; // US 4.1.14: Unique code (e.g., CTC001)
  categoryName: string;
  description: string;
  defaultDurationHours?: number; // US 4.1.14: Optional default duration
  expectedImpact?: string; // US 4.1.14: Optional impact description
  isActive: boolean;
  createdAt: Date;
  updatedAt: Date;
}

/**
 * Mongoose schema for TaskCategory aggregate
 * US 4.1.14: Enhanced with new fields
 */
const TaskCategorySchema = new Schema<ITaskCategoryDocument>(
  {
    taskCategoryId: {
      type: String,
      required: true,
      unique: true,
      index: true,
    },
    categoryCode: {
      // US 4.1.14: Unique code field
      type: String,
      required: true,
      unique: true,
      trim: true,
      match: /^CTC\d{3}$/, // Pattern: CTC001, CTC002, etc.
      index: true,
    },
    categoryName: {
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
    defaultDurationHours: {
      // US 4.1.14: Optional default duration
      type: Number,
      required: false,
      min: 0,
    },
    expectedImpact: {
      // US 4.1.14: Optional expected impact
      type: String,
      required: false,
      trim: true,
      maxlength: 200,
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
    collection: 'taskcategories',
  }
);

// Indexes for common queries
TaskCategorySchema.index({ isActive: 1, categoryName: 1 });

/**
 * Mongoose model for TaskCategory
 */
export const TaskCategoryModel = mongoose.model<ITaskCategoryDocument>(
  'TaskCategory',
  TaskCategorySchema
);
