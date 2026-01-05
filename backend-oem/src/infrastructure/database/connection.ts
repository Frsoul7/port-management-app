import mongoose from 'mongoose';
import { logger } from '@shared/utils/logger';
import { seedVveFromOperationPlanIfNeeded, seedTaskCategoriesIfNeeded } from './seeders';

/**
 * MongoDB connection manager
 */

const DATABASE_URL = process.env.DATABASE_URL || 'mongodb://localhost:27017/oem';

export async function connectDatabase(): Promise<void> {
  try {
    await mongoose.connect(DATABASE_URL);
    logger.info('MongoDB connected successfully', {
      host: mongoose.connection.host,
      database: mongoose.connection.name,
    });

    // Seed dev data if in development mode
    if (process.env.NODE_ENV !== 'production') {
      await seedTaskCategoriesIfNeeded(); // US 4.1.14: Seed task categories first
      await seedVveFromOperationPlanIfNeeded();
    }

    // Handle connection events
    mongoose.connection.on('error', (err) => {
      logger.error('MongoDB connection error:', err);
    });

    mongoose.connection.on('disconnected', () => {
      logger.warn('MongoDB disconnected');
    });

    // Graceful shutdown
    process.on('SIGINT', async () => {
      await mongoose.connection.close();
      logger.info('MongoDB connection closed through app termination');
      process.exit(0);
    });
  } catch (error) {
    logger.error('Failed to connect to MongoDB:', error);
    process.exit(1);
  }
}

export async function disconnectDatabase(): Promise<void> {
  await mongoose.connection.close();
  logger.info('MongoDB connection closed');
}
