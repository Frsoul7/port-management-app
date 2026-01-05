import { Request, Response, NextFunction } from 'express';
import { logger } from '@shared/utils/logger';
import { HTTP_STATUS } from '@shared/constants';

/**
 * Global error handling middleware
 */

export interface AppError extends Error {
  statusCode?: number;
  isOperational?: boolean;
}

export function errorMiddleware(
  err: AppError,
  req: Request,
  res: Response,
  _next: NextFunction
): void {
  const statusCode = err.statusCode || HTTP_STATUS.INTERNAL_SERVER_ERROR;
  const message = err.message || 'Internal server error';

  // Log error
  logger.error('Request error', {
    path: req.path,
    method: req.method,
    statusCode,
    message,
    stack: err.stack,
    user: req.user?.userId,
  });

  // Send error response
  res.status(statusCode).json({
    success: false,
    error: message,
    ...(process.env.NODE_ENV === 'development' && { stack: err.stack }),
  });
}

/**
 * 404 Not Found middleware
 */
export function notFoundMiddleware(req: Request, res: Response): void {
  logger.warn('Route not found', {
    path: req.path,
    method: req.method,
  });

  res.status(HTTP_STATUS.NOT_FOUND).json({
    success: false,
    error: `Route ${req.method} ${req.path} not found`,
  });
}

/**
 * Create operational error
 */
export function createError(message: string, statusCode: number = 500): AppError {
  const error: AppError = new Error(message);
  error.statusCode = statusCode;
  error.isOperational = true;
  return error;
}
