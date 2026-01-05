import { Request, Response, NextFunction } from 'express';
import jwt from 'jsonwebtoken';
import { JwtPayload } from '@shared/types';
import { logger } from '@shared/utils/logger';
import { HTTP_STATUS, ERROR_MESSAGES } from '@shared/constants';

/**
 * JWT Authentication Middleware
 * Validates JWT token from Authorization header
 * Token must be issued by Core Backend with same secret
 */

// Access process.env directly (not as module-level constant) to ensure dotenv is loaded first
function getJwtSecret(): string {
  return process.env.JWT_SECRET || 'your-secret-key-here';
}

function getJwtIssuer(): string {
  return process.env.JWT_ISSUER || 'PortManagementSystem';
}

// Extend Express Request to include user
declare global {
  namespace Express {
    interface Request {
      user?: JwtPayload;
    }
  }
}

export function authMiddleware(req: Request, res: Response, next: NextFunction): void {
  try {
    const authHeader = req.headers.authorization;

    // Check if Authorization header exists
    if (!authHeader || !authHeader.startsWith('Bearer ')) {
      logger.warn('Missing or invalid Authorization header', {
        path: req.path,
        method: req.method,
        authHeader: authHeader ? 'present but invalid format' : 'missing',
      });
      res.status(HTTP_STATUS.UNAUTHORIZED).json({
        success: false,
        error: ERROR_MESSAGES.UNAUTHORIZED,
      });
      return;
    }

    // Extract token (remove "Bearer " prefix)
    const token = authHeader.substring(7);
    const jwtSecret = getJwtSecret();

    logger.debug('Attempting to verify JWT token', {
      tokenLength: token.length,
      secretLength: jwtSecret.length,
      path: req.path,
    });

    // Verify token (no issuer validation to match Core Backend behavior)
    const decoded = jwt.verify(token, jwtSecret) as JwtPayload;

    // Attach user to request
    req.user = decoded;

    logger.debug('User authenticated', {
      userId: decoded.sub,
      email: decoded.email,
      role: decoded.role,
    });

    next();
  } catch (error) {
    if (error instanceof jwt.TokenExpiredError) {
      logger.warn('Expired token', { 
        path: req.path,
        expiredAt: error.expiredAt 
      });
      res.status(HTTP_STATUS.UNAUTHORIZED).json({
        success: false,
        error: 'Token expired',
      });
      return;
    }

    if (error instanceof jwt.JsonWebTokenError) {
      logger.warn('Invalid token', { 
        path: req.path,
        errorMessage: error.message,
        errorName: error.name
      });
      res.status(HTTP_STATUS.UNAUTHORIZED).json({
        success: false,
        error: ERROR_MESSAGES.INVALID_TOKEN,
      });
      return;
    }

    logger.error('Authentication error', error);
    res.status(HTTP_STATUS.INTERNAL_SERVER_ERROR).json({
      success: false,
      error: 'Authentication failed',
    });
  }
}

/**
 * Optional authentication middleware
 * Attaches user if token is valid, but doesn't fail if missing
 */
export function optionalAuthMiddleware(req: Request, _res: Response, next: NextFunction): void {
  const authHeader = req.headers.authorization;

  if (!authHeader || !authHeader.startsWith('Bearer ')) {
    return next();
  }

  try {
    const token = authHeader.substring(7);
    const decoded = jwt.verify(token, JWT_SECRET, {
      issuer: JWT_ISSUER,
    }) as JwtPayload;

    req.user = decoded;
    next();
  } catch (error) {
    // Silently fail and continue without user
    next();
  }
}

/**
 * Role-based authorization middleware
 * @param allowedRoles Array of roles that are allowed
 */
export function requireRole(...allowedRoles: string[]) {
  return (req: Request, res: Response, next: NextFunction): void => {
    if (!req.user) {
      res.status(HTTP_STATUS.UNAUTHORIZED).json({
        success: false,
        error: ERROR_MESSAGES.UNAUTHORIZED,
      });
      return;
    }

    if (!allowedRoles.includes(req.user.role)) {
      logger.warn('Insufficient permissions', {
        userId: req.user.userId,
        role: req.user.role,
        requiredRoles: allowedRoles,
      });
      res.status(HTTP_STATUS.FORBIDDEN).json({
        success: false,
        error: ERROR_MESSAGES.FORBIDDEN,
      });
      return;
    }

    next();
  };
}
