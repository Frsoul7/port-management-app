/**
 * Application constants
 */

// Processing time calculation constants
export const CONTAINER_HANDLING_TIME_HOURS = 0.033; // 2 minutes per container
export const SETUP_TIME_HOURS = 0.5; // 30 minutes setup time

// API versioning
export const API_VERSION = 'v1';
export const API_PREFIX = '/api';

// HTTP status codes
export const HTTP_STATUS = {
  OK: 200,
  CREATED: 201,
  NO_CONTENT: 204,
  BAD_REQUEST: 400,
  UNAUTHORIZED: 401,
  FORBIDDEN: 403,
  NOT_FOUND: 404,
  CONFLICT: 409,
  INTERNAL_SERVER_ERROR: 500,
  SERVICE_UNAVAILABLE: 503,
} as const;

// Default pagination
export const DEFAULT_PAGE = 1;
export const DEFAULT_LIMIT = 20;
export const MAX_LIMIT = 100;

// Date format
export const DATE_FORMAT = 'YYYY-MM-DD';
export const DATETIME_FORMAT = 'YYYY-MM-DD HH:mm:ss';

// Timeout durations (milliseconds)
export const CORE_BACKEND_TIMEOUT = 10000; // 10 seconds
export const PLANNING_SERVICE_TIMEOUT = 30000; // 30 seconds

// Error messages
export const ERROR_MESSAGES = {
  INVALID_TOKEN: 'Invalid or expired token',
  UNAUTHORIZED: 'Authentication required',
  FORBIDDEN: 'Insufficient permissions',
  NOT_FOUND: 'Resource not found',
  VALIDATION_ERROR: 'Validation failed',
  DATABASE_ERROR: 'Database operation failed',
  EXTERNAL_SERVICE_ERROR: 'External service unavailable',
} as const;
