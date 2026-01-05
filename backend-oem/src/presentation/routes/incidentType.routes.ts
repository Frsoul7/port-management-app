import { Router } from 'express';
import { Container } from '@infrastructure/container';
import { body, query, param, validationResult } from 'express-validator';
import { Request, Response, NextFunction } from 'express';
import { authMiddleware } from '@infrastructure/middleware/auth.middleware';

/**
 * Incident Type Routes
 * US 4.1.12: Manage hierarchical incident type catalog
 * 
 * Authentication: All routes require valid JWT token from Core Backend
 * Authorization: RBAC enforced based on user roles
 */

const router = Router();

// Apply authentication to ALL routes
router.use(authMiddleware);

// Get controller from DI container
const controller = Container.getIncidentTypeController();

// Validation middleware
const handleValidationErrors = (req: Request, res: Response, next: NextFunction) => {
  const errors = validationResult(req);
  if (!errors.isEmpty()) {
    res.status(400).json({
      success: false,
      message: 'Validation failed',
      errors: errors.array(),
    });
    return;
  }
  next();
};

/**
 * US 4.1.12: Create incident type
 * POST /api/v1/incident-types
 */
router.post(
  '/',
  [
    body('code')
      .isString()
      .matches(/^T-INC\d{3}$/)
      .withMessage('Code must follow format T-INC### (e.g., T-INC001)'),
    body('typeName').isString().trim().notEmpty().withMessage('Type name is required'),
    body('description').isString().trim().notEmpty().withMessage('Description is required'),
    body('defaultSeverity')
      .isString()
      .isIn(['LOW', 'MEDIUM', 'HIGH', 'CRITICAL'])
      .withMessage('Invalid severity level'),
    body('categoryCode')
      .isString()
      .isIn(['ENV', 'OPS', 'SAF'])
      .withMessage('Category code must be ENV, OPS, or SAF'),
    body('parentTypeId').optional().isString().withMessage('Parent type ID must be a string'),
    body('requiresExternalEntities')
      .optional()
      .isBoolean()
      .withMessage('Requires external entities must be boolean'),
    body('estimatedResolutionTimeHours')
      .isInt({ min: 1, max: 720 })
      .withMessage('Estimated resolution time must be between 1 and 720 hours'),
    handleValidationErrors,
  ],
  controller.createIncidentType.bind(controller)
);

/**
 * US 4.1.12: Get root incident types
 * GET /api/v1/incident-types/root
 * Must come before /:id route to avoid conflict
 */
router.get('/root', controller.getRootIncidentTypes.bind(controller));

/**
 * US 4.1.12: Get incident type by code
 * GET /api/v1/incident-types/code/:code
 */
router.get(
  '/code/:code',
  [param('code').isString().withMessage('Code must be a string'), handleValidationErrors],
  controller.getIncidentTypeByCode.bind(controller)
);

/**
 * US 4.1.12: List all incident types
 * GET /api/v1/incident-types
 * Query params: activeOnly, category, sortBy, sortOrder
 */
router.get(
  '/',
  [
    query('activeOnly').optional().isBoolean().withMessage('Active only must be boolean'),
    query('category')
      .optional()
      .isIn(['ENV', 'OPS', 'SAF'])
      .withMessage('Category must be ENV, OPS, or SAF'),
    query('sortBy').optional().isString().withMessage('Sort by must be a string'),
    query('sortOrder')
      .optional()
      .isIn(['asc', 'desc'])
      .withMessage('Sort order must be asc or desc'),
    handleValidationErrors,
  ],
  controller.listIncidentTypes.bind(controller)
);

/**
 * US 4.1.12: Get incident type by ID
 * GET /api/v1/incident-types/:id
 */
router.get(
  '/:id',
  [param('id').isString().withMessage('ID must be a string'), handleValidationErrors],
  controller.getIncidentTypeById.bind(controller)
);

/**
 * US 4.1.12: Get children of incident type
 * GET /api/v1/incident-types/:id/children
 */
router.get(
  '/:id/children',
  [param('id').isString().withMessage('ID must be a string'), handleValidationErrors],
  controller.getChildIncidentTypes.bind(controller)
);

/**
 * US 4.1.12: Update incident type
 * PUT /api/v1/incident-types/:id
 */
router.put(
  '/:id',
  [
    param('id').isString().withMessage('ID must be a string'),
    body('typeName')
      .optional()
      .isString()
      .trim()
      .notEmpty()
      .withMessage('Type name must be non-empty'),
    body('description')
      .optional()
      .isString()
      .trim()
      .notEmpty()
      .withMessage('Description must be non-empty'),
    body('defaultSeverity')
      .optional()
      .isIn(['LOW', 'MEDIUM', 'HIGH', 'CRITICAL'])
      .withMessage('Invalid severity level'),
    body('categoryCode')
      .optional()
      .isIn(['ENV', 'OPS', 'SAF'])
      .withMessage('Category code must be ENV, OPS, or SAF'),
    body('parentTypeId').optional().isString().withMessage('Parent type ID must be a string'),
    body('requiresExternalEntities')
      .optional()
      .isBoolean()
      .withMessage('Requires external entities must be boolean'),
    body('estimatedResolutionTimeHours')
      .optional()
      .isInt({ min: 1, max: 720 })
      .withMessage('Estimated resolution time must be between 1 and 720 hours'),
    handleValidationErrors,
  ],
  controller.updateIncidentType.bind(controller)
);

/**
 * US 4.1.12: Deactivate incident type
 * POST /api/v1/incident-types/:id/deactivate
 */
router.post(
  '/:id/deactivate',
  [param('id').isString().withMessage('ID must be a string'), handleValidationErrors],
  controller.deactivateIncidentType.bind(controller)
);

/**
 * US 4.1.12: Reactivate incident type
 * POST /api/v1/incident-types/:id/reactivate
 */
router.post(
  '/:id/reactivate',
  [param('id').isString().withMessage('ID must be a string'), handleValidationErrors],
  controller.reactivateIncidentType.bind(controller)
);

/**
 * US 4.1.12: Delete incident type
 * DELETE /api/v1/incident-types/:id
 */
router.delete(
  '/:id',
  [param('id').isString().withMessage('ID must be a string'), handleValidationErrors],
  controller.deleteIncidentType.bind(controller)
);

export default router;
