import { Router } from 'express';
import { Container } from '@infrastructure/container';
import { body, query, param, validationResult } from 'express-validator';
import { Request, Response, NextFunction } from 'express';
import { authMiddleware } from '@infrastructure/middleware/auth.middleware';

/**
 * Incident Routes
 * US 4.1.13: Record and manage operational incidents
 * 
 * Authentication: All routes require valid JWT token from Core Backend
 * Authorization: RBAC enforced based on user roles
 */

const router = Router();

// Apply authentication to ALL routes
router.use(authMiddleware);

// Get controller from DI container
const controller = Container.getIncidentController();

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
 * US 4.1.13: Create incident
 * POST /api/v1/incidents
 */
router.post(
  '/',
  [
    body('incidentTypeId').isString().trim().notEmpty().withMessage('Incident type ID is required'),
    body('vveId').optional().isString().withMessage('VVE ID must be a string'),
    body('title')
      .isString()
      .trim()
      .notEmpty()
      .withMessage('Title is required')
      .isLength({ max: 200 })
      .withMessage('Title must not exceed 200 characters'),
    body('description').isString().trim().notEmpty().withMessage('Description is required'),
    body('severity')
      .isString()
      .isIn(['LOW', 'MEDIUM', 'HIGH', 'CRITICAL'])
      .withMessage('Invalid severity level'),
    body('reportedBy').isString().trim().notEmpty().withMessage('Reporter information is required'),
    body('impactDescription').optional().isString().withMessage('Impact description must be a string'),
    body('externalEntitiesInvolved')
      .optional()
      .isArray()
      .withMessage('External entities must be an array'),
    handleValidationErrors,
  ],
  controller.createIncident.bind(controller)
);

/**
 * US 4.1.13: Get active incidents
 * GET /api/v1/incidents/active
 * Must come before /:id route to avoid conflict
 */
router.get('/active', controller.getActiveIncidents.bind(controller));

/**
 * US 4.1.13: Get critical incidents
 * GET /api/v1/incidents/critical
 */
router.get('/critical', controller.getCriticalIncidents.bind(controller));

/**
 * US 4.1.13: List incidents with filtering
 * GET /api/v1/incidents
 * Query params: status, severity, incidentTypeId, vveId, reportedBy, fromDate, toDate, externalEntity, limit, offset
 */
router.get(
  '/',
  [
    query('status')
      .optional()
      .isIn(['REPORTED', 'UNDER_INVESTIGATION', 'RESOLVED', 'CLOSED'])
      .withMessage('Invalid status'),
    query('severity')
      .optional()
      .isIn(['LOW', 'MEDIUM', 'HIGH', 'CRITICAL'])
      .withMessage('Invalid severity level'),
    query('incidentTypeId').optional().isString().withMessage('Incident type ID must be a string'),
    query('vveId').optional().isString().withMessage('VVE ID must be a string'),
    query('reportedBy').optional().isString().withMessage('Reporter must be a string'),
    query('fromDate').optional().isISO8601().withMessage('From date must be valid ISO 8601 date'),
    query('toDate').optional().isISO8601().withMessage('To date must be valid ISO 8601 date'),
    query('externalEntity').optional().isString().withMessage('External entity must be a string'),
    query('limit')
      .optional()
      .isInt({ min: 1, max: 1000 })
      .withMessage('Limit must be between 1 and 1000'),
    query('offset')
      .optional()
      .isInt({ min: 0 })
      .withMessage('Offset must be a positive integer'),
    handleValidationErrors,
  ],
  controller.listIncidents.bind(controller)
);

/**
 * US 4.1.13: Get incident by ID
 * GET /api/v1/incidents/:id
 */
router.get(
  '/:id',
  [param('id').isString().withMessage('Incident ID must be a string'), handleValidationErrors],
  controller.getIncidentById.bind(controller)
);

/**
 * US 4.1.13: Update incident
 * PUT /api/v1/incidents/:id
 */
router.put(
  '/:id',
  [
    param('id').isString().withMessage('Incident ID must be a string'),
    body('title')
      .optional()
      .isString()
      .trim()
      .notEmpty()
      .isLength({ max: 200 })
      .withMessage('Title must not exceed 200 characters'),
    body('description')
      .optional()
      .isString()
      .trim()
      .notEmpty()
      .withMessage('Description cannot be empty'),
    body('severity')
      .optional()
      .isString()
      .isIn(['LOW', 'MEDIUM', 'HIGH', 'CRITICAL'])
      .withMessage('Invalid severity level'),
    body('impactDescription').optional().isString().withMessage('Impact description must be a string'),
    handleValidationErrors,
  ],
  controller.updateIncident.bind(controller)
);

/**
 * US 4.1.13: Delete incident
 * DELETE /api/v1/incidents/:id
 */
router.delete(
  '/:id',
  [param('id').isString().withMessage('Incident ID must be a string'), handleValidationErrors],
  controller.deleteIncident.bind(controller)
);

/**
 * US 4.1.13: Start investigation
 * PATCH /api/v1/incidents/:id/investigate
 */
router.patch(
  '/:id/investigate',
  [
    param('id').isString().withMessage('Incident ID must be a string'),
    body('investigatedBy')
      .isString()
      .trim()
      .notEmpty()
      .withMessage('Investigator information is required'),
    handleValidationErrors,
  ],
  controller.startInvestigation.bind(controller)
);

/**
 * US 4.1.13: Resolve incident
 * PATCH /api/v1/incidents/:id/resolve
 */
router.patch(
  '/:id/resolve',
  [
    param('id').isString().withMessage('Incident ID must be a string'),
    body('resolvedBy').isString().trim().notEmpty().withMessage('Resolver information is required'),
    body('resolutionSummary')
      .isString()
      .trim()
      .notEmpty()
      .withMessage('Resolution summary is required'),
    handleValidationErrors,
  ],
  controller.resolveIncident.bind(controller)
);

/**
 * US 4.1.13: Close incident
 * PATCH /api/v1/incidents/:id/close
 */
router.patch(
  '/:id/close',
  [
    param('id').isString().withMessage('Incident ID must be a string'),
    body('closedBy').isString().trim().notEmpty().withMessage('Closer information is required'),
    handleValidationErrors,
  ],
  controller.closeIncident.bind(controller)
);

/**
 * US 4.1.13: Add note to incident
 * POST /api/v1/incidents/:id/notes
 */
router.post(
  '/:id/notes',
  [
    param('id').isString().withMessage('Incident ID must be a string'),
    body('author').isString().trim().notEmpty().withMessage('Author is required'),
    body('content').isString().trim().notEmpty().withMessage('Note content is required'),
    handleValidationErrors,
  ],
  controller.addNote.bind(controller)
);

/**
 * US 4.1.13: Involve external entity
 * POST /api/v1/incidents/:id/external-entities
 */
router.post(
  '/:id/external-entities',
  [
    param('id').isString().withMessage('Incident ID must be a string'),
    body('entityName').isString().trim().notEmpty().withMessage('Entity name is required'),
    handleValidationErrors,
  ],
  controller.involveExternalEntity.bind(controller)
);

export default router;
