import { Router } from 'express';
import { Container } from '@infrastructure/container';
import { authMiddleware } from '@infrastructure/middleware/auth.middleware';
import { body, query, param, validationResult } from 'express-validator';
import { Request, Response, NextFunction } from 'express';

/**
 * Operation Plan Routes
 * Covers US 4.1.1-4.1.6
 */

const router = Router();

// Get controller from DI container
const controller = Container.getOperationPlanController();

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
 * @swagger
 * /api/v1/operation-plans/generate:
 *   post:
 *     summary: Generate operation plan
 *     description: Generate an optimized operation plan for a specific date using scheduling algorithms
 *     tags: [Operation Plans]
 *     security:
 *       - BearerAuth: []
 *     requestBody:
 *       required: true
 *       content:
 *         application/json:
 *           schema:
 *             $ref: '#/components/schemas/GenerateOperationPlanRequest'
 *     responses:
 *       201:
 *         description: Operation plan generated successfully
 *         content:
 *           application/json:
 *             schema:
 *               allOf:
 *                 - $ref: '#/components/schemas/ApiSuccessResponse'
 *                 - type: object
 *                   properties:
 *                     data:
 *                       $ref: '#/components/schemas/OperationPlan'
 *       400:
 *         description: Validation error or plan already exists for date
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/ApiErrorResponse'
 *       401:
 *         description: Unauthorized - Invalid or missing JWT token
 *       500:
 *         description: Internal server error
 */
router.post(
  '/generate',
  authMiddleware,
  [
    body('targetDate')
      .notEmpty()
      .withMessage('Target date is required')
      .isISO8601()
      .withMessage('Target date must be a valid ISO 8601 date'),
    body('algorithm')
      .notEmpty()
      .withMessage('Algorithm is required')
      .isIn(['optimal', 'weighted', 'multi_cranes'])
      .withMessage('Algorithm must be one of: optimal, weighted, multi_cranes'),
    body('vvnIds').optional().isArray().withMessage('VVN IDs must be an array'),
    handleValidationErrors,
  ],
  (req: Request, res: Response, next: NextFunction) => controller.generatePlan(req, res, next)
);

/**
 * @swagger
 * /api/v1/operation-plans/missing:
 *   get:
 *     summary: Get VVNs without operation plans
 *     description: Find all approved VVNs that do not have an operation plan for a specific date (US 4.1.5)
 *     tags: [Operation Plans]
 *     security:
 *       - BearerAuth: []
 *     parameters:
 *       - in: query
 *         name: date
 *         required: true
 *         schema:
 *           type: string
 *           format: date
 *         description: Target date to check for missing plans
 *         example: '2025-12-15'
 *     responses:
 *       200:
 *         description: List of VVNs without plans
 *         content:
 *           application/json:
 *             schema:
 *               allOf:
 *                 - $ref: '#/components/schemas/ApiSuccessResponse'
 *                 - type: object
 *                   properties:
 *                     data:
 *                       type: array
 *                       items:
 *                         type: object
 *                         properties:
 *                           vvnId:
 *                             type: string
 *                           vesselImo:
 *                             type: string
 *                           eta:
 *                             type: string
 *                             format: date-time
 *       400:
 *         description: Invalid date format
 *       401:
 *         description: Unauthorized
 */
router.get(
  '/missing',
  authMiddleware,
  [
    query('date')
      .notEmpty()
      .withMessage('Date parameter is required')
      .isISO8601()
      .withMessage('Date must be a valid ISO 8601 date'),
    handleValidationErrors,
  ],
  (req: Request, res: Response, next: NextFunction) => controller.getMissingPlans(req, res, next)
);

/**
 * @swagger
 * /api/v1/operation-plans/resource-allocation:
 *   get:
 *     summary: Query resource allocation time
 *     description: Get time periods when a specific resource (crane, dock, or staff) is allocated in operation plans (US 4.1.6)
 *     tags: [Operation Plans]
 *     security:
 *       - BearerAuth: []
 *     parameters:
 *       - in: query
 *         name: resourceType
 *         required: true
 *         schema:
 *           type: string
 *           enum: [crane, dock, staff]
 *         description: Type of resource
 *       - in: query
 *         name: resourceId
 *         required: true
 *         schema:
 *           type: string
 *         description: Unique identifier of the resource
 *       - in: query
 *         name: fromDate
 *         required: true
 *         schema:
 *           type: string
 *           format: date
 *         description: Start date of query range
 *       - in: query
 *         name: toDate
 *         required: true
 *         schema:
 *           type: string
 *           format: date
 *         description: End date of query range
 *     responses:
 *       200:
 *         description: Resource allocation time slots
 *         content:
 *           application/json:
 *             schema:
 *               allOf:
 *                 - $ref: '#/components/schemas/ApiSuccessResponse'
 *                 - type: object
 *                   properties:
 *                     data:
 *                       type: array
 *                       items:
 *                         type: object
 *                         properties:
 *                           planId:
 *                             type: string
 *                           vvnId:
 *                             type: string
 *                           startTime:
 *                             type: string
 *                             format: date-time
 *                           endTime:
 *                             type: string
 *                             format: date-time
 *       400:
 *         description: Invalid parameters
 *       401:
 *         description: Unauthorized
 */
router.get(
  '/resource-allocation',
  authMiddleware,
  [
    query('resourceType')
      .notEmpty()
      .withMessage('Resource type is required')
      .isIn(['crane', 'dock', 'staff'])
      .withMessage('Resource type must be one of: crane, dock, staff'),
    query('resourceId').notEmpty().withMessage('Resource ID is required'),
    query('fromDate')
      .notEmpty()
      .withMessage('From date is required')
      .isISO8601()
      .withMessage('From date must be a valid ISO 8601 date'),
    query('toDate')
      .notEmpty()
      .withMessage('To date is required')
      .isISO8601()
      .withMessage('To date must be a valid ISO 8601 date'),
    handleValidationErrors,
  ],
  (req: Request, res: Response, next: NextFunction) =>
    controller.getResourceAllocation(req, res, next)
);

/**
 * @swagger
 * /api/v1/operation-plans/by-date/{date}:
 *   get:
 *     summary: Get operation plan by date
 *     description: Retrieve the operation plan for a specific target date
 *     tags: [Operation Plans]
 *     security:
 *       - BearerAuth: []
 *     parameters:
 *       - in: path
 *         name: date
 *         required: true
 *         schema:
 *           type: string
 *           format: date
 *         description: Target date (YYYY-MM-DD)
 *         example: '2025-12-15'
 *     responses:
 *       200:
 *         description: Operation plan for the date
 *         content:
 *           application/json:
 *             schema:
 *               allOf:
 *                 - $ref: '#/components/schemas/ApiSuccessResponse'
 *                 - type: object
 *                   properties:
 *                     data:
 *                       $ref: '#/components/schemas/OperationPlan'
 *       404:
 *         description: No plan found for date
 *       401:
 *         description: Unauthorized
 */
router.get(
  '/by-date/:date',
  authMiddleware,
  [
    param('date').isISO8601().withMessage('Date must be a valid ISO 8601 date'),
    handleValidationErrors,
  ],
  (req: Request, res: Response, next: NextFunction) => controller.getPlanByDate(req, res, next)
);

/**
 * @swagger
 * /api/v1/operation-plans/{id}:
 *   get:
 *     summary: Get operation plan by ID
 *     description: Retrieve a specific operation plan by its MongoDB ID
 *     tags: [Operation Plans]
 *     security:
 *       - BearerAuth: []
 *     parameters:
 *       - in: path
 *         name: id
 *         required: true
 *         schema:
 *           type: string
 *         description: Operation plan MongoDB ID
 *     responses:
 *       200:
 *         description: Operation plan details
 *         content:
 *           application/json:
 *             schema:
 *               allOf:
 *                 - $ref: '#/components/schemas/ApiSuccessResponse'
 *                 - type: object
 *                   properties:
 *                     data:
 *                       $ref: '#/components/schemas/OperationPlan'
 *       404:
 *         description: Operation plan not found
 *       401:
 *         description: Unauthorized
 */
router.get(
  '/:id',
  authMiddleware,
  [param('id').isUUID(4).withMessage('Invalid operation plan ID'), handleValidationErrors],
  (req: Request, res: Response, next: NextFunction) => controller.getPlanById(req, res, next)
);

/**
 * @swagger
 * /api/v1/operation-plans:
 *   get:
 *     summary: List operation plans
 *     description: Retrieve all operation plans with optional filtering, sorting, and pagination (US 4.1.3)
 *     tags: [Operation Plans]
 *     security:
 *       - BearerAuth: []
 *     parameters:
 *       - in: query
 *         name: targetDate
 *         schema:
 *           type: string
 *           format: date
 *         description: Filter by target date
 *       - in: query
 *         name: status
 *         schema:
 *           type: string
 *           enum: [DRAFT, APPROVED, IN_PROGRESS, COMPLETED, CANCELLED]
 *         description: Filter by status
 *       - in: query
 *         name: algorithm
 *         schema:
 *           type: string
 *           enum: [optimal, weighted, multi-cranes, simple]
 *         description: Filter by algorithm
 *       - in: query
 *         name: vvnId
 *         schema:
 *           type: string
 *         description: Filter by vessel visit notification ID
 *       - in: query
 *         name: vesselImo
 *         schema:
 *           type: string
 *         description: Filter by vessel IMO number
 *       - in: query
 *         name: fromDate
 *         schema:
 *           type: string
 *           format: date
 *         description: Start date for date range filter
 *       - in: query
 *         name: toDate
 *         schema:
 *           type: string
 *           format: date
 *         description: End date for date range filter
 *       - in: query
 *         name: page
 *         schema:
 *           type: integer
 *           minimum: 1
 *           default: 1
 *         description: Page number
 *       - in: query
 *         name: limit
 *         schema:
 *           type: integer
 *           minimum: 1
 *           maximum: 100
 *           default: 10
 *         description: Items per page
 *       - in: query
 *         name: sortBy
 *         schema:
 *           type: string
 *         description: Field to sort by (e.g., targetDate, createdAt, status)
 *       - in: query
 *         name: sortOrder
 *         schema:
 *           type: string
 *           enum: [asc, desc]
 *         description: Sort order (ascending or descending)
 *     responses:
 *       200:
 *         description: List of operation plans
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/PaginationResponse'
 *       401:
 *         description: Unauthorized
 *       500:
 *         description: Internal server error
 */
router.get(
  '/',
  authMiddleware,
  [
    query('targetDate')
      .optional()
      .isISO8601()
      .withMessage('Target date must be a valid ISO 8601 date'),
    query('algorithm')
      .optional()
      .isIn(['optimal', 'weighted', 'multi_cranes'])
      .withMessage('Algorithm must be one of: optimal, weighted, multi_cranes'),
    query('status')
      .optional()
      .isIn(['GENERATED', 'APPROVED', 'IN_EXECUTION', 'COMPLETED', 'OUTDATED'])
      .withMessage('Invalid status'),
    query('vvnId').optional().isString().withMessage('VVN ID must be a string'),
    query('vesselImo').optional().isString().withMessage('Vessel IMO must be a string'),
    query('fromDate').optional().isISO8601().withMessage('From date must be a valid ISO 8601 date'),
    query('toDate').optional().isISO8601().withMessage('To date must be a valid ISO 8601 date'),
    query('page').optional().isInt({ min: 1 }).withMessage('Page must be a positive integer'),
    query('limit')
      .optional()
      .isInt({ min: 1, max: 100 })
      .withMessage('Limit must be between 1 and 100'),
    query('sortBy').optional().isString().withMessage('Sort by must be a string'),
    query('sortOrder')
      .optional()
      .isIn(['asc', 'desc'])
      .withMessage('Sort order must be asc or desc'),
    handleValidationErrors,
  ],
  (req: Request, res: Response, next: NextFunction) => controller.listPlans(req, res, next)
);

/**
 * @swagger
 * /api/v1/operation-plans/{id}/dock-assignments:
 *   patch:
 *     summary: Update dock assignment
 *     description: Change the assigned dock for a vessel visit in the operation plan
 *     tags: [Operation Plans]
 *     security:
 *       - BearerAuth: []
 *     parameters:
 *       - in: path
 *         name: id
 *         required: true
 *         schema:
 *           type: string
 *         description: Operation plan ID
 *     requestBody:
 *       required: true
 *       content:
 *         application/json:
 *           schema:
 *             $ref: '#/components/schemas/UpdateDockAssignmentRequest'
 *     responses:
 *       200:
 *         description: Dock assignment updated
 *         content:
 *           application/json:
 *             schema:
 *               allOf:
 *                 - $ref: '#/components/schemas/ApiSuccessResponse'
 *                 - type: object
 *                   properties:
 *                     data:
 *                       $ref: '#/components/schemas/OperationPlan'
 *       404:
 *         description: Operation plan or VVN not found
 *       401:
 *         description: Unauthorized
 */
router.patch(
  '/:id/dock',
  authMiddleware,
  [
    param('id').isUUID(4).withMessage('Invalid operation plan ID'),
    body('vvnId').notEmpty().withMessage('VVN ID is required'),
    body('dockId').notEmpty().withMessage('Dock ID is required'),
    handleValidationErrors,
  ],
  (req: Request, res: Response, next: NextFunction) =>
    controller.updateDockAssignment(req, res, next)
);

/**
 * @swagger
 * /api/v1/operation-plans/{id}/staff-assignments:
 *   patch:
 *     summary: Update staff assignment
 *     description: Assign or update staff members for a vessel visit in the operation plan
 *     tags: [Operation Plans]
 *     security:
 *       - BearerAuth: []
 *     parameters:
 *       - in: path
 *         name: id
 *         required: true
 *         schema:
 *           type: string
 *         description: Operation plan ID
 *     requestBody:
 *       required: true
 *       content:
 *         application/json:
 *           schema:
 *             $ref: '#/components/schemas/UpdateStaffAssignmentRequest'
 *     responses:
 *       200:
 *         description: Staff assignment updated
 *         content:
 *           application/json:
 *             schema:
 *               allOf:
 *                 - $ref: '#/components/schemas/ApiSuccessResponse'
 *                 - type: object
 *                   properties:
 *                     data:
 *                       $ref: '#/components/schemas/OperationPlan'
 *       404:
 *         description: Operation plan or VVN not found
 *       401:
 *         description: Unauthorized
 */
router.patch(
  '/:id/staff',
  authMiddleware,
  [
    param('id').isUUID(4).withMessage('Invalid operation plan ID'),
    body('vvnId').notEmpty().withMessage('VVN ID is required'),
    body('staffIds')
      .isArray()
      .withMessage('Staff IDs must be an array')
      .notEmpty()
      .withMessage('Staff IDs cannot be empty'),
    handleValidationErrors,
  ],
  (req: Request, res: Response, next: NextFunction) =>
    controller.updateStaffAssignment(req, res, next)
);

/**
 * @swagger
 * /api/v1/operation-plans/{id}/approve:
 *   patch:
 *     summary: Approve operation plan
 *     description: Change the status of an operation plan from GENERATED to APPROVED
 *     tags: [Operation Plans]
 *     security:
 *       - BearerAuth: []
 *     parameters:
 *       - in: path
 *         name: id
 *         required: true
 *         schema:
 *           type: string
 *         description: Operation plan MongoDB ID
 *     responses:
 *       200:
 *         description: Plan approved successfully
 *         content:
 *           application/json:
 *             schema:
 *               allOf:
 *                 - $ref: '#/components/schemas/ApiSuccessResponse'
 *                 - type: object
 *                   properties:
 *                     data:
 *                       $ref: '#/components/schemas/OperationPlan'
 *       400:
 *         description: Invalid status transition
 *       404:
 *         description: Operation plan not found
 *       401:
 *         description: Unauthorized
 */
router.patch(
  '/:id/approve',
  authMiddleware,
  [param('id').isUUID(4).withMessage('Invalid operation plan ID'), handleValidationErrors],
  (req: Request, res: Response, next: NextFunction) => controller.approvePlan(req, res, next)
);

/**
 * @swagger
 * /api/v1/operation-plans/{id}/operations:
 *   patch:
 *     summary: Update operation in plan (US 4.1.4 - General Update)
 *     description: Update any editable field(s) of a specific operation within the plan with audit logging
 *     tags: [Operation Plans]
 *     security:
 *       - BearerAuth: []
 *     parameters:
 *       - in: path
 *         name: id
 *         required: true
 *         schema:
 *           type: string
 *         description: Operation plan MongoDB ID
 *     requestBody:
 *       required: true
 *       content:
 *         application/json:
 *           schema:
 *             type: object
 *             required:
 *               - vvnId
 *               - reason
 *             properties:
 *               vvnId:
 *                 type: string
 *                 description: VVN ID of the operation to update
 *               updates:
 *                 type: object
 *                 properties:
 *                   plannedStart:
 *                     type: string
 *                     format: date-time
 *                   plannedEnd:
 *                     type: string
 *                     format: date-time
 *                   assignedCranes:
 *                     type: number
 *                     minimum: 1
 *                   assignedDock:
 *                     type: string
 *                   assignedStaff:
 *                     type: array
 *                     items:
 *                       type: string
 *               reason:
 *                 type: string
 *                 description: Reason for the change (required for audit)
 *     responses:
 *       200:
 *         description: Operation updated successfully
 *       400:
 *         description: Invalid update or business rule violation
 *       401:
 *         description: Unauthorized
 *       404:
 *         description: Operation plan or operation not found
 */
router.patch(
  '/:id/operations',
  authMiddleware,
  [
    param('id').isUUID(4).withMessage('Invalid operation plan ID'),
    body('vvnId').notEmpty().withMessage('VVN ID is required'),
    body('updates').isObject().withMessage('Updates must be an object'),
    body('reason').notEmpty().withMessage('Reason for change is required'),
    handleValidationErrors,
  ],
  (req: Request, res: Response, next: NextFunction) => controller.updateOperation(req, res, next)
);

/**
 * @swagger
 * /api/v1/operation-plans/{id}/conflicts:
 *   post:
 *     summary: Detect conflicts for proposed operation update (US 4.1.4)
 *     description: Check if proposed changes would create resource conflicts
 *     tags: [Operation Plans]
 *     security:
 *       - BearerAuth: []
 *     parameters:
 *       - in: path
 *         name: id
 *         required: true
 *         schema:
 *           type: string
 *         description: Operation plan MongoDB ID
 *     requestBody:
 *       required: true
 *       content:
 *         application/json:
 *           schema:
 *             type: object
 *             required:
 *               - vvnId
 *               - updates
 *             properties:
 *               vvnId:
 *                 type: string
 *               updates:
 *                 type: object
 *                 properties:
 *                   plannedStart:
 *                     type: string
 *                     format: date-time
 *                   plannedEnd:
 *                     type: string
 *                     format: date-time
 *                   assignedCranes:
 *                     type: number
 *                   assignedDock:
 *                     type: string
 *                   assignedStaff:
 *                     type: array
 *                     items:
 *                       type: string
 *     responses:
 *       200:
 *         description: Conflict detection result
 *         content:
 *           application/json:
 *             schema:
 *               type: object
 *               properties:
 *                 success:
 *                   type: boolean
 *                 data:
 *                   type: object
 *                   properties:
 *                     hasConflicts:
 *                       type: boolean
 *                     hasErrors:
 *                       type: boolean
 *                     hasWarnings:
 *                       type: boolean
 *                     conflicts:
 *                       type: array
 *                       items:
 *                         type: object
 *                     summary:
 *                       type: string
 */
router.post(
  '/:id/conflicts',
  authMiddleware,
  [
    param('id').isUUID(4).withMessage('Invalid operation plan ID'),
    body('vvnId').notEmpty().withMessage('VVN ID is required'),
    body('updates').isObject().withMessage('Updates must be an object'),
    handleValidationErrors,
  ],
  (req: Request, res: Response, next: NextFunction) => controller.detectConflicts(req, res, next)
);

/**
 * @swagger
 * /api/v1/operation-plans/{id}:
 *   delete:
 *     summary: Delete operation plan
 *     description: Delete an operation plan (only allowed if status is DRAFT)
 *     tags: [Operation Plans]
 *     security:
 *       - BearerAuth: []
 *     parameters:
 *       - in: path
 *         name: id
 *         required: true
 *         schema:
 *           type: string
 *         description: Operation plan ID
 *     responses:
 *       200:
 *         description: Operation plan deleted successfully
 *         content:
 *           application/json:
 *             schema:
 *               type: object
 *               properties:
 *                 success:
 *                   type: boolean
 *                   example: true
 *                 message:
 *                   type: string
 *                   example: 'Operation plan deleted successfully'
 *       400:
 *         description: Cannot delete - plan is not in DRAFT status
 *       404:
 *         description: Operation plan not found
 *       401:
 *         description: Unauthorized
 */
router.delete(
  '/:id',
  authMiddleware,
  [param('id').isUUID(4).withMessage('Invalid operation plan ID'), handleValidationErrors],
  (req: Request, res: Response, next: NextFunction) => controller.deletePlan(req, res, next)
);

export default router;
