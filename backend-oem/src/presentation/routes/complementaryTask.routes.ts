import { Router } from 'express';
import { Container } from '@infrastructure/container';
import { body, query, param, validationResult } from 'express-validator';
import { Request, Response, NextFunction } from 'express';
import { ComplementaryTaskStatus } from '@shared/types';
import { authMiddleware } from '@infrastructure/middleware/auth.middleware';

/**
 * Complementary Task Routes
 * US 4.1.15: Record and manage complementary tasks during vessel visits
 * 
 * Authentication: All routes require valid JWT token from Core Backend
 * Authorization: RBAC enforced based on user roles
 */

const router = Router();

// Apply authentication to ALL routes
router.use(authMiddleware);

// Get controller from DI container
const controller = Container.getComplementaryTaskController();

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
 * /api/v1/complementary-tasks/count:
 *   get:
 *     summary: Get count of complementary tasks
 *     description: Returns the total count of complementary tasks, optionally filtered by status
 *     tags: [Complementary Tasks]
 *     security:
 *       - BearerAuth: []
 *     parameters:
 *       - in: query
 *         name: status
 *         schema:
 *           type: string
 *           enum: [PLANNED, IN_PROGRESS, COMPLETED, CANCELLED]
 *         description: Filter by task status
 *     responses:
 *       200:
 *         description: Task count retrieved successfully
 *       401:
 *         description: Unauthorized - Invalid or missing JWT token
 *       400:
 *         description: Bad request - Invalid status value
 */
router.get(
  '/count',
  [
    query('status')
      .optional()
      .isIn(['PLANNED', 'IN_PROGRESS', 'COMPLETED', 'CANCELLED'])
      .withMessage('Invalid status'),
    handleValidationErrors,
  ],
  controller.getTaskCount.bind(controller)
);

/**
 * @swagger
 * /api/v1/complementary-tasks/overdue-count:
 *   get:
 *     summary: Get count of overdue tasks
 *     description: Returns the count of tasks that are past their due date
 *     tags: [Complementary Tasks]
 *     security:
 *       - BearerAuth: []
 *     responses:
 *       200:
 *         description: Overdue count retrieved successfully
 *       401:
 *         description: Unauthorized - Invalid or missing JWT token
 */
router.get('/overdue-count', controller.getOverdueCount.bind(controller));

/**
 * @swagger
 * /api/v1/complementary-tasks/statistics:
 *   get:
 *     summary: Get task statistics
 *     description: Returns statistical data about tasks within a date range
 *     tags: [Complementary Tasks]
 *     security:
 *       - BearerAuth: []
 *     parameters:
 *       - in: query
 *         name: fromDate
 *         required: true
 *         schema:
 *           type: string
 *           format: date-time
 *         description: Start date for statistics (ISO 8601 format)
 *       - in: query
 *         name: toDate
 *         required: true
 *         schema:
 *           type: string
 *           format: date-time
 *         description: End date for statistics (ISO 8601 format)
 *     responses:
 *       200:
 *         description: Statistics retrieved successfully
 *       401:
 *         description: Unauthorized - Invalid or missing JWT token
 *       400:
 *         description: Bad request - Invalid date format
 */
router.get(
  '/statistics',
  [
    query('fromDate').isISO8601().withMessage('fromDate must be valid ISO 8601 date'),
    query('toDate').isISO8601().withMessage('toDate must be valid ISO 8601 date'),
    handleValidationErrors,
  ],
  controller.getStatistics.bind(controller)
);

/**
 * @swagger
 * /api/v1/complementary-tasks/active-impacting:
 *   get:
 *     summary: Get active tasks impacting operations
 *     description: Returns tasks that are currently active and impact port operations
 *     tags: [Complementary Tasks]
 *     security:
 *       - BearerAuth: []
 *     responses:
 *       200:
 *         description: Active impacting tasks retrieved successfully
 *       401:
 *         description: Unauthorized - Invalid or missing JWT token
 */
router.get('/active-impacting', controller.getActiveImpactingTasks.bind(controller));

/**
 * @swagger
 * /api/v1/complementary-tasks/vve/{vveId}:
 *   get:
 *     summary: List tasks by Vessel Visit Execution ID
 *     description: Returns all complementary tasks associated with a specific VVE
 *     tags: [Complementary Tasks]
 *     security:
 *       - BearerAuth: []
 *     parameters:
 *       - in: path
 *         name: vveId
 *         required: true
 *         schema:
 *           type: string
 *         description: Vessel Visit Execution ID
 *       - in: query
 *         name: sortBy
 *         schema:
 *           type: string
 *         description: Field to sort by
 *       - in: query
 *         name: sortOrder
 *         schema:
 *           type: string
 *           enum: [asc, desc]
 *         description: Sort order
 *     responses:
 *       200:
 *         description: Tasks retrieved successfully
 *       401:
 *         description: Unauthorized - Invalid or missing JWT token
 *       400:
 *         description: Bad request - Invalid parameters
 */
router.get(
  '/vve/:vveId',
  [
    param('vveId').isString().notEmpty().withMessage('VVE ID is required'),
    query('sortBy').optional().isString().withMessage('sortBy must be a string'),
    query('sortOrder')
      .optional()
      .isIn(['asc', 'desc'])
      .withMessage('sortOrder must be asc or desc'),
    handleValidationErrors,
  ],
  controller.listTasksByVve.bind(controller)
);

/**
 * US 4.1.15: List tasks by status
 * GET /api/v1/complementary-tasks/status/:status
 */
router.get(
  '/status/:status',
  [
    param('status')
      .isIn(['PLANNED', 'IN_PROGRESS', 'COMPLETED', 'CANCELLED'])
      .withMessage('Invalid status'),
    query('sortBy').optional().isString().withMessage('sortBy must be a string'),
    query('sortOrder')
      .optional()
      .isIn(['asc', 'desc'])
      .withMessage('sortOrder must be asc or desc'),
    handleValidationErrors,
  ],
  controller.listTasksByStatus.bind(controller)
);


/**
 * @swagger
 * /api/v1/complementary-tasks:
 *   post:
 *     summary: Create a new complementary task
 *     description: Creates a new complementary task associated with a vessel visit execution
 *     tags: [Complementary Tasks]
 *     security:
 *       - BearerAuth: []
 *     requestBody:
 *       required: true
 *       content:
 *         application/json:
 *           schema:
 *             type: object
 *             required:
 *               - taskCategoryId
 *               - vveId
 *               - title
 *               - description
 *               - dueDate
 *               - createdBy
 *             properties:
 *               taskCategoryId:
 *                 type: string
 *                 description: ID of the task category
 *               vveId:
 *                 type: string
 *                 description: Vessel Visit Execution ID
 *               title:
 *                 type: string
 *                 maxLength: 200
 *                 description: Task title
 *               description:
 *                 type: string
 *                 maxLength: 1000
 *                 description: Detailed task description
 *               dueDate:
 *                 type: string
 *                 format: date-time
 *                 description: Task due date (ISO 8601 format)
 *               estimatedDurationHours:
 *                 type: number
 *                 minimum: 0.1
 *                 maximum: 168
 *                 description: Estimated duration in hours (optional)
 *               assignedTo:
 *                 type: string
 *                 description: Team or user assigned to the task (optional)
 *               createdBy:
 *                 type: string
 *                 description: User who created the task
 *     responses:
 *       201:
 *         description: Task created successfully
 *       401:
 *         description: Unauthorized - Invalid or missing JWT token
 *       400:
 *         description: Bad request - Validation errors
 */
router.post(
  '/',
  [
    body('taskCategoryId')
      .isString()
      .trim()
      .notEmpty()
      .withMessage('Task category ID is required'),
    body('vveId').isString().trim().notEmpty().withMessage('VVE ID is required'),
    body('title')
      .isString()
      .trim()
      .notEmpty()
      .withMessage('Title is required')
      .isLength({ max: 200 })
      .withMessage('Title must not exceed 200 characters'),
    body('description')
      .isString()
      .trim()
      .notEmpty()
      .withMessage('Description is required')
      .isLength({ max: 1000 })
      .withMessage('Description must not exceed 1000 characters'),
    body('dueDate').isISO8601().withMessage('Due date must be valid ISO 8601 date'),
    body('estimatedDurationHours')
      .optional()
      .isFloat({ min: 0.1, max: 168 })
      .withMessage('Estimated duration must be between 0.1 and 168 hours'),
    body('assignedTo').optional().isString().trim().withMessage('Assigned to must be a string'),
    body('createdBy')
      .isString()
      .trim()
      .notEmpty()
      .withMessage('Creator information is required'),
    handleValidationErrors,
  ],
  controller.createTask.bind(controller)
);

/**
 * US 4.1.15: List complementary tasks with filters
 * GET /api/v1/complementary-tasks
 */
router.get(
  '/',
  [
    query('status')
      .optional()
      .isIn(['PLANNED', 'IN_PROGRESS', 'COMPLETED', 'CANCELLED'])
      .withMessage('Invalid status'),
    query('vveId').optional().isString().withMessage('VVE ID must be a string'),
    query('taskCategoryId')
      .optional()
      .isString()
      .withMessage('Task category ID must be a string'),
    query('assignedTo').optional().isString().withMessage('Assigned to must be a string'),
    query('fromDate').optional().isISO8601().withMessage('fromDate must be valid ISO 8601 date'),
    query('toDate').optional().isISO8601().withMessage('toDate must be valid ISO 8601 date'),
    query('sortBy').optional().isString().withMessage('sortBy must be a string'),
    query('sortOrder')
      .optional()
      .isIn(['asc', 'desc'])
      .withMessage('sortOrder must be asc or desc'),
    query('page').optional().isInt({ min: 1 }).withMessage('page must be positive integer'),
    query('limit').optional().isInt({ min: 1 }).withMessage('limit must be positive integer'),
    handleValidationErrors,
  ],
  controller.listTasks.bind(controller)
);

/**
 * US 4.1.15: Get complementary task by ID
 * GET /api/v1/complementary-tasks/:id
 */
router.get(
  '/:id',
  [param('id').isString().notEmpty().withMessage('Task ID is required'), handleValidationErrors],
  controller.getTaskById.bind(controller)
);

/**
 * US 4.1.15: Update complementary task
 * PATCH /api/v1/complementary-tasks/:id
 */
router.patch(
  '/:id',
  [
    param('id').isString().notEmpty().withMessage('Task ID is required'),
    body('title')
      .optional()
      .isString()
      .trim()
      .isLength({ max: 200 })
      .withMessage('Title must not exceed 200 characters'),
    body('description')
      .optional()
      .isString()
      .trim()
      .isLength({ max: 1000 })
      .withMessage('Description must not exceed 1000 characters'),
    body('dueDate').optional().isISO8601().withMessage('Due date must be valid ISO 8601 date'),
    body('estimatedDurationHours')
      .optional()
      .isFloat({ min: 0.1, max: 168 })
      .withMessage('Estimated duration must be between 0.1 and 168 hours'),
    handleValidationErrors,
  ],
  controller.updateTask.bind(controller)
);

/**
 * US 4.1.15: Delete complementary task
 * DELETE /api/v1/complementary-tasks/:id
 */
router.delete(
  '/:id',
  [param('id').isString().notEmpty().withMessage('Task ID is required'), handleValidationErrors],
  controller.deleteTask.bind(controller)
);

/**
 * US 4.1.15: Assign task to team/user
 * PATCH /api/v1/complementary-tasks/:id/assign
 */
router.patch(
  '/:id/assign',
  [
    param('id').isString().notEmpty().withMessage('Task ID is required'),
    body('assignedTo')
      .isString()
      .trim()
      .notEmpty()
      .withMessage('Assigned to is required'),
    handleValidationErrors,
  ],
  controller.assignTask.bind(controller)
);

/**
 * US 4.1.15: Start task
 * PATCH /api/v1/complementary-tasks/:id/start
 */
router.patch(
  '/:id/start',
  [
    param('id').isString().notEmpty().withMessage('Task ID is required'),
    body('startedAt').optional().isISO8601().withMessage('Started at must be valid ISO 8601 date'),
    handleValidationErrors,
  ],
  controller.startTask.bind(controller)
);

/**
 * US 4.1.15: Complete task
 * PATCH /api/v1/complementary-tasks/:id/complete
 */
router.patch(
  '/:id/complete',
  [
    param('id').isString().notEmpty().withMessage('Task ID is required'),
    body('completedBy')
      .isString()
      .trim()
      .notEmpty()
      .withMessage('Completed by is required'),
    body('completedAt')
      .optional()
      .isISO8601()
      .withMessage('Completed at must be valid ISO 8601 date'),
    handleValidationErrors,
  ],
  controller.completeTask.bind(controller)
);

/**
 * US 4.1.15: Cancel task
 * PATCH /api/v1/complementary-tasks/:id/cancel
 */
router.patch(
  '/:id/cancel',
  [
    param('id').isString().notEmpty().withMessage('Task ID is required'),
    body('cancelledBy')
      .isString()
      .trim()
      .notEmpty()
      .withMessage('Cancelled by is required'),
    body('cancellationReason')
      .isString()
      .trim()
      .notEmpty()
      .withMessage('Cancellation reason is required')
      .isLength({ max: 500 })
      .withMessage('Cancellation reason must not exceed 500 characters'),
    body('cancelledAt')
      .optional()
      .isISO8601()
      .withMessage('Cancelled at must be valid ISO 8601 date'),
    handleValidationErrors,
  ],
  controller.cancelTask.bind(controller)
);

/**
 * US 4.1.15: Add note to task
 * POST /api/v1/complementary-tasks/:id/notes
 */
router.post(
  '/:id/notes',
  [
    param('id').isString().notEmpty().withMessage('Task ID is required'),
    body('content')
      .isString()
      .trim()
      .notEmpty()
      .withMessage('Note content is required')
      .isLength({ max: 1000 })
      .withMessage('Note content must not exceed 1000 characters'),
    body('author').isString().trim().notEmpty().withMessage('Author is required'),
    handleValidationErrors,
  ],
  controller.addNote.bind(controller)
);

export default router;
