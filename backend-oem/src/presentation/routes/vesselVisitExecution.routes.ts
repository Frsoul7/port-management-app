import { Router } from 'express';
import { Container } from '@infrastructure/container';
import { authMiddleware } from '@infrastructure/middleware/auth.middleware';
import { body, query, param, validationResult } from 'express-validator';
import { Request, Response, NextFunction } from 'express';

/**
 * Vessel Visit Execution Routes
 * Covers US 4.1.7-4.1.11
 */

const router = Router();

// Get controller from DI container
const controller = Container.getVveController();

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
 * /api/v1/vessel-visit-executions:
 *   post:
 *     summary: Create vessel visit execution
 *     description: Initialize a new vessel visit execution record from an approved operation plan
 *     tags: [Vessel Visit Executions]
 *     security:
 *       - BearerAuth: []
 *     requestBody:
 *       required: true
 *       content:
 *         application/json:
 *           schema:
 *             $ref: '#/components/schemas/CreateVesselVisitExecutionRequest'
 *     responses:
 *       201:
 *         description: VVE created successfully
 *         content:
 *           application/json:
 *             schema:
 *               allOf:
 *                 - $ref: '#/components/schemas/ApiSuccessResponse'
 *                 - type: object
 *                   properties:
 *                     data:
 *                       $ref: '#/components/schemas/VesselVisitExecution'
 *       400:
 *         description: Validation error
 *       404:
 *         description: Operation plan not found
 *       401:
 *         description: Unauthorized
 */
router.post(
  '/initialize',
  authMiddleware,
  [
    body('operationPlanId')
      .notEmpty()
      .withMessage('Operation plan ID is required')
      .isUUID(4)
      .withMessage('Invalid operation plan ID'),
    body('vvnId')
      .notEmpty()
      .withMessage('VVN ID is required'),
    body('arrivalTime')
      .notEmpty()
      .withMessage('Arrival time is required')
      .isISO8601()
      .withMessage('Arrival time must be a valid ISO 8601 date-time'),
    handleValidationErrors,
  ],
  (req: Request, res: Response, next: NextFunction) => controller.initializeFromPlan(req, res, next)
);

/**
 * @swagger
 * /api/v1/vessel-visit-executions/by-vvn/{vvnId}:
 *   get:
 *     summary: Get VVE by VVN ID
 *     description: Retrieve vessel visit execution by Vessel Visit Notification ID
 *     tags: [Vessel Visit Executions]
 *     security:
 *       - BearerAuth: []
 *     parameters:
 *       - in: path
 *         name: vvnId
 *         required: true
 *         schema:
 *           type: string
 *         description: Vessel Visit Notification ID
 *         example: '2025-PTLEI-000001'
 *     responses:
 *       200:
 *         description: VVE details
 *         content:
 *           application/json:
 *             schema:
 *               allOf:
 *                 - $ref: '#/components/schemas/ApiSuccessResponse'
 *                 - type: object
 *                   properties:
 *                     data:
 *                       $ref: '#/components/schemas/VesselVisitExecution'
 *       404:
 *         description: VVE not found for VVN
 *       401:
 *         description: Unauthorized
 */
router.get(
  '/by-vvn/:vvnId',
  authMiddleware,
  [param('vvnId').notEmpty().withMessage('VVN ID is required'), handleValidationErrors],
  (req: Request, res: Response, next: NextFunction) =>
    controller.getExecutionByVvnId(req, res, next)
);

/**
 * @swagger
 * /api/v1/vessel-visit-executions/{id}:
 *   get:
 *     summary: Get VVE by ID
 *     description: Retrieve a specific vessel visit execution by its MongoDB ID
 *     tags: [Vessel Visit Executions]
 *     security:
 *       - BearerAuth: []
 *     parameters:
 *       - in: path
 *         name: id
 *         required: true
 *         schema:
 *           type: string
 *         description: VVE MongoDB ID
 *     responses:
 *       200:
 *         description: VVE details
 *         content:
 *           application/json:
 *             schema:
 *               allOf:
 *                 - $ref: '#/components/schemas/ApiSuccessResponse'
 *                 - type: object
 *                   properties:
 *                     data:
 *                       $ref: '#/components/schemas/VesselVisitExecution'
 *       404:
 *         description: VVE not found
 *       401:
 *         description: Unauthorized
 */
router.get(
  '/:id',
  authMiddleware,
  [param('id').notEmpty().withMessage('VVE ID is required'), handleValidationErrors],
  (req: Request, res: Response, next: NextFunction) => controller.getExecutionById(req, res, next)
);

/**
 * @swagger
 * /api/v1/vessel-visit-executions:
 *   get:
 *     summary: List vessel visit executions
 *     description: Retrieve all vessel visit executions with optional filtering, sorting, and pagination
 *     tags: [Vessel Visit Executions]
 *     security:
 *       - BearerAuth: []
 *     parameters:
 *       - in: query
 *         name: vvnId
 *         schema:
 *           type: string
 *         description: Filter by VVN ID
 *       - in: query
 *         name: status
 *         schema:
 *           type: string
 *           enum: [SCHEDULED, IN_PROGRESS, COMPLETED, CANCELLED, DELAYED]
 *         description: Filter by execution status
 *       - in: query
 *         name: fromDate
 *         schema:
 *           type: string
 *           format: date
 *         description: Filter from date
 *       - in: query
 *         name: toDate
 *         schema:
 *           type: string
 *           format: date
 *         description: Filter to date
 *       - in: query
 *         name: page
 *         schema:
 *           type: integer
 *           minimum: 1
 *           default: 1
 *       - in: query
 *         name: limit
 *         schema:
 *           type: integer
 *           minimum: 1
 *           maximum: 100
 *           default: 10
 *     responses:
 *       200:
 *         description: List of vessel visit executions
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/PaginationResponse'
 *       401:
 *         description: Unauthorized
 */
router.get(
  '/',
  authMiddleware,
  [
    query('vvnId').optional().isString().withMessage('VVN ID must be a string'),
    query('status')
      .optional()
      .isIn(['PENDING', 'IN_PROGRESS', 'COMPLETED', 'DISRUPTED'])
      .withMessage('Invalid status'),
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
  (req: Request, res: Response, next: NextFunction) => controller.listExecutions(req, res, next)
);

/**
 * @swagger
 * /api/v1/vessel-visit-executions/{id}/status:
 *   patch:
 *     summary: Update VVE status
 *     description: Change the execution status (start, complete, cancel, delay)
 *     tags: [Vessel Visit Executions]
 *     security:
 *       - BearerAuth: []
 *     parameters:
 *       - in: path
 *         name: id
 *         required: true
 *         schema:
 *           type: string
 *         description: VVE ID
 *     requestBody:
 *       required: true
 *       content:
 *         application/json:
 *           schema:
 *             $ref: '#/components/schemas/UpdateVesselVisitExecutionStatusRequest'
 *     responses:
 *       200:
 *         description: Status updated
 *         content:
 *           application/json:
 *             schema:
 *               allOf:
 *                 - $ref: '#/components/schemas/ApiSuccessResponse'
 *                 - type: object
 *                   properties:
 *                     data:
 *                       $ref: '#/components/schemas/VesselVisitExecution'
 *       400:
 *         description: Invalid status transition
 *       404:
 *         description: VVE not found
 */
router.post(
  '/:id/arrival',
  authMiddleware,
  [
    param('id').notEmpty().withMessage('VVE ID is required'),
    body('arrivalTime')
      .notEmpty()
      .withMessage('Arrival time is required')
      .isISO8601()
      .withMessage('Arrival time must be a valid ISO 8601 date'),
    handleValidationErrors,
  ],
  (req: Request, res: Response, next: NextFunction) => controller.recordPortArrival(req, res, next)
);

/**
 * @swagger
 * /api/v1/vessel-visit-executions/{id}/berth:
 *   post:
 *     summary: Record vessel berthing
 *     description: Record when a vessel berths at a dock with dock assignment (US 4.1.8)
 *     tags: [Vessel Visit Executions]
 *     security:
 *       - BearerAuth: []
 *     parameters:
 *       - in: path
 *         name: id
 *         required: true
 *         schema:
 *           type: string
 *         description: VVE MongoDB ID
 *     requestBody:
 *       required: true
 *       content:
 *         application/json:
 *           schema:
 *             type: object
 *             required:
 *               - berthTime
 *               - dockId
 *             properties:
 *               berthTime:
 *                 type: string
 *                 format: date-time
 *                 description: Timestamp when vessel berthed
 *               dockId:
 *                 type: string
 *                 description: ID of the assigned dock
 *     responses:
 *       200:
 *         description: Berthing recorded
 *         content:
 *           application/json:
 *             schema:
 *               allOf:
 *                 - $ref: '#/components/schemas/ApiSuccessResponse'
 *                 - type: object
 *                   properties:
 *                     data:
 *                       $ref: '#/components/schemas/VesselVisitExecution'
 *       404:
 *         description: VVE not found
 *       401:
 *         description: Unauthorized
 */
router.post(
  '/:id/berth',
  authMiddleware,
  [
    param('id').notEmpty().withMessage('VVE ID is required'),
    body('berthTime')
      .notEmpty()
      .withMessage('Berth time is required')
      .isISO8601()
      .withMessage('Berth time must be a valid ISO 8601 date'),
    body('dockId').notEmpty().withMessage('Dock ID is required'),
    handleValidationErrors,
  ],
  (req: Request, res: Response, next: NextFunction) => controller.recordBerthing(req, res, next)
);

/**
 * @swagger
 * /api/v1/vessel-visit-executions/{id}/operations:
 *   post:
 *     summary: Add executed operation
 *     description: Record a completed cargo operation (loading/unloading) during vessel visit (US 4.1.9)
 *     tags: [Vessel Visit Executions]
 *     security:
 *       - BearerAuth: []
 *     parameters:
 *       - in: path
 *         name: id
 *         required: true
 *         schema:
 *           type: string
 *         description: VVE MongoDB ID
 *     requestBody:
 *       required: true
 *       content:
 *         application/json:
 *           schema:
 *             type: object
 *             required:
 *               - operationType
 *               - startTime
 *               - endTime
 *               - containersProcessed
 *               - cranesUsed
 *             properties:
 *               operationType:
 *                 type: string
 *                 enum: [LOAD, UNLOAD, BOTH, MAINTENANCE]
 *                 description: Type of operation performed
 *               startTime:
 *                 type: string
 *                 format: date-time
 *                 description: When operation started
 *               endTime:
 *                 type: string
 *                 format: date-time
 *                 description: When operation ended
 *               containersProcessed:
 *                 type: integer
 *                 minimum: 0
 *                 description: Number of containers loaded/unloaded
 *               cranesUsed:
 *                 type: integer
 *                 minimum: 1
 *                 description: Number of cranes used
 *               staffAssigned:
 *                 type: array
 *                 items:
 *                   type: string
 *                 description: IDs of staff members assigned
 *     responses:
 *       200:
 *         description: Operation recorded
 *         content:
 *           application/json:
 *             schema:
 *               allOf:
 *                 - $ref: '#/components/schemas/ApiSuccessResponse'
 *                 - type: object
 *                   properties:
 *                     data:
 *                       $ref: '#/components/schemas/VesselVisitExecution'
 *       404:
 *         description: VVE not found
 *       401:
 *         description: Unauthorized
 */
router.post(
  '/:id/operations',
  authMiddleware,
  [
    param('id').notEmpty().withMessage('VVE ID is required'),
    body('operationType')
      .notEmpty()
      .withMessage('Operation type is required')
      .isIn(['LOAD', 'UNLOAD', 'BOTH', 'MAINTENANCE'])
      .withMessage('Invalid operation type'),
    body('startTime')
      .notEmpty()
      .withMessage('Start time is required')
      .isISO8601()
      .withMessage('Start time must be a valid ISO 8601 date'),
    body('endTime')
      .notEmpty()
      .withMessage('End time is required')
      .isISO8601()
      .withMessage('End time must be a valid ISO 8601 date'),
    body('containersProcessed')
      .notEmpty()
      .withMessage('Containers processed is required')
      .isInt({ min: 0 })
      .withMessage('Containers processed must be a non-negative integer'),
    body('cranesUsed')
      .notEmpty()
      .withMessage('Cranes used is required')
      .isInt({ min: 1 })
      .withMessage('Cranes used must be a positive integer'),
    body('staffAssigned').optional().isArray().withMessage('Staff assigned must be an array'),
    handleValidationErrors,
  ],
  (req: Request, res: Response, next: NextFunction) =>
    controller.addExecutedOperation(req, res, next)
);

/**
 * @swagger
 * /api/v1/vessel-visit-executions/{id}/operations/{operationIndex}:
 *   patch:
 *     summary: Update executed operation (US 4.1.9)
 *     tags: [Vessel Visit Executions]
 *     security:
 *       - BearerAuth: []
 *     parameters:
 *       - in: path
 *         name: id
 *         required: true
 *         schema:
 *           type: string
 *       - in: path
 *         name: operationIndex
 *         required: true
 *         schema:
 *           type: integer
 *     requestBody:
 *       required: true
 *       content:
 *         application/json:
 *           schema:
 *             type: object
 *             properties:
 *               startTime:
 *                 type: string
 *                 format: date-time
 *               endTime:
 *                 type: string
 *                 format: date-time
 *               containersProcessed:
 *                 type: integer
 *               cranesUsed:
 *                 type: integer
 *               staffAssigned:
 *                 type: array
 *                 items:
 *                   type: string
 *               status:
 *                 type: string
 *                 enum: [STARTED, COMPLETED, DELAYED]
 *     responses:
 *       200:
 *         description: Operation updated successfully
 *       400:
 *         description: Invalid request
 *       404:
 *         description: VVE or operation not found
 */
router.patch(
  '/:id/operations/:operationIndex',
  authMiddleware,
  [
    param('id').notEmpty().withMessage('VVE ID is required'),
    param('operationIndex')
      .isInt({ min: 0 })
      .withMessage('Operation index must be a non-negative integer'),
    body('startTime').optional().isISO8601().withMessage('Invalid start time format'),
    body('endTime').optional().isISO8601().withMessage('Invalid end time format'),
    body('containersProcessed')
      .optional()
      .isInt({ min: 0 })
      .withMessage('Containers processed must be non-negative'),
    body('cranesUsed').optional().isInt({ min: 1 }).withMessage('At least one crane required'),
    body('staffAssigned').optional().isArray().withMessage('Staff assigned must be an array'),
    body('status')
      .optional()
      .isIn(['STARTED', 'COMPLETED', 'DELAYED'])
      .withMessage('Invalid status'),
    handleValidationErrors,
  ],
  (req: Request, res: Response, next: NextFunction) =>
    controller.updateExecutedOperation(req, res, next)
);

/**
 * @swagger
 * /api/v1/vessel-visit-executions/{id}/unberth:
 *   post:
 *     summary: Record vessel unberthing
 *     description: Record when a vessel leaves the dock (US 4.1.9)
 *     tags: [Vessel Visit Executions]
 *     security:
 *       - BearerAuth: []
 *     parameters:
 *       - in: path
 *         name: id
 *         required: true
 *         schema:
 *           type: string
 *         description: VVE MongoDB ID
 *     requestBody:
 *       required: true
 *       content:
 *         application/json:
 *           schema:
 *             type: object
 *             required:
 *               - unberthTime
 *             properties:
 *               unberthTime:
 *                 type: string
 *                 format: date-time
 *                 description: Timestamp when vessel left the dock
 *     responses:
 *       200:
 *         description: Unberthing recorded
 *         content:
 *           application/json:
 *             schema:
 *               allOf:
 *                 - $ref: '#/components/schemas/ApiSuccessResponse'
 *                 - type: object
 *                   properties:
 *                     data:
 *                       $ref: '#/components/schemas/VesselVisitExecution'
 *       404:
 *         description: VVE not found
 *       401:
 *         description: Unauthorized
 */
router.post(
  '/:id/unberth',
  authMiddleware,
  [
    param('id').notEmpty().withMessage('VVE ID is required'),
    body('unberthTime')
      .notEmpty()
      .withMessage('Unberth time is required')
      .isISO8601()
      .withMessage('Unberth time must be a valid ISO 8601 date'),
    handleValidationErrors,
  ],
  (req: Request, res: Response, next: NextFunction) => controller.recordUnberthing(req, res, next)
);

/**
 * @swagger
 * /api/v1/vessel-visit-executions/{id}/complete:
 *   post:
 *     summary: Complete vessel visit
 *     description: Record port departure and mark vessel visit execution as completed (US 4.1.11)
 *     tags: [Vessel Visit Executions]
 *     security:
 *       - BearerAuth: []
 *     parameters:
 *       - in: path
 *         name: id
 *         required: true
 *         schema:
 *           type: string
 *         description: VVE MongoDB ID
 *     requestBody:
 *       required: true
 *       content:
 *         application/json:
 *           schema:
 *             type: object
 *             required:
 *               - departureTime
 *             properties:
 *               departureTime:
 *                 type: string
 *                 format: date-time
 *                 description: Timestamp when vessel departed the port
 *     responses:
 *       200:
 *         description: Visit completed
 *         content:
 *           application/json:
 *             schema:
 *               allOf:
 *                 - $ref: '#/components/schemas/ApiSuccessResponse'
 *                 - type: object
 *                   properties:
 *                     data:
 *                       $ref: '#/components/schemas/VesselVisitExecution'
 *       404:
 *         description: VVE not found
 *       401:
 *         description: Unauthorized
 */
router.post(
  '/:id/complete',
  authMiddleware,
  [
    param('id').notEmpty().withMessage('VVE ID is required'),
    body('departureTime')
      .notEmpty()
      .withMessage('Departure time is required')
      .isISO8601()
      .withMessage('Departure time must be a valid ISO 8601 date'),
    handleValidationErrors,
  ],
  (req: Request, res: Response, next: NextFunction) => controller.recordCompletion(req, res, next)
);

/**
 * @swagger
 * /api/v1/vessel-visit-executions/{id}/incidents:
 *   post:
 *     summary: Link incident to VVE
 *     description: Associate an incident with this vessel visit execution
 *     tags: [Vessel Visit Executions]
 *     security:
 *       - BearerAuth: []
 *     parameters:
 *       - in: path
 *         name: id
 *         required: true
 *         schema:
 *           type: string
 *         description: VVE ID
 *     requestBody:
 *       required: true
 *       content:
 *         application/json:
 *           schema:
 *             $ref: '#/components/schemas/LinkIncidentRequest'
 *     responses:
 *       200:
 *         description: Incident linked successfully
 *         content:
 *           application/json:
 *             schema:
 *               allOf:
 *                 - $ref: '#/components/schemas/ApiSuccessResponse'
 *                 - type: object
 *                   properties:
 *                     data:
 *                       $ref: '#/components/schemas/VesselVisitExecution'
 *       404:
 *         description: VVE or incident not found
 */
router.post(
  '/:id/incidents',
  authMiddleware,
  [
    param('id').notEmpty().withMessage('VVE ID is required'),
    body('incidentId')
      .notEmpty()
      .withMessage('Incident ID is required')
      .isMongoId()
      .withMessage('Invalid incident ID'),
    handleValidationErrors,
  ],
  (req: Request, res: Response, next: NextFunction) => controller.linkIncident(req, res, next)
);

/**
 * @swagger
 * /api/v1/vessel-visit-executions/{id}/incidents/{incidentId}:
 *   delete:
 *     summary: Unlink incident from VVE
 *     description: Remove the association between an incident and this vessel visit execution
 *     tags: [Vessel Visit Executions]
 *     security:
 *       - BearerAuth: []
 *     parameters:
 *       - in: path
 *         name: id
 *         required: true
 *         schema:
 *           type: string
 *         description: VVE ID
 *       - in: path
 *         name: incidentId
 *         required: true
 *         schema:
 *           type: string
 *         description: Incident ID
 *     responses:
 *       200:
 *         description: Incident unlinked successfully
 *         content:
 *           application/json:
 *             schema:
 *               allOf:
 *                 - $ref: '#/components/schemas/ApiSuccessResponse'
 *                 - type: object
 *                   properties:
 *                     data:
 *                       $ref: '#/components/schemas/VesselVisitExecution'
 *       404:
 *         description: VVE or incident not found
 */
router.delete(
  '/:id/incidents/:incidentId',
  authMiddleware,
  [
    param('id').notEmpty().withMessage('VVE ID is required'),
    param('incidentId').isMongoId().withMessage('Invalid incident ID'),
    handleValidationErrors,
  ],
  (req: Request, res: Response, next: NextFunction) => controller.unlinkIncident(req, res, next)
);

/**
 * @swagger
 * /api/v1/vessel-visit-executions/{id}/metrics:
 *   get:
 *     summary: Get VVE metrics
 *     description: Retrieve execution metrics including delays, operations count, and incident statistics
 *     tags: [Vessel Visit Executions]
 *     security:
 *       - BearerAuth: []
 *     parameters:
 *       - in: path
 *         name: id
 *         required: true
 *         schema:
 *           type: string
 *         description: VVE ID
 *     responses:
 *       200:
 *         description: VVE metrics
 *         content:
 *           application/json:
 *             schema:
 *               allOf:
 *                 - $ref: '#/components/schemas/ApiSuccessResponse'
 *                 - type: object
 *                   properties:
 *                     data:
 *                       $ref: '#/components/schemas/VesselVisitExecutionMetrics'
 *       404:
 *         description: VVE not found
 */
router.get(
  '/:id/metrics',
  authMiddleware,
  [param('id').notEmpty().withMessage('VVE ID is required'), handleValidationErrors],
  (req: Request, res: Response, next: NextFunction) => controller.getMetrics(req, res, next)
);

export default router;
