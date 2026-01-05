import { Router } from 'express';
import { Container } from '@infrastructure/container';
import { body, query, param, validationResult } from 'express-validator';
import { Request, Response, NextFunction } from 'express';
import { authMiddleware } from '@infrastructure/middleware/auth.middleware';

/**
 * Task Category Routes
 * US 4.1.14: Manage complementary task category catalog
 * 
 * Authentication: All routes require valid JWT token from Core Backend
 * Authorization: RBAC enforced based on user roles
 */

const router = Router();

// Apply authentication to ALL routes
router.use(authMiddleware);

// Get controller from DI container
const controller = Container.getTaskCategoryController();

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
 * US 4.1.14: Create task category
 * POST /api/v1/task-categories
 * Note: categoryCode is auto-generated (CTC001, CTC002, etc.)
 */
router.post(
  '/',
  [
    body('categoryName')
      .isString()
      .trim()
      .notEmpty()
      .withMessage('Category name is required')
      .isLength({ max: 100 })
      .withMessage('Category name must not exceed 100 characters'),
    body('description')
      .isString()
      .trim()
      .notEmpty()
      .withMessage('Description is required')
      .isLength({ max: 500 })
      .withMessage('Description must not exceed 500 characters'),
    body('defaultDurationHours')
      .optional()
      .isFloat({ min: 0.1, max: 168 })
      .withMessage('Default duration must be between 0.1 and 168 hours'),
    body('expectedImpact')
      .optional()
      .isString()
      .trim()
      .isLength({ max: 200 })
      .withMessage('Expected impact must not exceed 200 characters'),
    handleValidationErrors,
  ],
  controller.createTaskCategory.bind(controller)
);

/**
 * US 4.1.14: Get task category by code
 * GET /api/v1/task-categories/code/:code
 */
router.get(
  '/code/:code',
  [
    param('code')
      .isString()
      .matches(/^CTC\d{3}$/)
      .withMessage('Code must follow format CTC### (e.g., CTC001)'),
    handleValidationErrors,
  ],
  controller.getTaskCategoryByCode.bind(controller)
);

/**
 * US 4.1.14: Get count of task categories
 * GET /api/v1/task-categories/count
 * Must come before /:id route to avoid conflict
 */
router.get(
  '/count',
  [
    query('activeOnly').optional().isBoolean().withMessage('Active only must be boolean'),
    handleValidationErrors,
  ],
  controller.getTaskCategoryCount.bind(controller)
);

/**
 * US 4.1.14: List all task categories
 * GET /api/v1/task-categories
 * Query params: activeOnly, sortBy, sortOrder
 */
router.get(
  '/',
  [
    query('activeOnly').optional().isBoolean().withMessage('Active only must be boolean'),
    query('sortBy').optional().isString().withMessage('Sort by must be a string'),
    query('sortOrder')
      .optional()
      .isIn(['asc', 'desc'])
      .withMessage('Sort order must be asc or desc'),
    handleValidationErrors,
  ],
  controller.listTaskCategories.bind(controller)
);

/**
 * US 4.1.14: Get task category by ID
 * GET /api/v1/task-categories/:id
 */
router.get(
  '/:id',
  [param('id').isString().withMessage('ID must be a string'), handleValidationErrors],
  controller.getTaskCategoryById.bind(controller)
);

/**
 * US 4.1.14: Update task category
 * PUT /api/v1/task-categories/:id
 */
router.put(
  '/:id',
  [
    param('id').isString().withMessage('ID must be a string'),
    body('categoryName')
      .optional()
      .isString()
      .trim()
      .notEmpty()
      .withMessage('Category name must be non-empty')
      .isLength({ max: 100 })
      .withMessage('Category name must not exceed 100 characters'),
    body('description')
      .optional()
      .isString()
      .trim()
      .notEmpty()
      .withMessage('Description must be non-empty')
      .isLength({ max: 500 })
      .withMessage('Description must not exceed 500 characters'),
    body('defaultDurationHours')
      .optional()
      .isFloat({ min: 0.1, max: 168 })
      .withMessage('Default duration must be between 0.1 and 168 hours'),
    body('expectedImpact')
      .optional()
      .isString()
      .trim()
      .isLength({ max: 200 })
      .withMessage('Expected impact must not exceed 200 characters'),
    handleValidationErrors,
  ],
  controller.updateTaskCategory.bind(controller)
);

/**
 * US 4.1.14: Deactivate task category
 * POST /api/v1/task-categories/:id/deactivate
 */
router.post(
  '/:id/deactivate',
  [param('id').isString().withMessage('ID must be a string'), handleValidationErrors],
  controller.deactivateTaskCategory.bind(controller)
);

/**
 * US 4.1.14: Reactivate task category
 * POST /api/v1/task-categories/:id/reactivate
 */
router.post(
  '/:id/reactivate',
  [param('id').isString().withMessage('ID must be a string'), handleValidationErrors],
  controller.reactivateTaskCategory.bind(controller)
);

/**
 * US 4.1.14: Delete task category
 * DELETE /api/v1/task-categories/:id
 */
router.delete(
  '/:id',
  [param('id').isString().withMessage('ID must be a string'), handleValidationErrors],
  controller.deleteTaskCategory.bind(controller)
);

export default router;
