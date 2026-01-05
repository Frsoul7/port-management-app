import { Router } from 'express';
import { body, validationResult } from 'express-validator';
import { Request, Response, NextFunction } from 'express';
import { DockRebalancingController } from '@presentation/controllers/DockRebalancingController';
import { Container } from '@infrastructure/container';
import { authMiddleware } from '@infrastructure/middleware/auth.middleware';

/**
 * Dock Rebalancing Routes
 * US 4.1.4: Genetic algorithm for dock optimization
 * 
 * Authentication: All routes require valid JWT token from Core Backend
 * Authorization: Port Authority Officer only
 */

const router = Router();

// Apply authentication to ALL routes
router.use(authMiddleware);

const controller: DockRebalancingController = Container.getDockRebalancingController();

// Validation middleware
const validateRequest = (req: Request, res: Response, next: NextFunction): void => {
  const errors = validationResult(req);
  if (!errors.isEmpty()) {
    res.status(400).json({ errors: errors.array() });
    return;
  }
  next();
};

// Port Authority Officer only - US 4.1.4 Genetic Algorithm
router.post(
  '/dock-rebalancing/propose',
  [
    body('targetDate').isString().notEmpty(),
    body('mode').optional().isIn(['single', 'multi']),
    body('params').optional().isObject(),
  ],
  validateRequest,
  controller.propose
);

router.post(
  '/dock-rebalancing/confirm',
  [
    body('proposal').isObject(),
  ],
  validateRequest,
  controller.confirm
);

export default router;
