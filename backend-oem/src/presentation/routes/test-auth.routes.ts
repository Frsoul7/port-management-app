import { Router, Request, Response } from 'express';
import jwt from 'jsonwebtoken';
import { authMiddleware } from '@infrastructure/middleware/auth.middleware';

const router = Router();

// Access process.env directly to ensure dotenv is loaded first
function getJwtSecret(): string {
  return process.env.JWT_SECRET || 'your-secret-key-here';
}

/**
 * Test endpoint to debug JWT token validation
 */
router.post('/debug-token', (req: Request, res: Response) => {
  try {
    const authHeader = req.headers.authorization;
    const token = authHeader?.substring(7); // Remove "Bearer "

    if (!token) {
      res.json({
        error: 'No token provided',
        authHeader: authHeader || 'missing',
      });
      return;
    }

    // Try to decode without verification first
    const decoded = jwt.decode(token, { complete: true });
    const jwtSecret = getJwtSecret();

    // Try to verify
    let verified;
    let verifyError;
    try {
      verified = jwt.verify(token, jwtSecret);
    } catch (err: any) {
      verifyError = {
        name: err.name,
        message: err.message,
      };
    }

    res.json({
      success: true,
      tokenLength: token.length,
      secretLength: jwtSecret.length,
      secretPreview: jwtSecret.substring(0, 20) + '...',
      decoded: decoded,
      verified: verified || null,
      verifyError: verifyError || null,
    });
  } catch (error: any) {
    res.status(500).json({
      error: 'Debug failed',
      message: error.message,
    });
  }
});

/**
 * Test endpoint with auth middleware
 */
router.get('/test-auth', authMiddleware, (req: Request, res: Response) => {
  res.json({
    success: true,
    message: 'Authentication successful!',
    user: req.user,
  });
});

export default router;
