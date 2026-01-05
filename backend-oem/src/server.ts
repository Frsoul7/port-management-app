// MUST be imported FIRST to load environment variables
import './config/env';

import express, { Application } from 'express';
import cors from 'cors';
import swaggerUi from 'swagger-ui-express';
import swaggerJsdoc from 'swagger-jsdoc';
import { logger } from '@shared/utils/logger';
import { connectDatabase } from '@infrastructure/database/connection';
import { errorMiddleware, notFoundMiddleware } from '@infrastructure/middleware/error.middleware';
import { swaggerOptions } from '@shared/swagger/openapi.config';

/**
 * OEM Backend Server
 * Operations & Execution Management Module
 */

const app: Application = express();
const PORT = process.env.PORT || 3000;
const HOST = process.env.HOST || 'localhost';

// CORS configuration
const corsOptions = {
  origin: process.env.CORS_ORIGIN || 'http://localhost:4200',
  credentials: process.env.CORS_CREDENTIALS === 'true',
  methods: ['GET', 'POST', 'PUT', 'PATCH', 'DELETE', 'OPTIONS'],
  allowedHeaders: ['Content-Type', 'Authorization', 'X-Org-Id', 'X-User-Id', 'X-Role'],
};

// Middleware
app.use(cors(corsOptions));
app.use(express.json());
app.use(express.urlencoded({ extended: true }));

// Request logging middleware
app.use((req, _res, next) => {
  logger.debug(`${req.method} ${req.path}`, {
    query: req.query,
    body: req.body,
  });
  next();
});

// Swagger API Documentation
const swaggerSpec = swaggerJsdoc(swaggerOptions);
app.use(
  '/api-docs',
  swaggerUi.serve,
  swaggerUi.setup(swaggerSpec, {
    customCss: '.swagger-ui .topbar { display: none }',
    customSiteTitle: 'OEM Backend API Documentation',
  })
);

logger.info('📚 Swagger documentation available at /api-docs');

// Health check endpoint (no authentication required)
/**
 * @swagger
 * /health:
 *   get:
 *     summary: Health check
 *     description: Check if the OEM Backend service is running
 *     tags: [Health]
 *     responses:
 *       200:
 *         description: Service is healthy
 *         content:
 *           application/json:
 *             schema:
 *               type: object
 *               properties:
 *                 success:
 *                   type: boolean
 *                 message:
 *                   type: string
 *                 timestamp:
 *                   type: string
 *                   format: date-time
 *                 environment:
 *                   type: string
 */
app.get('/health', (_req, res) => {
  res.json({
    success: true,
    message: 'OEM Backend is running',
    timestamp: new Date().toISOString(),
    environment: process.env.NODE_ENV || 'development',
  });
});

// API routes
import operationPlanRoutes from '@presentation/routes/operationPlan.routes';
import vesselVisitExecutionRoutes from '@presentation/routes/vesselVisitExecution.routes';
import incidentTypeRoutes from '@presentation/routes/incidentType.routes';
import incidentRoutes from '@presentation/routes/incident.routes';
import taskCategoryRoutes from '@presentation/routes/taskCategory.routes'; // US 4.1.14
import complementaryTaskRoutes from '@presentation/routes/complementaryTask.routes'; // US 4.1.15
import testAuthRoutes from '@presentation/routes/test-auth.routes';
import dockRebalancingRoutes from '@presentation/routes/dockRebalancing.routes'; // US 4.1.4 - Genetic Algorithm

app.use('/api/v1/operation-plans', operationPlanRoutes);
app.use('/api/v1/vessel-visit-executions', vesselVisitExecutionRoutes);
app.use('/api/v1/incident-types', incidentTypeRoutes);
app.use('/api/v1/incidents', incidentRoutes);
app.use('/api/v1/task-categories', taskCategoryRoutes); // US 4.1.14
app.use('/api/v1/complementary-tasks', complementaryTaskRoutes); // US 4.1.15
app.use('/api/v1/test', testAuthRoutes);
app.use('/api/v1', dockRebalancingRoutes); // US 4.1.4 - Dock Rebalancing with Genetic Algorithm

// 404 handler
app.use(notFoundMiddleware);

// Global error handler (must be last)
app.use(errorMiddleware);

// Start server
async function startServer() {
  try {
    // Connect to MongoDB
    await connectDatabase();

    // Start Express server
    app.listen(PORT, () => {
      logger.info(`OEM Backend started successfully`, {
        port: PORT,
        host: HOST,
        environment: process.env.NODE_ENV || 'development',
        url: `http://${HOST}:${PORT}`,
      });
      logger.info(`Health check available at: http://${HOST}:${PORT}/health`);
    });
  } catch (error) {
    logger.error('Failed to start server', error);
    process.exit(1);
  }
}

// Handle unhandled promise rejections
process.on('unhandledRejection', (reason: Error) => {
  logger.error('Unhandled Rejection:', reason);
  process.exit(1);
});

// Handle uncaught exceptions
process.on('uncaughtException', (error: Error) => {
  logger.error('Uncaught Exception:', error);
  process.exit(1);
});

// Start the server
startServer();

export default app;
