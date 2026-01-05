import { Request, Response, NextFunction } from 'express';
import { IncidentTypeService } from '@application/services/IncidentTypeService';
import { logger } from '@shared/utils/logger';

/**
 * IncidentTypeController
 * Handles HTTP requests for Incident Type catalog management
 *
 * User Story:
 * - US 4.1.12: Manage hierarchical incident type catalog
 */
export class IncidentTypeController {
  constructor(private incidentTypeService: IncidentTypeService) {}

  /**
   * US 4.1.12: Create new incident type
   * POST /api/v1/incident-types
   */
  async createIncidentType(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const dto = req.body;

      logger.info('Creating incident type', { code: dto.code, typeName: dto.typeName });

      const incidentType = await this.incidentTypeService.createIncidentType(dto);

      res.status(201).json({
        success: true,
        message: 'Incident type created successfully',
        data: incidentType,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.12: Get incident type by ID
   * GET /api/v1/incident-types/:id
   */
  async getIncidentTypeById(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { id } = req.params;

      logger.info('Fetching incident type by ID', { id });

      const incidentType = await this.incidentTypeService.getIncidentTypeById(id!);

      res.json({
        success: true,
        data: incidentType,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.12: Get incident type by code
   * GET /api/v1/incident-types/code/:code
   */
  async getIncidentTypeByCode(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { code } = req.params;

      logger.info('Fetching incident type by code', { code });

      const incidentType = await this.incidentTypeService.getIncidentTypeByCode(code!);

      res.json({
        success: true,
        data: incidentType,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.12: List all incident types
   * GET /api/v1/incident-types
   * Query params: activeOnly, category, sortBy, sortOrder
   */
  async listIncidentTypes(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { activeOnly, category, sortBy, sortOrder } = req.query;

      logger.debug('Listing incident types', { activeOnly, category });

      const filters = {
        activeOnly: activeOnly === 'true',
        category: category as string | undefined,
      };

      const options = {
        sortBy: sortBy as string,
        sortOrder: sortOrder as 'asc' | 'desc',
      };

      const incidentTypes = await this.incidentTypeService.listIncidentTypes(filters, options);

      res.json({
        success: true,
        data: incidentTypes,
        count: incidentTypes.length,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.12: Get root incident types (no parents)
   * GET /api/v1/incident-types/root
   */
  async getRootIncidentTypes(_req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      logger.info('Fetching root incident types');

      const incidentTypes = await this.incidentTypeService.getRootIncidentTypes();

      res.json({
        success: true,
        data: incidentTypes,
        count: incidentTypes.length,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.12: Get children of a specific incident type
   * GET /api/v1/incident-types/:id/children
   */
  async getChildIncidentTypes(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { id } = req.params;

      logger.info('Fetching child incident types', { parentId: id });

      const incidentTypes = await this.incidentTypeService.getChildIncidentTypes(id!);

      res.json({
        success: true,
        data: incidentTypes,
        count: incidentTypes.length,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.12: Update incident type
   * PUT /api/v1/incident-types/:id
   */
  async updateIncidentType(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { id } = req.params;
      const dto = req.body;

      logger.info('Updating incident type', { id });

      const incidentType = await this.incidentTypeService.updateIncidentType(id!, dto);

      res.json({
        success: true,
        message: 'Incident type updated successfully',
        data: incidentType,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.12: Deactivate incident type
   * POST /api/v1/incident-types/:id/deactivate
   */
  async deactivateIncidentType(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { id } = req.params;

      logger.info('Deactivating incident type', { id });

      const incidentType = await this.incidentTypeService.deactivateIncidentType(id!);

      res.json({
        success: true,
        message: 'Incident type deactivated successfully',
        data: incidentType,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.12: Reactivate incident type
   * POST /api/v1/incident-types/:id/reactivate
   */
  async reactivateIncidentType(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { id } = req.params;

      logger.info('Reactivating incident type', { id });

      const incidentType = await this.incidentTypeService.reactivateIncidentType(id!);

      res.json({
        success: true,
        message: 'Incident type reactivated successfully',
        data: incidentType,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.12: Delete incident type permanently
   * DELETE /api/v1/incident-types/:id
   */
  async deleteIncidentType(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { id } = req.params;

      logger.info('Deleting incident type', { id });

      await this.incidentTypeService.deleteIncidentType(id!);

      res.json({
        success: true,
        message: 'Incident type deleted successfully',
      });
    } catch (error) {
      next(error);
    }
  }
}
