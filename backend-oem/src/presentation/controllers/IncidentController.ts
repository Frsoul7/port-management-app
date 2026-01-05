import { Request, Response, NextFunction } from 'express';
import { IncidentService } from '@application/services/IncidentService';
import { logger } from '@shared/utils/logger';

/**
 * IncidentController
 * Handles HTTP requests for incident recording and management
 *
 * User Story:
 * - US 4.1.13: Record and manage operational incidents
 */
export class IncidentController {
  constructor(private incidentService: IncidentService) {}

  /**
   * US 4.1.13: Create new incident
   * POST /api/v1/incidents
   */
  async createIncident(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const dto = req.body;

      logger.info('Creating incident', { title: dto.title, severity: dto.severity });

      const incident = await this.incidentService.createIncident(dto);

      res.status(201).json({
        success: true,
        message: 'Incident created successfully',
        data: incident,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.13: Get incident by ID
   * GET /api/v1/incidents/:id
   */
  async getIncidentById(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { id } = req.params;

      if (!id) {
        res.status(400).json({ success: false, message: 'Incident ID is required' });
        return;
      }

      logger.info('Fetching incident by ID', { id });

      const incident = await this.incidentService.getIncidentById(id);

      res.json({
        success: true,
        data: incident,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.13: List incidents with optional filtering
   * GET /api/v1/incidents
   * Query params: status, severity, incidentTypeId, vveId, reportedBy, fromDate, toDate, externalEntity, page, limit, sortBy, sortOrder
   */
  async listIncidents(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const query = req.query;

      logger.debug('Listing incidents', query);

      const result = await this.incidentService.listIncidentsPaginated(query);

      res.json({
        success: true,
        data: result.data,
        pagination: result.pagination,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.13: Get active incidents
   * GET /api/v1/incidents/active
   */
  async getActiveIncidents(_req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      logger.info('Fetching active incidents');

      const incidents = await this.incidentService.getActiveIncidents();

      res.json({
        success: true,
        data: incidents,
        count: incidents.length,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.13: Get critical incidents
   * GET /api/v1/incidents/critical
   */
  async getCriticalIncidents(_req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      logger.info('Fetching critical incidents');

      const incidents = await this.incidentService.getCriticalIncidents();

      res.json({
        success: true,
        data: incidents,
        count: incidents.length,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.13: Update incident
   * PUT /api/v1/incidents/:id
   */
  async updateIncident(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { id } = req.params;
      const dto = req.body;

      if (!id) {
        res.status(400).json({ success: false, message: 'Incident ID is required' });
        return;
      }

      logger.info('Updating incident', { id, dto });

      const incident = await this.incidentService.updateIncident(id, dto);

      res.json({
        success: true,
        message: 'Incident updated successfully',
        data: incident,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.13: Delete incident
   * DELETE /api/v1/incidents/:id
   */
  async deleteIncident(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { id } = req.params;

      if (!id) {
        res.status(400).json({ success: false, message: 'Incident ID is required' });
        return;
      }

      logger.info('Deleting incident', { id });

      await this.incidentService.deleteIncident(id);

      res.json({
        success: true,
        message: 'Incident deleted successfully',
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.13: Start investigation
   * PATCH /api/v1/incidents/:id/investigate
   */
  async startInvestigation(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { id } = req.params;
      const dto = req.body;

      if (!id) {
        res.status(400).json({ success: false, message: 'Incident ID is required' });
        return;
      }

      logger.info('Starting investigation for incident', { id, investigatedBy: dto.investigatedBy });

      const incident = await this.incidentService.startInvestigation(id, dto);

      res.json({
        success: true,
        message: 'Investigation started successfully',
        data: incident,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.13: Resolve incident
   * PATCH /api/v1/incidents/:id/resolve
   */
  async resolveIncident(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { id } = req.params;
      const dto = req.body;

      if (!id) {
        res.status(400).json({ success: false, message: 'Incident ID is required' });
        return;
      }

      logger.info('Resolving incident', { id, resolvedBy: dto.resolvedBy });

      const incident = await this.incidentService.resolveIncident(id, dto);

      res.json({
        success: true,
        message: 'Incident resolved successfully',
        data: incident,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.13: Close incident
   * PATCH /api/v1/incidents/:id/close
   */
  async closeIncident(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { id } = req.params;
      const dto = req.body;

      if (!id) {
        res.status(400).json({ success: false, message: 'Incident ID is required' });
        return;
      }

      logger.info('Closing incident', { id, closedBy: dto.closedBy });

      const incident = await this.incidentService.closeIncident(id, dto);

      res.json({
        success: true,
        message: 'Incident closed successfully',
        data: incident,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.13: Add note to incident
   * POST /api/v1/incidents/:id/notes
   */
  async addNote(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { id } = req.params;
      const dto = req.body;

      if (!id) {
        res.status(400).json({ success: false, message: 'Incident ID is required' });
        return;
      }

      logger.info('Adding note to incident', { id, author: dto.author });

      const incident = await this.incidentService.addNote(id, dto);

      res.json({
        success: true,
        message: 'Note added successfully',
        data: incident,
      });
    } catch (error) {
      next(error);
    }
  }

  /**
   * US 4.1.13: Involve external entity
   * POST /api/v1/incidents/:id/external-entities
   */
  async involveExternalEntity(req: Request, res: Response, next: NextFunction): Promise<void> {
    try {
      const { id } = req.params;
      const { entityName } = req.body;

      if (!id) {
        res.status(400).json({ success: false, message: 'Incident ID is required' });
        return;
      }

      logger.info('Involving external entity', { id, entityName });

      const incident = await this.incidentService.involveExternalEntity(id, entityName);

      res.json({
        success: true,
        message: 'External entity involved successfully',
        data: incident,
      });
    } catch (error) {
      next(error);
    }
  }
}
