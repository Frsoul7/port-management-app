import { Request, Response } from 'express';
import { DockRebalancingService } from '../../application/services/DockRebalancingService';

export class DockRebalancingController {
  constructor(private readonly service: DockRebalancingService) {}

  propose = async (req: Request, res: Response) => {
    const proposal = await this.service.propose(req.body);
    res.status(200).json(proposal);
  };

  confirm = async (req: Request, res: Response) => {
    const officerId = (req as any).user?.id || 'unknown';
    const result = await this.service.confirm(req.body.proposal, officerId);
    res.status(200).json(result);
  };
}
