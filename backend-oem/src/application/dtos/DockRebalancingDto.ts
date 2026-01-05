export type DockRebalancingMode = 'single' | 'multi';

export interface DockRebalancingParams {
  timeLimitSeconds?: number;   // opcional, para limitar cálculo
  movePenalty?: number;        // penaliza trocas (ex: 0.1)
}

export interface RebalanceProposeRequestDto {
  targetDate: string;               // YYYY-MM-DD
  mode?: DockRebalancingMode;       // single/multi (cranes)
  params?: DockRebalancingParams;
}

export interface DockAssignment {
  vvnId: string;
  vesselId: string;
  fromDockId: string;
  toDockId: string;
}

export interface RebalanceMetrics {
  totalDelayBefore: number;
  totalDelayAfter: number;
  improvement: number; // before - after
  movedCount: number;
  computedAt: string;
}

export interface RebalanceProposalDto {
  proposalId: string;
  targetDate: string;
  mode: DockRebalancingMode;
  assignments: DockAssignment[];
  metrics: RebalanceMetrics;
  beforeByDock: Record<string, string[]>; // dockId -> vvnIds
  afterByDock: Record<string, string[]>;
}

export interface RebalanceConfirmRequestDto {
  proposal: RebalanceProposalDto;
}
