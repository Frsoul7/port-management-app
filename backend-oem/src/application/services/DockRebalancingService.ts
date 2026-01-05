import crypto from 'crypto';
import { logger } from '@shared/utils/logger';
import {
  RebalanceProposeRequestDto,
  RebalanceProposalDto,
  DockAssignment,
  DockRebalancingMode
} from '../dtos/DockRebalancingDto';

type ApprovedVVN = {
  id: string;
  vesselId: string;
  provisionalDockId: string;
  desiredDepartureTime: string; // ISO
  arrivalTime: string;          // ISO
  unloadOps: number;            // simplificado (ou TEU/ops)
  loadOps: number;
  vesselType?: string;
  vesselLength?: number;
  vesselDraft?: number;
};

type Dock = {
  id: string;
  allowedVesselTypes?: string[];
  maxLength?: number;
  maxDraft?: number;
  craneCapacity?: number; // nº cranes disponíveis (agregado)
};

export class DockRebalancingService {
  constructor(
    private readonly vvnClient: any,
    private readonly dockClient: any,
    private readonly resourceClient: any,
    private readonly auditRepo: any
  ) {}

  async propose(req: RebalanceProposeRequestDto): Promise<RebalanceProposalDto> {
    const mode: DockRebalancingMode = req.mode ?? 'single';
    const movePenalty = req.params?.movePenalty ?? 0.10;

    // 1) buscar VVNs aprovadas e com dock provisória
    const vvns: ApprovedVVN[] = await this.vvnClient.getApprovedForDay(req.targetDate);
    const filtered = vvns.filter(v => !!v.provisionalDockId);

    // 2) buscar docas
    const docks: Dock[] = await this.dockClient.listAll();

    // 3) calcular cranes disponíveis por dock (resources com assignedArea=dockId)
    const craneCountByDock: Record<string, number> = await this.resourceClient.countCranesByDock();

    const enrichedDocks = docks.map(d => ({
      ...d,
      craneCapacity: craneCountByDock[d.id] ?? 0
    })).filter(d => (d.craneCapacity ?? 0) > 0); // docas sem cranes não entram

    // 4) construir baseline (before)
    const beforeByDock: Record<string, string[]> = {};
    for (const v of filtered) {
      if (!beforeByDock[v.provisionalDockId]) {
        beforeByDock[v.provisionalDockId] = [];
      }
      beforeByDock[v.provisionalDockId].push(v.id);
    }

    // 5) medir “delay before” (estimativa rápida)
    const totalDelayBefore = this.estimateTotalDelay(filtered, beforeByDock, enrichedDocks, mode);

    // 6) gerar proposta (greedy balance)
    const { afterByDock, assignments } = this.greedyRebalance(filtered, enrichedDocks, mode, movePenalty);

    const totalDelayAfter = this.estimateTotalDelay(filtered, afterByDock, enrichedDocks, mode);
    const proposalId = crypto.randomUUID();

    return {
      proposalId,
      targetDate: req.targetDate,
      mode,
      assignments,
      metrics: {
        totalDelayBefore,
        totalDelayAfter,
        improvement: totalDelayBefore - totalDelayAfter,
        movedCount: assignments.length,
        computedAt: new Date().toISOString()
      },
      beforeByDock,
      afterByDock
    };
  }

  async confirm(proposal: RebalanceProposalDto, officerId: string): Promise<{ applied: number }> {
    // aplicar update nas VVNs
    for (const a of proposal.assignments) {
      await this.vvnClient.updateProvisionalDock(a.vvnId, a.toDockId);
    }

    // log/audit
    await this.auditRepo.appendDockRebalanceLog({
      timestamp: new Date().toISOString(),
      officerId,
      targetDate: proposal.targetDate,
      proposalId: proposal.proposalId,
      original: proposal.beforeByDock,
      updated: proposal.afterByDock,
      metrics: proposal.metrics
    });

    logger.info('Dock rebalancing applied', {
      proposalId: proposal.proposalId,
      moved: proposal.assignments.length,
      officerId
    });

    return { applied: proposal.assignments.length };
  }

  // ---------------------------
  // Heuristic core
  // ---------------------------

  private isFeasible(v: ApprovedVVN, d: Dock): boolean {
    if (d.allowedVesselTypes?.length && v.vesselType && !d.allowedVesselTypes.includes(v.vesselType)) return false;
    if (d.maxLength && v.vesselLength && v.vesselLength > d.maxLength) return false;
    if (d.maxDraft && v.vesselDraft && v.vesselDraft > d.maxDraft) return false;
    return true;
  }

  private workload(v: ApprovedVVN): number {
    // simples: número de ops (podes trocar por TEU/duração real se tiveres)
    return (v.unloadOps ?? 0) + (v.loadOps ?? 0);
  }

  private estimateTotalDelay(
    vvns: ApprovedVVN[],
    byDock: Record<string, string[]>,
    docks: Dock[],
    mode: DockRebalancingMode
  ): number {
    // estimativa: cada dock processa em série com throughput ~ cranes (multi melhora throughput)
    // delay = max(0, finishTime - desiredDeparture) agregado (simplificado por workload)
    const dockMap = new Map(docks.map(d => [d.id, d]));
    const vMap = new Map(vvns.map(v => [v.id, v]));

    let total = 0;

    for (const [dockId, ids] of Object.entries(byDock)) {
      const dock = dockMap.get(dockId);
      if (!dock) continue;

      const cranes = Math.max(1, dock.craneCapacity ?? 1);
      const eff = mode === 'multi' ? cranes : 1;

      let timeCursor = 0; // unidade abstrata de tempo
      for (const vvnId of ids) {
        const v = vMap.get(vvnId);
        if (!v) continue;
        timeCursor += this.workload(v) / eff;

        // desired departure em “unidades” -> aqui só medimos relativo: penaliza docks mais carregadas
        const desired = this.workload(v); // placeholder; se tiveres timestamps reais substitui
        total += Math.max(0, timeCursor - desired);
      }
    }
    return Number(total.toFixed(4));
  }

  private greedyRebalance(
    vvns: ApprovedVVN[],
    docks: Dock[],
    mode: DockRebalancingMode,
    movePenalty: number
  ): { afterByDock: Record<string, string[]>; assignments: DockAssignment[] } {
    const afterByDock: Record<string, string[]> = {};
    const assignments: DockAssignment[] = [];

    // ordenar por “peso” descendente
    const sorted = [...vvns].sort((a, b) => this.workload(b) - this.workload(a));

    // estado incremental por dock (carga acumulada)
    const load: Record<string, number> = {};
    for (const d of docks) load[d.id] = 0;

    for (const v of sorted) {
      // escolher melhor dock viável minimizando custo incremental
      let bestDock: Dock | null = null;
      let bestScore = Number.POSITIVE_INFINITY;

      for (const d of docks) {
        if (!this.isFeasible(v, d)) continue;

        const cranes = Math.max(1, d.craneCapacity ?? 1);
        const eff = mode === 'multi' ? cranes : 1;

        const inc = this.workload(v) / eff;
        const moveCost = (v.provisionalDockId && v.provisionalDockId !== d.id) ? movePenalty * this.workload(v) : 0;
        const currentLoad = load[d.id] ?? 0;
        const score = currentLoad + inc + moveCost;

        if (score < bestScore) {
          bestScore = score;
          bestDock = d;
        }
      }

      // fallback: mantém dock atual se nada viável
      const chosenDockId = bestDock?.id ?? v.provisionalDockId;

      if (!afterByDock[chosenDockId]) {
        afterByDock[chosenDockId] = [];
      }
      afterByDock[chosenDockId].push(v.id);

      // atualizar carga
      const dChosen = docks.find(dd => dd.id === chosenDockId);
      const cranes = Math.max(1, dChosen?.craneCapacity ?? 1);
      const eff = mode === 'multi' ? cranes : 1;
      if (!load[chosenDockId]) {
        load[chosenDockId] = 0;
      }
      load[chosenDockId] += this.workload(v) / eff;

      if (v.provisionalDockId && chosenDockId !== v.provisionalDockId) {
        assignments.push({
          vvnId: v.id,
          vesselId: v.vesselId,
          fromDockId: v.provisionalDockId,
          toDockId: chosenDockId
        });
      }
    }

    return { afterByDock, assignments };
  }
}
