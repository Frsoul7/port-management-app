import { Injectable, inject } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';

export type DockRebalancingMode = 'single' | 'multi';

export interface DockRebalancingParams {
  timeLimitSeconds?: number;
  movePenalty?: number;
}

export interface RebalanceProposeRequest {
  targetDate: string;              // YYYY-MM-DD
  mode?: DockRebalancingMode;      // 'single' | 'multi'
  params?: DockRebalancingParams;  // opcional
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
  improvement: number;
  movedCount: number;
  computedAt: string;
}

export interface RebalanceProposal {
  proposalId: string;
  targetDate: string;
  mode: DockRebalancingMode;
  assignments: DockAssignment[];
  metrics: RebalanceMetrics;
  beforeByDock: Record<string, string[]>;
  afterByDock: Record<string, string[]>;
}

export interface RebalanceConfirmResponse {
  applied: number;
}

@Injectable({
  providedIn: 'root'
})
export class DockRebalancingService {
  private http = inject(HttpClient);

  // ✅ segue o mesmo padrão do teu projeto (hardcoded localhost + /api/v1)
  private apiUrl = 'http://localhost:3000/api/v1/dock-rebalancing';

  /**
   * US 4.3.3: Generate a rebalancing proposal (does NOT apply changes)
   */
  propose(request: RebalanceProposeRequest): Observable<RebalanceProposal> {
    return this.http.post<RebalanceProposal>(`${this.apiUrl}/propose`, request);
  }

  /**
   * US 4.3.3: Confirm/apply a proposal (applies changes + logs)
   */
  confirm(proposal: RebalanceProposal): Observable<RebalanceConfirmResponse> {
    return this.http.post<RebalanceConfirmResponse>(`${this.apiUrl}/confirm`, { proposal });
  }
}
