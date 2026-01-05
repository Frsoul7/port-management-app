import { PlanningClient } from '../PlanningClient';
import { PlanningVesselInput, PlanningAlgorithm } from '@shared/types';

describe('PlanningClient (Mock Mode)', () => {
  let client: PlanningClient;

  beforeEach(() => {
    client = new PlanningClient('http://localhost:5000', 30000, true);
  });

  const createMockVessel = (
    name: string,
    arrival: number,
    departure: number,
    unload: number,
    load: number
  ): PlanningVesselInput => ({
    name,
    arrival,
    departure,
    unload,
    load,
  });

  describe('generatePlan - OPTIMAL algorithm', () => {
    it('should generate mock plan for single vessel', async () => {
      const vessels: PlanningVesselInput[] = [
        createMockVessel('VVN-001', 8, 16, 3, 2), // Arrives at 8h, needs 5h total
      ];

      const targetDate = new Date('2025-12-10');
      const result = await client.generatePlan(vessels, PlanningAlgorithm.OPTIMAL, targetDate);

      expect(result.sequence).toHaveLength(1);
      expect(result.sequence[0]?.vessel).toBe('VVN-001');
      expect(result.sequence[0]?.start).toBe(8); // Starts on arrival
      expect(result.sequence[0]?.end).toBe(13); // 8 + 5h
      expect(result.total_delay).toBe(0); // No delay
      expect(result.sequence[0]?.cranes).toBeUndefined(); // Not multi_cranes
    });

    it('should generate mock plan for multiple vessels with sequential scheduling', async () => {
      const vessels: PlanningVesselInput[] = [
        createMockVessel('VVN-001', 8, 16, 2, 2), // Arrives at 8h, needs 4h
        createMockVessel('VVN-002', 9, 17, 3, 1), // Arrives at 9h, needs 4h
        createMockVessel('VVN-003', 10, 18, 2, 1), // Arrives at 10h, needs 3h
      ];

      const targetDate = new Date('2025-12-10');
      const result = await client.generatePlan(vessels, PlanningAlgorithm.OPTIMAL, targetDate);

      expect(result.sequence).toHaveLength(3);

      // VVN-001: starts at 8, ends at 12
      expect(result.sequence[0]?.vessel).toBe('VVN-001');
      expect(result.sequence[0]?.start).toBe(8);
      expect(result.sequence[0]?.end).toBe(12);

      // VVN-002: must wait for VVN-001, starts at 12, ends at 16
      expect(result.sequence[1]?.vessel).toBe('VVN-002');
      expect(result.sequence[1]?.start).toBe(12); // Delayed by 3h (arrived at 9)
      expect(result.sequence[1]?.end).toBe(16);

      // VVN-003: must wait for VVN-002, starts at 16, ends at 19
      expect(result.sequence[2]?.vessel).toBe('VVN-003');
      expect(result.sequence[2]?.start).toBe(16); // Delayed by 6h (arrived at 10)
      expect(result.sequence[2]?.end).toBe(19);

      // Total delay: 0 + 3 + 6 = 9 hours
      expect(result.total_delay).toBe(9);
    });

    it('should handle vessels arriving at different times', async () => {
      const vessels: PlanningVesselInput[] = [
        createMockVessel('VVN-001', 10, 18, 2, 2), // Arrives later
        createMockVessel('VVN-002', 6, 14, 2, 1), // Arrives early
      ];

      const targetDate = new Date('2025-12-10');
      const result = await client.generatePlan(vessels, PlanningAlgorithm.OPTIMAL, targetDate);

      // Should be sorted by arrival time
      expect(result.sequence[0]?.vessel).toBe('VVN-002'); // Arrives at 6h
      expect(result.sequence[0]?.start).toBe(6);
      expect(result.sequence[0]?.end).toBe(9); // 6 + 3h

      expect(result.sequence[1]?.vessel).toBe('VVN-001'); // Arrives at 10h
      expect(result.sequence[1]?.start).toBe(10); // Can start on arrival (9 < 10)
      expect(result.sequence[1]?.end).toBe(14); // 10 + 4h

      expect(result.total_delay).toBe(0); // No delays
    });
  });

  describe('generatePlan - WEIGHTED algorithm', () => {
    it('should generate plan with weighted algorithm (mock)', async () => {
      const vessels: PlanningVesselInput[] = [
        createMockVessel('VVN-001', 8, 16, 3, 2),
        createMockVessel('VVN-002', 9, 17, 2, 1),
      ];

      const targetDate = new Date('2025-12-10');
      const result = await client.generatePlan(vessels, PlanningAlgorithm.WEIGHTED, targetDate);

      expect(result.sequence).toHaveLength(2);
      expect(result.sequence[0]?.cranes).toBeUndefined(); // Not multi_cranes
      expect(result.total_delay).toBeGreaterThanOrEqual(0);
    });
  });

  describe('generatePlan - MULTI_CRANES algorithm', () => {
    it('should generate plan with crane allocation', async () => {
      const vessels: PlanningVesselInput[] = [
        createMockVessel('VVN-001', 8, 16, 4, 4), // Long operation (8h)
        createMockVessel('VVN-002', 9, 17, 2, 1), // Short operation (3h)
      ];

      const targetDate = new Date('2025-12-10');
      const result = await client.generatePlan(vessels, PlanningAlgorithm.MULTI_CRANES, targetDate);

      expect(result.sequence).toHaveLength(2);

      // Should have crane allocation for multi_cranes
      expect(result.sequence[0]?.cranes).toBeDefined();
      expect(result.sequence[0]?.cranes).toBeGreaterThanOrEqual(1);
      expect(result.sequence[0]?.cranes).toBeLessThanOrEqual(3);

      expect(result.sequence[1]?.cranes).toBeDefined();

      // Multi_cranes specific metrics
      expect(result.hours_with_2_cranes).toBeDefined();
      expect(result.total_cranes_needed).toBe(3); // Mock max
    });

    it('should allocate more cranes for longer operations', async () => {
      const vessels: PlanningVesselInput[] = [
        createMockVessel('VVN-001', 8, 20, 6, 6), // Very long (12h)
      ];

      const targetDate = new Date('2025-12-10');
      const result = await client.generatePlan(vessels, PlanningAlgorithm.MULTI_CRANES, targetDate);

      // Long operation should get 3 cranes (12h / 4 = 3)
      expect(result.sequence[0]?.cranes).toBe(3);
    });
  });

  describe('checkHealth', () => {
    it('should return false in mock mode', async () => {
      const isHealthy = await client.checkHealth();

      expect(isHealthy).toBe(false);
    });
  });

  describe('Edge Cases', () => {
    it('should handle empty vessel list', async () => {
      const vessels: PlanningVesselInput[] = [];
      const targetDate = new Date('2025-12-10');

      const result = await client.generatePlan(vessels, PlanningAlgorithm.OPTIMAL, targetDate);

      expect(result.sequence).toHaveLength(0);
      expect(result.total_delay).toBe(0);
    });

    it('should handle single vessel with zero processing time', async () => {
      const vessels: PlanningVesselInput[] = [createMockVessel('VVN-001', 8, 16, 0, 0)];

      const targetDate = new Date('2025-12-10');
      const result = await client.generatePlan(vessels, PlanningAlgorithm.OPTIMAL, targetDate);

      expect(result.sequence).toHaveLength(1);
      expect(result.sequence[0]?.start).toBe(8);
      expect(result.sequence[0]?.end).toBe(8); // Same time
    });

    it('should handle vessels with only unloading', async () => {
      const vessels: PlanningVesselInput[] = [
        createMockVessel('VVN-001', 8, 16, 4, 0), // Only unload
      ];

      const targetDate = new Date('2025-12-10');
      const result = await client.generatePlan(vessels, PlanningAlgorithm.OPTIMAL, targetDate);

      expect(result.sequence[0]?.end).toBe(12); // 8 + 4h unload
    });

    it('should handle vessels with only loading', async () => {
      const vessels: PlanningVesselInput[] = [
        createMockVessel('VVN-001', 8, 16, 0, 3), // Only load
      ];

      const targetDate = new Date('2025-12-10');
      const result = await client.generatePlan(vessels, PlanningAlgorithm.OPTIMAL, targetDate);

      expect(result.sequence[0]?.end).toBe(11); // 8 + 3h load
    });
  });

  describe('Realistic Scenarios', () => {
    it('should handle realistic port operation scenario', async () => {
      const vessels: PlanningVesselInput[] = [
        createMockVessel('MAERSK-001', 6, 18, 3, 2), // Early arrival
        createMockVessel('MSC-002', 8, 20, 4, 3), // Mid-morning
        createMockVessel('CMA-003', 9, 19, 2, 2), // Late morning
        createMockVessel('HAPAG-004', 14, 22, 3, 1), // Afternoon
      ];

      const targetDate = new Date('2025-12-10');
      const result = await client.generatePlan(vessels, PlanningAlgorithm.OPTIMAL, targetDate);

      expect(result.sequence).toHaveLength(4);

      // Verify all vessels are scheduled
      const vesselNames = result.sequence.map((s) => s.vessel);
      expect(vesselNames).toContain('MAERSK-001');
      expect(vesselNames).toContain('MSC-002');
      expect(vesselNames).toContain('CMA-003');
      expect(vesselNames).toContain('HAPAG-004');

      // Verify no overlaps (each starts after previous ends)
      for (let i = 1; i < result.sequence.length; i++) {
        const prevEnd = result.sequence[i - 1]?.end ?? 0;
        const currStart = result.sequence[i]?.start ?? 0;
        expect(currStart).toBeGreaterThanOrEqual(prevEnd);
      }

      // Total delay should be calculated
      expect(result.total_delay).toBeGreaterThanOrEqual(0);
    });
  });
});
