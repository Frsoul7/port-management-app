import { PlanningVesselInput, PlanningResponse, PlanningAlgorithm } from '@shared/types';

/**
 * Port interface for Planning Engine integration
 * Defines the contract for generating operation plans using scheduling algorithms
 * 
 * This interface follows the Dependency Inversion Principle (DIP):
 * - Domain/Application layers depend on this abstraction
 * - Infrastructure layer provides the concrete implementation (Prolog engine or mock)
 */
export interface IPlanningClient {
  /**
   * Generate operation plan using specified algorithm
   * @param vessels - Array of vessel visit data for planning
   * @param algorithm - Planning algorithm to use (optimal, weighted, multi_cranes)
   * @param targetDate - Target date for the plan
   * @returns Planning response with sequence and delay information
   */
  generatePlan(
    vessels: PlanningVesselInput[],
    algorithm: PlanningAlgorithm,
    targetDate: Date
  ): Promise<PlanningResponse>;

  /**
   * Check health/availability of planning engine
   * @returns true if planning engine is available, false otherwise
   */
  checkHealth(): Promise<boolean>;
}
