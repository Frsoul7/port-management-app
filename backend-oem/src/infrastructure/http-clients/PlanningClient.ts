import { PlanningVesselInput, PlanningResponse, PlanningAlgorithm } from '@shared/types';
import { logger } from '@shared/utils/logger';
import { IPlanningClient } from '@domain/ports';

/**
 * ⚠️ PLACEHOLDER for Prolog Planning Engine Integration
 *
 * This client defines the interface for interacting with the Prolog-based
 * planning engine (SWI-Prolog with HTTP server at http://localhost:5000).
 *
 * IMPORTANT: This is NOT yet implemented. The actual HTTP integration will
 * be completed in a future sprint when the Prolog planning service is ready.
 *
 * For now, this class provides:
 * 1. Interface definition for the planning engine contract
 * 2. Mock responses for testing and development
 * 3. Documentation of expected request/response formats
 *
 * FUTURE IMPLEMENTATION NOTES:
 * - The Prolog engine will expose REST endpoints for each planning algorithm:
 *   - POST /optimal - For optimal algorithm
 *   - POST /weighted - For weighted algorithm
 *   - POST /multi_cranes - For multi-cranes algorithm
 * - Request format: { vessels: PlanningVesselInput[], date: string }
 * - Response format: PlanningResponse (defined in @shared/types)
 * - Will require axios HTTP client similar to CoreBackendClient
 * - Should include retry logic for resilience
 * - Need proper error handling for Prolog engine failures
 * 
 * Implements IPlanningClient interface for Clean Architecture compliance
 */
export class PlanningClient implements IPlanningClient {
  // @ts-ignore
  private readonly baseURL: string;
  // @ts-ignore
  private readonly timeout: number;
  private readonly mockMode: boolean;

  constructor(
    baseURL: string = 'http://localhost:5000',
    timeout: number = 30000,
    mockMode: boolean = true // Currently forced to true
  ) {
    this.baseURL = baseURL;
    this.timeout = timeout;
    this.mockMode = mockMode; // Always true until Prolog integration is ready

    logger.warn('PlanningClient initialized in MOCK MODE. Prolog integration not yet implemented.');
  }

  /**
   * Generate operation plan using specified algorithm
   *
   * CURRENT: Returns mock data for testing
   * FUTURE: Will make HTTP POST request to Prolog planning engine
   *
   * @param vessels - Array of vessel visit data for planning
   * @param algorithm - Planning algorithm to use (optimal, weighted, multi_cranes)
   * @param targetDate - Target date for the plan
   * @returns Planning response with sequence and delay information
   */
  async generatePlan(
    vessels: PlanningVesselInput[],
    algorithm: PlanningAlgorithm,
    _targetDate: Date // Prefix with _ to indicate future use
  ): Promise<PlanningResponse> {
    logger.info(
      `PlanningClient: Generating plan with ${algorithm} algorithm for ${vessels.length} vessels`
    );

    // MOCK MODE: Return simulated planning results
    if (this.mockMode) {
      return this.generateMockPlan(vessels, algorithm);
    }

    // FUTURE IMPLEMENTATION:
    // const endpoint = this.getAlgorithmEndpoint(algorithm);
    // const response = await this.client.post<PlanningResponse>(endpoint, {
    //   vessels,
    //   date: targetDate.toISOString().split('T')[0]
    // });
    // return response.data;

    throw new Error('Prolog integration not yet implemented. PlanningClient is in mock mode only.');
  }

  /**
   * Check health/availability of Prolog planning engine
   *
   * CURRENT: Always returns false (service not available)
   * FUTURE: Will ping Prolog server health endpoint
   *
   * @returns true if Prolog engine is available, false otherwise
   */
  async checkHealth(): Promise<boolean> {
    if (this.mockMode) {
      logger.debug('PlanningClient health check: MOCK MODE - returning false');
      return false;
    }

    // FUTURE IMPLEMENTATION:
    // try {
    //   const response = await this.client.get('/health', { timeout: 5000 });
    //   return response.status === 200;
    // } catch (error) {
    //   logger.error('Prolog planning engine health check failed:', error);
    //   return false;
    // }

    return false;
  }

  /**
   * MOCK: Generate simulated planning results for testing
   *
   * This creates realistic mock data that follows the same structure
   * that the real Prolog engine will return.
   *
   * Mock logic:
   * - Sorts vessels by arrival time
   * - Assigns sequential time slots with minimal gaps
   * - Calculates delays based on ETA vs actual start time
   * - For multi_cranes: simulates crane allocation decisions
   */
  private generateMockPlan(
    vessels: PlanningVesselInput[],
    algorithm: PlanningAlgorithm
  ): PlanningResponse {
    logger.debug(`Generating MOCK plan for ${vessels.length} vessels using ${algorithm} algorithm`);

    // Sort vessels by arrival time
    const sortedVessels = [...vessels].sort((a, b) => a.arrival - b.arrival);

    let currentTime = 0;
    let totalDelay = 0;
    const sequence = sortedVessels.map((vessel) => {
      // Start time is the later of: current time or vessel arrival
      const start = Math.max(currentTime, vessel.arrival);

      // Calculate processing time (unload + load)
      const processingTime = vessel.unload + vessel.load;
      const end = start + processingTime;

      // Calculate delay (difference between scheduled start and actual arrival)
      const delay = Math.max(0, start - vessel.arrival);
      totalDelay += delay;

      // Update current time for next vessel
      currentTime = end;

      // For multi_cranes algorithm, assign crane count (mock: 1-3 cranes)
      const cranes =
        algorithm === PlanningAlgorithm.MULTI_CRANES
          ? Math.min(3, Math.max(1, Math.ceil(processingTime / 4)))
          : undefined;

      return {
        vessel: vessel.name,
        start,
        end,
        cranes,
      };
    });

    // Additional metrics for multi_cranes algorithm
    const multiCranesMetrics =
      algorithm === PlanningAlgorithm.MULTI_CRANES
        ? {
            hours_with_2_cranes: Math.floor(currentTime * 0.3), // Mock: ~30% of time
            total_cranes_needed: 3, // Mock: maximum cranes used
          }
        : {};

    const response: PlanningResponse = {
      sequence,
      total_delay: totalDelay,
      ...multiCranesMetrics,
    };

    logger.info(`MOCK plan generated: ${sequence.length} operations, total delay: ${totalDelay}h`);

    return response;
  }

  /**
   * FUTURE: Map algorithm enum to Prolog endpoint
   *
   * This will be used when actual HTTP integration is implemented.
   */
  // @ts-ignore
  private getAlgorithmEndpoint(algorithm: PlanningAlgorithm): string {
    switch (algorithm) {
      case PlanningAlgorithm.OPTIMAL:
        return '/optimal';
      case PlanningAlgorithm.WEIGHTED:
        return '/weighted';
      case PlanningAlgorithm.MULTI_CRANES:
        return '/multi_cranes';
      default:
        throw new Error(`Unknown planning algorithm: ${algorithm}`);
    }
  }
}

/**
 * Singleton instance of PlanningClient (MOCK MODE)
 */
export const planningClient = new PlanningClient(
  process.env.PROLOG_SERVICE_URL || 'http://localhost:5000',
  30000,
  true // Force mock mode
);
