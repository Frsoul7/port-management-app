/**
 * Domain Ports (Interfaces for External Services)
 * 
 * These interfaces define the contracts for external service integrations.
 * Following the Dependency Inversion Principle (DIP), the domain layer
 * defines these abstractions, and the infrastructure layer provides
 * the concrete implementations.
 */

export { ICoreBackendClient } from './ICoreBackendClient';
export { IPlanningClient } from './IPlanningClient';
