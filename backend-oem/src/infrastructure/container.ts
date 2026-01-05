/**
 * Dependency Injection Container
 * 
 * This module provides a composition root for the application,
 * centralizing the creation and wiring of all dependencies.
 * 
 * Benefits:
 * - Single point of dependency configuration
 * - Easy to swap implementations (e.g., for testing)
 * - Cleaner route files focused on routing logic
 * - Better testability with reset() method
 */

// Repositories
import { VesselVisitExecutionRepository } from './repositories/VesselVisitExecutionRepository';
import { OperationPlanRepository } from './repositories/OperationPlanRepository';
import { IncidentRepository } from './repositories/IncidentRepository';
import { IncidentTypeRepository } from './repositories/IncidentTypeRepository';
import { ComplementaryTaskRepository } from './repositories/ComplementaryTaskRepository';
import { TaskCategoryRepository } from './repositories/TaskCategoryRepository';

// HTTP Clients
import { CoreBackendClient, getCoreBackendClient } from './http-clients/CoreBackendClient';
import { PlanningClient } from './http-clients/PlanningClient';

// Services
import { VesselVisitExecutionService } from '@application/services/VesselVisitExecutionService';
import { OperationPlanService } from '@application/services/OperationPlanService';
import { IncidentService } from '@application/services/IncidentService';
import { IncidentTypeService } from '@application/services/IncidentTypeService';
import { ComplementaryTaskService } from '@application/services/ComplementaryTaskService';
import { TaskCategoryService } from '@application/services/TaskCategoryService';
import { DockRebalancingService } from '@application/services/DockRebalancingService';

// Controllers
import { VesselVisitExecutionController } from '@presentation/controllers/VesselVisitExecutionController';
import { OperationPlanController } from '@presentation/controllers/OperationPlanController';
import { IncidentController } from '@presentation/controllers/IncidentController';
import { IncidentTypeController } from '@presentation/controllers/IncidentTypeController';
import { ComplementaryTaskController } from '@presentation/controllers/ComplementaryTaskController';
import { TaskCategoryController } from '@presentation/controllers/TaskCategoryController';
import { DockRebalancingController } from '@presentation/controllers/DockRebalancingController';

// Port interfaces
import { ICoreBackendClient, IPlanningClient } from '@domain/ports';
import { IVesselVisitExecutionRepository } from '@domain/repositories/IVesselVisitExecutionRepository';
import { IOperationPlanRepository } from '@domain/repositories/IOperationPlanRepository';
import { IIncidentRepository } from '@domain/repositories/IIncidentRepository';
import { IIncidentTypeRepository } from '@domain/repositories/IIncidentTypeRepository';
import { IComplementaryTaskRepository } from '@domain/repositories/IComplementaryTaskRepository';
import { ITaskCategoryRepository } from '@domain/repositories/ITaskCategoryRepository';

/**
 * Container class for dependency injection
 * Uses singleton pattern with lazy initialization
 */
export class Container {
  // Singleton instances - repositories
  private static _vveRepository: IVesselVisitExecutionRepository | null = null;
  private static _operationPlanRepository: IOperationPlanRepository | null = null;
  private static _incidentRepository: IIncidentRepository | null = null;
  private static _incidentTypeRepository: IIncidentTypeRepository | null = null;
  private static _complementaryTaskRepository: IComplementaryTaskRepository | null = null;
  private static _taskCategoryRepository: ITaskCategoryRepository | null = null;

  // Singleton instances - HTTP clients
  private static _coreBackendClient: ICoreBackendClient | null = null;
  private static _planningClient: IPlanningClient | null = null;

  // Singleton instances - services
  private static _vveService: VesselVisitExecutionService | null = null;
  private static _operationPlanService: OperationPlanService | null = null;
  private static _incidentService: IncidentService | null = null;
  private static _incidentTypeService: IncidentTypeService | null = null;
  private static _complementaryTaskService: ComplementaryTaskService | null = null;
  private static _taskCategoryService: TaskCategoryService | null = null;
  private static _dockRebalancingService: DockRebalancingService | null = null;

  // Singleton instances - controllers
  private static _vveController: VesselVisitExecutionController | null = null;
  private static _operationPlanController: OperationPlanController | null = null;
  private static _incidentController: IncidentController | null = null;
  private static _incidentTypeController: IncidentTypeController | null = null;
  private static _complementaryTaskController: ComplementaryTaskController | null = null;
  private static _taskCategoryController: TaskCategoryController | null = null;
  private static _dockRebalancingController: DockRebalancingController | null = null;

  // ==================== Repositories ====================

  static getVveRepository(): IVesselVisitExecutionRepository {
    if (!this._vveRepository) {
      this._vveRepository = new VesselVisitExecutionRepository();
    }
    return this._vveRepository;
  }

  static getOperationPlanRepository(): IOperationPlanRepository {
    if (!this._operationPlanRepository) {
      this._operationPlanRepository = new OperationPlanRepository();
    }
    return this._operationPlanRepository;
  }

  static getIncidentRepository(): IIncidentRepository {
    if (!this._incidentRepository) {
      this._incidentRepository = new IncidentRepository();
    }
    return this._incidentRepository;
  }

  static getIncidentTypeRepository(): IIncidentTypeRepository {
    if (!this._incidentTypeRepository) {
      this._incidentTypeRepository = new IncidentTypeRepository();
    }
    return this._incidentTypeRepository;
  }

  static getComplementaryTaskRepository(): IComplementaryTaskRepository {
    if (!this._complementaryTaskRepository) {
      this._complementaryTaskRepository = new ComplementaryTaskRepository();
    }
    return this._complementaryTaskRepository;
  }

  static getTaskCategoryRepository(): ITaskCategoryRepository {
    if (!this._taskCategoryRepository) {
      this._taskCategoryRepository = new TaskCategoryRepository();
    }
    return this._taskCategoryRepository;
  }

  // ==================== HTTP Clients ====================

  static getCoreBackendClient(): ICoreBackendClient {
    if (!this._coreBackendClient) {
      this._coreBackendClient = getCoreBackendClient();
    }
    return this._coreBackendClient;
  }

  static getPlanningClient(): IPlanningClient {
    if (!this._planningClient) {
      this._planningClient = new PlanningClient();
    }
    return this._planningClient;
  }

  // ==================== Services ====================

  static getVveService(): VesselVisitExecutionService {
    if (!this._vveService) {
      this._vveService = new VesselVisitExecutionService(
        this.getVveRepository(),
        this.getOperationPlanRepository()
      );
    }
    return this._vveService;
  }

  static getOperationPlanService(): OperationPlanService {
    if (!this._operationPlanService) {
      this._operationPlanService = new OperationPlanService(
        this.getOperationPlanRepository(),
        this.getCoreBackendClient(),
        this.getPlanningClient()
      );
    }
    return this._operationPlanService;
  }

  static getIncidentService(): IncidentService {
    if (!this._incidentService) {
      this._incidentService = new IncidentService(this.getIncidentRepository());
    }
    return this._incidentService;
  }

  static getIncidentTypeService(): IncidentTypeService {
    if (!this._incidentTypeService) {
      this._incidentTypeService = new IncidentTypeService(this.getIncidentTypeRepository());
    }
    return this._incidentTypeService;
  }

  static getComplementaryTaskService(): ComplementaryTaskService {
    if (!this._complementaryTaskService) {
      this._complementaryTaskService = new ComplementaryTaskService(
        this.getComplementaryTaskRepository(),
        this.getTaskCategoryRepository()
      );
    }
    return this._complementaryTaskService;
  }

  static getTaskCategoryService(): TaskCategoryService {
    if (!this._taskCategoryService) {
      this._taskCategoryService = new TaskCategoryService(this.getTaskCategoryRepository());
    }
    return this._taskCategoryService;
  }

  // ==================== Controllers ====================

  static getVveController(): VesselVisitExecutionController {
    if (!this._vveController) {
      this._vveController = new VesselVisitExecutionController(this.getVveService());
    }
    return this._vveController;
  }

  static getOperationPlanController(): OperationPlanController {
    if (!this._operationPlanController) {
      this._operationPlanController = new OperationPlanController(this.getOperationPlanService());
    }
    return this._operationPlanController;
  }

  static getIncidentController(): IncidentController {
    if (!this._incidentController) {
      this._incidentController = new IncidentController(this.getIncidentService());
    }
    return this._incidentController;
  }

  static getIncidentTypeController(): IncidentTypeController {
    if (!this._incidentTypeController) {
      this._incidentTypeController = new IncidentTypeController(this.getIncidentTypeService());
    }
    return this._incidentTypeController;
  }

  static getComplementaryTaskController(): ComplementaryTaskController {
    if (!this._complementaryTaskController) {
      this._complementaryTaskController = new ComplementaryTaskController(
        this.getComplementaryTaskService()
      );
    }
    return this._complementaryTaskController;
  }

  static getTaskCategoryController(): TaskCategoryController {
    if (!this._taskCategoryController) {
      this._taskCategoryController = new TaskCategoryController(this.getTaskCategoryService());
    }
    return this._taskCategoryController;
  }

  // ==================== DockRebalancing - US 4.1.4 ====================

  static getDockRebalancingService(): DockRebalancingService {
    if (!this._dockRebalancingService) {
      this._dockRebalancingService = new DockRebalancingService(
        this.getCoreBackendClient(), // vvnClient
        this.getCoreBackendClient(), // dockClient
        this.getCoreBackendClient(), // resourceClient
        null // auditRepo - will be added later if needed
      );
    }
    return this._dockRebalancingService;
  }

  static getDockRebalancingController(): DockRebalancingController {
    if (!this._dockRebalancingController) {
      this._dockRebalancingController = new DockRebalancingController(this.getDockRebalancingService());
    }
    return this._dockRebalancingController;
  }

  // ==================== Testing Support ====================

  /**
   * Reset all singleton instances
   * Useful for testing to ensure clean state between tests
   */
  static reset(): void {
    // Reset repositories
    this._vveRepository = null;
    this._operationPlanRepository = null;
    this._incidentRepository = null;
    this._incidentTypeRepository = null;
    this._complementaryTaskRepository = null;
    this._taskCategoryRepository = null;

    // Reset HTTP clients
    this._coreBackendClient = null;
    this._planningClient = null;

    // Reset services
    this._vveService = null;
    this._operationPlanService = null;
    this._incidentService = null;
    this._incidentTypeService = null;
    this._complementaryTaskService = null;
    this._taskCategoryService = null;
    this._dockRebalancingService = null;

    // Reset controllers
    this._vveController = null;
    this._operationPlanController = null;
    this._incidentController = null;
    this._incidentTypeController = null;
    this._complementaryTaskController = null;
    this._taskCategoryController = null;
    this._dockRebalancingController = null;
  }

  /**
   * Allow injecting mock dependencies for testing
   */
  static setVveRepository(repo: IVesselVisitExecutionRepository): void {
    this._vveRepository = repo;
    this._vveService = null; // Force recreation of dependent services
    this._vveController = null;
  }

  static setOperationPlanRepository(repo: IOperationPlanRepository): void {
    this._operationPlanRepository = repo;
    this._operationPlanService = null;
    this._operationPlanController = null;
    this._vveService = null;
    this._vveController = null;
  }

  static setIncidentRepository(repo: IIncidentRepository): void {
    this._incidentRepository = repo;
    this._incidentService = null;
    this._incidentController = null;
  }

  static setIncidentTypeRepository(repo: IIncidentTypeRepository): void {
    this._incidentTypeRepository = repo;
    this._incidentTypeService = null;
    this._incidentTypeController = null;
  }

  static setComplementaryTaskRepository(repo: IComplementaryTaskRepository): void {
    this._complementaryTaskRepository = repo;
    this._complementaryTaskService = null;
    this._complementaryTaskController = null;
  }

  static setTaskCategoryRepository(repo: ITaskCategoryRepository): void {
    this._taskCategoryRepository = repo;
    this._taskCategoryService = null;
    this._taskCategoryController = null;
    this._complementaryTaskService = null;
    this._complementaryTaskController = null;
  }

  static setCoreBackendClient(client: ICoreBackendClient): void {
    this._coreBackendClient = client;
    this._operationPlanService = null;
    this._operationPlanController = null;
  }

  static setPlanningClient(client: IPlanningClient): void {
    this._planningClient = client;
    this._operationPlanService = null;
    this._operationPlanController = null;
  }
}
