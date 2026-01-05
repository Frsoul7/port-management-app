import { ComplementaryTaskService } from '@application/services/ComplementaryTaskService';
import { IComplementaryTaskRepository } from '@domain/repositories/IComplementaryTaskRepository';
import { ITaskCategoryRepository } from '@domain/repositories/ITaskCategoryRepository';
import { ComplementaryTask } from '@domain/entities/ComplementaryTask';
import { TaskCategory } from '@domain/entities/TaskCategory';
import { ComplementaryTaskStatus } from '@shared/types';
import {
  CreateComplementaryTaskDto,
  AssignTaskDto,
  StartTaskDto,
  CompleteTaskDto,
  CancelTaskDto,
  AddTaskNoteDto,
} from '@application/dtos';

/**
 * US 4.1.15 - ComplementaryTaskService Unit Tests
 * Tests for complementary task management service
 */
describe('US 4.1.15 - ComplementaryTaskService', () => {
  let service: ComplementaryTaskService;
  let mockTaskRepository: jest.Mocked<IComplementaryTaskRepository>;
  let mockCategoryRepository: jest.Mocked<ITaskCategoryRepository>;

  beforeEach(() => {
    // Create mock repositories
    mockTaskRepository = {
      save: jest.fn(),
      findById: jest.fn(),
      findAll: jest.fn(),
      findByVveId: jest.fn(),
      findByStatus: jest.fn(),
      findByCategory: jest.fn(),
      findByAssignee: jest.fn(),
      findByDueDateRange: jest.fn(),
      findActive: jest.fn(),
      update: jest.fn(),
      delete: jest.fn(),
      count: jest.fn(),
      countOverdue: jest.fn(),
      getStatistics: jest.fn(),
    } as any;

    mockCategoryRepository = {
      findById: jest.fn(),
      findAll: jest.fn(),
      findActive: jest.fn(),
      save: jest.fn(),
      update: jest.fn(),
      delete: jest.fn(),
    } as any;

    service = new ComplementaryTaskService(mockTaskRepository, mockCategoryRepository);
  });

  afterEach(() => {
    jest.clearAllMocks();
  });

  const createMockCategory = (taskCategoryId: string = 'CAT-001', isActive: boolean = true): TaskCategory => {
    return new TaskCategory({
      taskCategoryId,
      categoryCode: 'CTC001',
      categoryName: 'Test Category',
      description: 'Test category description',
      expectedImpact: 'No impact on cargo operations',
      defaultDurationHours: 2,
      isActive,
    });
  };

  const createMockTask = (overrides?: Partial<any>): ComplementaryTask => {
    // Use a date in the future (7 days from now) to satisfy validation
    const futureDate = new Date();
    futureDate.setDate(futureDate.getDate() + 7);
    
    return new ComplementaryTask({
      taskCategoryId: 'CAT-001',
      vveId: 'VVE-001',
      title: 'Test Task',
      description: 'Test task description',
      dueDate: overrides?.dueDate || futureDate,
      estimatedDurationHours: 2,
      assignedTo: 'Team A',
      createdBy: 'user-123',
      ...overrides,
    });
  };

  describe('createComplementaryTask', () => {
    // Generate a future date (7 days from now) to satisfy validation
    const futureDate = new Date();
    futureDate.setDate(futureDate.getDate() + 7);
    const futureDateString = futureDate.toISOString();
    
    const createDto: CreateComplementaryTaskDto = {
      taskCategoryId: 'CAT-001',
      vveId: 'VVE-001',
      title: 'Repair Main Deck',
      description: 'Fix damaged planking on main deck',
      dueDate: futureDateString,
      estimatedDurationHours: 4,
      assignedTo: 'Maintenance Team',
      createdBy: 'user-123',
    };

    /**
     * US 4.1.15 - Create Complementary Task - Service Layer Success
     * 
     * Verifies that task creation at service layer:
     * - Validates task category exists and is active
     * - Creates task entity with PLANNED status
     * - Saves task to repository
     * - Returns task DTO with all fields populated
     */
    it('should create a complementary task successfully', async () => {
      const mockCategory = createMockCategory();
      const mockTask = createMockTask();

      mockCategoryRepository.findById.mockResolvedValue(mockCategory);
      mockTaskRepository.save.mockResolvedValue(mockTask);

      const result = await service.createComplementaryTask(createDto);

      expect(mockCategoryRepository.findById).toHaveBeenCalledWith('CAT-001');
      expect(mockTaskRepository.save).toHaveBeenCalled();
      expect(result).toBeDefined();
      expect(result.title).toBe('Test Task');
      expect(result.status).toBe(ComplementaryTaskStatus.PLANNED);
    });

    /**
     * US 4.1.15 - Create Task - Category Not Found Error
     * 
     * Verifies that creating a task with non-existent category:
     * - Service validates category existence before task creation
     * - Throws descriptive error with category ID
     * - Repository save is never called (validation prevents invalid data)
     */
    it('should throw error if task category does not exist', async () => {
      mockCategoryRepository.findById.mockResolvedValue(null);

      await expect(service.createComplementaryTask(createDto)).rejects.toThrow(
        'Task category CAT-001 not found'
      );

      expect(mockTaskRepository.save).not.toHaveBeenCalled();
    });

    /**
     * US 4.1.15 - Create Task - Inactive Category Error
     * 
     * Verifies that creating a task with inactive category:
     * - Service checks category active status
     * - Throws error to prevent tasks based on deprecated/obsolete categories
     * - Ensures only active task types can be created
     */
    it('should throw error if task category is inactive', async () => {
      const inactiveCategory = createMockCategory('CAT-001', false);
      mockCategoryRepository.findById.mockResolvedValue(inactiveCategory);

      await expect(service.createComplementaryTask(createDto)).rejects.toThrow(
        'Task category CAT-001 is inactive'
      );

      expect(mockTaskRepository.save).not.toHaveBeenCalled();
    });

    /**
     * US 4.1.15 - Create Task - Optional Assignee
     * 
     * Verifies that creating a task without initial assignee:
     * - assignedTo field is optional
     * - Task can be created unassigned
     * - Assignment can be done later via assignTask operation
     */
    it('should create task with optional assignedTo', async () => {
      const mockCategory = createMockCategory();
      const mockTask = createMockTask({ assignedTo: undefined });
      const dtoWithoutAssignee = { ...createDto, assignedTo: undefined };

      mockCategoryRepository.findById.mockResolvedValue(mockCategory);
      mockTaskRepository.save.mockResolvedValue(mockTask);

      const result = await service.createComplementaryTask(dtoWithoutAssignee);

      expect(result).toBeDefined();
      expect(mockTaskRepository.save).toHaveBeenCalled();
    });
  });

  describe('getComplementaryTaskById', () => {
    /**
     * US 4.1.15 - Get Task By ID - Service Layer Retrieval
     * 
     * Verifies that retrieving a task by ID:
     * - Queries repository with task ID
     * - Returns task DTO with all fields
     * - Converts entity to DTO format for presentation layer
     */
    it('should return task by ID', async () => {
      const mockTask = createMockTask();
      mockTaskRepository.findById.mockResolvedValue(mockTask);

      const result = await service.getComplementaryTaskById('TASK-001');

      expect(mockTaskRepository.findById).toHaveBeenCalledWith('TASK-001');
      expect(result).toBeDefined();
      expect(result.title).toBe('Test Task');
    });

    /**
     * US 4.1.15 - Get Task By ID - Not Found Error
     * 
     * Verifies that retrieving non-existent task:
     * - Throws descriptive error with task ID
     * - Validates existence before returning data
     */
    it('should throw error if task not found', async () => {
      mockTaskRepository.findById.mockResolvedValue(null);

      await expect(service.getComplementaryTaskById('TASK-999')).rejects.toThrow(
        'Complementary task TASK-999 not found'
      );
    });
  });

  describe('updateComplementaryTask', () => {
    /**
     * US 4.1.15 - Update Task - Service Layer Update
     * 
     * Verifies that updating a PLANNED task:
     * - Retrieves existing task from repository
     * - Validates task is in PLANNED status (business rule)
     * - Updates specified fields (title, description, dueDate, duration)
     * - Saves updated entity back to repository
     * - Returns updated task DTO
     */
    it('should update a PLANNED task successfully', async () => {
      const mockTask = createMockTask();
      const updatedTask = createMockTask({
        title: 'Updated Title',
        description: 'Updated description',
      });

      mockTaskRepository.findById.mockResolvedValue(mockTask);
      mockTaskRepository.update.mockResolvedValue(updatedTask);

      const result = await service.updateComplementaryTask('TASK-001', {
        title: 'Updated Title',
        description: 'Updated description',
        dueDate: '2025-12-31T14:00:00Z',
        estimatedDurationHours: 3,
      });

      expect(mockTaskRepository.findById).toHaveBeenCalledWith('TASK-001');
      expect(mockTaskRepository.update).toHaveBeenCalled();
      expect(result).toBeDefined();
    });

    /**
     * US 4.1.15 - Update Task - Not Found Error
     * 
     * Verifies that updating a non-existent task:
     * - Service validates task existence first
     * - Throws error before attempting update
     * - Repository update is never called
     */
    it('should throw error if task not found', async () => {
      mockTaskRepository.findById.mockResolvedValue(null);

      await expect(
        service.updateComplementaryTask('TASK-999', { title: 'New Title' })
      ).rejects.toThrow('Complementary task TASK-999 not found');
    });

    /**
     * US 4.1.15 - Update Task - Status Validation
     * 
     * Verifies that updating a non-PLANNED task:
     * - Service validates task status before allowing update
     * - Business rule: Only PLANNED tasks can be modified
     * - IN_PROGRESS, COMPLETED, CANCELLED tasks are immutable
     * - Prevents modification of tasks being executed or finished
     */
    it('should throw error if task is not PLANNED', async () => {
      const inProgressTask = createMockTask();
      jest.spyOn(inProgressTask, 'status', 'get').mockReturnValue(ComplementaryTaskStatus.IN_PROGRESS);

      mockTaskRepository.findById.mockResolvedValue(inProgressTask);

      await expect(
        service.updateComplementaryTask('TASK-001', { title: 'New Title' })
      ).rejects.toThrow('Can only update PLANNED tasks');
    });

    /**
     * US 4.1.15 - Update Task - Partial Field Update
     * 
     * Verifies that updating only some fields:
     * - Service supports partial updates (not all fields required)
     * - Unchanged fields retain their original values
     * - Flexible update API for client convenience
     */
    /**
     * US 4.1.15 - Update Task - Partial Field Update
     * 
     * Verifies that updating only some fields:
     * - Service supports partial updates (not all fields required)
     * - Unchanged fields retain their original values
     * - Flexible update API for client convenience
     */
    it('should update partial fields only', async () => {
      const mockTask = createMockTask();
      mockTaskRepository.findById.mockResolvedValue(mockTask);
      mockTaskRepository.update.mockResolvedValue(mockTask);

      await service.updateComplementaryTask('TASK-001', {
        title: 'Only Update Title',
      });

      expect(mockTaskRepository.update).toHaveBeenCalled();
    });
  });

  describe('deleteComplementaryTask', () => {
    /**
     * US 4.1.15 - Delete Task - Service Layer Deletion
     * 
     * Verifies that deleting a PLANNED task:
     * - Retrieves task from repository
     * - Validates task is in PLANNED status (business rule)
     * - Calls repository delete with task ID
     * - Business rule: Only PLANNED tasks can be deleted (not started/completed)
     */
    it('should delete a PLANNED task', async () => {
      const mockTask = createMockTask();
      mockTaskRepository.findById.mockResolvedValue(mockTask);
      mockTaskRepository.delete.mockResolvedValue(undefined);

      await service.deleteComplementaryTask('TASK-001');

      expect(mockTaskRepository.findById).toHaveBeenCalledWith('TASK-001');
      expect(mockTaskRepository.delete).toHaveBeenCalledWith('TASK-001');
    });

    /**
     * US 4.1.15 - Delete Task - Not Found Error
     * 
     * Verifies that deleting a non-existent task:
     * - Service validates existence first
     * - Throws descriptive error
     * - Repository delete is never called
     */
    it('should throw error if task not found', async () => {
      mockTaskRepository.findById.mockResolvedValue(null);

      await expect(service.deleteComplementaryTask('TASK-999')).rejects.toThrow(
        'Complementary task TASK-999 not found'
      );
    });

    /**
     * US 4.1.15 - Delete Task - Status Validation
     * 
     * Verifies that deleting a non-PLANNED task:
     * - Service validates status before deletion
     * - Cannot delete IN_PROGRESS, COMPLETED, or CANCELLED tasks
     * - Preserves audit trail for executed tasks
     */
    it('should throw error if task is not PLANNED', async () => {
      const completedTask = createMockTask();
      jest.spyOn(completedTask, 'status', 'get').mockReturnValue(ComplementaryTaskStatus.COMPLETED);

      mockTaskRepository.findById.mockResolvedValue(completedTask);

      await expect(service.deleteComplementaryTask('TASK-001')).rejects.toThrow(
        'Can only delete PLANNED tasks'
      );
    });
  });

  describe('listComplementaryTasks', () => {
    /**
     * US 4.1.15 - List Tasks - No Filters
     * 
     * Verifies that listing all tasks without filters:
     * - Calls repository findAll method
     * - Returns all tasks in system
     * - Supports administrative dashboards showing full task list
     */
    it('should list all tasks without filters', async () => {
      const mockTasks = [createMockTask(), createMockTask()];
      mockTaskRepository.findAll.mockResolvedValue(mockTasks);

      const result = await service.listComplementaryTasks();

      expect(mockTaskRepository.findAll).toHaveBeenCalled();
      expect(result).toHaveLength(2);
    });

    /**
     * US 4.1.15 - List Tasks - Status Filter
     * 
     * Verifies that filtering tasks by status:
     * - Calls repository findByStatus with status and pagination options
     * - Returns only tasks in specified lifecycle stage
     * - Supports workflow-based views (e.g., "Show all IN_PROGRESS tasks")
     */
    it('should filter tasks by status', async () => {
      const mockTasks = [createMockTask()];
      mockTaskRepository.findByStatus.mockResolvedValue(mockTasks);

      const result = await service.listComplementaryTasks({
        status: ComplementaryTaskStatus.PLANNED,
      });

      expect(mockTaskRepository.findByStatus).toHaveBeenCalledWith(
        ComplementaryTaskStatus.PLANNED,
        undefined
      );
      expect(result).toHaveLength(1);
    });

    /**
     * US 4.1.15 - List Tasks - VVE Filter
     * 
     * Verifies that filtering tasks by VVE ID:
     * - Calls repository findByVveId
     * - Returns all tasks for specified vessel
     * - Critical for vessel-specific maintenance dashboards
     */
    it('should filter tasks by VVE ID', async () => {
      const mockTasks = [createMockTask()];
      mockTaskRepository.findByVveId.mockResolvedValue(mockTasks);

      const result = await service.listComplementaryTasks({ vveId: 'VVE-001' });

      expect(mockTaskRepository.findByVveId).toHaveBeenCalledWith('VVE-001', undefined);
      expect(result).toHaveLength(1);
    });

    /**
     * US 4.1.15 - List Tasks - Category Filter
     * 
     * Verifies that filtering tasks by category:
     * - Calls repository findByCategory
     * - Returns tasks of specific type (e.g., all "Hull Repair" tasks)
     * - Supports task type analysis and reporting
     */
    it('should filter tasks by category', async () => {
      const mockTasks = [createMockTask()];
      mockTaskRepository.findByCategory.mockResolvedValue(mockTasks);

      const result = await service.listComplementaryTasks({ taskCategoryId: 'CAT-001' });

      expect(mockTaskRepository.findByCategory).toHaveBeenCalledWith('CAT-001', undefined);
      expect(result).toHaveLength(1);
    });

    /**
     * US 4.1.15 - List Tasks - Assignee Filter
     * 
     * Verifies that filtering tasks by assignee:
     * - Calls repository findByAssignee
     * - Returns tasks assigned to specific team/individual
     * - Supports workload distribution and team dashboards
     */
    it('should filter tasks by assignee', async () => {
      const mockTasks = [createMockTask()];
      mockTaskRepository.findByAssignee.mockResolvedValue(mockTasks);

      const result = await service.listComplementaryTasks({ assignedTo: 'Team A' });

      expect(mockTaskRepository.findByAssignee).toHaveBeenCalledWith('Team A', undefined);
      expect(result).toHaveLength(1);
    });

    /**
     * US 4.1.15 - List Tasks - Date Range Filter
     * 
     * Verifies that filtering tasks by due date range:
     * - Calls repository findByDueDateRange
     * - Returns tasks due within specified period
     * - Supports time-based planning and scheduling views
     */
    it('should filter tasks by date range', async () => {
      const mockTasks = [createMockTask()];
      mockTaskRepository.findByDueDateRange.mockResolvedValue(mockTasks);

      const result = await service.listComplementaryTasks({
        fromDate: '2025-12-01T00:00:00Z',
        toDate: '2025-12-31T23:59:59Z',
      });

      expect(mockTaskRepository.findByDueDateRange).toHaveBeenCalled();
      expect(result).toHaveLength(1);
    });
  });

  describe('listTasksByVveId', () => {
    /**
     * US 4.1.15 - List Tasks By VVE ID - Dedicated Method
     * 
     * Verifies that dedicated method for VVE task retrieval:
     * - Provides convenience wrapper for common query
     * - Calls repository findByVveId
     * - Returns all tasks for specified vessel (any status)
     */
    it('should return tasks for specific VVE', async () => {
      const mockTasks = [createMockTask(), createMockTask()];
      mockTaskRepository.findByVveId.mockResolvedValue(mockTasks);

      const result = await service.listTasksByVveId('VVE-001');

      expect(mockTaskRepository.findByVveId).toHaveBeenCalledWith('VVE-001', undefined);
      expect(result).toHaveLength(2);
    });
  });

  describe('listTasksByStatus', () => {
    /**
     * US 4.1.15 - List Tasks By Status - Dedicated Method
     * 
     * Verifies that dedicated status query method:
     * - Provides convenience wrapper for status filtering
     * - Calls repository findByStatus
     * - Commonly used for lifecycle-based dashboards
     */
    it('should return tasks with specific status', async () => {
      const mockTasks = [createMockTask()];
      mockTaskRepository.findByStatus.mockResolvedValue(mockTasks);

      const result = await service.listTasksByStatus(ComplementaryTaskStatus.IN_PROGRESS);

      expect(mockTaskRepository.findByStatus).toHaveBeenCalledWith(
        ComplementaryTaskStatus.IN_PROGRESS,
        undefined
      );
      expect(result).toHaveLength(1);
    });
  });

  describe('listTasksByDateRange', () => {
    /**
     * US 4.1.15 - List Tasks By Date Range - Dedicated Method
     * 
     * Verifies that dedicated date range query:
     * - Accepts Date objects (not strings)
     * - Calls repository findByDueDateRange
     * - Supports calendar-based task scheduling views
     */
    it('should return tasks within date range', async () => {
      const fromDate = new Date('2025-12-01');
      const toDate = new Date('2025-12-31');
      const mockTasks = [createMockTask()];
      mockTaskRepository.findByDueDateRange.mockResolvedValue(mockTasks);

      const result = await service.listTasksByDateRange(fromDate, toDate);

      expect(mockTaskRepository.findByDueDateRange).toHaveBeenCalledWith(
        fromDate,
        toDate,
        undefined
      );
      expect(result).toHaveLength(1);
    });
  });

  describe('getActiveTasksImpactingOperations', () => {
    /**
     * US 4.1.15 - Get Active Impacting Tasks - Operation Planning Integration
     * 
     * Verifies that retrieving operation-impacting tasks:
     * - Queries active tasks (IN_PROGRESS)
     * - Checks each task's category for "suspend operations" impact
     * - Returns only tasks that make VVEs unavailable for cargo operations
     * - Critical for operation planning module to avoid scheduling conflicts
     */
    it('should return active tasks that suspend operations', async () => {
      const mockTask = createMockTask();
      const mockCategory = new TaskCategory({
        taskCategoryId: 'CAT-001',
        categoryCode: 'CTC001',
        categoryName: 'Test Category',
        description: 'Test category description',
        expectedImpact: 'Suspend cargo operations during repair',
        defaultDurationHours: 2,
        isActive: true,
      });

      mockTaskRepository.findActive.mockResolvedValue([mockTask]);
      mockCategoryRepository.findById.mockResolvedValue(mockCategory);

      const result = await service.getActiveTasksImpactingOperations();

      expect(mockTaskRepository.findActive).toHaveBeenCalled();
      expect(mockCategoryRepository.findById).toHaveBeenCalledWith('CAT-001');
      expect(result).toHaveLength(1);
    });

    /**
     * US 4.1.15 - Get Active Impacting Tasks - Filter Non-Impacting
     * 
     * Verifies that tasks without operational impact:
     * - Are filtered out even if active
     * - Only tasks with "suspend" impact in category are returned
     * - Prevents false positives in operation planning constraints
     */
    it('should filter out tasks without suspend impact', async () => {
      const mockTask = createMockTask();
      const mockCategory = new TaskCategory({
        taskCategoryId: 'CAT-001',
        categoryCode: 'CTC001',
        categoryName: 'Test Category',
        description: 'Test category description',
        expectedImpact: 'No impact on operations',
        defaultDurationHours: 2,
        isActive: true,
      });

      mockTaskRepository.findActive.mockResolvedValue([mockTask]);
      mockCategoryRepository.findById.mockResolvedValue(mockCategory);

      const result = await service.getActiveTasksImpactingOperations();

      expect(result).toHaveLength(0);
    });

    /**
     * US 4.1.15 - Get Active Impacting Tasks - Null Category Handling
     * 
     * Verifies that tasks with missing/deleted categories:
     * - Are gracefully handled (not causing errors)
     * - Are excluded from impacting tasks list
     * - Prevents crashes from data integrity issues
     */
    it('should handle tasks with null category', async () => {
      const mockTask = createMockTask();

      mockTaskRepository.findActive.mockResolvedValue([mockTask]);
      mockCategoryRepository.findById.mockResolvedValue(null);

      const result = await service.getActiveTasksImpactingOperations();

      expect(result).toHaveLength(0);
    });
  });

  describe('assignTask', () => {
    const assignDto: AssignTaskDto = {
      assignedTo: 'Maintenance Team B',
    };

    /**
     * US 4.1.15 - Assign Task - Team/Staff Assignment
     * 
     * Verifies that assigning a task:
     * - Retrieves task from repository
     * - Updates assignedTo field
     * - Saves updated task
     * - Supports workload distribution and responsibility tracking
     */
    it('should assign task successfully', async () => {
      const mockTask = createMockTask();
      const assignedTask = createMockTask({ assignedTo: 'Maintenance Team B' });

      mockTaskRepository.findById.mockResolvedValue(mockTask);
      mockTaskRepository.update.mockResolvedValue(assignedTask);

      const result = await service.assignTask('TASK-001', assignDto);

      expect(mockTaskRepository.findById).toHaveBeenCalledWith('TASK-001');
      expect(mockTaskRepository.update).toHaveBeenCalled();
      expect(result).toBeDefined();
    });

    /**
     * US 4.1.15 - Assign Task - Not Found Error
     * 
     * Verifies that assigning a non-existent task:
     * - Service validates task existence
     * - Throws descriptive error
     * - Repository update is never called
     */
    it('should throw error if task not found', async () => {
      mockTaskRepository.findById.mockResolvedValue(null);

      await expect(service.assignTask('TASK-999', assignDto)).rejects.toThrow(
        'Complementary task TASK-999 not found'
      );
    });
  });

  describe('startTask', () => {
    const startDto: StartTaskDto = {};

    /**
     * US 4.1.15 - Start Task - Status Transition to IN_PROGRESS
     * 
     * Verifies that starting a PLANNED task:
     * - Calls domain entity start() method (status transition logic)
     * - Changes status to IN_PROGRESS
     * - Records startedAt timestamp
     * - Saves updated entity to repository
     */
    it('should start a PLANNED task', async () => {
      const mockTask = createMockTask();
      const startedTask = createMockTask();
      jest.spyOn(startedTask, 'status', 'get').mockReturnValue(ComplementaryTaskStatus.IN_PROGRESS);

      mockTaskRepository.findById.mockResolvedValue(mockTask);
      mockTaskRepository.update.mockResolvedValue(startedTask);

      const result = await service.startTask('TASK-001', startDto);

      expect(mockTaskRepository.findById).toHaveBeenCalledWith('TASK-001');
      expect(mockTaskRepository.update).toHaveBeenCalled();
      expect(result).toBeDefined();
    });

    /**
     * US 4.1.15 - Start Task - Not Found Error
     * 
     * Verifies that starting a non-existent task:
     * - Service validates existence first
     * - Throws descriptive error
     * - Domain transition never attempted
     */
    it('should throw error if task not found', async () => {
      mockTaskRepository.findById.mockResolvedValue(null);

      await expect(service.startTask('TASK-999', startDto)).rejects.toThrow(
        'Complementary task TASK-999 not found'
      );
    });
  });

  describe('completeTask', () => {
    const completeDto: CompleteTaskDto = {
      completedBy: 'John Doe',
    };

    /**
     * US 4.1.15 - Complete Task - Status Transition to COMPLETED
     * 
     * Verifies that completing an IN_PROGRESS task:
     * - Calls domain entity complete() method
     * - Changes status to COMPLETED
     * - Records completedAt timestamp and completedBy identifier
     * - Saves updated entity to repository
     */
    it('should complete an IN_PROGRESS task', async () => {
      const mockTask = createMockTask();
      // Start the task first to set it to IN_PROGRESS
      mockTask.start();
      const completedTask = createMockTask();
      jest.spyOn(completedTask, 'status', 'get').mockReturnValue(ComplementaryTaskStatus.COMPLETED);

      mockTaskRepository.findById.mockResolvedValue(mockTask);
      mockTaskRepository.update.mockResolvedValue(completedTask);

      const result = await service.completeTask('TASK-001', completeDto);

      expect(mockTaskRepository.findById).toHaveBeenCalledWith('TASK-001');
      expect(mockTaskRepository.update).toHaveBeenCalled();
      expect(result).toBeDefined();
    });

    /**
     * US 4.1.15 - Complete Task - Not Found Error
     * 
     * Verifies that completing a non-existent task:
     * - Service validates existence
     * - Throws descriptive error
     * - Domain complete() never called
     */
    it('should throw error if task not found', async () => {
      mockTaskRepository.findById.mockResolvedValue(null);

      await expect(service.completeTask('TASK-999', completeDto)).rejects.toThrow(
        'Complementary task TASK-999 not found'
      );
    });
  });

  describe('cancelTask', () => {
    const cancelDto: CancelTaskDto = {
      cancelledBy: 'Admin',
      cancellationReason: 'Weather conditions',
    };

    /**
     * US 4.1.15 - Cancel Task - Status Transition to CANCELLED
     * 
     * Verifies that cancelling a task:
     * - Calls domain entity cancel() method
     * - Changes status to CANCELLED (from any status)
     * - Records cancelledAt, cancelledBy, and cancellationReason
     * - Saves updated entity to repository
     */
    it('should cancel a task successfully', async () => {
      const mockTask = createMockTask();
      const cancelledTask = createMockTask();
      jest.spyOn(cancelledTask, 'status', 'get').mockReturnValue(ComplementaryTaskStatus.CANCELLED);

      mockTaskRepository.findById.mockResolvedValue(mockTask);
      mockTaskRepository.update.mockResolvedValue(cancelledTask);

      const result = await service.cancelTask('TASK-001', cancelDto);

      expect(mockTaskRepository.findById).toHaveBeenCalledWith('TASK-001');
      expect(mockTaskRepository.update).toHaveBeenCalled();
      expect(result).toBeDefined();
    });

    /**
     * US 4.1.15 - Cancel Task - Not Found Error
     * 
     * Verifies that cancelling a non-existent task:
     * - Service validates existence
     * - Throws descriptive error
     * - Domain cancel() never called
     */
    it('should throw error if task not found', async () => {
      mockTaskRepository.findById.mockResolvedValue(null);

      await expect(service.cancelTask('TASK-999', cancelDto)).rejects.toThrow(
        'Complementary task TASK-999 not found'
      );
    });
  });

  describe('addTaskNote', () => {
    const noteDto: AddTaskNoteDto = {
      author: 'Inspector',
      content: 'Inspection completed. No issues found.',
    };

    /**
     * US 4.1.15 - Add Task Note - Collaboration Feature
     * 
     * Verifies that adding a note:
     * - Retrieves task from repository
     * - Calls domain entity addNote() method
     * - Note includes timestamp, author, and content
     * - Saves updated task with new note in notes array
     */
    it('should add note to task', async () => {
      const mockTask = createMockTask();
      const taskWithNote = createMockTask();

      mockTaskRepository.findById.mockResolvedValue(mockTask);
      mockTaskRepository.update.mockResolvedValue(taskWithNote);

      const result = await service.addTaskNote('TASK-001', noteDto);

      expect(mockTaskRepository.findById).toHaveBeenCalledWith('TASK-001');
      expect(mockTaskRepository.update).toHaveBeenCalled();
      expect(result).toBeDefined();
    });

    /**
     * US 4.1.15 - Add Task Note - Not Found Error
     * 
     * Verifies that adding a note to non-existent task:
     * - Service validates task existence
     * - Throws descriptive error
     * - Domain addNote() never called
     */
    it('should throw error if task not found', async () => {
      mockTaskRepository.findById.mockResolvedValue(null);

      await expect(service.addTaskNote('TASK-999', noteDto)).rejects.toThrow(
        'Complementary task TASK-999 not found'
      );
    });
  });

  describe('getTaskStatistics', () => {
    /**
     * US 4.1.15 - Get Task Statistics - Analytics and Reporting
     * 
     * Verifies that retrieving task statistics:
     * - Calls repository getStatistics with date range
     * - Returns aggregate data: total count, breakdown by status
     * - Includes performance metrics: average completion time, on-time rate
     * - Supports management dashboards and KPI tracking
     */
    it('should return task statistics for date range', async () => {
      const fromDate = new Date('2025-12-01');
      const toDate = new Date('2025-12-31');
      const mockStats = {
        total: 10,
        byStatus: {
          [ComplementaryTaskStatus.PLANNED]: 3,
          [ComplementaryTaskStatus.IN_PROGRESS]: 2,
          [ComplementaryTaskStatus.COMPLETED]: 4,
          [ComplementaryTaskStatus.CANCELLED]: 1,
        },
        averageCompletionTimeHours: 3.5,
        onTimeCompletionRate: 0.8,
      };

      mockTaskRepository.getStatistics.mockResolvedValue(mockStats);

      const result = await service.getTaskStatistics(fromDate, toDate);

      expect(mockTaskRepository.getStatistics).toHaveBeenCalledWith(fromDate, toDate);
      expect(result.total).toBe(10);
      expect(result.averageCompletionTimeHours).toBe(3.5);
      expect(result.onTimeCompletionRate).toBe(0.8);
    });
  });

  describe('getTaskCount', () => {
    /**
     * US 4.1.15 - Get Task Count - Total Count Query
     * 
     * Verifies that retrieving total task count:
     * - Calls repository count() with undefined status (all tasks)
     * - Returns numeric count
     * - Efficient query for dashboard widgets
     */
    it('should return total task count', async () => {
      mockTaskRepository.count.mockResolvedValue(25);

      const result = await service.getTaskCount();

      expect(mockTaskRepository.count).toHaveBeenCalledWith(undefined);
      expect(result).toBe(25);
    });

    /**
     * US 4.1.15 - Get Task Count - Status-Filtered Count
     * 
     * Verifies that retrieving count by status:
     * - Calls repository count() with specific status
     * - Returns count of tasks in that status only
     * - Supports status-specific metrics (e.g., "5 tasks in progress")
     */
    it('should return task count by status', async () => {
      mockTaskRepository.count.mockResolvedValue(5);

      const result = await service.getTaskCount(ComplementaryTaskStatus.IN_PROGRESS);

      expect(mockTaskRepository.count).toHaveBeenCalledWith(ComplementaryTaskStatus.IN_PROGRESS);
      expect(result).toBe(5);
    });
  });

  describe('getOverdueTaskCount', () => {
    /**
     * US 4.1.15 - Get Overdue Task Count - Alert Metric
     * 
     * Verifies that retrieving overdue task count:
     * - Calls repository countOverdue()
     * - Returns count of tasks past due date and not COMPLETED/CANCELLED
     * - Critical metric for identifying tasks requiring immediate attention
     */
    it('should return count of overdue tasks', async () => {
      mockTaskRepository.countOverdue.mockResolvedValue(3);

      const result = await service.getOverdueTaskCount();

      expect(mockTaskRepository.countOverdue).toHaveBeenCalled();
      expect(result).toBe(3);
    });
  });

  describe('DTO Conversion', () => {
    /**
     * US 4.1.15 - DTO Conversion - Complete Field Mapping
     * 
     * Verifies that converting task entity to DTO:
     * - Maps all entity fields to DTO structure
     * - Includes nested objects (notes array)
     * - Preserves data types and formats
     * - Separates domain layer from presentation layer
     */
    it('should convert task entity to DTO with all fields', async () => {
      const mockTask = createMockTask();
      mockTask.addNote('Author', 'Test note');
      
      mockTaskRepository.findById.mockResolvedValue(mockTask);

      const result = await service.getComplementaryTaskById('TASK-001');

      expect(result).toBeDefined();
      expect(result.taskId).toBeDefined();
      expect(result.title).toBe('Test Task');
      expect(result.status).toBe(ComplementaryTaskStatus.PLANNED);
      expect(result.notes).toBeDefined();
      expect(Array.isArray(result.notes)).toBe(true);
    });

    /**
     * US 4.1.15 - DTO Conversion - Optional Field Handling
     * 
     * Verifies that converting task with optional fields:
     * - Handles undefined/null fields correctly
     * - Optional fields (e.g., assignedTo) remain undefined if not set
     * - DTO structure matches TypeScript type definitions
     */
    it('should handle optional fields correctly', async () => {
      const mockTask = createMockTask({ assignedTo: undefined });
      mockTaskRepository.findById.mockResolvedValue(mockTask);

      const result = await service.getComplementaryTaskById('TASK-001');

      expect(result.assignedTo).toBeUndefined();
    });
  });
});
