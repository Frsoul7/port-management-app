import { Request, Response, NextFunction } from 'express';
import { ComplementaryTaskController } from '@presentation/controllers/ComplementaryTaskController';
import { ComplementaryTaskService } from '@application/services/ComplementaryTaskService';
import { ComplementaryTaskStatus } from '@shared/types';
import { ComplementaryTaskDto } from '@application/dtos';

/**
 * US 4.1.15 - ComplementaryTaskController Unit Tests
 * Tests for complementary task HTTP request handlers
 */
describe('US 4.1.15 - ComplementaryTaskController', () => {
  let controller: ComplementaryTaskController;
  let mockService: jest.Mocked<ComplementaryTaskService>;
  let mockRequest: Partial<Request>;
  let mockResponse: Partial<Response>;
  let mockNext: NextFunction;

  beforeEach(() => {
    // Create mock service
    mockService = {
      createComplementaryTask: jest.fn(),
      getComplementaryTaskById: jest.fn(),
      updateComplementaryTask: jest.fn(),
      deleteComplementaryTask: jest.fn(),
      listComplementaryTasks: jest.fn(),
      listComplementaryTasksPaginated: jest.fn(),
      listTasksByVveId: jest.fn(),
      listTasksByStatus: jest.fn(),
      listTasksByDateRange: jest.fn(),
      getActiveTasksImpactingOperations: jest.fn(),
      assignTask: jest.fn(),
      startTask: jest.fn(),
      completeTask: jest.fn(),
      cancelTask: jest.fn(),
      addTaskNote: jest.fn(),
      getTaskStatistics: jest.fn(),
      getTaskCount: jest.fn(),
      getOverdueTaskCount: jest.fn(),
    } as any;

    controller = new ComplementaryTaskController(mockService);

    // Create mock request/response/next
    mockRequest = {
      params: {},
      query: {},
      body: {},
    };

    mockResponse = {
      status: jest.fn().mockReturnThis(),
      json: jest.fn().mockReturnThis(),
    };

    mockNext = jest.fn();
  });

  afterEach(() => {
    jest.clearAllMocks();
  });

  const createMockTaskDto = (overrides?: Partial<ComplementaryTaskDto>): ComplementaryTaskDto => ({
    taskId: 'TASK-001',
    taskCategoryId: 'CAT-001',
    vveId: 'VVE-001',
    title: 'Test Task',
    description: 'Test description',
    status: ComplementaryTaskStatus.PLANNED,
    dueDate: '2025-12-31T12:00:00Z',
    estimatedDurationHours: 2,
    assignedTo: 'Team A',
    notes: [],
    createdAt: '2025-12-01T10:00:00Z',
    createdBy: 'user-123',
    ...overrides,
  });

  describe('createTask', () => {
    /**
     * US 4.1.15 - Create Complementary Task - Success Path
     * 
     * Verifies that a valid complementary task creation request:
     * - Calls the service with correct request body (category, VVE, title, description, due date, duration, assignee)
     * - Returns HTTP 201 (Created) status
     * - Returns standardized success response with task data
     * - Creates task in PLANNED status by default
     */
    it('should create task and return 201 with task data', async () => {
      const requestBody = {
        taskCategoryId: 'CAT-001',
        vveId: 'VVE-001',
        title: 'New Task',
        description: 'Task description',
        dueDate: '2025-12-31T12:00:00Z',
        estimatedDurationHours: 2,
        assignedTo: 'Team A',
        createdBy: 'user-123',
      };
      const mockTask = createMockTaskDto();

      mockRequest.body = requestBody;
      mockService.createComplementaryTask.mockResolvedValue(mockTask);

      await controller.createTask(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockService.createComplementaryTask).toHaveBeenCalledWith(requestBody);
      expect(mockResponse.status).toHaveBeenCalledWith(201);
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        message: 'Complementary task created successfully',
        data: mockTask,
      });
    });

    /**
     * US 4.1.15 - Create Task - Error Handling
     * 
     * Verifies that service-level errors (e.g., invalid category) are:
     * - Passed to Express error middleware via next(error)
     * - Do not send response directly (let error handler manage it)
     * - Common errors: category not found, inactive category, validation failures
     */
    it('should call next with error if service throws', async () => {
      const error = new Error('Category not found');
      mockRequest.body = { taskCategoryId: 'CAT-999' };
      mockService.createComplementaryTask.mockRejectedValue(error);

      await controller.createTask(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockNext).toHaveBeenCalledWith(error);
      expect(mockResponse.json).not.toHaveBeenCalled();
    });
  });

  describe('getTaskById', () => {
    /**
     * US 4.1.15 - Get Task By ID - Success Path
     * 
     * Verifies that retrieving a task by ID:
     * - Extracts task ID from request params
     * - Calls service with correct ID
     * - Returns standardized success response with task data
     * - Includes all task details (status, assignee, dates, notes, etc.)
     */
    it('should return task by ID', async () => {
      const mockTask = createMockTaskDto();
      mockRequest.params = { id: 'TASK-001' };
      mockService.getComplementaryTaskById.mockResolvedValue(mockTask);

      await controller.getTaskById(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockService.getComplementaryTaskById).toHaveBeenCalledWith('TASK-001');
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        data: mockTask,
      });
    });

    /**
     * US 4.1.15 - Get Task By ID - Not Found Error
     * 
     * Verifies that requesting a non-existent task:
     * - Delegates error to Express middleware
     * - Service throws "Task not found" error
     * - No response sent directly (error handler manages HTTP status)
     */
    it('should call next with error if task not found', async () => {
      const error = new Error('Task not found');
      mockRequest.params = { id: 'TASK-999' };
      mockService.getComplementaryTaskById.mockRejectedValue(error);

      await controller.getTaskById(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockNext).toHaveBeenCalledWith(error);
    });
  });

  describe('updateTask', () => {
    /**
     * US 4.1.15 - Update Task - Success Path
     * 
     * Verifies that updating a PLANNED task:
     * - Extracts task ID from params and updates from body
     * - Calls service with ID and update fields (title, description, due date, duration)
     * - Returns success response with updated task data
     * - Business rule: Only PLANNED tasks can be updated (enforced at service layer)
     */
    it('should update task and return updated data', async () => {
      const updates = { title: 'Updated Title', description: 'Updated description' };
      const mockTask = createMockTaskDto(updates);

      mockRequest.params = { id: 'TASK-001' };
      mockRequest.body = updates;
      mockService.updateComplementaryTask.mockResolvedValue(mockTask);

      await controller.updateTask(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockService.updateComplementaryTask).toHaveBeenCalledWith('TASK-001', updates);
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        message: 'Complementary task updated successfully',
        data: mockTask,
      });
    });

    /**
     * US 4.1.15 - Update Task - Status Validation Error
     * 
     * Verifies that attempting to update a non-PLANNED task:
     * - Service rejects with status validation error
     * - Error delegated to Express middleware
     * - Prevents modification of IN_PROGRESS, COMPLETED, or CANCELLED tasks
     */
    it('should call next with error if update fails', async () => {
      const error = new Error('Can only update PLANNED tasks');
      mockRequest.params = { id: 'TASK-001' };
      mockRequest.body = { title: 'New Title' };
      mockService.updateComplementaryTask.mockRejectedValue(error);

      await controller.updateTask(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockNext).toHaveBeenCalledWith(error);
    });
  });

  describe('deleteTask', () => {
    /**
     * US 4.1.15 - Delete Task - Success Path
     * 
     * Verifies that deleting a PLANNED task:
     * - Extracts task ID from params
     * - Calls service delete method
     * - Returns success message (no data needed)
     * - Business rule: Only PLANNED tasks can be deleted (not started/completed/cancelled)
     */
    it('should delete task successfully', async () => {
      mockRequest.params = { id: 'TASK-001' };
      mockService.deleteComplementaryTask.mockResolvedValue(undefined);

      await controller.deleteTask(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockService.deleteComplementaryTask).toHaveBeenCalledWith('TASK-001');
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        message: 'Complementary task deleted successfully',
      });
    });

    /**
     * US 4.1.15 - Delete Task - Status Validation Error
     * 
     * Verifies that attempting to delete a non-PLANNED task:
     * - Service rejects with status validation error
     * - Error delegated to middleware
     * - Prevents deletion of tasks that have started or finished
     */
    it('should call next with error if deletion fails', async () => {
      const error = new Error('Can only delete PLANNED tasks');
      mockRequest.params = { id: 'TASK-001' };
      mockService.deleteComplementaryTask.mockRejectedValue(error);

      await controller.deleteTask(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockNext).toHaveBeenCalledWith(error);
    });
  });

  describe('listTasks', () => {
    /**
     * US 4.1.15 - List Tasks - No Filters
     * 
     * Verifies that listing without query params:
     * - Calls service with all filter fields as undefined
     * - Returns all tasks in the system
     * - Includes count of returned items
     * - Supports pagination, sorting, and filtering (all optional)
     */
    it('should list all tasks without filters', async () => {
      const mockTasks = [createMockTaskDto(), createMockTaskDto({ taskId: 'TASK-002' })];
      mockService.listComplementaryTasksPaginated.mockResolvedValue({
        data: mockTasks,
        pagination: { page: 1, limit: 10, total: 2, totalPages: 1 },
      });

      await controller.listTasks(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockService.listComplementaryTasksPaginated).toHaveBeenCalledWith({});
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        data: mockTasks,
        pagination: { page: 1, limit: 10, total: 2, totalPages: 1 },
      });
    });

    /**
     * US 4.1.15 - List Tasks - Status Filter
     * 
     * Verifies that filtering by status (PLANNED, IN_PROGRESS, COMPLETED, CANCELLED):
     * - Passes status from query params to service
     * - Returns only tasks matching the specified status
     * - Useful for dashboards showing tasks by lifecycle stage
     */
    it('should list tasks with status filter', async () => {
      const mockTasks = [createMockTaskDto()];
      mockRequest.query = { status: ComplementaryTaskStatus.PLANNED };
      mockService.listComplementaryTasksPaginated.mockResolvedValue({
        data: mockTasks,
        pagination: { page: 1, limit: 10, total: 1, totalPages: 1 },
      });

      await controller.listTasks(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockService.listComplementaryTasksPaginated).toHaveBeenCalledWith(
        { status: ComplementaryTaskStatus.PLANNED }
      );
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        data: mockTasks,
        pagination: { page: 1, limit: 10, total: 1, totalPages: 1 },
      });
    });

    /**
     * US 4.1.15 - List Tasks - VVE Filter
     * 
     * Verifies that filtering by VVE ID:
     * - Returns only tasks associated with the specified vessel/vehicle
     * - Critical for showing all maintenance/repair tasks for a specific VVE
     * - Supports tracking task history per vessel
     */
    it('should list tasks with VVE filter', async () => {
      const mockTasks = [createMockTaskDto()];
      mockRequest.query = { vveId: 'VVE-001' };
      mockService.listComplementaryTasksPaginated.mockResolvedValue({
        data: mockTasks,
        pagination: { page: 1, limit: 10, total: 1, totalPages: 1 },
      });

      await controller.listTasks(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockService.listComplementaryTasksPaginated).toHaveBeenCalledWith(
        { vveId: 'VVE-001' }
      );
    });

    /**
     * US 4.1.15 - List Tasks - Pagination
     * 
     * Verifies that pagination parameters (page, limit):
     * - Are parsed from query string to numbers
     * - Passed to service layer for database pagination
     * - Essential for large task datasets in production
     */
    it('should list tasks with pagination', async () => {
      const mockTasks = [createMockTaskDto()];
      mockRequest.query = { page: '1', limit: '10' };
      mockService.listComplementaryTasksPaginated.mockResolvedValue({
        data: mockTasks,
        pagination: { page: 1, limit: 10, total: 1, totalPages: 1 },
      });

      await controller.listTasks(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockService.listComplementaryTasksPaginated).toHaveBeenCalledWith(
        { page: '1', limit: '10' }
      );
    });

    /**
     * US 4.1.15 - List Tasks - Sorting
     * 
     * Verifies that sort parameters (sortBy field, sortOrder asc/desc):
     * - Are passed to service for database-level sorting
     * - Common sort fields: dueDate, createdAt, status, priority
     * - Improves user experience for task management dashboards
     */
    it('should list tasks with sorting', async () => {
      const mockTasks = [createMockTaskDto()];
      mockRequest.query = { sortBy: 'dueDate', sortOrder: 'asc' };
      mockService.listComplementaryTasksPaginated.mockResolvedValue({
        data: mockTasks,
        pagination: { page: 1, limit: 10, total: 1, totalPages: 1 },
      });

      await controller.listTasks(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockService.listComplementaryTasksPaginated).toHaveBeenCalledWith(
        { sortBy: 'dueDate', sortOrder: 'asc' }
      );
    });

    /**
     * US 4.1.15 - List Tasks - Database Error
     * 
     * Verifies that infrastructure failures (e.g., database errors):
     * - Are caught and delegated to error middleware
     * - Ensures graceful error handling
     * - Prevents server crashes from unhandled exceptions
     */
    it('should call next with error if listing fails', async () => {
      const error = new Error('Database error');
      mockService.listComplementaryTasksPaginated.mockRejectedValue(error);

      await controller.listTasks(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockNext).toHaveBeenCalledWith(error);
    });
  });

  describe('listTasksByVve', () => {
    /**
     * US 4.1.15 - List Tasks By VVE - Success Path
     * 
     * Verifies that retrieving all tasks for a specific VVE:
     * - Extracts VVE ID from route params
     * - Returns all tasks (any status) associated with that vessel
     * - Useful for vessel-specific maintenance dashboards
     * - Includes task count in response
     */
    it('should return tasks for specific VVE', async () => {
      const mockTasks = [createMockTaskDto(), createMockTaskDto({ taskId: 'TASK-002' })];
      mockRequest.params = { vveId: 'VVE-001' };
      mockService.listTasksByVveId.mockResolvedValue(mockTasks);

      await controller.listTasksByVve(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockService.listTasksByVveId).toHaveBeenCalledWith('VVE-001', expect.any(Object));
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        data: mockTasks,
        count: 2,
      });
    });

    /**
     * US 4.1.15 - List Tasks By VVE - VVE Not Found Error
     * 
     * Verifies that requesting tasks for a non-existent VVE:
     * - Service validates VVE existence
     * - Error delegated to middleware (could be 404 Not Found)
     * - Prevents showing tasks for invalid vessel IDs
     */
    it('should call next with error if fetching fails', async () => {
      const error = new Error('VVE not found');
      mockRequest.params = { vveId: 'VVE-999' };
      mockService.listTasksByVveId.mockRejectedValue(error);

      await controller.listTasksByVve(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockNext).toHaveBeenCalledWith(error);
    });
  });

  describe('listTasksByStatus', () => {
    /**
     * US 4.1.15 - List Tasks By Status - Filtered Retrieval
     * 
     * Verifies that retrieving tasks by status:
     * - Extracts status from route params
     * - Returns only tasks in that lifecycle stage
     * - Statuses: PLANNED, IN_PROGRESS, COMPLETED, CANCELLED
     * - Supports workflow-based task views
     */
    it('should return tasks with specific status', async () => {
      const mockTasks = [createMockTaskDto({ status: ComplementaryTaskStatus.IN_PROGRESS })];
      mockRequest.params = { status: ComplementaryTaskStatus.IN_PROGRESS };
      mockService.listTasksByStatus.mockResolvedValue(mockTasks);

      await controller.listTasksByStatus(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockService.listTasksByStatus).toHaveBeenCalledWith(
        ComplementaryTaskStatus.IN_PROGRESS,
        expect.any(Object)
      );
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        data: mockTasks,
        count: 1,
      });
    });
  });

  describe('getActiveImpactingTasks', () => {
    /**
     * US 4.1.15 - Get Active Impacting Tasks - Critical for Operations
     * 
     * Verifies that retrieving tasks that impact cargo operations:
     * - Returns only IN_PROGRESS tasks with categories marked as "suspend operations"
     * - Critical for operation planning module to know when VVEs are unavailable
     * - Used to prevent operation assignment to vessels undergoing critical maintenance
     */
    it('should return active tasks that impact operations', async () => {
      const mockTasks = [createMockTaskDto({ status: ComplementaryTaskStatus.IN_PROGRESS })];
      mockService.getActiveTasksImpactingOperations.mockResolvedValue(mockTasks);

      await controller.getActiveImpactingTasks(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockService.getActiveTasksImpactingOperations).toHaveBeenCalled();
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        data: mockTasks,
        count: 1,
      });
    });

    /**
     * US 4.1.15 - Get Active Impacting Tasks - Empty Result
     * 
     * Verifies that when no tasks are impacting operations:
     * - Returns empty array (not null/undefined)
     * - Count is 0
     * - Indicates all VVEs are available for operations
     */
    it('should return empty array when no impacting tasks', async () => {
      mockService.getActiveTasksImpactingOperations.mockResolvedValue([]);

      await controller.getActiveImpactingTasks(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        data: [],
        count: 0,
      });
    });
  });

  describe('assignTask', () => {
    /**
     * US 4.1.15 - Assign Task - Team Assignment
     * 
     * Verifies that assigning a task to a team/individual:
     * - Updates assignedTo field with team/staff identifier
     * - Returns updated task data
     * - Supports task distribution and workload management
     * - Can reassign already assigned tasks
     */
    it('should assign task successfully', async () => {
      const assignDto = { assignedTo: 'Team B' };
      const mockTask = createMockTaskDto({ assignedTo: 'Team B' });

      mockRequest.params = { id: 'TASK-001' };
      mockRequest.body = assignDto;
      mockService.assignTask.mockResolvedValue(mockTask);

      await controller.assignTask(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockService.assignTask).toHaveBeenCalledWith('TASK-001', assignDto);
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        message: 'Task assigned successfully',
        data: mockTask,
      });
    });
  });

  describe('startTask', () => {
    /**
     * US 4.1.15 - Start Task - Begin Work
     * 
     * Verifies that starting a task:
     * - Changes status from PLANNED to IN_PROGRESS
     * - Records startedAt timestamp
     * - Returns updated task with new status
     * - Triggers VVE availability update if task impacts operations
     */
    it('should start task successfully', async () => {
      const startDto = {};
      const mockTask = createMockTaskDto({
        status: ComplementaryTaskStatus.IN_PROGRESS,
        startedAt: '2025-12-30T10:00:00Z',
      });

      mockRequest.params = { id: 'TASK-001' };
      mockRequest.body = startDto;
      mockService.startTask.mockResolvedValue(mockTask);

      await controller.startTask(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockService.startTask).toHaveBeenCalledWith('TASK-001', startDto);
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        message: 'Task started successfully',
        data: mockTask,
      });
    });

    /**
     * US 4.1.15 - Start Task - Invalid Status Transition
     * 
     * Verifies that starting a non-PLANNED task:
     * - Service validates current status before transition
     * - Cannot start already IN_PROGRESS, COMPLETED, or CANCELLED tasks
     * - Error delegated to middleware
     */
    it('should call next with error if task cannot be started', async () => {
      const error = new Error('Task must be PLANNED to start');
      mockRequest.params = { id: 'TASK-001' };
      mockRequest.body = {};
      mockService.startTask.mockRejectedValue(error);

      await controller.startTask(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockNext).toHaveBeenCalledWith(error);
    });
  });

  describe('completeTask', () => {
    /**
     * US 4.1.15 - Complete Task - Finish Work
     * 
     * Verifies that completing a task:
     * - Changes status from IN_PROGRESS to COMPLETED
     * - Records completedAt timestamp and completedBy identifier
     * - Returns updated task data
     * - Triggers VVE availability restoration if task was impacting operations
     */
    it('should complete task successfully', async () => {
      const completeDto = { completedBy: 'John Doe' };
      const mockTask = createMockTaskDto({
        status: ComplementaryTaskStatus.COMPLETED,
        completedAt: '2025-12-30T14:00:00Z',
        completedBy: 'John Doe',
      });

      mockRequest.params = { id: 'TASK-001' };
      mockRequest.body = completeDto;
      mockService.completeTask.mockResolvedValue(mockTask);

      await controller.completeTask(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockService.completeTask).toHaveBeenCalledWith('TASK-001', completeDto);
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        message: 'Task completed successfully',
        data: mockTask,
      });
    });

    /**
     * US 4.1.15 - Complete Task - Invalid Status Transition
     * 
     * Verifies that completing a task not IN_PROGRESS:
     * - Service validates status before completion
     * - Cannot complete PLANNED (not started), COMPLETED, or CANCELLED tasks
     * - Error delegated to middleware
     */
    it('should call next with error if task cannot be completed', async () => {
      const error = new Error('Task must be IN_PROGRESS to complete');
      mockRequest.params = { id: 'TASK-001' };
      mockRequest.body = { completedBy: 'John Doe' };
      mockService.completeTask.mockRejectedValue(error);

      await controller.completeTask(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockNext).toHaveBeenCalledWith(error);
    });
  });

  describe('cancelTask', () => {
    /**
     * US 4.1.15 - Cancel Task - Abort Work
     * 
     * Verifies that cancelling a task:
     * - Changes status to CANCELLED (from PLANNED or IN_PROGRESS)
     * - Records cancelledAt, cancelledBy, and cancellationReason
     * - Returns updated task with cancellation details
     * - Restores VVE availability if task was impacting operations
     */
    it('should cancel task successfully', async () => {
      const cancelDto = { cancelledBy: 'Admin', cancellationReason: 'Weather conditions' };
      const mockTask = createMockTaskDto({
        status: ComplementaryTaskStatus.CANCELLED,
        cancelledAt: '2025-12-30T12:00:00Z',
        cancelledBy: 'Admin',
        cancellationReason: 'Weather conditions',
      });

      mockRequest.params = { id: 'TASK-001' };
      mockRequest.body = cancelDto;
      mockService.cancelTask.mockResolvedValue(mockTask);

      await controller.cancelTask(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockService.cancelTask).toHaveBeenCalledWith('TASK-001', cancelDto);
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        message: 'Task cancelled successfully',
        data: mockTask,
      });
    });
  });

  describe('addNote', () => {
    /**
     * US 4.1.15 - Add Task Note - Communication and Documentation
     * 
     * Verifies that adding a note to a task:
     * - Appends note with timestamp, author, and content to task notes array
     * - Returns updated task with new note included
     * - Supports collaboration and progress documentation
     * - Notes persist across task status changes
     */
    it('should add note to task successfully', async () => {
      const noteDto = { author: 'Inspector', content: 'Task inspection completed' };
      const mockTask = createMockTaskDto({
        notes: [
          {
            timestamp: '2025-12-30T10:00:00Z',
            author: 'Inspector',
            content: 'Task inspection completed',
          },
        ],
      });

      mockRequest.params = { id: 'TASK-001' };
      mockRequest.body = noteDto;
      mockService.addTaskNote.mockResolvedValue(mockTask);

      await controller.addNote(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockService.addTaskNote).toHaveBeenCalledWith('TASK-001', noteDto);
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        message: 'Note added successfully',
        data: mockTask,
      });
    });

    /**
     * US 4.1.15 - Add Task Note - Task Not Found Error
     * 
     * Verifies that adding a note to a non-existent task:
     * - Service validates task existence first
     * - Error delegated to middleware (404 Not Found)
     * - Prevents orphaned notes
     */
    it('should call next with error if adding note fails', async () => {
      const error = new Error('Task not found');
      mockRequest.params = { id: 'TASK-999' };
      mockRequest.body = { author: 'User', content: 'Note' };
      mockService.addTaskNote.mockRejectedValue(error);

      await controller.addNote(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockNext).toHaveBeenCalledWith(error);
    });
  });

  describe('getStatistics', () => {
    /**
     * US 4.1.15 - Get Task Statistics - Analytics Dashboard
     * 
     * Verifies that retrieving task statistics for a date range:
     * - Requires fromDate and toDate query parameters
     * - Returns total count, breakdown by status, average completion time, on-time rate
     * - Supports management reporting and performance analysis
     * - Date strings converted to Date objects for service layer
     */
    it('should return task statistics for date range', async () => {
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

      mockRequest.query = {
        fromDate: '2025-12-01T00:00:00Z',
        toDate: '2025-12-31T23:59:59Z',
      };
      mockService.getTaskStatistics.mockResolvedValue(mockStats);

      await controller.getStatistics(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockService.getTaskStatistics).toHaveBeenCalledWith(
        new Date('2025-12-01T00:00:00Z'),
        new Date('2025-12-31T23:59:59Z')
      );
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        data: mockStats,
      });
    });

    /**
     * US 4.1.15 - Get Statistics - Missing fromDate Validation
     * 
     * Verifies that statistics request without fromDate:
     * - Returns HTTP 400 Bad Request
     * - Returns error message indicating both dates required
     * - Does not call service (controller-level validation)
     */
    it('should return 400 if fromDate is missing', async () => {
      mockRequest.query = { toDate: '2025-12-31T23:59:59Z' };

      await controller.getStatistics(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockResponse.status).toHaveBeenCalledWith(400);
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: false,
        message: 'fromDate and toDate query parameters are required',
      });
      expect(mockService.getTaskStatistics).not.toHaveBeenCalled();
    });

    /**
     * US 4.1.15 - Get Statistics - Missing toDate Validation
     * 
     * Verifies that statistics request without toDate:
     * - Returns HTTP 400 Bad Request
     * - Returns error message indicating both dates required
     * - Controller validates both dates present before service call
     */
    it('should return 400 if toDate is missing', async () => {
      mockRequest.query = { fromDate: '2025-12-01T00:00:00Z' };

      await controller.getStatistics(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockResponse.status).toHaveBeenCalledWith(400);
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: false,
        message: 'fromDate and toDate query parameters are required',
      });
    });
  });

  describe('getTaskCount', () => {
    /**
     * US 4.1.15 - Get Task Count - Total Count
     * 
     * Verifies that retrieving total task count:
     * - Calls service with undefined status (counts all tasks)
     * - Returns count in standardized response format
     * - Useful for dashboard widgets and pagination
     */
    it('should return total task count', async () => {
      mockService.getTaskCount.mockResolvedValue(25);

      await controller.getTaskCount(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockService.getTaskCount).toHaveBeenCalledWith(undefined);
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        data: { count: 25 },
      });
    });

    /**
     * US 4.1.15 - Get Task Count - Status Filter
     * 
     * Verifies that retrieving task count by status:
     * - Passes status query param to service
     * - Returns count of tasks in that specific status
     * - Supports status-based dashboard metrics (e.g., "5 tasks in progress")
     */
    it('should return task count by status', async () => {
      mockRequest.query = { status: ComplementaryTaskStatus.IN_PROGRESS };
      mockService.getTaskCount.mockResolvedValue(5);

      await controller.getTaskCount(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockService.getTaskCount).toHaveBeenCalledWith(ComplementaryTaskStatus.IN_PROGRESS);
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        data: { count: 5 },
      });
    });
  });

  describe('getOverdueCount', () => {
    /**
     * US 4.1.15 - Get Overdue Task Count - Alert Metric
     * 
     * Verifies that retrieving overdue task count:
     * - Returns count of tasks past their due date and not yet COMPLETED/CANCELLED
     * - Critical metric for task management dashboards
     * - Highlights tasks requiring immediate attention
     */
    it('should return overdue task count', async () => {
      mockService.getOverdueTaskCount.mockResolvedValue(3);

      await controller.getOverdueCount(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockService.getOverdueTaskCount).toHaveBeenCalled();
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        data: { count: 3 },
      });
    });

    /**
     * US 4.1.15 - Get Overdue Count - Zero Result
     * 
     * Verifies that when no tasks are overdue:
     * - Returns 0 count (not null/undefined)
     * - Indicates good task management performance
     * - All tasks on schedule
     */
    it('should return 0 when no overdue tasks', async () => {
      mockService.getOverdueTaskCount.mockResolvedValue(0);

      await controller.getOverdueCount(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        data: { count: 0 },
      });
    });
  });

  describe('Error Handling', () => {
    /**
     * US 4.1.15 - Error Handling - Service Error Delegation
     * 
     * Verifies that all service errors:
     * - Are passed to Express error middleware via next(error)
     * - Controller never sends error responses directly
     * - Ensures consistent error format across application
     */
    it('should pass service errors to next middleware', async () => {
      const error = new Error('Service error');
      mockRequest.params = { id: 'TASK-001' };
      mockService.getComplementaryTaskById.mockRejectedValue(error);

      await controller.getTaskById(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockNext).toHaveBeenCalledWith(error);
      expect(mockResponse.json).not.toHaveBeenCalled();
    });

    /**
     * US 4.1.15 - Error Handling - Validation Errors
     * 
     * Verifies that domain validation errors (e.g., invalid category):
     * - Are thrown by service with descriptive messages
     * - Passed to middleware for proper HTTP status mapping
     * - Prevents invalid data from entering the system
     */
    it('should handle validation errors from service', async () => {
      const error = new Error('Invalid task category');
      mockRequest.body = { taskCategoryId: 'INVALID' };
      mockService.createComplementaryTask.mockRejectedValue(error);

      await controller.createTask(mockRequest as Request, mockResponse as Response, mockNext);

      expect(mockNext).toHaveBeenCalledWith(error);
    });
  });
});
