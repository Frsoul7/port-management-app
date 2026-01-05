import { Request, Response, NextFunction } from 'express';
import { TaskCategoryController } from '@presentation/controllers/TaskCategoryController';
import { TaskCategoryService } from '@application/services/TaskCategoryService';

/**
 * Unit Tests for TaskCategoryController
 * US 4.1.14: Task Category Catalog Management - Presentation Layer
 * 
 * Test Coverage:
 * - HTTP request handling
 * - Request validation
 * - Response formatting
 * - Error handling and status codes
 * - Query parameter parsing
 * - Filter building for activeOnly flag
 */

describe('TaskCategoryController', () => {
  let taskCategoryController: TaskCategoryController;
  let mockTaskCategoryService: jest.Mocked<TaskCategoryService>;
  let mockRequest: Partial<Request>;
  let mockResponse: Partial<Response>;
  let mockNext: jest.MockedFunction<NextFunction>;

  beforeEach(() => {
    // Create mock service
    mockTaskCategoryService = {
      createTaskCategory: jest.fn(),
      getTaskCategoryById: jest.fn(),
      getTaskCategoryByCode: jest.fn(),
      listAllTaskCategories: jest.fn(),
      listActiveTaskCategories: jest.fn(),
      listTaskCategories: jest.fn(),
      updateTaskCategory: jest.fn(),
      deactivateTaskCategory: jest.fn(),
      reactivateTaskCategory: jest.fn(),
      deleteTaskCategory: jest.fn(),
      getTaskCategoryCount: jest.fn(),
    } as any;

    taskCategoryController = new TaskCategoryController(mockTaskCategoryService);

    // Create mock request and response
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

  /**
   * Create Task Category Tests
   */
  describe('createTaskCategory', () => {
    it('should create task category and return 201 status', async () => {
      const categoryDto = {
        categoryName: 'Vessel Inspection',
        description: 'Safety and compliance inspection',
        defaultDurationHours: 2,
        expectedImpact: 'Requires vessel to be stationary',
      };

      mockRequest.body = categoryDto;

      const mockResult = {
        taskCategoryId: 'category-001',
        categoryCode: 'CTC001',
        categoryName: 'Vessel Inspection',
        description: 'Safety and compliance inspection',
        defaultDurationHours: 2,
        expectedImpact: 'Requires vessel to be stationary',
        isActive: true,
        createdAt: new Date().toISOString(),
        updatedAt: new Date().toISOString(),
      };

      mockTaskCategoryService.createTaskCategory.mockResolvedValue(mockResult);

      await taskCategoryController.createTaskCategory(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockTaskCategoryService.createTaskCategory).toHaveBeenCalledWith(categoryDto);
      expect(mockResponse.status).toHaveBeenCalledWith(201);
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        message: 'Task category created successfully',
        data: mockResult,
      });
      expect(mockNext).not.toHaveBeenCalled();
    });

    it('should create category with minimal fields', async () => {
      const categoryDto = {
        categoryName: 'Documentation Review',
        description: 'Review vessel documentation',
      };

      mockRequest.body = categoryDto;

      const mockResult = {
        taskCategoryId: 'category-002',
        categoryCode: 'CTC002',
        categoryName: 'Documentation Review',
        description: 'Review vessel documentation',
        isActive: true,
        createdAt: new Date().toISOString(),
        updatedAt: new Date().toISOString(),
      };

      mockTaskCategoryService.createTaskCategory.mockResolvedValue(mockResult);

      await taskCategoryController.createTaskCategory(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockTaskCategoryService.createTaskCategory).toHaveBeenCalledWith(categoryDto);
      expect(mockResponse.status).toHaveBeenCalledWith(201);
    });

    it('should call next with error on service failure', async () => {
      mockRequest.body = {
        categoryName: 'Test Category',
        description: 'Test',
      };

      const error = new Error('Name already exists');
      mockTaskCategoryService.createTaskCategory.mockRejectedValue(error);

      await taskCategoryController.createTaskCategory(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockNext).toHaveBeenCalledWith(error);
      expect(mockResponse.status).not.toHaveBeenCalled();
    });
  });

  /**
   * Get Task Category By ID Tests
   */
  describe('getTaskCategoryById', () => {
    it('should return task category when found', async () => {
      mockRequest.params = { id: 'category-001' };

      const mockCategory = {
        taskCategoryId: 'category-001',
        categoryCode: 'CTC001',
        categoryName: 'Vessel Inspection',
        description: 'Safety inspection',
        isActive: true,
        createdAt: new Date().toISOString(),
        updatedAt: new Date().toISOString(),
      };

      mockTaskCategoryService.getTaskCategoryById.mockResolvedValue(mockCategory);

      await taskCategoryController.getTaskCategoryById(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockTaskCategoryService.getTaskCategoryById).toHaveBeenCalledWith('category-001');
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        data: mockCategory,
      });
    });

    it('should call next with error when service fails', async () => {
      mockRequest.params = { id: 'non-existent' };

      const error = new Error('Category not found');
      mockTaskCategoryService.getTaskCategoryById.mockRejectedValue(error);

      await taskCategoryController.getTaskCategoryById(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockNext).toHaveBeenCalledWith(error);
    });
  });

  /**
   * Get Task Category By Code Tests
   */
  describe('getTaskCategoryByCode', () => {
    it('should return task category when code found', async () => {
      mockRequest.params = { code: 'CTC001' };

      const mockCategory = {
        taskCategoryId: 'category-001',
        categoryCode: 'CTC001',
        categoryName: 'Vessel Inspection',
        description: 'Safety inspection',
        isActive: true,
        createdAt: new Date().toISOString(),
        updatedAt: new Date().toISOString(),
      };

      mockTaskCategoryService.getTaskCategoryByCode.mockResolvedValue(mockCategory);

      await taskCategoryController.getTaskCategoryByCode(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockTaskCategoryService.getTaskCategoryByCode).toHaveBeenCalledWith('CTC001');
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        data: mockCategory,
      });
    });

    it('should call next with error when code not found', async () => {
      mockRequest.params = { code: 'CTC999' };

      const error = new Error('Code not found');
      mockTaskCategoryService.getTaskCategoryByCode.mockRejectedValue(error);

      await taskCategoryController.getTaskCategoryByCode(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockNext).toHaveBeenCalledWith(error);
    });
  });

  /**
   * List Task Categories Tests
   */
  describe('listTaskCategories', () => {
    it('should list all categories when activeOnly not specified', async () => {
      mockRequest.query = {};

      const mockCategories = [
        {
          taskCategoryId: 'category-001',
          categoryCode: 'CTC001',
          categoryName: 'Category 1',
          isActive: true,
        },
        {
          taskCategoryId: 'category-002',
          categoryCode: 'CTC002',
          categoryName: 'Category 2',
          isActive: false,
        },
      ];

      mockTaskCategoryService.listTaskCategories.mockResolvedValue(mockCategories as any);

      await taskCategoryController.listTaskCategories(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockTaskCategoryService.listTaskCategories).toHaveBeenCalledWith(
        { activeOnly: false },
        expect.any(Object)
      );
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        data: mockCategories,
        count: 2,
      });
    });

    it('should list only active categories when activeOnly=true', async () => {
      mockRequest.query = { activeOnly: 'true' };

      const mockActiveCategories = [
        {
          taskCategoryId: 'category-001',
          categoryCode: 'CTC001',
          categoryName: 'Active Category',
          isActive: true,
        },
      ];

      mockTaskCategoryService.listTaskCategories.mockResolvedValue(mockActiveCategories as any);

      await taskCategoryController.listTaskCategories(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockTaskCategoryService.listTaskCategories).toHaveBeenCalledWith(
        { activeOnly: true },
        expect.any(Object)
      );
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        data: mockActiveCategories,
        count: 1,
      });
    });

    it('should pass sorting options to service', async () => {
      mockRequest.query = {
        sortBy: 'categoryName',
        sortOrder: 'asc',
      };

      mockTaskCategoryService.listTaskCategories.mockResolvedValue([]);

      await taskCategoryController.listTaskCategories(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockTaskCategoryService.listTaskCategories).toHaveBeenCalledWith(
        { activeOnly: false },
        { sortBy: 'categoryName', sortOrder: 'asc' }
      );
    });

    it('should handle empty result', async () => {
      mockRequest.query = {};

      mockTaskCategoryService.listTaskCategories.mockResolvedValue([]);

      await taskCategoryController.listTaskCategories(
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

    it('should call next with error on service failure', async () => {
      mockRequest.query = {};

      const error = new Error('Database error');
      mockTaskCategoryService.listTaskCategories.mockRejectedValue(error);

      await taskCategoryController.listTaskCategories(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockNext).toHaveBeenCalledWith(error);
    });
  });

  /**
   * Update Task Category Tests
   */
  describe('updateTaskCategory', () => {
    it('should update task category successfully', async () => {
      mockRequest.params = { id: 'category-001' };
      mockRequest.body = {
        categoryName: 'Updated Name',
        description: 'Updated description',
        defaultDurationHours: 3,
      };

      const mockUpdated = {
        taskCategoryId: 'category-001',
        categoryCode: 'CTC001',
        categoryName: 'Updated Name',
        description: 'Updated description',
        defaultDurationHours: 3,
        isActive: true,
        createdAt: new Date().toISOString(),
        updatedAt: new Date().toISOString(),
      };

      mockTaskCategoryService.updateTaskCategory.mockResolvedValue(mockUpdated);

      await taskCategoryController.updateTaskCategory(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockTaskCategoryService.updateTaskCategory).toHaveBeenCalledWith('category-001', {
        categoryName: 'Updated Name',
        description: 'Updated description',
        defaultDurationHours: 3,
      });
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        message: 'Task category updated successfully',
        data: mockUpdated,
      });
    });

    it('should handle partial updates', async () => {
      mockRequest.params = { id: 'category-001' };
      mockRequest.body = {
        description: 'New description only',
      };

      const mockUpdated = {
        taskCategoryId: 'category-001',
        categoryCode: 'CTC001',
        categoryName: 'Original Name',
        description: 'New description only',
        isActive: true,
        createdAt: new Date().toISOString(),
        updatedAt: new Date().toISOString(),
      };

      mockTaskCategoryService.updateTaskCategory.mockResolvedValue(mockUpdated);

      await taskCategoryController.updateTaskCategory(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockTaskCategoryService.updateTaskCategory).toHaveBeenCalledWith('category-001', {
        description: 'New description only',
      });
    });

    it('should call next with error on service failure', async () => {
      mockRequest.params = { id: 'category-001' };
      mockRequest.body = { categoryName: 'Duplicate Name' };

      const error = new Error('Name already exists');
      mockTaskCategoryService.updateTaskCategory.mockRejectedValue(error);

      await taskCategoryController.updateTaskCategory(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockNext).toHaveBeenCalledWith(error);
    });
  });

  /**
   * Deactivate Task Category Tests
   */
  describe('deactivateTaskCategory', () => {
    it('should deactivate task category successfully', async () => {
      mockRequest.params = { id: 'category-001' };

      const mockDeactivated = {
        taskCategoryId: 'category-001',
        categoryCode: 'CTC001',
        categoryName: 'Test Category',
        description: 'Test description',
        isActive: false,
        createdAt: new Date().toISOString(),
        updatedAt: new Date().toISOString(),
      };

      mockTaskCategoryService.deactivateTaskCategory.mockResolvedValue(mockDeactivated);

      await taskCategoryController.deactivateTaskCategory(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockTaskCategoryService.deactivateTaskCategory).toHaveBeenCalledWith('category-001');
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        message: 'Task category deactivated successfully',
        data: mockDeactivated,
      });
    });

    it('should call next with error when category in use', async () => {
      mockRequest.params = { id: 'category-001' };

      const error = new Error('Category is used in existing tasks');
      mockTaskCategoryService.deactivateTaskCategory.mockRejectedValue(error);

      await taskCategoryController.deactivateTaskCategory(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockNext).toHaveBeenCalledWith(error);
    });
  });

  /**
   * Reactivate Task Category Tests
   */
  describe('reactivateTaskCategory', () => {
    it('should reactivate task category successfully', async () => {
      mockRequest.params = { id: 'category-001' };

      const mockReactivated = {
        taskCategoryId: 'category-001',
        categoryCode: 'CTC001',
        categoryName: 'Test Category',
        description: 'Test description',
        isActive: true,
        createdAt: new Date().toISOString(),
        updatedAt: new Date().toISOString(),
      };

      mockTaskCategoryService.reactivateTaskCategory.mockResolvedValue(mockReactivated);

      await taskCategoryController.reactivateTaskCategory(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockTaskCategoryService.reactivateTaskCategory).toHaveBeenCalledWith('category-001');
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        message: 'Task category reactivated successfully',
        data: mockReactivated,
      });
    });

    it('should call next with error on service failure', async () => {
      mockRequest.params = { id: 'non-existent' };

      const error = new Error('Category not found');
      mockTaskCategoryService.reactivateTaskCategory.mockRejectedValue(error);

      await taskCategoryController.reactivateTaskCategory(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockNext).toHaveBeenCalledWith(error);
    });
  });

  /**
   * Delete Task Category Tests
   */
  describe('deleteTaskCategory', () => {
    it('should delete task category successfully', async () => {
      mockRequest.params = { id: 'category-001' };

      mockTaskCategoryService.deleteTaskCategory.mockResolvedValue(undefined);

      await taskCategoryController.deleteTaskCategory(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockTaskCategoryService.deleteTaskCategory).toHaveBeenCalledWith('category-001');
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        message: 'Task category deleted successfully',
      });
    });

    it('should call next with error when category in use', async () => {
      mockRequest.params = { id: 'category-001' };

      const error = new Error('Cannot delete category used in tasks');
      mockTaskCategoryService.deleteTaskCategory.mockRejectedValue(error);

      await taskCategoryController.deleteTaskCategory(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockNext).toHaveBeenCalledWith(error);
    });

    it('should call next with error when category not found', async () => {
      mockRequest.params = { id: 'non-existent' };

      const error = new Error('Category not found');
      mockTaskCategoryService.deleteTaskCategory.mockRejectedValue(error);

      await taskCategoryController.deleteTaskCategory(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockNext).toHaveBeenCalledWith(error);
    });
  });

  /**
   * Get Task Category Count Tests
   */
  describe('getTaskCategoryCount', () => {
    it('should return total count', async () => {
      mockRequest.query = {};

      mockTaskCategoryService.getTaskCategoryCount.mockResolvedValue(10);

      await taskCategoryController.getTaskCategoryCount(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockTaskCategoryService.getTaskCategoryCount).toHaveBeenCalledWith(false);
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        data: { count: 10 },
      });
    });

    it('should return active count when activeOnly=true', async () => {
      mockRequest.query = { activeOnly: 'true' };

      mockTaskCategoryService.getTaskCategoryCount.mockResolvedValue(8);

      await taskCategoryController.getTaskCategoryCount(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockTaskCategoryService.getTaskCategoryCount).toHaveBeenCalledWith(true);
      expect(mockResponse.json).toHaveBeenCalledWith({
        success: true,
        data: { count: 8 },
      });
    });

    it('should handle errors', async () => {
      mockRequest.query = {};

      const error = new Error('Database error');
      mockTaskCategoryService.getTaskCategoryCount.mockRejectedValue(error);

      await taskCategoryController.getTaskCategoryCount(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockNext).toHaveBeenCalledWith(error);
    });
  });

  /**
   * Request Validation Tests
   */
  describe('request validation', () => {
    it('should handle missing parameters gracefully', async () => {
      mockRequest.params = {};

      await taskCategoryController.getTaskCategoryById(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      // Service will be called with undefined, which should trigger error in service
      expect(mockTaskCategoryService.getTaskCategoryById).toHaveBeenCalledWith(undefined);
    });

    it('should pass through all query parameters for sorting', async () => {
      mockRequest.query = {
        sortBy: 'categoryCode',
        sortOrder: 'desc',
        activeOnly: 'false',
      };

      mockTaskCategoryService.listTaskCategories.mockResolvedValue([]);

      await taskCategoryController.listTaskCategories(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      expect(mockTaskCategoryService.listTaskCategories).toHaveBeenCalledWith(
        { activeOnly: false },
        { sortBy: 'categoryCode', sortOrder: 'desc' }
      );
    });
  });

  /**
   * Response Format Tests
   */
  describe('response formatting', () => {
    it('should return consistent success response format', async () => {
      mockRequest.params = { id: 'category-001' };
      mockRequest.body = { description: 'Updated' };

      const mockResult = {
        taskCategoryId: 'category-001',
        categoryCode: 'CTC001',
        categoryName: 'Test',
        description: 'Updated',
        isActive: true,
        createdAt: new Date().toISOString(),
        updatedAt: new Date().toISOString(),
      };

      mockTaskCategoryService.updateTaskCategory.mockResolvedValue(mockResult);

      await taskCategoryController.updateTaskCategory(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      const response = (mockResponse.json as jest.Mock).mock.calls[0][0];
      expect(response).toHaveProperty('success', true);
      expect(response).toHaveProperty('message');
      expect(response).toHaveProperty('data');
    });

    it('should include count in list responses', async () => {
      mockRequest.query = {};

      mockTaskCategoryService.listTaskCategories.mockResolvedValue([
        { taskCategoryId: '1', categoryCode: 'CTC001' },
        { taskCategoryId: '2', categoryCode: 'CTC002' },
      ] as any);

      await taskCategoryController.listTaskCategories(
        mockRequest as Request,
        mockResponse as Response,
        mockNext
      );

      const response = (mockResponse.json as jest.Mock).mock.calls[0][0];
      expect(response).toHaveProperty('success', true);
      expect(response).toHaveProperty('data');
      expect(response).toHaveProperty('count', 2);
    });
  });
});
