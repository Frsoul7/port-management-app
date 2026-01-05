import { TaskCategoryService } from '@application/services/TaskCategoryService';
import { ITaskCategoryRepository } from '@domain/repositories/ITaskCategoryRepository';
import { TaskCategory } from '@domain/entities/TaskCategory';

/**
 * Unit Tests for TaskCategoryService
 * US 4.1.14: Task Category Catalog Management - Application Service Layer
 * 
 * Test Coverage:
 * - Task category creation with code generation
 * - Task category retrieval (by ID, by code)
 * - Listing categories (all, active only)
 * - Update operations
 * - Activation/deactivation workflow
 * - Delete operations
 * - Error handling
 * - DTO conversion
 */

describe('TaskCategoryService', () => {
  let taskCategoryService: TaskCategoryService;
  let mockRepository: jest.Mocked<ITaskCategoryRepository>;
  let mockTaskCategory: TaskCategory;

  beforeEach(() => {
    // Create mock repository
    mockRepository = {
      save: jest.fn(),
      update: jest.fn(),
      delete: jest.fn(),
      findById: jest.fn(),
      findByCode: jest.fn(),
      findByName: jest.fn(),
      findAll: jest.fn(),
      findActive: jest.fn(),
      existsByCode: jest.fn(),
      existsByName: jest.fn(),
      getNextSequentialNumber: jest.fn(),
      isUsedInTasks: jest.fn(),
      count: jest.fn(),
    } as any;

    taskCategoryService = new TaskCategoryService(mockRepository);

    // Create mock task category
    mockTaskCategory = new TaskCategory({
      categoryCode: 'CTC001',
      categoryName: 'Vessel Inspection',
      description: 'Safety and compliance inspection',
      defaultDurationHours: 2,
      expectedImpact: 'Requires vessel to be stationary',
    });
  });

  /**
   * Create Task Category Tests
   */
  describe('createTaskCategory', () => {
    /**
     * Test: Creates task category with auto-generated CTC code
     * Business rule: System generates sequential codes (CTC001, CTC002, etc.)
     */
    it('should create task category with auto-generated code', async () => {
      const createDto = {
        categoryName: 'Equipment Maintenance',
        description: 'Regular maintenance of port equipment',
        defaultDurationHours: 3,
        expectedImpact: 'Minor delay in operations',
      };

      mockRepository.findByName.mockResolvedValue(null);
      mockRepository.getNextSequentialNumber.mockResolvedValue(1);
      mockRepository.findByCode.mockResolvedValue(null);
      mockRepository.save.mockResolvedValue(mockTaskCategory);

      const result = await taskCategoryService.createTaskCategory(createDto);

      expect(mockRepository.findByName).toHaveBeenCalledWith('Equipment Maintenance');
      expect(mockRepository.getNextSequentialNumber).toHaveBeenCalled();
      expect(mockRepository.findByCode).toHaveBeenCalledWith('CTC001');
      expect(mockRepository.save).toHaveBeenCalled();
      expect(result).toBeDefined();
      expect(result.categoryName).toBe('Vessel Inspection');
    });

    /**
     * Test: Generates sequential category codes
     * Business rule: Code numbering follows CTC### pattern with auto-increment
     */
    it('should generate sequential codes (CTC001, CTC002, etc.)', async () => {
      const createDto = {
        categoryName: 'Test Category',
        description: 'Test description',
      };

      mockRepository.findByName.mockResolvedValue(null);
      mockRepository.getNextSequentialNumber.mockResolvedValue(5);
      mockRepository.findByCode.mockResolvedValue(null);

      const categoryWithCode = new TaskCategory({
        categoryCode: 'CTC005',
        categoryName: 'Test Category',
        description: 'Test description',
      });

      mockRepository.save.mockResolvedValue(categoryWithCode);

      await taskCategoryService.createTaskCategory(createDto);

      expect(mockRepository.getNextSequentialNumber).toHaveBeenCalled();
      expect(mockRepository.findByCode).toHaveBeenCalledWith('CTC005');
    });

    it('should throw error when category name already exists', async () => {
      const createDto = {
        categoryName: 'Vessel Inspection',
        description: 'Duplicate name',
      };

      mockRepository.findByName.mockResolvedValue(mockTaskCategory);

      await expect(taskCategoryService.createTaskCategory(createDto)).rejects.toThrow(
        'Task category with name Vessel Inspection already exists'
      );

      expect(mockRepository.save).not.toHaveBeenCalled();
    });

    it('should create category with optional fields', async () => {
      const createDto = {
        categoryName: 'Emergency Repair',
        description: 'Urgent equipment repair',
        defaultDurationHours: 4,
        expectedImpact: 'Significant operational impact',
      };

      mockRepository.findByName.mockResolvedValue(null);
      mockRepository.getNextSequentialNumber.mockResolvedValue(10);
      mockRepository.findByCode.mockResolvedValue(null);

      const category = new TaskCategory({
        categoryCode: 'CTC010',
        categoryName: 'Emergency Repair',
        description: 'Urgent equipment repair',
        defaultDurationHours: 4,
        expectedImpact: 'Significant operational impact',
      });

      mockRepository.save.mockResolvedValue(category);

      const result = await taskCategoryService.createTaskCategory(createDto);

      expect(result.defaultDurationHours).toBe(4);
      expect(result.expectedImpact).toBe('Significant operational impact');
    });

    it('should create category without optional fields', async () => {
      const createDto = {
        categoryName: 'Documentation Review',
        description: 'Review of vessel documentation',
      };

      mockRepository.findByName.mockResolvedValue(null);
      mockRepository.getNextSequentialNumber.mockResolvedValue(15);
      mockRepository.findByCode.mockResolvedValue(null);

      const category = new TaskCategory({
        categoryCode: 'CTC015',
        categoryName: 'Documentation Review',
        description: 'Review of vessel documentation',
      });

      mockRepository.save.mockResolvedValue(category);

      const result = await taskCategoryService.createTaskCategory(createDto);

      expect(result.defaultDurationHours).toBeUndefined();
      expect(result.expectedImpact).toBeUndefined();
    });
  });

  /**
   * Get Task Category Tests
   */
  describe('getTaskCategoryById', () => {
    /**
     * Test: Retrieves category by unique identifier
     * Business rule: Each category has unique ID for reference
     */
    it('should retrieve task category by ID', async () => {
      mockRepository.findById.mockResolvedValue(mockTaskCategory);

      const result = await taskCategoryService.getTaskCategoryById('category-001');

      expect(mockRepository.findById).toHaveBeenCalledWith('category-001');
      expect(result.categoryCode).toBe('CTC001');
      expect(result.categoryName).toBe('Vessel Inspection');
    });

    /**
     * Test: Throws error for non-existent category ID
     * Business rule: Invalid category IDs result in clear error messages
     */
    it('should throw error when category not found', async () => {
      mockRepository.findById.mockResolvedValue(null);

      await expect(
        taskCategoryService.getTaskCategoryById('non-existent')
      ).rejects.toThrow('Task category non-existent not found');
    });
  });

  describe('getTaskCategoryByCode', () => {
    /**
     * Test: Retrieves category by CTC code
     * Business rule: Categories can be looked up by readable code
     */
    it('should retrieve task category by code', async () => {
      mockRepository.findByCode.mockResolvedValue(mockTaskCategory);

      const result = await taskCategoryService.getTaskCategoryByCode('CTC001');

      expect(mockRepository.findByCode).toHaveBeenCalledWith('CTC001');
      expect(result.categoryCode).toBe('CTC001');
    });

    /**
     * Test: Throws error for non-existent category code
     * Business rule: Invalid codes result in descriptive errors
     */
    it('should throw error when code not found', async () => {
      mockRepository.findByCode.mockResolvedValue(null);

      await expect(
        taskCategoryService.getTaskCategoryByCode('CTC999')
      ).rejects.toThrow('Task category with code CTC999 not found');
    });
  });

  /**
   * List Task Categories Tests
   */
  describe('listAllTaskCategories', () => {
    /**
     * Test: Returns all task categories in system
     * Business rule: Provides complete catalog of task types
     */
    it('should list all categories', async () => {
      const categories = [
        mockTaskCategory,
        new TaskCategory({
          categoryCode: 'CTC002',
          categoryName: 'Equipment Maintenance',
          description: 'Maintenance tasks',
        }),
      ];

      mockRepository.findAll.mockResolvedValue(categories);

      const result = await taskCategoryService.listAllTaskCategories();

      expect(mockRepository.findAll).toHaveBeenCalled();
      expect(result).toHaveLength(2);
      expect(result[0]!.categoryCode).toBe('CTC001');
      expect(result[1]!.categoryCode).toBe('CTC002');
    });

    /**
     * Test: Applies pagination and sorting to category list
     * Business rule: Supports large category catalogs with query options
     */
    it('should support query options', async () => {
      mockRepository.findAll.mockResolvedValue([mockTaskCategory]);

      const options = { sortBy: 'categoryName', sortOrder: 'asc' as const };
      await taskCategoryService.listAllTaskCategories(options);

      expect(mockRepository.findAll).toHaveBeenCalledWith(options);
    });

    /**
     * Test: Returns empty list when no categories defined
     * Business rule: Empty catalog is valid state for new systems
     */
    it('should return empty array when no categories exist', async () => {
      mockRepository.findAll.mockResolvedValue([]);

      const result = await taskCategoryService.listAllTaskCategories();

      expect(result).toHaveLength(0);
    });
  });

  describe('listActiveTaskCategories', () => {
    /**
     * Test: Returns only active (not deactivated) categories
     * Business rule: Filters out deprecated task types
     */
    it('should list only active categories', async () => {
      const activeCategories = [mockTaskCategory];

      mockRepository.findActive.mockResolvedValue(activeCategories);

      const result = await taskCategoryService.listActiveTaskCategories();

      expect(mockRepository.findActive).toHaveBeenCalled();
      expect(result).toHaveLength(1);
      expect(result[0]!.isActive).toBe(true);
    });

    it('should not include deactivated categories', async () => {
      mockRepository.findActive.mockResolvedValue([]);

      const result = await taskCategoryService.listActiveTaskCategories();

      expect(result).toHaveLength(0);
    });
  });

  /**
   * Update Task Category Tests
   */
  describe('updateTaskCategory', () => {
    /**
     * Test: Updates category name while ensuring uniqueness
     * Business rule: Category names must remain unique after updates
     */
    it('should update category name', async () => {
      const updateDto = {
        categoryName: 'Updated Vessel Inspection',
      };

      mockRepository.findById.mockResolvedValue(mockTaskCategory);
      mockRepository.existsByName.mockResolvedValue(false);
      mockRepository.update.mockResolvedValue(mockTaskCategory);

      const result = await taskCategoryService.updateTaskCategory('category-001', updateDto);

      expect(mockRepository.findById).toHaveBeenCalledWith('category-001');
      expect(mockRepository.existsByName).toHaveBeenCalledWith(
        'Updated Vessel Inspection',
        'category-001'
      );
      expect(mockRepository.update).toHaveBeenCalled();
      expect(result).toBeDefined();
    });

    /**
     * Test: Updates category description field only
     * Business rule: Supports partial updates for category information
     */
    it('should update description', async () => {
      const updateDto = {
        description: 'Comprehensive safety inspection',
      };

      mockRepository.findById.mockResolvedValue(mockTaskCategory);
      mockRepository.update.mockResolvedValue(mockTaskCategory);

      await taskCategoryService.updateTaskCategory('category-001', updateDto);

      expect(mockRepository.update).toHaveBeenCalled();
    });

    /**
     * Test: Updates default task duration estimate
     * Business rule: Duration helps in planning and scheduling
     */
    it('should update default duration', async () => {
      const updateDto = {
        defaultDurationHours: 3,
      };

      mockRepository.findById.mockResolvedValue(mockTaskCategory);
      mockRepository.update.mockResolvedValue(mockTaskCategory);

      await taskCategoryService.updateTaskCategory('category-001', updateDto);

      expect(mockRepository.update).toHaveBeenCalled();
    });

    /**
     * Test: Updates operational impact description
     * Business rule: Impact field describes effect on operations
     */
    it('should update expected impact', async () => {
      const updateDto = {
        expectedImpact: 'Moderate operational impact',
      };

      mockRepository.findById.mockResolvedValue(mockTaskCategory);
      mockRepository.update.mockResolvedValue(mockTaskCategory);

      await taskCategoryService.updateTaskCategory('category-001', updateDto);

      expect(mockRepository.update).toHaveBeenCalled();
    });

    /**
     * Test: Updates multiple category fields in single operation
     * Business rule: Atomic updates ensure consistency
     */
    it('should update multiple fields at once', async () => {
      const updateDto = {
        categoryName: 'Enhanced Inspection',
        description: 'Enhanced safety protocols',
        defaultDurationHours: 4,
        expectedImpact: 'High impact',
      };

      mockRepository.findById.mockResolvedValue(mockTaskCategory);
      mockRepository.existsByName.mockResolvedValue(false);
      mockRepository.update.mockResolvedValue(mockTaskCategory);

      await taskCategoryService.updateTaskCategory('category-001', updateDto);

      expect(mockRepository.update).toHaveBeenCalled();
    });

    /**
     * Test: Validates category exists before update
     * Business rule: Cannot update non-existent categories
     */
    it('should throw error when updating non-existent category', async () => {
      const updateDto = { categoryName: 'New Name' };

      mockRepository.findById.mockResolvedValue(null);

      await expect(
        taskCategoryService.updateTaskCategory('non-existent', updateDto)
      ).rejects.toThrow('Task category non-existent not found');
    });

    /**
     * Test: Prevents updating to duplicate category name
     * Business rule: Name uniqueness enforced even during updates
     */
    it('should throw error when new name already exists', async () => {
      const updateDto = { categoryName: 'Existing Name' };

      mockRepository.findById.mockResolvedValue(mockTaskCategory);
      mockRepository.existsByName.mockResolvedValue(true);

      await expect(
        taskCategoryService.updateTaskCategory('category-001', updateDto)
      ).rejects.toThrow('Task category with name Existing Name already exists');
    });

    /**
     * Test: Skips uniqueness check for non-name updates
     * Business rule: Performance optimization - only validate when needed
     */
    it('should not check name uniqueness when name not changing', async () => {
      const updateDto = {
        description: 'Updated description',
        defaultDurationHours: 3,
      };

      mockRepository.findById.mockResolvedValue(mockTaskCategory);
      mockRepository.update.mockResolvedValue(mockTaskCategory);

      await taskCategoryService.updateTaskCategory('category-001', updateDto);

      expect(mockRepository.existsByName).not.toHaveBeenCalled();
    });
  });

  /**
   * Deactivate Task Category Tests
   */
  describe('deactivateTaskCategory', () => {
    /**
     * Test: Deactivates unused category
     * Business rule: Categories not in use can be retired
     */
    it('should deactivate category not used in tasks', async () => {
      mockRepository.findById.mockResolvedValue(mockTaskCategory);
      mockRepository.isUsedInTasks.mockResolvedValue(false);
      mockRepository.update.mockResolvedValue(mockTaskCategory);

      const result = await taskCategoryService.deactivateTaskCategory('category-001');

      expect(mockRepository.isUsedInTasks).toHaveBeenCalledWith('category-001');
      expect(mockRepository.update).toHaveBeenCalled();
      expect(result).toBeDefined();
    });

    /**
     * Test: Prevents deactivating categories with existing tasks
     * Business rule: Cannot deactivate categories in active use
     */
    it('should throw error when category is used in tasks', async () => {
      mockRepository.findById.mockResolvedValue(mockTaskCategory);
      mockRepository.isUsedInTasks.mockResolvedValue(true);

      await expect(
        taskCategoryService.deactivateTaskCategory('category-001')
      ).rejects.toThrow('Cannot deactivate task category that is used in existing complementary tasks');

      expect(mockRepository.update).not.toHaveBeenCalled();
    });

    /**
     * Test: Validates category exists before deactivation
     * Business rule: Cannot deactivate non-existent categories
     */
    it('should throw error when category not found', async () => {
      mockRepository.findById.mockResolvedValue(null);

      await expect(
        taskCategoryService.deactivateTaskCategory('non-existent')
      ).rejects.toThrow('Task category non-existent not found');
    });
  });

  /**
   * Reactivate Task Category Tests
   */
  describe('reactivateTaskCategory', () => {
    it('should reactivate deactivated category', async () => {
      // Create deactivated category
      const deactivated = new TaskCategory({
        categoryCode: 'CTC001',
        categoryName: 'Test Category',
        description: 'Test',
      });
      deactivated.deactivate();

      mockRepository.findById.mockResolvedValue(deactivated);
      mockRepository.update.mockResolvedValue(deactivated);

      const result = await taskCategoryService.reactivateTaskCategory('category-001');

      expect(mockRepository.update).toHaveBeenCalled();
      expect(result).toBeDefined();
    });

    /**
     * Test: Validates category exists before reactivation
     * Business rule: Cannot reactivate non-existent categories
     */
    it('should throw error when category not found', async () => {
      mockRepository.findById.mockResolvedValue(null);

      await expect(
        taskCategoryService.reactivateTaskCategory('non-existent')
      ).rejects.toThrow('Task category non-existent not found');
    });
  });

  /**
   * Delete Task Category Tests
   */
  describe('deleteTaskCategory', () => {
    /**
     * Test: Deletes unused category from system
     * Business rule: Only unused categories can be permanently deleted
     */
    it('should delete category not used in tasks', async () => {
      mockRepository.findById.mockResolvedValue(mockTaskCategory);
      mockRepository.isUsedInTasks.mockResolvedValue(false);
      mockRepository.delete.mockResolvedValue(undefined);

      await taskCategoryService.deleteTaskCategory('category-001');

      expect(mockRepository.isUsedInTasks).toHaveBeenCalledWith('category-001');
      expect(mockRepository.delete).toHaveBeenCalledWith('category-001');
    });

    /**
     * Test: Prevents deleting categories with existing tasks
     * Business rule: Referential integrity - cannot delete categories in use
     */
    it('should throw error when category is used in tasks', async () => {
      mockRepository.findById.mockResolvedValue(mockTaskCategory);
      mockRepository.isUsedInTasks.mockResolvedValue(true);

      await expect(
        taskCategoryService.deleteTaskCategory('category-001')
      ).rejects.toThrow('Cannot delete task category that is used in existing complementary tasks');

      expect(mockRepository.delete).not.toHaveBeenCalled();
    });

    /**
     * Test: Validates category exists before deletion
     * Business rule: Cannot delete non-existent categories
     */
    it('should throw error when category not found', async () => {
      mockRepository.findById.mockResolvedValue(null);

      await expect(
        taskCategoryService.deleteTaskCategory('non-existent')
      ).rejects.toThrow('Task category non-existent not found');
    });
  });

  /**
   * Get Count Tests
   */
  describe('getTaskCategoryCount', () => {
    /**
     * Test: Returns total count including active and inactive
     * Business rule: Count supports dashboard metrics
     */
    it('should get total count of all categories', async () => {
      mockRepository.count.mockResolvedValue(10);

      const result = await taskCategoryService.getTaskCategoryCount(false);

      expect(mockRepository.count).toHaveBeenCalledWith(false);
      expect(result).toBe(10);
    });

    /**
     * Test: Returns count of only active categories
     * Business rule: Active count shows available task types
     */
    it('should get count of active categories only', async () => {
      mockRepository.count.mockResolvedValue(8);

      const result = await taskCategoryService.getTaskCategoryCount(true);

      expect(mockRepository.count).toHaveBeenCalledWith(true);
      expect(result).toBe(8);
    });
  });

  /**
   * DTO Conversion Tests
   */
  describe('DTO conversion', () => {
    /**
     * Test: Converts complete entity to DTO with all fields
     * Business rule: DTO separates domain from presentation layer
     */
    it('should convert category to DTO with all fields', async () => {
      mockRepository.findById.mockResolvedValue(mockTaskCategory);

      const result = await taskCategoryService.getTaskCategoryById('category-001');

      // Verify all DTO fields are present
      expect(result).toHaveProperty('taskCategoryId');
      expect(result).toHaveProperty('categoryCode');
      expect(result).toHaveProperty('categoryName');
      expect(result).toHaveProperty('description');
      expect(result).toHaveProperty('defaultDurationHours');
      expect(result).toHaveProperty('expectedImpact');
      expect(result).toHaveProperty('isActive');
      expect(result).toHaveProperty('createdAt');
      expect(result).toHaveProperty('updatedAt');
    });

    /**
     * Test: Handles optional fields correctly in DTO
     * Business rule: DTO represents undefined optional fields properly
     */
    it('should convert category with optional fields as undefined', async () => {
      const minimalCategory = new TaskCategory({
        categoryCode: 'CTC020',
        categoryName: 'Minimal Category',
        description: 'Minimal description',
      });

      mockRepository.findById.mockResolvedValue(minimalCategory);

      const result = await taskCategoryService.getTaskCategoryById('category-020');

      expect(result.defaultDurationHours).toBeUndefined();
      expect(result.expectedImpact).toBeUndefined();
    });

    /**
     * Test: Converts timestamps to ISO string format
     * Business rule: DTO uses standard ISO format for dates
     */
    it('should convert timestamps to ISO strings', async () => {
      mockRepository.findById.mockResolvedValue(mockTaskCategory);

      const result = await taskCategoryService.getTaskCategoryById('category-001');

      expect(typeof result.createdAt).toBe('string');
      expect(typeof result.updatedAt).toBe('string');
      expect(() => new Date(result.createdAt)).not.toThrow();
      expect(() => new Date(result.updatedAt)).not.toThrow();
    });
  });

  /**
   * Business Rule Tests
   */
  describe('business rules', () => {
    /**
     * Test: Enforces unique name constraint on creation
     * Business rule: Category names must be unique system-wide
     */
    it('should enforce unique category names', async () => {
      const createDto = {
        categoryName: 'Duplicate Name',
        description: 'Test',
      };

      mockRepository.findByName.mockResolvedValue(mockTaskCategory);

      await expect(taskCategoryService.createTaskCategory(createDto)).rejects.toThrow(
        'Task category with name Duplicate Name already exists'
      );
    });

    /**
     * Test: Enforces unique name constraint on update
     * Business rule: Cannot update to name already in use
     */
    it('should prevent updating to existing name', async () => {
      const updateDto = { categoryName: 'Existing Name' };

      mockRepository.findById.mockResolvedValue(mockTaskCategory);
      mockRepository.existsByName.mockResolvedValue(true);

      await expect(
        taskCategoryService.updateTaskCategory('category-001', updateDto)
      ).rejects.toThrow('Task category with name Existing Name already exists');
    });

    /**
     * Test: Prevents deactivation of categories with active tasks
     * Business rule: Referential integrity - cannot deactivate if in use
     */
    it('should prevent deactivation of categories in use', async () => {
      mockRepository.findById.mockResolvedValue(mockTaskCategory);
      mockRepository.isUsedInTasks.mockResolvedValue(true);

      await expect(
        taskCategoryService.deactivateTaskCategory('category-001')
      ).rejects.toThrow('Cannot deactivate task category that is used in existing complementary tasks');
    });

    /**
     * Test: Prevents deletion of categories with active tasks
     * Business rule: Referential integrity - cannot delete if in use
     */
    it('should prevent deletion of categories in use', async () => {
      mockRepository.findById.mockResolvedValue(mockTaskCategory);
      mockRepository.isUsedInTasks.mockResolvedValue(true);

      await expect(
        taskCategoryService.deleteTaskCategory('category-001')
      ).rejects.toThrow('Cannot delete task category that is used in existing complementary tasks');
    });
  });

  /**
   * Error Handling Tests
   */
  describe('error handling', () => {
    /**
     * Test: Propagates repository errors during creation
     * Business rule: Database errors bubble up to caller
     */
    it('should handle repository errors during creation', async () => {
      const createDto = {
        categoryName: 'Test Category',
        description: 'Test',
      };

      mockRepository.findByName.mockResolvedValue(null);
      mockRepository.getNextSequentialNumber.mockRejectedValue(new Error('Database error'));

      await expect(taskCategoryService.createTaskCategory(createDto)).rejects.toThrow(
        'Database error'
      );
    });

    /**
     * Test: Propagates repository errors during updates
     * Business rule: Update failures are reported to caller
     */
    it('should handle repository errors during update', async () => {
      const updateDto = { description: 'Updated' };

      mockRepository.findById.mockResolvedValue(mockTaskCategory);
      mockRepository.update.mockRejectedValue(new Error('Update failed'));

      await expect(
        taskCategoryService.updateTaskCategory('category-001', updateDto)
      ).rejects.toThrow('Update failed');
    });

    /**
     * Test: Propagates repository errors during queries
     * Business rule: Query failures are reported to caller
     */
    it('should handle repository errors during list operations', async () => {
      mockRepository.findAll.mockRejectedValue(new Error('Query failed'));

      await expect(taskCategoryService.listAllTaskCategories()).rejects.toThrow('Query failed');
    });
  });
});
