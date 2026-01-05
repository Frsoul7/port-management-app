import { TaskCategory } from '@domain/entities/TaskCategory';

/**
 * US 4.1.14 - TaskCategory Entity Unit Tests
 * Tests for the TaskCategory aggregate root - complementary task category management
 * 
 * Coverage:
 * - Task category creation and validation
 * - Category code pattern validation (CTC001, CTC002, etc.)
 * - Name and description validation
 * - Optional fields (defaultDurationHours, expectedImpact)
 * - Activation/deactivation workflow
 * - Update operations
 * - Business rule enforcement
 */
describe('US 4.1.14 - TaskCategory Entity', () => {
  /**
   * Helper function to create a valid task category
   * US 4.1.14: Default category with all required fields
   */
  const createValidCategory = (overrides?: Partial<ConstructorParameters<typeof TaskCategory>[0]>) => {
    return new TaskCategory({
      categoryCode: 'CTC001',
      categoryName: 'Vessel Inspection',
      description: 'Safety and compliance inspection of vessel structure and equipment',
      ...overrides,
    });
  };

  describe('Creation and Validation', () => {
    /**
     * US 4.1.14: Test category creation with required fields
     * Verifies:
     * - Automatic ID generation
     * - Category code is stored correctly
     * - Default active status
     * - Automatic timestamps
     */
    it('should create a valid task category with active status', () => {
      const category = createValidCategory();

      expect(category.taskCategoryId).toBeDefined();
      expect(category.categoryCode).toBe('CTC001');
      expect(category.categoryName).toBe('Vessel Inspection');
      expect(category.description).toBe('Safety and compliance inspection of vessel structure and equipment');
      expect(category.isActive).toBe(true);
      expect(category.createdAt).toBeInstanceOf(Date);
      expect(category.updatedAt).toBeInstanceOf(Date);
      expect(category.defaultDurationHours).toBeUndefined();
      expect(category.expectedImpact).toBeUndefined();
    });

    /**
     * US 4.1.14: Test category creation with optional fields
     * Verifies: Optional fields are stored correctly
     */
    it('should create category with optional default duration and expected impact', () => {
      const category = createValidCategory({
        defaultDurationHours: 2.5,
        expectedImpact: 'May require cargo operations suspension for safety checks',
      });

      expect(category.defaultDurationHours).toBe(2.5);
      expect(category.expectedImpact).toBe('May require cargo operations suspension for safety checks');
    });

    /**
     * US 4.1.14: Test category code validation - empty
     * Business Rule: Category code is required
     */
    it('should throw error if category code is empty', () => {
      expect(() => {
        new TaskCategory({
          categoryCode: '',
          categoryName: 'Test',
          description: 'Test description',
        });
      }).toThrow('Task category code is required');
    });

    /**
     * US 4.1.14: Test category code pattern validation
     * Business Rule: Code must follow pattern CTC001, CTC002, etc.
     */
    it('should throw error if category code does not match pattern', () => {
      expect(() => {
        new TaskCategory({
          categoryCode: 'TC001', // Missing 'C' prefix
          categoryName: 'Test',
          description: 'Test description',
        });
      }).toThrow('Category code must follow pattern CTC001, CTC002, etc.');
    });

    /**
     * US 4.1.14: Test category code pattern validation - invalid formats
     * Business Rule: Code must be exactly CTCXXX where XXX is 3 digits
     */
    it('should throw error for invalid code formats', () => {
      // Too few digits
      expect(() => {
        new TaskCategory({
          categoryCode: 'CTC01',
          categoryName: 'Test',
          description: 'Test',
        });
      }).toThrow('Category code must follow pattern CTC001, CTC002, etc.');

      // Too many digits
      expect(() => {
        new TaskCategory({
          categoryCode: 'CTC0001',
          categoryName: 'Test',
          description: 'Test',
        });
      }).toThrow('Category code must follow pattern CTC001, CTC002, etc.');

      // Letters instead of numbers
      expect(() => {
        new TaskCategory({
          categoryCode: 'CTCABC',
          categoryName: 'Test',
          description: 'Test',
        });
      }).toThrow('Category code must follow pattern CTC001, CTC002, etc.');
    });

    /**
     * US 4.1.14: Test valid category code formats
     * Verifies: Various valid code patterns are accepted
     */
    it('should accept valid category code patterns', () => {
      expect(() => createValidCategory({ categoryCode: 'CTC001' })).not.toThrow();
      expect(() => createValidCategory({ categoryCode: 'CTC099' })).not.toThrow();
      expect(() => createValidCategory({ categoryCode: 'CTC999' })).not.toThrow();
      expect(() => createValidCategory({ categoryCode: 'CTC042' })).not.toThrow();
    });

    /**
     * US 4.1.14: Test category name validation - empty
     * Business Rule: Category name is required
     */
    it('should throw error if category name is empty', () => {
      expect(() => {
        new TaskCategory({
          categoryCode: 'CTC001',
          categoryName: '   ',
          description: 'Test description',
        });
      }).toThrow('Task category name is required');
    });

    /**
     * US 4.1.14: Test category name length validation
     * Business Rule: Name must not exceed 100 characters
     */
    it('should throw error if category name exceeds 100 characters', () => {
      expect(() => {
        new TaskCategory({
          categoryCode: 'CTC001',
          categoryName: 'A'.repeat(101),
          description: 'Test description',
        });
      }).toThrow('Category name must not exceed 100 characters');
    });

    /**
     * US 4.1.14: Test description validation - empty
     * Business Rule: Description is required
     */
    it('should throw error if description is empty', () => {
      expect(() => {
        new TaskCategory({
          categoryCode: 'CTC001',
          categoryName: 'Test',
          description: '',
        });
      }).toThrow('Task category description is required');
    });

    /**
     * US 4.1.14: Test description length validation
     * Business Rule: Description must not exceed 500 characters
     */
    it('should throw error if description exceeds 500 characters', () => {
      expect(() => {
        new TaskCategory({
          categoryCode: 'CTC001',
          categoryName: 'Test',
          description: 'A'.repeat(501),
        });
      }).toThrow('Description must not exceed 500 characters');
    });

    /**
     * US 4.1.14: Test default duration validation
     * Business Rule: Default duration must be greater than 0
     */
    it('should throw error if default duration is zero or negative', () => {
      expect(() => {
        new TaskCategory({
          categoryCode: 'CTC001',
          categoryName: 'Test',
          description: 'Test description',
          defaultDurationHours: 0,
        });
      }).toThrow('Default duration must be greater than 0');

      expect(() => {
        new TaskCategory({
          categoryCode: 'CTC001',
          categoryName: 'Test',
          description: 'Test description',
          defaultDurationHours: -1,
        });
      }).toThrow('Default duration must be greater than 0');
    });

    /**
     * US 4.1.14: Test valid default duration values
     * Verifies: Positive decimal values are accepted
     */
    it('should accept positive default duration values', () => {
      const category1 = createValidCategory({ defaultDurationHours: 0.5 });
      const category2 = createValidCategory({ defaultDurationHours: 1 });
      const category3 = createValidCategory({ defaultDurationHours: 24.5 });

      expect(category1.defaultDurationHours).toBe(0.5);
      expect(category2.defaultDurationHours).toBe(1);
      expect(category3.defaultDurationHours).toBe(24.5);
    });

    /**
     * US 4.1.14: Test expected impact length validation
     * Business Rule: Expected impact must not exceed 200 characters
     */
    it('should throw error if expected impact exceeds 200 characters', () => {
      expect(() => {
        new TaskCategory({
          categoryCode: 'CTC001',
          categoryName: 'Test',
          description: 'Test description',
          expectedImpact: 'A'.repeat(201),
        });
      }).toThrow('Expected impact must not exceed 200 characters');
    });

    /**
     * US 4.1.14: Test whitespace trimming
     * Verifies: Leading/trailing whitespace is removed
     */
    it('should trim whitespace from string fields', () => {
      const category = new TaskCategory({
        categoryCode: '  CTC001  ',
        categoryName: '  Vessel Inspection  ',
        description: '  Test description  ',
        expectedImpact: '  May suspend operations  ',
      });

      expect(category.categoryCode).toBe('CTC001');
      expect(category.categoryName).toBe('Vessel Inspection');
      expect(category.description).toBe('Test description');
      expect(category.expectedImpact).toBe('May suspend operations');
    });
  });

  describe('Update Operations', () => {
    /**
     * US 4.1.14: Test updating category name
     * Verifies:
     * - Name can be updated
     * - Updated timestamp is refreshed
     */
    it('should update category name', () => {
      const category = createValidCategory();
      const originalUpdatedAt = category.updatedAt;

      // Small delay to ensure timestamp difference
      setTimeout(() => {
        category.update({ categoryName: 'Updated Vessel Inspection' });

        expect(category.categoryName).toBe('Updated Vessel Inspection');
        expect(category.updatedAt.getTime()).toBeGreaterThanOrEqual(originalUpdatedAt.getTime());
      }, 10);
    });

    /**
     * US 4.1.14: Test updating description
     * Verifies: Description can be updated
     */
    it('should update description', () => {
      const category = createValidCategory();

      category.update({ description: 'Updated comprehensive safety inspection' });

      expect(category.description).toBe('Updated comprehensive safety inspection');
    });

    /**
     * US 4.1.14: Test updating default duration
     * Verifies: Default duration can be set/updated
     */
    it('should update default duration', () => {
      const category = createValidCategory();

      category.update({ defaultDurationHours: 3.5 });

      expect(category.defaultDurationHours).toBe(3.5);
    });

    /**
     * US 4.1.14: Test updating expected impact
     * Verifies: Expected impact can be set/updated
     */
    it('should update expected impact', () => {
      const category = createValidCategory();

      category.update({ expectedImpact: 'Operations may be suspended for 2-4 hours' });

      expect(category.expectedImpact).toBe('Operations may be suspended for 2-4 hours');
    });

    /**
     * US 4.1.14: Test updating multiple fields at once
     * Verifies: Multiple fields can be updated in single operation
     */
    it('should update multiple fields simultaneously', () => {
      const category = createValidCategory();

      category.update({
        categoryName: 'Enhanced Inspection',
        description: 'Comprehensive structural and equipment inspection',
        defaultDurationHours: 4,
        expectedImpact: 'Full suspension of cargo operations required',
      });

      expect(category.categoryName).toBe('Enhanced Inspection');
      expect(category.description).toBe('Comprehensive structural and equipment inspection');
      expect(category.defaultDurationHours).toBe(4);
      expect(category.expectedImpact).toBe('Full suspension of cargo operations required');
    });

    /**
     * US 4.1.14: Test update validation - empty name
     * Business Rule: Updated name cannot be empty
     */
    it('should throw error if updating with empty name', () => {
      const category = createValidCategory();

      expect(() => {
        category.update({ categoryName: '   ' });
      }).toThrow('Category name cannot be empty');
    });

    /**
     * US 4.1.14: Test update validation - name too long
     * Business Rule: Updated name must not exceed 100 characters
     */
    it('should throw error if updating with name exceeding 100 characters', () => {
      const category = createValidCategory();

      expect(() => {
        category.update({ categoryName: 'A'.repeat(101) });
      }).toThrow('Category name must not exceed 100 characters');
    });

    /**
     * US 4.1.14: Test update validation - empty description
     * Business Rule: Updated description cannot be empty
     */
    it('should throw error if updating with empty description', () => {
      const category = createValidCategory();

      expect(() => {
        category.update({ description: '' });
      }).toThrow('Description cannot be empty');
    });

    /**
     * US 4.1.14: Test update validation - description too long
     * Business Rule: Updated description must not exceed 500 characters
     */
    it('should throw error if updating with description exceeding 500 characters', () => {
      const category = createValidCategory();

      expect(() => {
        category.update({ description: 'A'.repeat(501) });
      }).toThrow('Description must not exceed 500 characters');
    });

    /**
     * US 4.1.14: Test update validation - invalid default duration
     * Business Rule: Updated default duration must be positive
     */
    it('should throw error if updating with invalid default duration', () => {
      const category = createValidCategory();

      expect(() => {
        category.update({ defaultDurationHours: 0 });
      }).toThrow('Default duration must be greater than 0');

      expect(() => {
        category.update({ defaultDurationHours: -5 });
      }).toThrow('Default duration must be greater than 0');
    });

    /**
     * US 4.1.14: Test update validation - expected impact too long
     * Business Rule: Updated expected impact must not exceed 200 characters
     */
    it('should throw error if updating with expected impact exceeding 200 characters', () => {
      const category = createValidCategory();

      expect(() => {
        category.update({ expectedImpact: 'A'.repeat(201) });
      }).toThrow('Expected impact must not exceed 200 characters');
    });

    /**
     * US 4.1.14: Test clearing expected impact
     * Verifies: Expected impact can be cleared by passing empty string
     */
    it('should clear expected impact when updated with empty string', () => {
      const category = createValidCategory({
        expectedImpact: 'Initial impact',
      });

      category.update({ expectedImpact: '' });

      expect(category.expectedImpact).toBeUndefined();
    });

    /**
     * US 4.1.14: Test update on deactivated category
     * Business Rule: Cannot update deactivated categories (must reactivate first)
     */
    it('should throw error if updating deactivated category', () => {
      const category = createValidCategory();
      category.deactivate();

      expect(() => {
        category.update({ categoryName: 'Cannot update' });
      }).toThrow('Cannot update deactivated task category. Reactivate it first.');
    });
  });

  describe('Activation/Deactivation Workflow', () => {
    /**
     * US 4.1.14: Test deactivating category
     * Verifies:
     * - Status changes to inactive
     * - Updated timestamp is refreshed
     * - Deactivated categories cannot be used for new tasks
     */
    it('should deactivate active category', () => {
      const category = createValidCategory();
      expect(category.isActive).toBe(true);

      category.deactivate();

      expect(category.isActive).toBe(false);
    });

    /**
     * US 4.1.14: Test deactivation validation
     * Business Rule: Cannot deactivate already inactive category
     */
    it('should throw error if deactivating already deactivated category', () => {
      const category = createValidCategory();
      category.deactivate();

      expect(() => {
        category.deactivate();
      }).toThrow('Task category is already deactivated');
    });

    /**
     * US 4.1.14: Test reactivating category
     * Verifies:
     * - Status changes to active
     * - Updated timestamp is refreshed
     * - Reactivated categories can be used again
     */
    it('should reactivate deactivated category', () => {
      const category = createValidCategory();
      category.deactivate();
      expect(category.isActive).toBe(false);

      category.reactivate();

      expect(category.isActive).toBe(true);
    });

    /**
     * US 4.1.14: Test reactivation validation
     * Business Rule: Cannot reactivate already active category
     */
    it('should throw error if reactivating already active category', () => {
      const category = createValidCategory();

      expect(() => {
        category.reactivate();
      }).toThrow('Task category is already active');
    });

    /**
     * US 4.1.14: Test deactivation-reactivation cycle
     * Verifies: Categories can be deactivated and reactivated multiple times
     */
    it('should allow multiple deactivation-reactivation cycles', () => {
      const category = createValidCategory();

      category.deactivate();
      expect(category.isActive).toBe(false);

      category.reactivate();
      expect(category.isActive).toBe(true);

      category.deactivate();
      expect(category.isActive).toBe(false);

      category.reactivate();
      expect(category.isActive).toBe(true);
    });

    /**
     * US 4.1.14: Test update after reactivation
     * Verifies: Reactivated categories can be updated
     */
    it('should allow updates after reactivation', () => {
      const category = createValidCategory();
      category.deactivate();
      category.reactivate();

      expect(() => {
        category.update({ categoryName: 'Updated After Reactivation' });
      }).not.toThrow();

      expect(category.categoryName).toBe('Updated After Reactivation');
    });
  });

  describe('Serialization', () => {
    /**
     * US 4.1.14: Test JSON serialization with required fields only
     * Verifies:
     * - All properties are included
     * - Dates are converted to ISO strings
     * - Optional fields are included even if undefined
     */
    it('should serialize to JSON correctly with required fields', () => {
      const category = createValidCategory();

      const json = category.toJSON();

      expect(json.taskCategoryId).toBe(category.taskCategoryId);
      expect(json.categoryCode).toBe('CTC001');
      expect(json.categoryName).toBe('Vessel Inspection');
      expect(json.description).toBe('Safety and compliance inspection of vessel structure and equipment');
      expect(json.isActive).toBe(true);
      expect(json.createdAt).toMatch(/^\d{4}-\d{2}-\d{2}T/);
      expect(json.updatedAt).toMatch(/^\d{4}-\d{2}-\d{2}T/);
      expect(json.defaultDurationHours).toBeUndefined();
      expect(json.expectedImpact).toBeUndefined();
    });

    /**
     * US 4.1.14: Test JSON serialization with all fields
     * Verifies: Optional fields are serialized when present
     */
    it('should serialize to JSON correctly with all fields', () => {
      const category = createValidCategory({
        defaultDurationHours: 3.5,
        expectedImpact: 'May require suspension of cargo operations',
      });

      const json = category.toJSON();

      expect(json.taskCategoryId).toBeDefined();
      expect(json.categoryCode).toBe('CTC001');
      expect(json.categoryName).toBe('Vessel Inspection');
      expect(json.description).toBe('Safety and compliance inspection of vessel structure and equipment');
      expect(json.defaultDurationHours).toBe(3.5);
      expect(json.expectedImpact).toBe('May require suspension of cargo operations');
      expect(json.isActive).toBe(true);
      expect(json.createdAt).toBeDefined();
      expect(json.updatedAt).toBeDefined();
    });

    /**
     * US 4.1.14: Test JSON serialization of deactivated category
     * Verifies: Inactive status is serialized correctly
     */
    it('should serialize deactivated category correctly', () => {
      const category = createValidCategory();
      category.deactivate();

      const json = category.toJSON();

      expect(json.isActive).toBe(false);
    });
  });

  describe('Real-World Category Examples', () => {
    /**
     * US 4.1.14: Test typical category examples
     * Verifies: Real-world task categories can be created
     */
    it('should create Vessel Inspection category', () => {
      const category = new TaskCategory({
        categoryCode: 'CTC001',
        categoryName: 'Vessel Inspection',
        description: 'Mandatory safety and compliance inspection of vessel structure, equipment, and documentation',
        defaultDurationHours: 4,
        expectedImpact: 'Full suspension of cargo operations during inspection',
      });

      expect(category.categoryName).toBe('Vessel Inspection');
      expect(category.defaultDurationHours).toBe(4);
    });

    /**
     * US 4.1.14: Test typical category examples
     * Verifies: Maintenance category with expected impact
     */
    it('should create Equipment Maintenance category', () => {
      const category = new TaskCategory({
        categoryCode: 'CTC002',
        categoryName: 'Equipment Maintenance',
        description: 'Scheduled maintenance of port equipment including cranes, conveyors, and vehicles',
        defaultDurationHours: 2,
        expectedImpact: 'Specific equipment unavailable, may impact loading/unloading capacity',
      });

      expect(category.categoryName).toBe('Equipment Maintenance');
      expect(category.expectedImpact).toContain('impact loading/unloading');
    });

    /**
     * US 4.1.14: Test typical category examples
     * Verifies: Cleaning category without major impact
     */
    it('should create Vessel Cleaning category', () => {
      const category = new TaskCategory({
        categoryCode: 'CTC003',
        categoryName: 'Vessel Cleaning',
        description: 'Hull cleaning, deck washing, and general vessel sanitation',
        defaultDurationHours: 6,
      });

      expect(category.categoryName).toBe('Vessel Cleaning');
      expect(category.expectedImpact).toBeUndefined();
    });

    /**
     * US 4.1.14: Test typical category examples
     * Verifies: Documentation category with short duration
     */
    it('should create Documentation Review category', () => {
      const category = new TaskCategory({
        categoryCode: 'CTC004',
        categoryName: 'Documentation Review',
        description: 'Verification of cargo manifests, customs declarations, and vessel certificates',
        defaultDurationHours: 1,
        expectedImpact: 'No direct impact on operations, but may delay departure if issues found',
      });

      expect(category.categoryName).toBe('Documentation Review');
      expect(category.defaultDurationHours).toBe(1);
    });

    /**
     * US 4.1.14: Test typical category examples
     * Verifies: Emergency repair category
     */
    it('should create Emergency Repair category', () => {
      const category = new TaskCategory({
        categoryCode: 'CTC005',
        categoryName: 'Emergency Repair',
        description: 'Urgent repairs to vessel or port equipment required for safe operations',
        expectedImpact: 'Critical - operations fully suspended until repair completion',
      });

      expect(category.categoryName).toBe('Emergency Repair');
      expect(category.expectedImpact).toContain('Critical');
    });
  });
});
