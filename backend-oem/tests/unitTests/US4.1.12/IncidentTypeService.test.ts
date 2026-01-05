import { IncidentTypeService } from '../../../src/application/services/IncidentTypeService';
import { CreateIncidentTypeDto, UpdateIncidentTypeDto } from '../../../src/application/dtos/IncidentTypeDto';
import { IIncidentTypeRepository } from '../../../src/domain/repositories/IIncidentTypeRepository';
import { IncidentType } from '../../../src/domain/entities/IncidentType';
import { IncidentSeverity } from '../../../src/shared/types';

describe('US 4.1.12 - IncidentTypeService', () => {
  let service: IncidentTypeService;
  let mockRepository: jest.Mocked<IIncidentTypeRepository>;

  beforeEach(() => {
    mockRepository = {
      save: jest.fn(),
      findById: jest.fn(),
      findByCode: jest.fn(),
      findByName: jest.fn(),
      findAll: jest.fn(),
      findActive: jest.fn(),
      findByCategory: jest.fn(),
      findRootTypes: jest.fn(),
      findChildrenOf: jest.fn(),
      update: jest.fn(),
      delete: jest.fn(),
      existsByName: jest.fn(),
      isUsedInIncidents: jest.fn(),
    } as any;

    service = new IncidentTypeService(mockRepository);
  });

  describe('Create Incident Type', () => {
    /**
     * Test: Creates incident type with valid data
     * Business rule: Should create with unique code and name
     */
    it('should create incident type with valid data', async () => {
      const dto: CreateIncidentTypeDto = {
        code: 'T-INC001',
        typeName: 'Equipment Failure',
        description: 'Mechanical or electrical equipment malfunction',
        defaultSeverity: IncidentSeverity.HIGH,
        categoryCode: 'OPS',
        estimatedResolutionTimeHours: 4,
      };

      mockRepository.findByCode.mockResolvedValue(null);
      mockRepository.findByName.mockResolvedValue(null);

      const mockSaved = new IncidentType({
        code: dto.code,
        typeName: dto.typeName,
        description: dto.description,
        defaultSeverity: dto.defaultSeverity as any,
        categoryCode: dto.categoryCode,
        parentTypeId: null,
        estimatedResolutionTimeHours: dto.estimatedResolutionTimeHours,
      });

      mockRepository.save.mockResolvedValue(mockSaved);

      const result = await service.createIncidentType(dto);

      expect(mockRepository.findByCode).toHaveBeenCalledWith(dto.code);
      expect(mockRepository.findByName).toHaveBeenCalledWith(dto.typeName);
      expect(mockRepository.save).toHaveBeenCalled();
      expect(result.code).toBe(dto.code);
      expect(result.typeName).toBe(dto.typeName);
    });

    /**
     * Test: Creates hierarchical incident type
     * Business rule: Should allow parent-child relationships
     */
    it('should create child incident type with parent reference', async () => {
      const parentType = new IncidentType({
        code: 'T-INC001',
        typeName: 'Environmental Conditions',
        description: 'Weather and environmental factors',
        defaultSeverity: IncidentSeverity.LOW,
        categoryCode: 'ENV',
        parentTypeId: null,
        estimatedResolutionTimeHours: 2,
      });

      const dto: CreateIncidentTypeDto = {
        code: 'T-INC002',
        typeName: 'Fog',
        description: 'Dense fog reducing visibility',
        defaultSeverity: IncidentSeverity.HIGH,
        categoryCode: 'ENV',
        parentTypeId: 'parent-id-123',
        estimatedResolutionTimeHours: 3,
      };

      mockRepository.findByCode.mockResolvedValue(null);
      mockRepository.findByName.mockResolvedValue(null);
      mockRepository.findById.mockResolvedValue(parentType);

      const mockSaved = new IncidentType({
        code: dto.code,
        typeName: dto.typeName,
        description: dto.description,
        defaultSeverity: dto.defaultSeverity as any,
        categoryCode: dto.categoryCode,
        parentTypeId: dto.parentTypeId!,
        estimatedResolutionTimeHours: dto.estimatedResolutionTimeHours,
      });

      mockRepository.save.mockResolvedValue(mockSaved);

      const result = await service.createIncidentType(dto);

      expect(mockRepository.findById).toHaveBeenCalledWith(dto.parentTypeId);
      expect(result.parentTypeId).toBe(dto.parentTypeId);
    });

    /**
     * Test: Rejects duplicate code
     * Business rule: Code must be unique across all types
     */
    it('should reject duplicate incident type code', async () => {
      const existing = new IncidentType({
        code: 'T-INC001',
        typeName: 'Existing Type',
        description: 'Already exists',
        defaultSeverity: IncidentSeverity.LOW,
        categoryCode: 'OPS',
        parentTypeId: null,
        estimatedResolutionTimeHours: 2,
      });

      const dto: CreateIncidentTypeDto = {
        code: 'T-INC001',
        typeName: 'New Type',
        description: 'Should fail',
        defaultSeverity: IncidentSeverity.HIGH,
        categoryCode: 'OPS',
        estimatedResolutionTimeHours: 3,
      };

      mockRepository.findByCode.mockResolvedValue(existing);

      await expect(service.createIncidentType(dto)).rejects.toThrow(
        'Incident type with code T-INC001 already exists'
      );

      expect(mockRepository.save).not.toHaveBeenCalled();
    });

    /**
     * Test: Rejects duplicate name
     * Business rule: Name must be unique
     */
    it('should reject duplicate incident type name', async () => {
      const existing = new IncidentType({
        code: 'T-INC001',
        typeName: 'Equipment Failure',
        description: 'Already exists',
        defaultSeverity: IncidentSeverity.LOW,
        categoryCode: 'OPS',
        parentTypeId: null,
        estimatedResolutionTimeHours: 2,
      });

      const dto: CreateIncidentTypeDto = {
        code: 'T-INC002',
        typeName: 'Equipment Failure',
        description: 'Different code, same name',
        defaultSeverity: IncidentSeverity.HIGH,
        categoryCode: 'OPS',
        estimatedResolutionTimeHours: 3,
      };

      mockRepository.findByCode.mockResolvedValue(null);
      mockRepository.findByName.mockResolvedValue(existing);

      await expect(service.createIncidentType(dto)).rejects.toThrow(
        'Incident type with name Equipment Failure already exists'
      );

      expect(mockRepository.save).not.toHaveBeenCalled();
    });

    /**
     * Test: Validates parent existence
     * Business rule: Parent must exist before creating child
     */
    it('should reject invalid parent type reference', async () => {
      const dto: CreateIncidentTypeDto = {
        code: 'T-INC002',
        typeName: 'Fog',
        description: 'Dense fog',
        defaultSeverity: IncidentSeverity.HIGH,
        categoryCode: 'ENV',
        parentTypeId: 'nonexistent-parent',
        estimatedResolutionTimeHours: 3,
      };

      mockRepository.findByCode.mockResolvedValue(null);
      mockRepository.findByName.mockResolvedValue(null);
      mockRepository.findById.mockResolvedValue(null);

      await expect(service.createIncidentType(dto)).rejects.toThrow(
        'Parent incident type nonexistent-parent not found'
      );

      expect(mockRepository.save).not.toHaveBeenCalled();
    });
  });

  describe('Retrieve Incident Types', () => {
    /**
     * Test: Gets incident type by ID
     * Business rule: Should retrieve by unique ID
     */
    it('should get incident type by ID', async () => {
      const mockType = new IncidentType({
        code: 'T-INC001',
        typeName: 'Equipment Failure',
        description: 'Mechanical failure',
        defaultSeverity: IncidentSeverity.HIGH,
        categoryCode: 'OPS',
        parentTypeId: null,
        estimatedResolutionTimeHours: 4,
      });

      mockRepository.findById.mockResolvedValue(mockType);

      const result = await service.getIncidentTypeById('type-id-123');

      expect(mockRepository.findById).toHaveBeenCalledWith('type-id-123');
      expect(result.code).toBe('T-INC001');
      expect(result.typeName).toBe('Equipment Failure');
    });

    /**
     * Test: Gets incident type by code
     * Business rule: Should retrieve by unique code
     */
    it('should get incident type by code', async () => {
      const mockType = new IncidentType({
        code: 'T-INC001',
        typeName: 'Equipment Failure',
        description: 'Mechanical failure',
        defaultSeverity: IncidentSeverity.HIGH,
        categoryCode: 'OPS',
        parentTypeId: null,
        estimatedResolutionTimeHours: 4,
      });

      mockRepository.findByCode.mockResolvedValue(mockType);

      const result = await service.getIncidentTypeByCode('T-INC001');

      expect(mockRepository.findByCode).toHaveBeenCalledWith('T-INC001');
      expect(result.typeName).toBe('Equipment Failure');
    });

    /**
     * Test: Handles not found by ID
     * Business rule: Should throw error if not found
     */
    it('should throw error when incident type not found by ID', async () => {
      mockRepository.findById.mockResolvedValue(null);

      await expect(service.getIncidentTypeById('nonexistent')).rejects.toThrow(
        'Incident type nonexistent not found'
      );
    });

    /**
     * Test: Handles not found by code
     * Business rule: Should throw error if code not found
     */
    it('should throw error when incident type not found by code', async () => {
      mockRepository.findByCode.mockResolvedValue(null);

      await expect(service.getIncidentTypeByCode('T-NONEXIST')).rejects.toThrow(
        'Incident type with code T-NONEXIST not found'
      );
    });
  });

  describe('List Incident Types', () => {
    /**
     * Test: Lists all incident types
     * Business rule: Should return all types including inactive
     */
    it('should list all incident types', async () => {
      const mockTypes = [
        new IncidentType({
          code: 'T-INC001',
          typeName: 'Equipment Failure',
          description: 'Mechanical failure',
          defaultSeverity: IncidentSeverity.HIGH,
          categoryCode: 'OPS',
          parentTypeId: null,
          estimatedResolutionTimeHours: 4,
        }),
        new IncidentType({
          code: 'T-INC002',
          typeName: 'Power Outage',
          description: 'Electrical failure',
          defaultSeverity: IncidentSeverity.CRITICAL,
          categoryCode: 'OPS',
          parentTypeId: null,
          estimatedResolutionTimeHours: 2,
        }),
      ];

      mockRepository.findAll.mockResolvedValue(mockTypes);

      const result = await service.listAllIncidentTypes();

      expect(mockRepository.findAll).toHaveBeenCalled();
      expect(result).toHaveLength(2);
      expect(result[0]!.code).toBe('T-INC001');
      expect(result[1]!.code).toBe('T-INC002');
    });

    /**
     * Test: Lists active incident types only
     * Business rule: Should filter out deactivated types
     */
    it('should list active incident types only', async () => {
      const activeType = new IncidentType({
        code: 'T-INC001',
        typeName: 'Equipment Failure',
        description: 'Mechanical failure',
        defaultSeverity: IncidentSeverity.HIGH,
        categoryCode: 'OPS',
        parentTypeId: null,
        estimatedResolutionTimeHours: 4,
      });

      mockRepository.findActive.mockResolvedValue([activeType]);

      const result = await service.listActiveIncidentTypes();

      expect(mockRepository.findActive).toHaveBeenCalled();
      expect(result).toHaveLength(1);
      expect(result[0]!.isActive).toBe(true);
    });

    /**
     * Test: Lists incident types by category
     * Business rule: Should filter by ENV/OPS/SAF categories
     */
    it('should list incident types by category', async () => {
      const envTypes = [
        new IncidentType({
          code: 'T-INC001',
          typeName: 'Fog',
          description: 'Dense fog',
          defaultSeverity: IncidentSeverity.HIGH,
          categoryCode: 'ENV',
          parentTypeId: null,
          estimatedResolutionTimeHours: 3,
        }),
      ];

      mockRepository.findByCategory.mockResolvedValue(envTypes);

      const result = await service.listIncidentTypesByCategory('ENV');

      expect(mockRepository.findByCategory).toHaveBeenCalledWith('ENV', undefined);
      expect(result).toHaveLength(1);
      expect(result[0]!.categoryCode).toBe('ENV');
    });

    /**
     * Test: Lists root incident types
     * Business rule: Should return only types without parents
     */
    it('should list root incident types', async () => {
      const rootTypes = [
        new IncidentType({
          code: 'T-INC001',
          typeName: 'Environmental Conditions',
          description: 'Weather factors',
          defaultSeverity: IncidentSeverity.LOW,
          categoryCode: 'ENV',
          parentTypeId: null,
          estimatedResolutionTimeHours: 2,
        }),
        new IncidentType({
          code: 'T-INC001',
          typeName: 'Operational Failures',
          description: 'System failures',
          defaultSeverity: IncidentSeverity.HIGH,
          categoryCode: 'OPS',
          parentTypeId: null,
          estimatedResolutionTimeHours: 3,
        }),
      ];

      mockRepository.findRootTypes.mockResolvedValue(rootTypes);

      const result = await service.getRootIncidentTypes();

      expect(mockRepository.findRootTypes).toHaveBeenCalled();
      expect(result).toHaveLength(2);
      expect(result.every((t) => t.isRootType)).toBe(true);
    });

    /**
     * Test: Lists child incident types
     * Business rule: Should return children of specific parent
     */
    it('should list child incident types of a parent', async () => {
      const childTypes = [
        new IncidentType({
          code: 'T-INC002',
          typeName: 'Fog',
          description: 'Dense fog',
          defaultSeverity: IncidentSeverity.HIGH,
          categoryCode: 'ENV',
          parentTypeId: 'parent-env-001',
          estimatedResolutionTimeHours: 3,
        }),
        new IncidentType({
          code: 'T-INC003',
          typeName: 'Strong Winds',
          description: 'High wind speeds',
          defaultSeverity: IncidentSeverity.HIGH,
          categoryCode: 'ENV',
          parentTypeId: 'parent-env-001',
          estimatedResolutionTimeHours: 4,
        }),
      ];

      mockRepository.findChildrenOf.mockResolvedValue(childTypes);

      const result = await service.getChildIncidentTypes('parent-env-001');

      expect(mockRepository.findChildrenOf).toHaveBeenCalledWith('parent-env-001', undefined);
      expect(result).toHaveLength(2);
      expect(result.every((t) => t.parentTypeId === 'parent-env-001')).toBe(true);
    });
  });

  describe('Update Incident Type', () => {
    /**
     * Test: Updates incident type successfully
     * Business rule: Should allow modifying type properties
     */
    it('should update incident type with valid data', async () => {
      const existingType = new IncidentType({
        code: 'T-INC001',
        typeName: 'Equipment Failure',
        description: 'Old description',
        defaultSeverity: IncidentSeverity.HIGH,
        categoryCode: 'OPS',
        parentTypeId: null,
        estimatedResolutionTimeHours: 4,
      });

      const dto: UpdateIncidentTypeDto = {
        description: 'Updated mechanical or electrical equipment malfunction',
        estimatedResolutionTimeHours: 5,
      };

      mockRepository.findById.mockResolvedValue(existingType);
      mockRepository.update.mockResolvedValue(existingType);

      const result = await service.updateIncidentType('type-id-123', dto);

      expect(mockRepository.findById).toHaveBeenCalledWith('type-id-123');
      expect(mockRepository.update).toHaveBeenCalled();
      expect(result.code).toBe('T-INC001');
    });

    /**
     * Test: Validates name uniqueness on update
     * Business rule: New name must not exist elsewhere
     */
    it('should reject update with duplicate name', async () => {
      const existingType = new IncidentType({
        code: 'T-INC001',
        typeName: 'Equipment Failure',
        description: 'Description',
        defaultSeverity: IncidentSeverity.HIGH,
        categoryCode: 'OPS',
        parentTypeId: null,
        estimatedResolutionTimeHours: 4,
      });

      const dto: UpdateIncidentTypeDto = {
        typeName: 'Power Outage', // Already exists
      };

      mockRepository.findById.mockResolvedValue(existingType);
      mockRepository.existsByName.mockResolvedValue(true);

      await expect(service.updateIncidentType('type-id-123', dto)).rejects.toThrow(
        'Incident type with name Power Outage already exists'
      );

      expect(mockRepository.update).not.toHaveBeenCalled();
    });

    /**
     * Test: Validates parent existence on update
     * Business rule: New parent must exist
     */
    it('should validate parent exists when updating parent reference', async () => {
      const existingType = new IncidentType({
        code: 'T-INC002',
        typeName: 'Fog',
        description: 'Dense fog',
        defaultSeverity: IncidentSeverity.HIGH,
        categoryCode: 'ENV',
        parentTypeId: null,
        estimatedResolutionTimeHours: 3,
      });

      const dto: UpdateIncidentTypeDto = {
        parentTypeId: 'nonexistent-parent',
      };

      mockRepository.findById.mockResolvedValueOnce(existingType);
      mockRepository.findById.mockResolvedValueOnce(null);

      await expect(service.updateIncidentType('type-id-123', dto)).rejects.toThrow(
        'Parent incident type nonexistent-parent not found'
      );

      expect(mockRepository.update).not.toHaveBeenCalled();
    });

    /**
     * Test: Prevents self-referencing parent
     * Business rule: Type cannot be its own parent
     */
    it('should reject setting type as its own parent', async () => {
      const existingType = new IncidentType({
        code: 'T-INC001',
        typeName: 'Equipment Failure',
        description: 'Description',
        defaultSeverity: IncidentSeverity.HIGH,
        categoryCode: 'OPS',
        parentTypeId: null,
        estimatedResolutionTimeHours: 4,
      });

      const dto: UpdateIncidentTypeDto = {
        parentTypeId: 'type-id-123', // Same as its own ID
      };

      mockRepository.findById.mockResolvedValue(existingType);

      await expect(service.updateIncidentType('type-id-123', dto)).rejects.toThrow(
        'Incident type cannot be its own parent'
      );

      expect(mockRepository.update).not.toHaveBeenCalled();
    });

    /**
     * Test: Prevents circular references
     * Business rule: Cannot create hierarchy loops
     */
    it('should prevent circular reference in hierarchy', async () => {
      // Type A (child) trying to set Type B (grandparent) as parent
      const typeA = new IncidentType({
        code: 'T-INC003',
        typeName: 'Heavy Rain',
        description: 'Intense rainfall',
        defaultSeverity: IncidentSeverity.HIGH,
        categoryCode: 'ENV',
        parentTypeId: 'type-b-id',
        estimatedResolutionTimeHours: 2,
      });

      const typeB = new IncidentType({
        code: 'T-INC002',
        typeName: 'Fog',
        description: 'Dense fog',
        defaultSeverity: IncidentSeverity.HIGH,
        categoryCode: 'ENV',
        parentTypeId: 'type-a-id', // Points to A
        estimatedResolutionTimeHours: 3,
      });

      const dto: UpdateIncidentTypeDto = {
        parentTypeId: 'type-b-id',
      };

      // Mock A trying to update its parent
      mockRepository.findById.mockImplementation(async (id: string) => {
        if (id === 'type-a-id') return typeA;
        if (id === 'type-b-id') return typeB;
        return null;
      });

      await expect(service.updateIncidentType('type-a-id', dto)).rejects.toThrow();

      expect(mockRepository.update).not.toHaveBeenCalled();
    });
  });

  describe('Deactivate Incident Type', () => {
    /**
     * Test: Deactivates unused incident type
     * Business rule: Should soft delete by setting isActive=false
     */
    it('should deactivate incident type not used in incidents', async () => {
      const existingType = new IncidentType({
        code: 'T-INC001',
        typeName: 'Equipment Failure',
        description: 'Description',
        defaultSeverity: IncidentSeverity.HIGH,
        categoryCode: 'OPS',
        parentTypeId: null,
        estimatedResolutionTimeHours: 4,
      });

      mockRepository.findById.mockResolvedValue(existingType);
      mockRepository.isUsedInIncidents.mockResolvedValue(false);
      mockRepository.update.mockResolvedValue(existingType);

      const result = await service.deactivateIncidentType('type-id-123');

      expect(mockRepository.isUsedInIncidents).toHaveBeenCalledWith('type-id-123');
      expect(mockRepository.update).toHaveBeenCalled();
      expect(result.code).toBe('T-INC001');
    });

    /**
     * Test: Prevents deactivation of used types
     * Business rule: Cannot deactivate if referenced in incidents
     */
    it('should reject deactivation of type used in incidents', async () => {
      const existingType = new IncidentType({
        code: 'T-INC001',
        typeName: 'Equipment Failure',
        description: 'Description',
        defaultSeverity: IncidentSeverity.HIGH,
        categoryCode: 'OPS',
        parentTypeId: null,
        estimatedResolutionTimeHours: 4,
      });

      mockRepository.findById.mockResolvedValue(existingType);
      mockRepository.isUsedInIncidents.mockResolvedValue(true);

      await expect(service.deactivateIncidentType('type-id-123')).rejects.toThrow(
        'Cannot deactivate incident type that is used in existing incidents'
      );

      expect(mockRepository.update).not.toHaveBeenCalled();
    });

    /**
     * Test: Handles not found on deactivation
     * Business rule: Should throw error if type doesn't exist
     */
    it('should throw error when deactivating nonexistent type', async () => {
      mockRepository.findById.mockResolvedValue(null);

      await expect(service.deactivateIncidentType('nonexistent')).rejects.toThrow(
        'Incident type nonexistent not found'
      );

      expect(mockRepository.update).not.toHaveBeenCalled();
    });
  });

  describe('Delete Incident Type', () => {
    /**
     * Test: Deletes unused incident type
     * Business rule: Permanent deletion only if unused
     */
    it('should delete incident type not used in incidents', async () => {
      const existingType = new IncidentType({
        code: 'T-INC001',
        typeName: 'Equipment Failure',
        description: 'Description',
        defaultSeverity: IncidentSeverity.HIGH,
        categoryCode: 'OPS',
        parentTypeId: null,
        estimatedResolutionTimeHours: 4,
      });

      mockRepository.findById.mockResolvedValue(existingType);
      mockRepository.isUsedInIncidents.mockResolvedValue(false);
      mockRepository.findChildrenOf.mockResolvedValue([]);
      mockRepository.delete.mockResolvedValue();

      await service.deleteIncidentType('type-id-123');

      expect(mockRepository.isUsedInIncidents).toHaveBeenCalledWith('type-id-123');
      expect(mockRepository.findChildrenOf).toHaveBeenCalledWith('type-id-123');
      expect(mockRepository.delete).toHaveBeenCalledWith('type-id-123');
    });

    /**
     * Test: Prevents deletion of used types
     * Business rule: Cannot delete if referenced
     */
    it('should reject deletion of type used in incidents', async () => {
      const existingType = new IncidentType({
        code: 'T-INC001',
        typeName: 'Equipment Failure',
        description: 'Description',
        defaultSeverity: IncidentSeverity.HIGH,
        categoryCode: 'OPS',
        parentTypeId: null,
        estimatedResolutionTimeHours: 4,
      });

      mockRepository.findById.mockResolvedValue(existingType);
      mockRepository.isUsedInIncidents.mockResolvedValue(true);

      await expect(service.deleteIncidentType('type-id-123')).rejects.toThrow(
        'Cannot delete incident type that is used in existing incidents'
      );

      expect(mockRepository.delete).not.toHaveBeenCalled();
    });

    /**
     * Test: Prevents deletion of parent types
     * Business rule: Must delete children first
     */
    it('should reject deletion of type with children', async () => {
      const parentType = new IncidentType({
        code: 'T-INC001',
        typeName: 'Environmental Conditions',
        description: 'Weather factors',
        defaultSeverity: IncidentSeverity.LOW,
        categoryCode: 'ENV',
        parentTypeId: null,
        estimatedResolutionTimeHours: 2,
      });

      const childType = new IncidentType({
        code: 'T-INC002',
        typeName: 'Fog',
        description: 'Dense fog',
        defaultSeverity: IncidentSeverity.HIGH,
        categoryCode: 'ENV',
        parentTypeId: 'parent-id',
        estimatedResolutionTimeHours: 3,
      });

      mockRepository.findById.mockResolvedValue(parentType);
      mockRepository.isUsedInIncidents.mockResolvedValue(false);
      mockRepository.findChildrenOf.mockResolvedValue([childType]);

      await expect(service.deleteIncidentType('parent-id')).rejects.toThrow(
        'Cannot delete a parent incident type when it has child incident types'
      );

      expect(mockRepository.delete).not.toHaveBeenCalled();
    });
  });

  describe('DTO Conversion', () => {
    /**
     * Test: Converts entity to DTO correctly
     * Business rule: Should include all fields with proper formatting
     */
    it('should convert incident type to DTO with all fields', async () => {
      const mockType = new IncidentType({
        code: 'T-INC001',
        typeName: 'Equipment Failure',
        description: 'Mechanical failure',
        defaultSeverity: IncidentSeverity.HIGH,
        categoryCode: 'OPS',
        parentTypeId: null,
        estimatedResolutionTimeHours: 4,
        requiresExternalEntities: true,
      });

      mockRepository.findById.mockResolvedValue(mockType);

      const result = await service.getIncidentTypeById('type-id-123');

      expect(result).toHaveProperty('incidentTypeId');
      expect(result).toHaveProperty('code', 'T-INC001');
      expect(result).toHaveProperty('typeName', 'Equipment Failure');
      expect(result).toHaveProperty('description');
      expect(result).toHaveProperty('defaultSeverity', IncidentSeverity.HIGH);
      expect(result).toHaveProperty('categoryCode', 'OPS');
      expect(result).toHaveProperty('categoryName');
      expect(result).toHaveProperty('isActive', true);
      expect(result).toHaveProperty('isRootType');
      expect(result).toHaveProperty('estimatedResolutionTimeHours', 4);
      expect(result).toHaveProperty('requiresExternalEntities', true);
      expect(result).toHaveProperty('createdAt');
      expect(result).toHaveProperty('updatedAt');
    });
  });
});
