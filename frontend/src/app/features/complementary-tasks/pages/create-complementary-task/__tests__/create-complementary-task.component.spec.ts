import { ComponentFixture, TestBed } from '@angular/core/testing';
import { Router } from '@angular/router';
import { of, throwError } from 'rxjs';
import { NO_ERRORS_SCHEMA } from '@angular/core';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { CreateComplementaryTaskComponent } from '../create-complementary-task.component';
import { ComplementaryTaskService } from '../../../../../core/services/complementary-task.service';
import { TaskCategoryService } from '../../../../../core/services/task-category.service';

describe('CreateComplementaryTaskComponent', () => {
  let component: CreateComplementaryTaskComponent;
  let fixture: ComponentFixture<CreateComplementaryTaskComponent>;
  let mockTaskService: jasmine.SpyObj<ComplementaryTaskService>;
  let mockCategoryService: jasmine.SpyObj<TaskCategoryService>;
  let mockRouter: jasmine.SpyObj<Router>;

  beforeEach(async () => {
    mockTaskService = jasmine.createSpyObj('ComplementaryTaskService', ['createTask']);
    mockCategoryService = jasmine.createSpyObj('TaskCategoryService', ['listActiveTaskCategories']);
    mockRouter = jasmine.createSpyObj('Router', ['navigate']);

    await TestBed.configureTestingModule({
      imports: [CreateComplementaryTaskComponent, HttpClientTestingModule],
      providers: [
        { provide: ComplementaryTaskService, useValue: mockTaskService },
        { provide: TaskCategoryService, useValue: mockCategoryService },
        { provide: Router, useValue: mockRouter }
      ],
      schemas: [NO_ERRORS_SCHEMA]
    }).compileComponents();

    mockCategoryService.listActiveTaskCategories.and.returnValue(of({
      success: true,
      count: 1,
      data: [
        {
          taskCategoryId: 'CAT-001',
          categoryCode: 'CTC001',
          categoryName: 'Equipment Repair',
          description: 'Repair equipment',
          isActive: true,
          createdAt: '2024-01-01T00:00:00Z',
          updatedAt: '2024-01-01T00:00:00Z',
          defaultDurationHours: 2
        }
      ]
    }));

    fixture = TestBed.createComponent(CreateComplementaryTaskComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should load task categories on init', () => {
    expect(mockCategoryService.listActiveTaskCategories).toHaveBeenCalled();
    expect(component.categories().length).toBe(1);
  });

  describe('Form Validation', () => {
    it('should require VVE ID', () => {
      component.formData.vveId = '';
      expect(component.validateForm()).toBe(false);
      expect(component.validationErrors.vveId).toBeTruthy();
    });

    it('should require task category', () => {
      component.formData.taskCategoryId = '';
      expect(component.validateForm()).toBe(false);
      expect(component.validationErrors.taskCategoryId).toBeTruthy();
    });

    it('should require title', () => {
      component.formData.title = '';
      expect(component.validateForm()).toBe(false);
      expect(component.validationErrors.title).toBeTruthy();
    });

    it('should validate title length', () => {
      component.formData.title = 'abc';
      expect(component.validateForm()).toBe(false);
      expect(component.validationErrors.title).toContain('between 5 and 200');
    });

    it('should require description', () => {
      component.formData.description = '';
      expect(component.validateForm()).toBe(false);
      expect(component.validationErrors.description).toBeTruthy();
    });

    it('should validate description length', () => {
      component.formData.description = 'short';
      expect(component.validateForm()).toBe(false);
      expect(component.validationErrors.description).toContain('between 10 and 1000');
    });

    it('should require due date', () => {
      component.formData.dueDate = '';
      expect(component.validateForm()).toBe(false);
      expect(component.validationErrors.dueDate).toBeTruthy();
    });

    it('should require estimated duration', () => {
      component.formData.estimatedDurationHours = 0;
      expect(component.validateForm()).toBe(false);
      expect(component.validationErrors.estimatedDuration).toBeTruthy();
    });

    it('should validate estimated duration maximum', () => {
      component.formData.estimatedDurationHours = 25;
      expect(component.validateForm()).toBe(false);
      expect(component.validationErrors.estimatedDuration).toContain('cannot exceed 24');
    });

    it('should pass validation with valid data', () => {
      component.formData = {
        vveId: 'VVE-001',
        taskCategoryId: 'CAT-001',
        title: 'Valid Task Title',
        description: 'Valid task description with sufficient length',
        dueDate: new Date(Date.now() + 86400000).toISOString(),
        estimatedDurationHours: 2,
        assignedTo: 'Team A',
        createdBy: 'user-001'
      };
      expect(component.validateForm()).toBe(true);
    });
  });

  describe('Task Creation', () => {
    beforeEach(() => {
      component.formData = {
        vveId: 'VVE-001',
        taskCategoryId: 'CAT-001',
        title: 'Test Task',
        description: 'Test task description',
        dueDate: new Date(Date.now() + 86400000).toISOString(),
        estimatedDurationHours: 2,
        assignedTo: 'Team A',
        createdBy: 'user-001'
      };
    });

    it('should create task successfully', () => {
      mockTaskService.createTask.and.returnValue(of({
        success: true,
        message: 'Task created',
        data: {
          taskId: 'TASK-001',
          taskCategoryId: 'CAT-001',
          vveId: 'VVE-001',
          title: 'Test Task',
          description: 'Test task description',
          status: 'PLANNED' as any,
          dueDate: new Date().toISOString(),
          estimatedDurationHours: 2,
          createdAt: new Date().toISOString(),
          notes: [],
          createdBy: 'user-001'
        }
      }));

      component.onSubmit();

      expect(mockTaskService.createTask).toHaveBeenCalledWith(component.formData);
      expect(mockRouter.navigate).toHaveBeenCalledWith(['/complementary-tasks'], jasmine.any(Object));
    });

    it('should handle creation error', () => {
      mockTaskService.createTask.and.returnValue(throwError(() => ({ error: { message: 'Creation failed' } })));

      component.onSubmit();

      expect(component.errorMessage()).toBe('Creation failed');
      expect(component.loading()).toBe(false);
    });

    it('should not submit if validation fails', () => {
      component.formData.title = '';

      component.onSubmit();

      expect(mockTaskService.createTask).not.toHaveBeenCalled();
    });
  });

  it('should cancel with confirmation', () => {
    spyOn(window, 'confirm').and.returnValue(true);

    component.cancel();

    expect(mockRouter.navigate).toHaveBeenCalledWith(['/complementary-tasks']);
  });

  it('should not cancel without confirmation', () => {
    spyOn(window, 'confirm').and.returnValue(false);

    component.cancel();

    expect(mockRouter.navigate).not.toHaveBeenCalled();
  });

  it('should get selected category info', () => {
    component.formData.taskCategoryId = 'CAT-001';
    
    const category = component.getSelectedCategoryInfo();
    
    expect(category).toBeTruthy();
    expect(category?.taskCategoryId).toBe('CAT-001');
  });

  it('should return undefined when no category selected', () => {
    component.formData.taskCategoryId = '';
    
    const category = component.getSelectedCategoryInfo();
    
    expect(category).toBeUndefined();
  });
});
