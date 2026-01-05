import { ComponentFixture, TestBed } from '@angular/core/testing';
import { Router, ActivatedRoute } from '@angular/router';
import { of, throwError } from 'rxjs';
import { NO_ERRORS_SCHEMA } from '@angular/core';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { EditComplementaryTaskComponent } from '../edit-complementary-task.component';
import { ComplementaryTaskService } from '../../../../../core/services/complementary-task.service';
import { TaskCategoryService } from '../../../../../core/services/task-category.service';
import { ComplementaryTaskStatus } from '../../../../../core/models/complementary-task.model';

describe('EditComplementaryTaskComponent', () => {
  let component: EditComplementaryTaskComponent;
  let fixture: ComponentFixture<EditComplementaryTaskComponent>;
  let mockTaskService: jasmine.SpyObj<ComplementaryTaskService>;
  let mockCategoryService: jasmine.SpyObj<TaskCategoryService>;
  let mockRouter: jasmine.SpyObj<Router>;
  let mockActivatedRoute: any;

  const mockTask = {
    taskId: 'TASK-001',
    taskCategoryId: 'CAT-001',
    vveId: 'VVE-001',
    title: 'Test Task',
    description: 'Test Description',
    status: ComplementaryTaskStatus.PLANNED,
    dueDate: new Date().toISOString(),
    estimatedDurationHours: 2,
    createdAt: new Date().toISOString(),
    createdBy: 'user-001',
    notes: []
  };

  beforeEach(async () => {
    mockTaskService = jasmine.createSpyObj('ComplementaryTaskService', [
      'getTaskById', 'updateTask', 'assignTask', 'startTask', 
      'completeTask', 'cancelTask', 'addNote'
    ]);
    mockCategoryService = jasmine.createSpyObj('TaskCategoryService', ['listActiveTaskCategories']);
    mockRouter = jasmine.createSpyObj('Router', ['navigate']);
    mockActivatedRoute = {
      snapshot: {
        paramMap: {
          get: jasmine.createSpy('get').and.returnValue('TASK-001')
        }
      }
    };

    await TestBed.configureTestingModule({
      imports: [EditComplementaryTaskComponent, HttpClientTestingModule],
      providers: [
        { provide: ComplementaryTaskService, useValue: mockTaskService },
        { provide: TaskCategoryService, useValue: mockCategoryService },
        { provide: ActivatedRoute, useValue: mockActivatedRoute },
        { provide: Router, useValue: mockRouter }
      ],
      schemas: [NO_ERRORS_SCHEMA]
    }).compileComponents();

    mockTaskService.getTaskById.and.returnValue(of({
      success: true,
      data: mockTask
    }));

    mockCategoryService.listActiveTaskCategories.and.returnValue(of({
      success: true,
      count: 1,
      data: [{
        taskCategoryId: 'CAT-001',
        categoryCode: 'CTC001',
        categoryName: 'Equipment Repair',
        description: 'Repair',
        isActive: true,
        createdAt: '2024-01-01T00:00:00Z',
        updatedAt: '2024-01-01T00:00:00Z',
        defaultDurationHours: 2
      }]
    }));

    fixture = TestBed.createComponent(EditComplementaryTaskComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should load task on init', () => {
    expect(mockTaskService.getTaskById).toHaveBeenCalledWith('TASK-001');
    expect(component.task()).toBeTruthy();
  });

  it('should load task categories', () => {
    expect(mockCategoryService.listActiveTaskCategories).toHaveBeenCalled();
    expect(component.categories().length).toBeGreaterThan(0);
  });

  it('should update task successfully', () => {
    mockTaskService.updateTask.and.returnValue(of({
      success: true,
      message: 'Updated',
      data: mockTask
    }));

    component.formData = {
      title: 'Updated Title',
      description: 'Updated description text',
      dueDate: new Date(Date.now() + 86400000).toISOString(),
      estimatedDurationHours: 3
    };

    component.onSubmit();

    expect(mockTaskService.updateTask).toHaveBeenCalled();
    expect(component.successMessage()).toBeTruthy();
  });

  it('should handle update error', () => {
    mockTaskService.updateTask.and.returnValue(throwError(() => ({ error: { message: 'Update failed' } })));

    component.formData = {
      title: 'Updated Title',
      description: 'Updated description text',
      dueDate: new Date(Date.now() + 86400000).toISOString(),
      estimatedDurationHours: 3
    };

    component.onSubmit();

    expect(component.errorMessage()).toBe('Update failed');
  });

  it('should cancel with confirmation', () => {
    spyOn(window, 'confirm').and.returnValue(true);

    component.cancel();

    expect(mockRouter.navigate).toHaveBeenCalledWith(['/complementary-tasks']);
  });

  it('should start task', () => {
    mockTaskService.startTask.and.returnValue(of({
      success: true,
      message: 'Started',
      data: { ...mockTask, status: ComplementaryTaskStatus.IN_PROGRESS }
    }));

    component.startTask();

    expect(mockTaskService.startTask).toHaveBeenCalledWith('TASK-001');
  });

  it('should complete task', () => {
    spyOn(window, 'prompt').and.returnValue('John Doe');
    mockTaskService.completeTask.and.returnValue(of({
      success: true,
      message: 'Completed',
      data: { ...mockTask, status: ComplementaryTaskStatus.COMPLETED }
    }));

    component.completeTask();

    expect(mockTaskService.completeTask).toHaveBeenCalled();
  });

  it('should cancel task', () => {
    spyOn(window, 'prompt').and.returnValues('John Doe', 'Not needed');
    mockTaskService.cancelTask.and.returnValue(of({
      success: true,
      message: 'Cancelled',
      data: { ...mockTask, status: ComplementaryTaskStatus.CANCELLED }
    }));

    component.cancelTask();

    expect(mockTaskService.cancelTask).toHaveBeenCalled();
  });

  it('should add note', () => {
    mockTaskService.addNote.and.returnValue(of({
      success: true,
      message: 'Note added',
      data: mockTask
    }));

    const noteToAdd = { content: 'Test note', author: 'user-001' };
    component.newNote = noteToAdd;
    component.addNote();

    expect(mockTaskService.addNote).toHaveBeenCalledWith('TASK-001', noteToAdd);
  });

  it('should not add empty note', () => {
    component.newNote = { content: '', author: 'user-001' };
    component.addNote();

    expect(mockTaskService.addNote).not.toHaveBeenCalled();
  });

  it('should validate form', () => {
    component.formData.title = '';
    expect(component.validateForm()).toBe(false);

    component.formData.title = 'Valid Title';
    component.formData.description = 'Valid description with enough text';
    component.formData.dueDate = new Date(Date.now() + 86400000).toISOString();
    component.formData.estimatedDurationHours = 2;
    expect(component.validateForm()).toBe(true);
  });
});
