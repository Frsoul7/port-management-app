import { ComponentFixture, TestBed } from '@angular/core/testing';
import { Router } from '@angular/router';
import { RouterTestingModule } from '@angular/router/testing';
import { of, throwError } from 'rxjs';
import { NO_ERRORS_SCHEMA } from '@angular/core';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { ListComplementaryTasksComponent } from '../list-complementary-tasks.component';
import { ComplementaryTaskService } from '../../../../../core/services/complementary-task.service';
import { TaskCategoryService } from '../../../../../core/services/task-category.service';
import { ComplementaryTask, ComplementaryTaskStatus } from '../../../../../core/models/complementary-task.model';

describe('ListComplementaryTasksComponent', () => {
  let component: ListComplementaryTasksComponent;
  let fixture: ComponentFixture<ListComplementaryTasksComponent>;
  let mockTaskService: jasmine.SpyObj<ComplementaryTaskService>;
  let mockCategoryService: jasmine.SpyObj<TaskCategoryService>;
  let mockRouter: Router;

  const mockTasks = [
    {
      taskId: 'TASK-001',
      taskCategoryId: 'CAT-001',
      vveId: 'VVE-001',
      title: 'Task 1',
      description: 'Description 1',
      status: ComplementaryTaskStatus.PLANNED,
      dueDate: new Date().toISOString(),
      estimatedDurationHours: 2,
      createdAt: new Date().toISOString(),
      createdBy: 'user-001',
      notes: []
    },
    {
      taskId: 'TASK-002',
      taskCategoryId: 'CAT-001',
      vveId: 'VVE-002',
      title: 'Task 2',
      description: 'Description 2',
      status: ComplementaryTaskStatus.IN_PROGRESS,
      dueDate: new Date().toISOString(),
      estimatedDurationHours: 3,
      createdAt: new Date().toISOString(),
      createdBy: 'user-002',
      notes: []
    }
  ];

  beforeEach(async () => {
    mockTaskService = jasmine.createSpyObj('ComplementaryTaskService', [
      'listTasks', 'deleteTask', 'startTask', 'completeTask', 'cancelTask'
    ]);
    mockCategoryService = jasmine.createSpyObj('TaskCategoryService', ['listActiveTaskCategories']);

    await TestBed.configureTestingModule({
      imports: [
        ListComplementaryTasksComponent,
        HttpClientTestingModule,
        RouterTestingModule.withRoutes([])
      ],
      providers: [
        { provide: ComplementaryTaskService, useValue: mockTaskService },
        { provide: TaskCategoryService, useValue: mockCategoryService }
      ],
      schemas: [NO_ERRORS_SCHEMA]
    }).compileComponents();

    fixture = TestBed.createComponent(ListComplementaryTasksComponent);
    component = fixture.componentInstance;
    mockRouter = TestBed.inject(Router);
    spyOn(mockRouter, 'navigate');

    mockTaskService.listTasks.and.returnValue(of({
      success: true,
      count: mockTasks.length,
      data: mockTasks
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

    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should load tasks on init', () => {
    expect(mockTaskService.listTasks).toHaveBeenCalled();
    expect(component.tasks().length).toBe(2);
  });

  it('should load task categories', () => {
    expect(mockCategoryService.listActiveTaskCategories).toHaveBeenCalled();
    expect(component.categories().length).toBeGreaterThan(0);
  });

  it('should filter tasks by search', () => {
    component.filters.search = 'Task 1';
    component.applyFilters();

    expect(component.filteredTasks().length).toBe(1);
    expect(component.filteredTasks()[0].title).toBe('Task 1');
  });

  it('should filter tasks by status', () => {
    // Ensure tasks are loaded fresh for this test
    const inProgressTask = {
      taskId: 'TASK-002',
      taskCategoryId: 'CAT-001',
      vveId: 'VVE-002',
      title: 'Task 2',
      description: 'Description 2',
      status: ComplementaryTaskStatus.IN_PROGRESS,
      dueDate: new Date().toISOString(),
      estimatedDurationHours: 3,
      createdAt: new Date().toISOString(),
      createdBy: 'user-002',
      notes: []
    };
    component.tasks.set([inProgressTask]);
    component.filters.status = ComplementaryTaskStatus.IN_PROGRESS;
    component.applyFilters();

    expect(component.filteredTasks().length).toBe(1);
    expect(component.filteredTasks()[0].status).toBe(ComplementaryTaskStatus.IN_PROGRESS);
  });

  it('should clear filters', () => {
    component.filters.search = 'test';
    component.filters.status = ComplementaryTaskStatus.COMPLETED;
    
    component.clearFilters();

    expect(component.filters.search).toBe('');
    expect(component.filters.status).toBe('');
  });

  it('should navigate to create task', () => {
    component.createTask();
    expect(mockRouter.navigate).toHaveBeenCalledWith(['/complementary-tasks/create']);
  });

  it('should navigate to edit task', () => {
    component.editTask(mockTasks[0]);
    expect(mockRouter.navigate).toHaveBeenCalledWith(['/complementary-tasks', 'TASK-001', 'edit']);
  });

  it('should delete task with confirmation', () => {
    spyOn(window, 'confirm').and.returnValue(true);
    mockTaskService.deleteTask.and.returnValue(of({ success: true, message: 'Deleted' }));

    component.deleteTask(mockTasks[0]);

    expect(mockTaskService.deleteTask).toHaveBeenCalledWith('TASK-001');
    expect(component.successMessage()).toBeTruthy();
  });

  it('should not delete task without confirmation', () => {
    spyOn(window, 'confirm').and.returnValue(false);

    component.deleteTask(mockTasks[0]);

    expect(mockTaskService.deleteTask).not.toHaveBeenCalled();
  });

  it('should start task', () => {
    mockTaskService.startTask.and.returnValue(of({
      success: true,
      message: 'Started',
      data: { ...mockTasks[0], status: ComplementaryTaskStatus.IN_PROGRESS }
    }));

    component.startTask(mockTasks[0]);

    expect(mockTaskService.startTask).toHaveBeenCalledWith('TASK-001');
  });

  it('should complete task', () => {
    spyOn(window, 'prompt').and.returnValue('John Doe');
    mockTaskService.completeTask.and.returnValue(of({
      success: true,
      message: 'Completed',
      data: { ...mockTasks[0], status: ComplementaryTaskStatus.COMPLETED }
    }));

    component.completeTask(mockTasks[0]);

    expect(mockTaskService.completeTask).toHaveBeenCalled();
  });

  it('should cancel task', () => {
    spyOn(window, 'prompt').and.returnValues('John Doe', 'Not needed');
    mockTaskService.cancelTask.and.returnValue(of({
      success: true,
      message: 'Cancelled',
      data: { ...mockTasks[0], status: ComplementaryTaskStatus.CANCELLED }
    }));

    component.cancelTask(mockTasks[0]);

    expect(mockTaskService.cancelTask).toHaveBeenCalled();
  });

  it('should navigate to next page', () => {
    component.totalItems = 25;
    component.currentPage = 1;

    component.nextPage();

    expect(component.currentPage).toBe(2);
  });

  it('should navigate to previous page', () => {
    component.currentPage = 2;

    component.previousPage();

    expect(component.currentPage).toBe(1);
  });

  it('should calculate total pages', () => {
    component.totalItems = 25;
    component.pageSize = 10;

    expect(component.totalPages).toBe(3);
  });

  it('should go back to dashboard', () => {
    component.goBack();
    expect(mockRouter.navigate).toHaveBeenCalledWith(['/dashboard']);
  });

  it('should handle load error', () => {
    mockTaskService.listTasks.and.returnValue(throwError(() => ({ error: { message: 'Load failed' } })));

    component.loadTasks();

    expect(component.errorMessage()).toBe('Failed to load complementary tasks');
  });
});
