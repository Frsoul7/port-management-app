import { TestBed } from '@angular/core/testing';
import { HttpClientTestingModule, HttpTestingController } from '@angular/common/http/testing';
import { ComplementaryTaskService } from '../complementary-task.service';
import {
  ComplementaryTaskStatus,
  CreateComplementaryTaskRequest,
  UpdateComplementaryTaskRequest,
  AssignTaskRequest,
  CompleteTaskRequest,
  CancelTaskRequest,
  AddTaskNoteRequest,
  TaskFilters,
} from '../../models/complementary-task.model';

/**
 * US 4.1.15 - ComplementaryTaskService Frontend Tests
 * Tests for HTTP service managing complementary tasks
 */
describe('ComplementaryTaskService', () => {
  let service: ComplementaryTaskService;
  let httpMock: HttpTestingController;
  const apiUrl = 'http://localhost:3000/api/v1/complementary-tasks';

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [HttpClientTestingModule],
      providers: [ComplementaryTaskService]
    });

    service = TestBed.inject(ComplementaryTaskService);
    httpMock = TestBed.inject(HttpTestingController);
  });

  afterEach(() => {
    httpMock.verify();
  });

  describe('createTask', () => {
    it('should create a new complementary task', () => {
      const request: CreateComplementaryTaskRequest = {
        taskCategoryId: 'CAT-001',
        vveId: 'VVE-001',
        title: 'Test Task',
        description: 'Test description',
        dueDate: '2025-12-31T12:00:00Z',
        estimatedDurationHours: 2,
        assignedTo: 'Team A',
        createdBy: 'user-123'
      };

      const mockResponse = {
        success: true,
        message: 'Task created successfully',
        data: {
          taskId: 'TASK-001',
          ...request,
          status: ComplementaryTaskStatus.PLANNED,
          notes: [],
          createdAt: '2025-12-30T10:00:00Z'
        }
      };

      service.createTask(request).subscribe(response => {
        expect(response.success).toBe(true);
        expect(response.data.taskId).toBe('TASK-001');
        expect(response.data.title).toBe('Test Task');
      });

      const req = httpMock.expectOne(apiUrl);
      expect(req.request.method).toBe('POST');
      expect(req.request.body).toEqual(request);
      req.flush(mockResponse);
    });
  });

  describe('getTaskById', () => {
    it('should retrieve a task by ID', () => {
      const taskId = 'TASK-001';
      const mockResponse = {
        success: true,
        data: {
          taskId,
          taskCategoryId: 'CAT-001',
          vveId: 'VVE-001',
          title: 'Test Task',
          description: 'Test description',
          status: ComplementaryTaskStatus.PLANNED,
          dueDate: '2025-12-31T12:00:00Z',
          estimatedDurationHours: 2,
          notes: [],
          createdAt: '2025-12-30T10:00:00Z',
          createdBy: 'user-123'
        }
      };

      service.getTaskById(taskId).subscribe(response => {
        expect(response.success).toBe(true);
        expect(response.data.taskId).toBe(taskId);
      });

      const req = httpMock.expectOne(`${apiUrl}/${taskId}`);
      expect(req.request.method).toBe('GET');
      req.flush(mockResponse);
    });
  });

  describe('updateTask', () => {
    it('should update a task', () => {
      const taskId = 'TASK-001';
      const updates: UpdateComplementaryTaskRequest = {
        title: 'Updated Title',
        description: 'Updated description'
      };

      const mockResponse = {
        success: true,
        message: 'Task updated successfully',
        data: {
          taskId,
          ...updates,
          status: ComplementaryTaskStatus.PLANNED,
          createdAt: '2025-12-30T10:00:00Z'
        }
      };

      service.updateTask(taskId, updates).subscribe(response => {
        expect(response.success).toBe(true);
        expect(response.data.title).toBe('Updated Title');
      });

      const req = httpMock.expectOne(`${apiUrl}/${taskId}`);
      expect(req.request.method).toBe('PATCH');
      expect(req.request.body).toEqual(updates);
      req.flush(mockResponse);
    });
  });

  describe('deleteTask', () => {
    it('should delete a task', () => {
      const taskId = 'TASK-001';
      const mockResponse = {
        success: true,
        message: 'Task deleted successfully'
      };

      service.deleteTask(taskId).subscribe(response => {
        expect(response.success).toBe(true);
        expect(response.message).toContain('deleted');
      });

      const req = httpMock.expectOne(`${apiUrl}/${taskId}`);
      expect(req.request.method).toBe('DELETE');
      req.flush(mockResponse);
    });
  });

  describe('listTasks', () => {
    it('should list all tasks without filters', () => {
      const mockResponse = {
        success: true,
        data: [
          {
            taskId: 'TASK-001',
            title: 'Task 1',
            status: ComplementaryTaskStatus.PLANNED,
            createdAt: '2025-12-30T10:00:00Z'
          },
          {
            taskId: 'TASK-002',
            title: 'Task 2',
            status: ComplementaryTaskStatus.IN_PROGRESS,
            createdAt: '2025-12-30T11:00:00Z'
          }
        ],
        count: 2
      };

      service.listTasks().subscribe(response => {
        expect(response.success).toBe(true);
        expect(response.data.length).toBe(2);
        expect(response.count).toBe(2);
      });

      const req = httpMock.expectOne(apiUrl);
      expect(req.request.method).toBe('GET');
      req.flush(mockResponse);
    });

    it('should list tasks with filters', () => {
      const filters: TaskFilters = {
        status: ComplementaryTaskStatus.PLANNED,
        vveId: 'VVE-001',
        taskCategoryId: 'CAT-001',
        assignedTo: 'Team A',
        fromDate: '2025-12-01',
        toDate: '2025-12-31'
      };

      const mockResponse = {
        success: true,
        data: [],
        count: 0
      };

      service.listTasks(filters).subscribe(response => {
        expect(response.success).toBe(true);
      });

      const req = httpMock.expectOne((request) => {
        return request.url === apiUrl && 
               request.params.has('status') &&
               request.params.get('status') === ComplementaryTaskStatus.PLANNED &&
               request.params.get('vveId') === 'VVE-001';
      });
      expect(req.request.method).toBe('GET');
      req.flush(mockResponse);
    });
  });

  describe('listTasksByVveId', () => {
    it('should list tasks for specific VVE', () => {
      const vveId = 'VVE-001';
      const mockResponse = {
        success: true,
        data: [],
        count: 0
      };

      service.listTasksByVve(vveId).subscribe(response => {
        expect(response.success).toBe(true);
      });

      const req = httpMock.expectOne(`${apiUrl}/vve/${vveId}`);
      expect(req.request.method).toBe('GET');
      req.flush(mockResponse);
    });
  });

  describe('listTasksByStatus', () => {
    it('should list tasks by status', () => {
      const status = ComplementaryTaskStatus.IN_PROGRESS;
      const mockResponse = {
        success: true,
        data: [],
        count: 0
      };

      service.listTasksByStatus(status).subscribe(response => {
        expect(response.success).toBe(true);
      });

      const req = httpMock.expectOne(`${apiUrl}/status/${status}`);
      expect(req.request.method).toBe('GET');
      req.flush(mockResponse);
    });
  });

  describe('getActiveImpactingTasks', () => {
    it('should get active tasks impacting operations', () => {
      const mockResponse = {
        success: true,
        data: [],
        count: 0
      };

      service.getActiveImpactingTasks().subscribe(response => {
        expect(response.success).toBe(true);
      });

      const req = httpMock.expectOne(`${apiUrl}/active-impacting`);
      expect(req.request.method).toBe('GET');
      req.flush(mockResponse);
    });
  });

  describe('assignTask', () => {
    it('should assign a task', () => {
      const taskId = 'TASK-001';
      const request: AssignTaskRequest = {
        assignedTo: 'Team B'
      };

      const mockResponse = {
        success: true,
        message: 'Task assigned successfully',
        data: {
          taskId,
          assignedTo: 'Team B',
          status: ComplementaryTaskStatus.PLANNED
        }
      };

      service.assignTask(taskId, request).subscribe(response => {
        expect(response.success).toBe(true);
        expect(response.data.assignedTo).toBe('Team B');
      });

      const req = httpMock.expectOne(`${apiUrl}/${taskId}/assign`);
      expect(req.request.method).toBe('PATCH');
      expect(req.request.body).toEqual(request);
      req.flush(mockResponse);
    });
  });

  describe('startTask', () => {
    it('should start a task', () => {
      const taskId = 'TASK-001';
      const mockResponse = {
        success: true,
        message: 'Task started successfully',
        data: {
          taskId,
          status: ComplementaryTaskStatus.IN_PROGRESS,
          startedAt: '2025-12-30T12:00:00Z'
        }
      };

      service.startTask(taskId, {}).subscribe(response => {
        expect(response.success).toBe(true);
        expect(response.data.status).toBe(ComplementaryTaskStatus.IN_PROGRESS);
      });

      const req = httpMock.expectOne(`${apiUrl}/${taskId}/start`);
      expect(req.request.method).toBe('PATCH');
      req.flush(mockResponse);
    });
  });

  describe('completeTask', () => {
    it('should complete a task', () => {
      const taskId = 'TASK-001';
      const request: CompleteTaskRequest = {
        completedBy: 'John Doe'
      };

      const mockResponse = {
        success: true,
        message: 'Task completed successfully',
        data: {
          taskId,
          status: ComplementaryTaskStatus.COMPLETED,
          completedAt: '2025-12-30T14:00:00Z',
          completedBy: 'John Doe'
        }
      };

      service.completeTask(taskId, request).subscribe(response => {
        expect(response.success).toBe(true);
        expect(response.data.status).toBe(ComplementaryTaskStatus.COMPLETED);
      });

      const req = httpMock.expectOne(`${apiUrl}/${taskId}/complete`);
      expect(req.request.method).toBe('PATCH');
      expect(req.request.body).toEqual(request);
      req.flush(mockResponse);
    });
  });

  describe('cancelTask', () => {
    it('should cancel a task', () => {
      const taskId = 'TASK-001';
      const request: CancelTaskRequest = {
        cancelledBy: 'Admin',
        cancellationReason: 'Weather conditions'
      };

      const mockResponse = {
        success: true,
        message: 'Task cancelled successfully',
        data: {
          taskId,
          status: ComplementaryTaskStatus.CANCELLED,
          cancelledAt: '2025-12-30T12:00:00Z',
          cancelledBy: 'Admin',
          cancellationReason: 'Weather conditions'
        }
      };

      service.cancelTask(taskId, request).subscribe(response => {
        expect(response.success).toBe(true);
        expect(response.data.status).toBe(ComplementaryTaskStatus.CANCELLED);
      });

      const req = httpMock.expectOne(`${apiUrl}/${taskId}/cancel`);
      expect(req.request.method).toBe('PATCH');
      expect(req.request.body).toEqual(request);
      req.flush(mockResponse);
    });
  });

  describe('addTaskNote', () => {
    it('should add a note to task', () => {
      const taskId = 'TASK-001';
      const request: AddTaskNoteRequest = {
        author: 'Inspector',
        content: 'Task inspection completed'
      };

      const mockResponse = {
        success: true,
        message: 'Note added successfully',
        data: {
          taskId,
          notes: [{
            timestamp: '2025-12-30T10:00:00Z',
            author: 'Inspector',
            content: 'Task inspection completed'
          }]
        }
      };

      service.addNote(taskId, request).subscribe(response => {
        expect(response.success).toBe(true);
        expect(response.data.notes.length).toBe(1);
      });

      const req = httpMock.expectOne(`${apiUrl}/${taskId}/notes`);
      expect(req.request.method).toBe('POST');
      expect(req.request.body).toEqual(request);
      req.flush(mockResponse);
    });
  });

  describe('getTaskStatistics', () => {
    it('should get task statistics', () => {
      const fromDate = '2025-12-01';
      const toDate = '2025-12-31';

      const mockResponse = {
        success: true,
        data: {
          total: 10,
          byStatus: {
            [ComplementaryTaskStatus.PLANNED]: 3,
            [ComplementaryTaskStatus.IN_PROGRESS]: 2,
            [ComplementaryTaskStatus.COMPLETED]: 4,
            [ComplementaryTaskStatus.CANCELLED]: 1
          },
          averageCompletionTimeHours: 3.5,
          onTimeCompletionRate: 0.8
        }
      };

      service.getStatistics(fromDate, toDate).subscribe(response => {
        expect(response.success).toBe(true);
        expect(response.data.total).toBe(10);
      });

      const req = httpMock.expectOne((request) => {
        return request.url === `${apiUrl}/statistics` &&
               request.params.get('fromDate') === fromDate &&
               request.params.get('toDate') === toDate;
      });
      expect(req.request.method).toBe('GET');
      req.flush(mockResponse);
    });
  });

  describe('getTaskCount', () => {
    it('should get total task count', () => {
      const mockResponse = {
        success: true,
        data: { count: 25 }
      };

      service.getTaskCount().subscribe(response => {
        expect(response.success).toBe(true);
        expect(response.data.count).toBe(25);
      });

      const req = httpMock.expectOne(`${apiUrl}/count`);
      expect(req.request.method).toBe('GET');
      req.flush(mockResponse);
    });

    it('should get task count by status', () => {
      const status = ComplementaryTaskStatus.IN_PROGRESS;
      const mockResponse = {
        success: true,
        data: { count: 5 }
      };

      service.getTaskCount(status).subscribe(response => {
        expect(response.success).toBe(true);
        expect(response.data.count).toBe(5);
      });

      const req = httpMock.expectOne((request) => {
        return request.url === `${apiUrl}/count` &&
               request.params.get('status') === status;
      });
      expect(req.request.method).toBe('GET');
      req.flush(mockResponse);
    });
  });

  describe('getOverdueCount', () => {
    it('should get overdue task count', () => {
      const mockResponse = {
        success: true,
        data: { count: 3 }
      };

      service.getOverdueCount().subscribe(response => {
        expect(response.success).toBe(true);
        expect(response.data.count).toBe(3);
      });

      const req = httpMock.expectOne(`${apiUrl}/overdue-count`);
      expect(req.request.method).toBe('GET');
      req.flush(mockResponse);
    });
  });

  describe('Error Handling', () => {
    it('should handle HTTP errors', () => {
      const taskId = 'TASK-999';

      service.getTaskById(taskId).subscribe({
        next: () => fail('should have failed'),
        error: (error) => {
          expect(error.status).toBe(404);
        }
      });

      const req = httpMock.expectOne(`${apiUrl}/${taskId}`);
      req.flush('Task not found', { status: 404, statusText: 'Not Found' });
    });
  });
});
