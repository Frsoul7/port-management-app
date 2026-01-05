import { Options } from 'swagger-jsdoc';

export const swaggerOptions: Options = {
  definition: {
    openapi: '3.0.0',
    info: {
      title: 'OEM Backend API - Operations & Execution Management',
      version: '1.0.0',
      description: `
## Operations & Execution Management (OEM) Module

This API provides comprehensive management of port operations, including:
- **Operation Plans**: Generate, approve, and manage daily operation plans
- **Vessel Visit Executions**: Track real-time execution of vessel operations
- **Incidents**: Record and manage operational incidents
- **Complementary Tasks**: Manage additional port tasks

### Architecture
- **Technology**: Node.js 20+ | TypeScript 5+ | Express | MongoDB
- **Design**: Clean Architecture + Domain-Driven Design (DDD)
- **Authentication**: JWT tokens from Core Backend (.NET)

### Key Features
- Integration with Core Backend for VVN data
- Integration with Prolog Planning Service for scheduling
- Real-time operation tracking
- Incident management
- RBAC/ABAC authorization

### Sprint C - US 4.1.1
This module fulfills the requirement for a modular, decentralized backend service
with proper REST API documentation and authentication integration.
      `,
      contact: {
        name: 'DEI - ISEP',
        url: 'https://www.isep.ipp.pt',
      },
      license: {
        name: 'MIT',
        url: 'https://opensource.org/licenses/MIT',
      },
    },
    servers: [
      {
        url: 'http://localhost:3000',
        description: 'Development server',
      },
      {
        url: 'http://localhost:3000/api/v1',
        description: 'Development server (with API prefix)',
      },
    ],
    tags: [
      {
        name: 'Operation Plans',
        description: 'Endpoints for managing daily operation plans',
      },
      {
        name: 'Vessel Visit Executions',
        description: 'Endpoints for tracking vessel operation executions',
      },
      {
        name: 'Health',
        description: 'Health check endpoints',
      },
    ],
    components: {
      securitySchemes: {
        BearerAuth: {
          type: 'http',
          scheme: 'bearer',
          bearerFormat: 'JWT',
          description: 'JWT token from Core Backend authentication endpoint',
        },
      },
      schemas: {
        // ==================== OPERATION PLAN SCHEMAS ====================
        OperationPlan: {
          type: 'object',
          properties: {
            id: {
              type: 'string',
              description: 'Unique operation plan identifier',
              example: '674f9c8e1234567890abcdef',
            },
            targetDate: {
              type: 'string',
              format: 'date',
              description: 'Date for which the plan is generated',
              example: '2025-12-15',
            },
            status: {
              type: 'string',
              enum: ['DRAFT', 'APPROVED', 'IN_PROGRESS', 'COMPLETED', 'CANCELLED'],
              description: 'Current status of the operation plan',
              example: 'APPROVED',
            },
            algorithm: {
              type: 'string',
              enum: ['optimal', 'weighted', 'multi-cranes', 'simple'],
              description: 'Algorithm used for scheduling',
              example: 'optimal',
            },
            totalDelay: {
              type: 'number',
              description: 'Total delay in minutes calculated by planning algorithm',
              example: 45.5,
            },
            vesselVisitAllocations: {
              type: 'array',
              items: { $ref: '#/components/schemas/VesselVisitAllocation' },
              description: 'Scheduled operations for vessel visits',
            },
            createdBy: {
              type: 'string',
              description: 'User ID who created the plan',
              example: '11111111-1111-1111-1111-111111111111',
            },
            createdAt: {
              type: 'string',
              format: 'date-time',
              description: 'Timestamp when plan was created',
            },
            updatedAt: {
              type: 'string',
              format: 'date-time',
              description: 'Timestamp when plan was last updated',
            },
          },
        },
        VesselVisitAllocation: {
          type: 'object',
          properties: {
            vesselVisitNotificationId: {
              type: 'string',
              description: 'VVN ID from Core Backend',
              example: '2025-PTLEI-000001',
            },
            scheduledStartTime: {
              type: 'string',
              format: 'date-time',
              description: 'Planned start time for operations',
            },
            scheduledEndTime: {
              type: 'string',
              format: 'date-time',
              description: 'Planned end time for operations',
            },
            assignedDockId: {
              type: 'string',
              description: 'Dock identifier',
              example: 'DOCK-A1',
            },
            assignedStaff: {
              type: 'array',
              items: { type: 'string' },
              description: 'Staff member IDs assigned to this operation',
              example: ['STAFF-001', 'STAFF-002'],
            },
            operations: {
              type: 'array',
              items: { $ref: '#/components/schemas/PlannedOperation' },
              description: 'List of planned operations',
            },
          },
        },
        PlannedOperation: {
          type: 'object',
          properties: {
            type: {
              type: 'string',
              enum: ['UNLOAD', 'LOAD', 'MAINTENANCE', 'INSPECTION'],
              description: 'Type of operation',
              example: 'UNLOAD',
            },
            estimatedDuration: {
              type: 'number',
              description: 'Estimated duration in minutes',
              example: 120,
            },
            containerCount: {
              type: 'number',
              description: 'Number of containers to process',
              example: 50,
            },
            requiredCranes: {
              type: 'number',
              description: 'Number of cranes required',
              example: 2,
            },
          },
        },
        GenerateOperationPlanRequest: {
          type: 'object',
          required: ['targetDate', 'algorithm'],
          properties: {
            targetDate: {
              type: 'string',
              format: 'date',
              description: 'Date for which to generate the plan',
              example: '2025-12-15',
            },
            algorithm: {
              type: 'string',
              enum: ['optimal', 'weighted', 'multi-cranes', 'simple'],
              description: 'Scheduling algorithm to use',
              example: 'optimal',
            },
            vvnIds: {
              type: 'array',
              items: { type: 'string' },
              description:
                'Optional: Specific VVNs to include (if empty, fetches all approved for date)',
              example: ['2025-PTLEI-000001', '2025-PTLEI-000002'],
            },
          },
        },
        UpdateOperationPlanStatusRequest: {
          type: 'object',
          required: ['status'],
          properties: {
            status: {
              type: 'string',
              enum: ['APPROVED', 'IN_PROGRESS', 'COMPLETED', 'CANCELLED'],
              description: 'New status for the operation plan',
              example: 'APPROVED',
            },
          },
        },
        UpdateDockAssignmentRequest: {
          type: 'object',
          required: ['vvnId', 'dockId'],
          properties: {
            vvnId: {
              type: 'string',
              description: 'Vessel Visit Notification ID',
              example: '2025-PTLEI-000001',
            },
            dockId: {
              type: 'string',
              description: 'New dock identifier',
              example: 'DOCK-B2',
            },
          },
        },
        UpdateStaffAssignmentRequest: {
          type: 'object',
          required: ['vvnId', 'staffIds'],
          properties: {
            vvnId: {
              type: 'string',
              description: 'Vessel Visit Notification ID',
              example: '2025-PTLEI-000001',
            },
            staffIds: {
              type: 'array',
              items: { type: 'string' },
              description: 'Array of staff member IDs',
              example: ['STAFF-001', 'STAFF-002', 'STAFF-003'],
            },
          },
        },

        // ==================== VESSEL VISIT EXECUTION SCHEMAS ====================
        VesselVisitExecution: {
          type: 'object',
          properties: {
            id: {
              type: 'string',
              description: 'Unique execution identifier',
              example: '674f9c8e1234567890abcdef',
            },
            vesselVisitNotificationId: {
              type: 'string',
              description: 'VVN ID from Core Backend',
              example: '2025-PTLEI-000001',
            },
            operationPlanId: {
              type: 'string',
              description: 'Reference to operation plan',
              example: '674f9c8e1234567890abcdef',
            },
            status: {
              type: 'string',
              enum: ['SCHEDULED', 'IN_PROGRESS', 'COMPLETED', 'CANCELLED', 'DELAYED'],
              description: 'Current execution status',
              example: 'IN_PROGRESS',
            },
            estimatedArrivalTime: {
              type: 'string',
              format: 'date-time',
              description: 'Estimated arrival time',
            },
            actualPortArrival: {
              type: 'string',
              format: 'date-time',
              description: 'Actual port arrival time',
            },
            actualBerthing: {
              type: 'string',
              format: 'date-time',
              description: 'Actual berthing time',
            },
            actualUnberthing: {
              type: 'string',
              format: 'date-time',
              description: 'Actual unberthing time',
            },
            actualPortDeparture: {
              type: 'string',
              format: 'date-time',
              description: 'Actual port departure time',
            },
            assignedDock: {
              type: 'string',
              description: 'Assigned dock identifier',
              example: 'DOCK-A1',
            },
            operations: {
              type: 'array',
              items: { $ref: '#/components/schemas/ExecutedOperation' },
              description: 'List of executed operations',
            },
            incidents: {
              type: 'array',
              items: { type: 'string' },
              description: 'Incident IDs linked to this execution',
              example: ['INC-001', 'INC-002'],
            },
            createdBy: {
              type: 'string',
              description: 'User ID who created the execution record',
            },
            createdAt: {
              type: 'string',
              format: 'date-time',
            },
            updatedAt: {
              type: 'string',
              format: 'date-time',
            },
          },
        },
        ExecutedOperation: {
          type: 'object',
          properties: {
            type: {
              type: 'string',
              enum: ['UNLOAD', 'LOAD', 'MAINTENANCE', 'INSPECTION'],
              example: 'UNLOAD',
            },
            startTime: {
              type: 'string',
              format: 'date-time',
              description: 'Actual start time',
            },
            endTime: {
              type: 'string',
              format: 'date-time',
              description: 'Actual end time',
            },
            containerCount: {
              type: 'number',
              description: 'Number of containers processed',
              example: 48,
            },
            cranesUsed: {
              type: 'number',
              description: 'Number of cranes used',
              example: 2,
            },
            delayMinutes: {
              type: 'number',
              description: 'Delay compared to plan (minutes)',
              example: 15,
            },
          },
        },
        CreateVesselVisitExecutionRequest: {
          type: 'object',
          required: ['vesselVisitNotificationId', 'operationPlanId', 'estimatedArrivalTime'],
          properties: {
            vesselVisitNotificationId: {
              type: 'string',
              description: 'VVN ID',
              example: '2025-PTLEI-000001',
            },
            operationPlanId: {
              type: 'string',
              description: 'Operation plan ID',
              example: '674f9c8e1234567890abcdef',
            },
            estimatedArrivalTime: {
              type: 'string',
              format: 'date-time',
              description: 'Estimated arrival time',
              example: '2025-12-15T08:00:00Z',
            },
          },
        },
        UpdateVesselVisitExecutionStatusRequest: {
          type: 'object',
          required: ['status'],
          properties: {
            status: {
              type: 'string',
              enum: ['IN_PROGRESS', 'COMPLETED', 'CANCELLED', 'DELAYED'],
              description: 'New execution status',
              example: 'IN_PROGRESS',
            },
          },
        },
        LinkIncidentRequest: {
          type: 'object',
          required: ['incidentId'],
          properties: {
            incidentId: {
              type: 'string',
              description: 'Incident identifier',
              example: 'INC-001',
            },
          },
        },
        VesselVisitExecutionMetrics: {
          type: 'object',
          properties: {
            vveId: {
              type: 'string',
              description: 'Execution ID',
            },
            totalOperations: {
              type: 'number',
              description: 'Total number of operations',
              example: 4,
            },
            completedOperations: {
              type: 'number',
              description: 'Number of completed operations',
              example: 2,
            },
            averageDelayMinutes: {
              type: 'number',
              description: 'Average delay across operations',
              example: 12.5,
            },
            incidentCount: {
              type: 'number',
              description: 'Number of incidents',
              example: 1,
            },
            totalContainersProcessed: {
              type: 'number',
              description: 'Total containers processed',
              example: 150,
            },
          },
        },

        // ==================== COMMON SCHEMAS ====================
        ApiSuccessResponse: {
          type: 'object',
          properties: {
            success: {
              type: 'boolean',
              example: true,
            },
            data: {
              type: 'object',
              description: 'Response data',
            },
          },
        },
        ApiErrorResponse: {
          type: 'object',
          properties: {
            success: {
              type: 'boolean',
              example: false,
            },
            error: {
              type: 'string',
              description: 'Error message',
              example: 'Operation plan not found',
            },
          },
        },
        PaginationResponse: {
          type: 'object',
          properties: {
            success: {
              type: 'boolean',
              example: true,
            },
            data: {
              type: 'array',
              items: {},
            },
            pagination: {
              type: 'object',
              properties: {
                page: {
                  type: 'number',
                  example: 1,
                },
                limit: {
                  type: 'number',
                  example: 10,
                },
                total: {
                  type: 'number',
                  example: 25,
                },
                totalPages: {
                  type: 'number',
                  example: 3,
                },
              },
            },
          },
        },
      },
    },
    security: [
      {
        BearerAuth: [],
      },
    ],
  },
  apis: ['./src/presentation/routes/*.ts', './src/presentation/controllers/*.ts'],
};
