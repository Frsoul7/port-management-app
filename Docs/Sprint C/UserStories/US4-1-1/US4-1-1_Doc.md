# User Story 4.1.1 — Technical Analysis (Levels 1–4)

## US 4.1.1  
**As a Project Manager, I want the team to develop Operations & Execution Management (OEM) module as an independent back-end service so that the system architecture remains modular, decentralized, and maintainable, allowing each component to evolve independently while ensuring seamless integration with existing modules.**

---

## 1. Architectural Vision and Responsibility (WHY)

### Objective and Value
User Story 4.1.1 establishes the **architectural foundation** for the Operations & Execution Management (OEM) module as a standalone backend service. This module is responsible for:
- Managing operation plans generated from scheduling algorithms;
- Tracking real-time execution of vessel visit operations;
- Recording incidents and complementary tasks;
- Providing a REST API for integration with other system components.

By implementing OEM as an independent service, the system achieves:
- **Modularity**: Each module can be developed, deployed, and scaled independently;
- **Decentralization**: No single point of failure; services can be distributed;
- **Maintainability**: Clear boundaries and responsibilities reduce complexity;
- **Technology Independence**: OEM uses Node.js/TypeScript/MongoDB while Core Backend uses .NET/PostgreSQL;
- **Team Autonomy**: Different teams can work on different modules simultaneously.

### Architectural Approach
The OEM module follows **Clean Architecture** and **Domain-Driven Design (DDD)** principles, organizing code into distinct layers:

1. **Domain Layer** (Business Logic)
   - Entities: `OperationPlan`, `VesselVisitExecution`, `Incident`, `ComplementaryTask`
   - Value Objects: `PlannedOperation`, `ExecutedOperation`
   - Domain Services: `VveIdGenerator`, `ConflictDetectionService`
   - Repository Interfaces: `IOperationPlanRepository`, `IVesselVisitExecutionRepository`

2. **Application Layer** (Use Cases)
   - Application Services: Orchestrate business operations
   - DTOs: Data transfer objects for API communication
   - Interfaces: Define contracts for external services

3. **Infrastructure Layer** (External Concerns)
   - Repositories: MongoDB implementations
   - HTTP Clients: Integration with Core Backend and Planning Service
   - Middleware: Authentication, error handling, logging
   - Database Connection: MongoDB configuration

4. **Presentation Layer** (API)
   - Controllers: Handle HTTP requests and responses
   - Routes: Define REST endpoints with validation
   - Swagger Documentation: OpenAPI specification

### Technology Stack
- **Runtime**: Node.js 20+
- **Language**: TypeScript 5+
- **Framework**: Express.js
- **Database**: MongoDB with Mongoose ODM
- **Authentication**: JWT tokens (shared with Core Backend)
- **API Documentation**: Swagger/OpenAPI 3.0
- **Testing**: Jest + Supertest
- **Logging**: Winston

### Dependency Rule
All dependencies point inward toward the domain:
- **Domain** has zero external dependencies
- **Application** depends only on Domain interfaces
- **Infrastructure** implements Domain and Application interfaces
- **Presentation** depends on Application services

This ensures that business logic remains independent of frameworks, databases, and external services.

---

## 2. Functional Analysis and Business Rules (WHAT)

### Core Functionality Provided by OEM Module

#### Operation Plan Management (US 4.1.2 - 4.1.6)
- Generate operation plans for a given day using scheduling algorithms
- Store and manage operation plans with different statuses (GENERATED, APPROVED, IN_EXECUTION, COMPLETED)
- Query and filter operation plans by various criteria
- Update operation plans manually (dock, crane, staff assignments)
- Track changes with audit logs for accountability

#### Vessel Visit Execution Tracking (US 4.1.7 - 4.1.11)
- Create VVE records when vessels arrive at port
- Track actual timelines: arrival, berthing, operations, unberthing, departure
- Record executed operations with resources and containers processed
- Mark VVE as completed with validation of all operations finished
- Link VVE to approved operation plans

#### Incident Management (US 4.1.12 - 4.1.13)
- Record operational incidents during execution
- Link incidents to specific VVEs
- Track incident resolution status

#### Complementary Task Management (US 4.1.14 - 4.1.15)
- Define categories for non-cargo tasks
- Record complementary tasks associated with VVEs
- Track blocking vs. non-blocking tasks

### REST API Design Principles
All endpoints follow RESTful conventions:
- **Resource-oriented URLs**: `/api/v1/operation-plans`, `/api/v1/vessel-visit-executions`
- **HTTP verbs**: GET (read), POST (create), PATCH (partial update), DELETE
- **Status codes**: 200 (OK), 201 (Created), 400 (Bad Request), 401 (Unauthorized), 404 (Not Found), 409 (Conflict), 422 (Validation Error)
- **JSON payloads**: Request and response bodies use JSON format
- **Pagination**: Query parameters for page, limit, sortBy, sortOrder
- **Filtering**: Query parameters for resource-specific filters

### Authentication and Authorization
- **Authentication**: JWT tokens issued by Core Backend (.NET)
- **Token Format**: Bearer token in Authorization header
- **Token Validation**: Signature verification using shared JWT_SECRET
- **User Context**: Token payload contains user ID, email, role
- **Authorization**: Role-Based Access Control (RBAC) and Attribute-Based Access Control (ABAC)
- **Protected Endpoints**: All endpoints except `/health` require authentication

### Inter-Module Communication
- **No Direct Database Access**: OEM never accesses Core Backend's PostgreSQL database
- **REST API Calls Only**: All communication occurs via HTTP REST APIs
- **HTTP Client**: Axios-based clients for Core Backend and Planning Service
- **Error Handling**: Proper handling of network failures and timeouts
- **Data Validation**: Input validation at API boundaries

---

## 3. Technical and Structural Analysis (HOW)

### Project Structure
```
backend-oem/
├── src/
│   ├── server.ts                     # Application entry point
│   ├── application/                  # Use cases and DTOs
│   │   ├── dtos/                     # Data Transfer Objects
│   │   │   ├── OperationPlanDto.ts
│   │   │   ├── VesselVisitExecutionDto.ts
│   │   │   └── ...
│   │   └── services/                 # Application services
│   │       ├── OperationPlanService.ts
│   │       ├── VesselVisitExecutionService.ts
│   │       └── ...
│   ├── domain/                       # Business logic (framework-independent)
│   │   ├── entities/                 # Aggregate roots
│   │   │   ├── OperationPlan.ts
│   │   │   ├── VesselVisitExecution.ts
│   │   │   └── ...
│   │   ├── value-objects/           # Immutable domain objects
│   │   │   ├── PlannedOperation.ts
│   │   │   ├── ExecutedOperation.ts
│   │   │   └── ...
│   │   ├── repositories/            # Repository interfaces (DIP)
│   │   │   ├── IOperationPlanRepository.ts
│   │   │   └── IVesselVisitExecutionRepository.ts
│   │   └── services/                # Domain services
│   │       └── VveIdGenerator.ts
│   ├── infrastructure/              # External concerns
│   │   ├── database/
│   │   │   └── connection.ts        # MongoDB connection
│   │   ├── repositories/            # Repository implementations
│   │   │   ├── OperationPlanRepository.ts
│   │   │   └── VesselVisitExecutionRepository.ts
│   │   ├── http-clients/           # External API clients
│   │   │   ├── CoreBackendClient.ts
│   │   │   └── PlanningClient.ts
│   │   └── middleware/             # Express middleware
│   │       ├── auth.middleware.ts
│   │       └── error.middleware.ts
│   ├── presentation/               # HTTP layer
│   │   ├── controllers/           # Request handlers
│   │   │   ├── OperationPlanController.ts
│   │   │   └── VesselVisitExecutionController.ts
│   │   └── routes/                # Route definitions
│   │       ├── operationPlan.routes.ts
│   │       └── vesselVisitExecution.routes.ts
│   ├── shared/                    # Shared utilities
│   │   ├── types/                 # TypeScript types and enums
│   │   ├── utils/                 # Helper functions
│   │   ├── constants/             # Constants
│   │   └── swagger/               # OpenAPI configuration
│   │       └── openapi.config.ts
│   └── config/
│       └── env.ts                 # Environment configuration
├── tests/                         # Test files
│   ├── unit_tests/               # Unit tests
│   └── unitTests/                # Additional unit tests
├── package.json                   # Dependencies and scripts
├── tsconfig.json                  # TypeScript configuration
└── jest.config.js                # Jest configuration
```

### API Documentation with Swagger
The module exposes comprehensive API documentation at `/api-docs`:

**Configuration** (`src/shared/swagger/openapi.config.ts`):
- OpenAPI 3.0 specification
- Detailed endpoint descriptions
- Request/response schemas
- Authentication requirements
- Example payloads
- Error responses

**Access**: `http://localhost:3000/api-docs`

### Database Design
**MongoDB Collections**:
1. **operationplans**: Stores generated operation plans
   - Fields: operationPlanId, targetDate, algorithm, status, operations, auditLog
   - Indexes: targetDate, status, operationPlanId

2. **vesselvisitexecutions**: Tracks real-time execution
   - Fields: vveId, vvnId, operationPlanId, status, timestamps, operations, incidents
   - Indexes: vveId, vvnId, status

3. **incidents**: Records operational incidents
   - Fields: incidentId, vveId, type, description, status, timestamps

4. **complementarytasks**: Non-cargo tasks
   - Fields: taskId, vveId, categoryId, startTime, endTime, blocksOperations

5. **taskcategories**: Task type definitions
   - Fields: categoryId, name, description, blocksOperations

### Authentication Middleware
**Implementation** (`src/infrastructure/middleware/auth.middleware.ts`):
```typescript
export function authMiddleware(req: Request, res: Response, next: NextFunction) {
  // Extract token from Authorization: Bearer <token>
  const token = req.headers.authorization?.substring(7);
  
  // Verify token with JWT_SECRET
  const decoded = jwt.verify(token, process.env.JWT_SECRET);
  
  // Attach user to request
  req.user = decoded; // { sub, email, role, ... }
  
  next();
}
```

**Usage**: Applied to all protected routes
```typescript
router.post('/generate', authMiddleware, controller.generatePlan);
```

### Error Handling
**Global Error Middleware** (`src/infrastructure/middleware/error.middleware.ts`):
- Catches all unhandled errors
- Formats consistent error responses
- Logs errors with Winston
- Maps domain errors to HTTP status codes

**Error Response Format**:
```json
{
  "success": false,
  "error": "Error message",
  "details": "Additional context"
}
```

### Logging
**Winston Logger** (`src/shared/utils/logger.ts`):
- Structured logging with JSON format
- Log levels: error, warn, info, debug
- Console transport for development
- File transport for production (optional)
- Request/response logging

---

## 4. Quality Attributes, SOLID Principles, and Evolution (ROBUSTNESS)

### Architectural Quality

#### Single Responsibility Principle (SRP)
- **Controllers**: Handle HTTP concerns only
- **Services**: Coordinate use cases
- **Entities**: Enforce business rules
- **Repositories**: Manage persistence
- **Middleware**: Cross-cutting concerns (auth, logging, errors)

#### Open/Closed Principle (OCP)
- New algorithms can be added without modifying existing code
- New incident types or task categories can be added dynamically
- Extension through interfaces and polymorphism

#### Liskov Substitution Principle (LSP)
- Repository implementations can be swapped (MongoDB → PostgreSQL)
- HTTP clients can be mocked for testing

#### Interface Segregation Principle (ISP)
- Repository interfaces are focused and specific
- Services expose only necessary methods

#### Dependency Inversion Principle (DIP)
- Application services depend on repository **interfaces**, not concrete implementations
- Infrastructure provides concrete implementations
- Domain layer has zero dependencies on infrastructure

### Testability
- **Unit Tests**: Domain logic tested in isolation
- **Integration Tests**: API endpoints tested with supertest
- **Mocking**: Repository and HTTP client mocks for testing
- **Coverage**: Jest coverage reports

### Performance Considerations
- **Database Indexing**: Indexes on frequently queried fields
- **Pagination**: All list endpoints support pagination
- **Connection Pooling**: MongoDB connection pool configured
- **Caching**: Potential for Redis caching (future enhancement)

### Security
- **JWT Validation**: All protected endpoints validate tokens
- **Input Validation**: express-validator for request validation
- **SQL Injection Protection**: MongoDB parameterized queries
- **CORS**: Configured to allow only trusted origins
- **Rate Limiting**: Can be added with express-rate-limit (future)

### Scalability
- **Stateless Design**: No session state; JWT tokens enable horizontal scaling
- **Microservice Architecture**: OEM can be deployed independently
- **Load Balancing**: Multiple OEM instances can run behind a load balancer
- **Database Scaling**: MongoDB supports replica sets and sharding

### Observability
- **Health Check**: `/health` endpoint for monitoring
- **Structured Logging**: Winston JSON logs for log aggregation
- **Error Tracking**: All errors logged with context
- **Request Tracing**: Request ID correlation (future enhancement)

### Compliance with Acceptance Criteria

✅ **Architectural Good Practices**: Clean Architecture + DDD + SOLID principles  
✅ **REST API with CRUD**: Full CRUD operations on all business entities  
✅ **API Documentation**: Swagger/OpenAPI at `/api-docs`  
✅ **Inter-Module Communication**: REST API calls only, no direct database access  
✅ **Authentication Integration**: JWT tokens from Core Backend  
✅ **Authorization**: RBAC/ABAC via JWT role and custom middleware (can be extended)

### Current Limitations and Recommendations

**Identified Gaps**:
1. **ABAC Implementation**: While RBAC is present via JWT roles, fine-grained ABAC policies are not fully implemented
2. **Rate Limiting**: No rate limiting to prevent abuse
3. **Request Tracing**: No correlation IDs for distributed tracing
4. **Audit Trail**: Basic audit logs exist, but no comprehensive event sourcing
5. **API Versioning**: API uses `/api/v1` prefix, but no formal versioning strategy

**Recommended Improvements**:
1. Implement comprehensive ABAC with policy engine (e.g., Casbin)
2. Add rate limiting middleware (express-rate-limit)
3. Implement request correlation IDs for tracing
4. Add persistent event log for complete audit trail
5. Formalize API versioning strategy with deprecation policies
6. Add API response caching for frequently accessed data
7. Implement circuit breakers for external API calls
8. Add health checks for dependencies (MongoDB, Core Backend, Planning Service)

---

## Conclusion

User Story 4.1.1 successfully establishes the OEM module as an independent, well-architected backend service. The implementation demonstrates:

- **Strong architectural foundations** with Clean Architecture and DDD
- **Complete REST API** with comprehensive Swagger documentation
- **Proper authentication integration** with JWT tokens
- **Clear separation of concerns** across all layers
- **Technology independence** from other system components
- **High maintainability** through SOLID principles

The module provides a solid foundation for all subsequent operation and execution management user stories (4.1.2 - 4.1.15), while maintaining the flexibility to evolve independently as system requirements change.

The architectural decisions made in US 4.1.1 ensure that the OEM module can scale, be tested, and be maintained effectively while integrating seamlessly with the existing .NET Core Backend and Prolog Planning Service.
