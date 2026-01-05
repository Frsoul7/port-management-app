# User Story 4.1.2 — Technical Analysis (Levels 1–4)

## US 4.1.2  
**As a Logistics Operator, I want to automatically generate and store Operation Plans for all Vessel Visit Notifications (VVNs) scheduled for a given day using one of the available scheduling algorithms, so that cargo operations are efficiently organized and can later be monitored or adjusted.**

---

## 1. Architectural Vision and Responsibility (WHY)

### Objective and Value
User Story 4.1.2 enables the **automatic generation of operation plans** for vessel visits scheduled on a specific date. By generating optimized plans using scheduling algorithms, the system can:
- Efficiently allocate limited port resources (docks, cranes, staff);
- Minimize vessel waiting times and turnaround time;
- Optimize berth occupancy and resource utilization;
- Provide a baseline plan that can be manually adjusted if needed;
- Support operational decision-making with data-driven scheduling.

This functionality bridges the gap between **planning** (algorithmic optimization) and **execution** (real-time tracking), ensuring that operations are organized before vessels arrive.

### Role in the System
This User Story is part of the **Operations & Execution Management (OEM)** module and represents the **entry point** for the operational workflow:

1. **Logistics Operator** selects a target date and algorithm
2. **OEM Backend** fetches all approved VVNs for that date from Core Backend
3. **OEM Backend** calls the Planning & Scheduling Service (Prolog) to generate an optimized plan
4. **Generated plan** is stored in MongoDB with status "GENERATED"
5. **Operator** can review the plan in the SPA and approve it for execution

### Architectural Principles Applied
- **Separation of Concerns**: Planning logic (Prolog) is separate from execution management (OEM)
- **Service Integration**: OEM orchestrates calls to Core Backend and Planning Service
- **Data Ownership**: OEM stores and manages operation plans; Core Backend stores VVNs
- **Idempotency**: Cannot generate duplicate plans for the same date
- **Auditability**: Plan generation is logged with metadata (author, algorithm, timestamp)

---

## 2. Functional Analysis and Business Rules (WHAT)

### Functional Description
The logistics operator initiates the generation of an operation plan by providing:
- **Target Date**: The date for which the plan should be generated
- **Algorithm**: The scheduling algorithm to use (optimal, weighted, multi_cranes)
- **VVN IDs (optional)**: Specific vessel visits to include (if not provided, all VVNs for that date are used)

The system performs the following steps:
1. **Validate input**: Ensure date and algorithm are valid
2. **Check for existing plan**: Prevent duplicate plans for the same date
3. **Fetch VVN data**: Retrieve approved VVNs for the target date from Core Backend
4. **Call Planning Service**: Send VVN data to Prolog service with selected algorithm
5. **Parse response**: Extract planned operations with resource assignments and time windows
6. **Store plan**: Save the operation plan in MongoDB with status "GENERATED"
7. **Create audit log**: Record creation event with user ID, timestamp, and algorithm used
8. **Return plan**: Send the generated plan to the SPA for review

### Business Rules Implemented

#### Plan Generation Rules
1. **Cannot generate plans for past dates** (enforced in domain)
2. **Only one plan per target date** (uniqueness constraint)
3. **Only approved VVNs are included** (filtered by Core Backend)
4. **Algorithm must be valid**: optimal, weighted, multi_cranes
5. **Plan starts in GENERATED status** and must be approved before execution

#### Resource Allocation Rules (enforced by Planning Service)
- Each vessel is assigned to a specific dock
- Operations are scheduled with start and end times
- Cranes and staff are allocated based on availability
- Conflicts are avoided (no resource double-booking)
- Delays are minimized according to the algorithm's objective function

#### Audit Trail Rules
- **Creation event** is logged with:
  - User ID (from JWT token)
  - Timestamp
  - Algorithm used
  - Target date
- **Subsequent updates** (US 4.1.4) append to the audit log
- **Status transitions** (GENERATED → APPROVED → IN_EXECUTION → COMPLETED) are tracked

### Algorithm Options
The system supports multiple scheduling algorithms:

1. **Optimal (optimal)**: Prolog-based constraint satisfaction
   - Minimizes total delay
   - Considers all constraints (dock capacity, crane availability, staff)
   - Suitable for smaller datasets

2. **Weighted Priority (weighted)**: Heuristic-based scheduling
   - Prioritizes vessels based on weighted factors (ETA, cargo volume, priority flag)
   - Faster computation for larger datasets
   - Good balance between optimality and performance

3. **Multi-Cranes (multi_cranes)**: Multi-crane operation support
   - Allows multiple cranes to work on the same vessel simultaneously
   - Reduces turnaround time for large vessels
   - More complex resource allocation

### Typical Use Cases
- **Daily planning**: Generate plan for tomorrow's operations
- **Re-planning**: Regenerate plan if VVNs change (requires deletion of old plan)
- **What-if analysis**: Compare plans generated with different algorithms
- **Capacity planning**: Identify bottlenecks and resource constraints

### Exceptional Scenarios
- **No VVNs for date**: Plan cannot be generated (error returned)
- **Plan already exists**: Duplicate generation prevented (error returned)
- **Core Backend unavailable**: Network error handled gracefully (error returned)
- **Planning Service failure**: Timeout or error from Prolog service (error returned)
- **Invalid algorithm**: Validation error (400 Bad Request)
- **Past date**: Domain validation error (422 Unprocessable Entity)

---

## 3. Technical and Structural Analysis (HOW)

### REST API Endpoint

**Generate Operation Plan**
- **Method**: `POST`
- **Path**: `/api/v1/operation-plans/generate`
- **Authentication**: Required (JWT token)
- **Authorization**: Logistics Operator role

**Request Body**:
```json
{
  "targetDate": "2025-12-15",
  "algorithm": "optimal",
  "vvnIds": ["2025-PTLEI-000001", "2025-PTLEI-000002"]
}
```

**Response (201 Created)**:
```json
{
  "success": true,
  "message": "Operation plan generated successfully",
  "data": {
    "operationPlanId": "674f9c8e1234567890abcdef",
    "targetDate": "2025-12-15",
    "status": "GENERATED",
    "algorithm": "optimal",
    "createdBy": "user123",
    "createdAt": "2025-01-03T10:30:00Z",
    "totalDelay": 120,
    "operations": [
      {
        "vvnId": "2025-PTLEI-000001",
        "vesselImo": "9876543",
        "operationType": "LOADING",
        "assignedDock": "DOCK-01",
        "assignedCranes": ["CRANE-01", "CRANE-02"],
        "assignedStaff": ["STAFF-101", "STAFF-102"],
        "plannedStart": "2025-12-15T08:00:00Z",
        "plannedEnd": "2025-12-15T12:00:00Z",
        "containersToProcess": 50
      }
    ],
    "auditLog": [
      {
        "timestamp": "2025-01-03T10:30:00Z",
        "userId": "user123",
        "userName": "John Doe",
        "action": "CREATED",
        "changes": []
      }
    ]
  }
}
```

**Error Responses**:
- **400 Bad Request**: Invalid input (missing fields, invalid date format, invalid algorithm)
- **401 Unauthorized**: Missing or invalid JWT token
- **409 Conflict**: Plan already exists for target date
- **422 Unprocessable Entity**: Business rule violation (past date)
- **500 Internal Server Error**: Planning Service failure or unexpected error

### Component Responsibilities

#### Presentation Layer
**OperationPlanController** (`src/presentation/controllers/OperationPlanController.ts`):
- Extracts and validates request parameters
- Retrieves user ID from JWT token (`req.user.sub` or `req.user.email`)
- Delegates to `OperationPlanService.generatePlanSimple()`
- Returns standardized JSON response

**Route Definition** (`src/presentation/routes/operationPlan.routes.ts`):
- Defines POST endpoint with path `/generate`
- Applies `authMiddleware` for JWT validation
- Applies `express-validator` rules:
  - `targetDate`: required, ISO 8601 date format
  - `algorithm`: required, must be one of: optimal, weighted, multi_cranes
  - `vvnIds`: optional array of VVN IDs
- Calls controller method `generatePlan()`

#### Application Layer
**OperationPlanService** (`src/application/services/OperationPlanService.ts`):

**Method**: `generatePlanSimple(targetDate, algorithm, userId, vvnIds?)`

**Responsibilities**:
1. **Check for existing plan**: Call `operationPlanRepository.findByTargetDate(targetDate)`
   - If exists, throw error: "Operation plan already exists for this date"

2. **Fetch VVN data from Core Backend**:
   - Call `coreBackendClient.getApprovedVvnsForDate(targetDate)`
   - Filter by `vvnIds` if provided
   - If no VVNs, throw error: "No approved VVNs found for this date"

3. **Call Planning Service**:
   - Prepare payload with VVN data
   - Call `planningClient.generatePlan(targetDate, algorithm, vvns)`
   - Receive optimized schedule with resource assignments

4. **Parse Planning Service response**:
   - Extract planned operations with time windows
   - Map to `PlannedOperation` value objects
   - Calculate total delay

5. **Create OperationPlan entity**:
   - Instantiate with domain constructor
   - Status: GENERATED
   - Include audit log entry for creation

6. **Save to repository**:
   - Call `operationPlanRepository.save(operationPlan)`

7. **Return DTO**:
   - Map entity to `OperationPlanDto`
   - Return to controller

#### Domain Layer
**OperationPlan Entity** (`src/domain/entities/OperationPlan.ts`):

**Constructor validations**:
- `targetDate` is required
- `algorithm` is valid
- `createdBy` (user ID) is required
- `totalDelay >= 0`
- `operations` array is not empty
- **Cannot create plan for past dates** (only for new plans, not when loading from DB)

**Properties**:
- `operationPlanId`: UUID (auto-generated)
- `targetDate`: Date
- `algorithm`: PlanningAlgorithm enum
- `createdBy`: string (user ID)
- `createdAt`: Date (timestamp)
- `status`: OperationPlanStatus enum (default: GENERATED)
- `totalDelay`: number (minutes)
- `operations`: PlannedOperation[]
- `auditLog`: OperationPlanAuditEntry[]

**PlannedOperation Value Object** (`src/domain/value-objects/PlannedOperation.ts`):
- `vvnId`: string
- `vesselImo`: string
- `operationType`: OperationType (LOADING, UNLOADING, BOTH)
- `assignedDock`: string
- `assignedCranes`: string[]
- `assignedStaff`: string[]
- `plannedStart`: Date
- `plannedEnd`: Date
- `containersToProcess`: number

**OperationPlanAuditEntry**:
- `timestamp`: Date
- `userId`: string
- `userName`: string
- `action`: 'CREATED' | 'UPDATED' | 'APPROVED' | 'IN_EXECUTION' | 'COMPLETED'
- `changes`: { field, oldValue, newValue }[]
- `reason`: string (optional)

#### Infrastructure Layer
**OperationPlanRepository** (`src/infrastructure/repositories/OperationPlanRepository.ts`):
- Implements `IOperationPlanRepository` interface (DIP)
- Uses Mongoose for MongoDB operations
- Maps between domain entities and MongoDB documents
- Provides methods: `save()`, `findById()`, `findByTargetDate()`, `update()`, `delete()`

**CoreBackendClient** (`src/infrastructure/http-clients/CoreBackendClient.ts`):
- Axios-based HTTP client
- Base URL from environment: `CORE_BACKEND_URL`
- Method: `getApprovedVvnsForDate(date: Date): Promise<VvnDto[]>`
- Endpoint: `GET /api/vessel-visit-notifications?date={date}&status=APPROVED`
- Handles network errors and timeouts

**PlanningClient** (`src/infrastructure/http-clients/PlanningClient.ts`):
- Axios-based HTTP client
- Base URL from environment: `PLANNING_SERVICE_URL`
- Method: `generatePlan(targetDate, algorithm, vvns): Promise<PlanningResult>`
- Endpoint: `POST /generate-plan`
- Payload: `{ targetDate, algorithm, vessels: [...] }`
- Parses Prolog response format

### Data Flow
```
SPA → OEM API → OperationPlanController
  → OperationPlanService
    → CoreBackendClient → Core Backend (fetch VVNs)
    → PlanningClient → Planning Service (generate plan)
    → OperationPlan (domain entity creation)
    → OperationPlanRepository → MongoDB (persistence)
  ← OperationPlanDto
← JSON Response
```

### Integration Points
1. **Core Backend**: Source of VVN data (vessel information, cargo details, ETA)
2. **Planning Service**: Prolog-based optimization engine
3. **MongoDB**: Persistent storage for generated plans
4. **SPA**: User interface for plan review and approval

---

## 4. Quality Attributes, SOLID Principles, and Evolution (ROBUSTNESS)

### Architectural Quality

**Single Responsibility Principle (SRP)**:
- **Controller**: HTTP handling only
- **Service**: Use case orchestration
- **Entity**: Business rule enforcement
- **Repository**: Persistence logic
- **HTTP Clients**: External API communication

**Dependency Inversion Principle (DIP)**:
- Service depends on `IOperationPlanRepository` interface
- Service depends on HTTP client interfaces
- Concrete implementations in infrastructure layer

**Open/Closed Principle (OCP)**:
- New algorithms can be added to Planning Service without changing OEM code
- New validation rules can be added to entity without modifying service

### Validation Strategy
**Multi-Layer Validation**:
1. **Presentation Layer**: Input format validation (express-validator)
2. **Application Layer**: Business logic validation (duplicate plan check)
3. **Domain Layer**: Invariant enforcement (past date check, required fields)

### Error Handling
- **Network errors**: Wrapped and logged with context
- **Planning Service failures**: Timeout handling with meaningful messages
- **Domain violations**: Mapped to appropriate HTTP status codes (409, 422)
- **Validation errors**: Clear error messages with field-level details

### Auditability
- **Creation logged**: User ID, timestamp, algorithm recorded
- **Metadata preserved**: Original plan structure retained
- **Change tracking**: Audit log supports compliance and debugging

### Performance Considerations
- **Async operations**: All I/O operations are asynchronous (non-blocking)
- **Timeout configuration**: HTTP clients have configurable timeouts
- **Index on targetDate**: MongoDB index for fast duplicate detection
- **Plan size limits**: Large plans may require pagination (future enhancement)

### Testability
- **Unit tests**: Service logic can be tested with mocked repositories and HTTP clients
- **Integration tests**: API endpoint can be tested with test database
- **Mock Planning Service**: Test responses can be stubbed for predictable behavior

### Current Limitations and Recommendations

**Identified Gaps**:
1. **No plan versioning**: If a plan is regenerated, the old plan is lost
2. **No partial regeneration**: Cannot regenerate for specific VVNs within a date
3. **No algorithm comparison**: Cannot generate multiple plans with different algorithms for comparison
4. **Limited error recovery**: If Planning Service fails mid-generation, no retry logic
5. **No caching**: VVN data is fetched every time (could be cached temporarily)

**Recommended Improvements**:
1. Support plan versioning (v1, v2, etc.) for the same date
2. Allow selective regeneration for specific VVNs
3. Add "compare algorithms" feature to generate multiple plans
4. Implement retry logic with exponential backoff for Planning Service calls
5. Add short-term caching for VVN data (Redis)
6. Add plan complexity metrics (number of operations, resource utilization)
7. Support asynchronous plan generation for very large datasets (background job)

---

## Conclusion

User Story 4.1.2 successfully implements the automatic generation of operation plans, fulfilling the following requirements:

✅ **Operator can select target day**: Input parameter for `targetDate`  
✅ **Plans generated by Planning Service**: Integration with Prolog-based scheduling  
✅ **Multiple algorithm support**: optimal, weighted, multi_cranes  
✅ **Plans stored in OEM module**: MongoDB persistence with full plan data  
✅ **Auditability**: Creation date, author (user ID), and algorithm recorded  
✅ **SPA integration**: Plans can be viewed and approved in the frontend  

The implementation demonstrates:
- **Clean separation** between planning (Prolog) and execution management (OEM)
- **Robust integration** with Core Backend and Planning Service via REST APIs
- **Domain-driven design** with proper validation and invariant enforcement
- **Auditability** through structured audit logs
- **Extensibility** for future algorithm additions

This functionality serves as the foundation for the operational workflow, enabling operators to efficiently organize cargo operations before vessel arrival while maintaining flexibility for manual adjustments (US 4.1.4).
