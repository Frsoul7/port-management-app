# User Story 4.1.11 — Technical Analysis (Levels 1–4)

## US 4.1.11  
**As a Logistics Operator, I want to mark a Vessel Visit Execution (VVE) as completed by recording the vessel’s unberth time and port departure time, so that the visit lifecycle is correctly closed and operational statistics can be derived.**

---

## 1. Architectural Vision and Responsibility (WHY)

### Objective and Value
User Story 4.1.11 is responsible for **closing the lifecycle of a Vessel Visit Execution (VVE)**. By marking a VVE as completed, the system can:
- Consolidate the final operational state of a vessel visit;
- Enable the computation of operational metrics (e.g., turnaround time, berth occupancy);
- Preserve historical integrity by preventing uncontrolled modifications after completion.

This functionality directly supports operational analysis and performance evaluation.

### Responsibilities by Architectural Layer
In accordance with the layered / clean architecture principles described in the architectural reference PDF, responsibilities are distributed as follows:

- **Presentation Layer (HTTP / API)**  
  Exposes endpoints to register unberth and completion events, validates input, and returns HTTP responses.

- **Application Layer (Use Cases / Services)**  
  Orchestrates the completion use case: retrieves the VVE, invokes domain behavior, and persists the updated state.

- **Domain Layer (Entities / Business Rules)**  
  Enforces invariants such as:
  - Completion is only allowed if all cargo operations are finished;
  - Temporal consistency between berth, unberth, and departure times;
  - Valid state transitions.

- **Infrastructure Layer (Persistence / Adapters)**  
  Implements repositories (MongoDB) without leaking infrastructure concerns into the domain.

### Dependency Rule
The Application Layer depends on **repository interfaces**, while concrete MongoDB implementations reside in the Infrastructure Layer. This complies with the Dependency Inversion Principle and ensures independence from frameworks and databases.

---

## 2. Functional Analysis and Business Rules (WHAT)

### Expected Functional Flow
1. The logistics operator selects the option to mark a VVE as *Completed* in the SPA.
2. The system requires the registration of:
   - Actual unberth time;
   - Actual port departure time.
3. The system validates that **all associated cargo operations are completed**.
4. If validation succeeds, the VVE state is updated to *Completed*.
5. After completion, the VVE becomes **read-only**, except for authorized administrative corrections.
6. The action is logged for auditability (timestamp, user, changes).

### Observed Behavior in the Codebase
In the OEM backend, the completion process is implemented in two explicit steps:
- Registration of the **unberth time**.
- Registration of the **port departure time** and transition to the *Completed* state.

This design remains consistent with the acceptance criteria, provided the SPA guides the user through the full completion workflow.

### Business Rules Implemented in the Domain
Within the `VesselVisitExecution` aggregate:
- `recordUnberthing(unberthTime)` validates:
  - The VVE is in the *In Progress* state;
  - The unberth time occurs after the berth time.
- `markAsCompleted(departureTime, completedBy)` validates:
  - The VVE is in the *In Progress* state;
  - An unberth time has already been recorded;
  - The port departure time occurs after unberth;
  - **All cargo operations are completed**.

These rules ensure consistency and enforce the acceptance criteria at the domain level.

---

## 3. Technical and Structural Analysis (HOW)

### REST API Endpoints
The OEM service exposes the following endpoints relevant to this User Story:

- **Complete VVE**
- **HTTP Method:** `POST`
- Body: `departureTime` (ISO 8601)
- Middleware: authentication and authorization
- Validations: VVE identifier and departure time

- **Record Unberth**
- **HTTP Method:** `POST`
- Body: `unberthTime`

### Component Responsibilities

#### Presentation Layer
- **VesselVisitExecutionController**
- Extracts parameters and request body;
- Retrieves user identity from the authentication context;
- Delegates to the application service;
- Returns standardized JSON responses.

#### Application Layer
- **VesselVisitExecutionService**
- Loads the VVE using the repository;
- Invokes domain methods (`recordUnberthing`, `markAsCompleted`);
- Persists the updated aggregate;
- Returns a DTO representation.

#### Domain Layer
- **VesselVisitExecution**
- Encapsulates lifecycle state and timestamps;
- Enforces all business invariants related to completion;
- Prevents invalid transitions after completion.

#### Infrastructure Layer
- **VesselVisitExecutionRepository**
- Maps between domain objects and MongoDB documents;
- Uses atomic update operations to persist state changes;
- Remains isolated from application and domain logic.

### Technical Observations from the Code
1. **User identification inconsistency**  
 In some controllers, the authenticated user is accessed via `req.user.sub`, while the completion endpoint attempts to use `req.user.id`. If the JWT payload does not contain `id`, the `completedBy` field may be undefined, partially violating auditability requirements.

2. **Limited persistent audit information**  
 The MongoDB schema does not enable automatic timestamps. Although `completedAt` is stored, there is no persistent change history, limiting long-term traceability.

---

## 4. Quality Attributes, SOLID Principles, and Evolution (ROBUSTNESS)

### Architectural Quality
- **Single Responsibility Principle (SRP)**  
- Controllers handle HTTP concerns;
- Services coordinate use cases;
- Domain entities enforce business rules;
- Repositories handle persistence.

- **Dependency Inversion Principle (DIP)**  
- Application and domain layers depend on abstractions;
- MongoDB is confined to the infrastructure layer.

- **Encapsulation of Invariants**  
Critical rules (e.g., “completion only if all operations are finished”) are enforced within the domain, protecting system integrity even if endpoints are misused.

### Read-Only Enforcement After Completion
The domain logic prevents further state-changing operations once a VVE is completed, effectively enforcing a read-only state. However, a dedicated mechanism for *authorized administrative corrections* is not explicitly implemented.

### Auditability Assessment
- Runtime logging is present via the logging adapter.
- Domain fields such as `completedAt` and `completedBy` provide basic audit data.

**Identified gaps:**
- No persistent audit trail of changes over time;
- No explicit recording of correction reasons or previous values.

### Recommended Improvements
1. Normalize user identification across all endpoints (e.g., consistently use `req.user.sub`).
2. Implement a dedicated administrative correction use case with role-based authorization.
3. Enable persistent audit tracking (timestamps, change history, or event-based logging).

---

## Conclusion
Based on the analysis of the ZIP repository, User Story 4.1.11 is largely implemented and fulfills the core acceptance criteria:
- Lifecycle closure is supported;
- Completion is blocked unless all operations are finished;
- Domain rules enforce consistency.

However, full compliance with auditability and administrative correction requirements would benefit from additional refinements. Addressing these points would strengthen robustness, traceability, and long-term maintainability, while remaining aligned with Clean and Onion Architecture principles.

