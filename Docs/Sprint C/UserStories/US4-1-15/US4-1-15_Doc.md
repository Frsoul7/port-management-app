# User Story 4.1.15 — Technical Documentation (Levels 1–4)

## US 4.1.15  
**As a Logistics Operator, I want to record and manage complementary tasks associated with a Vessel Visit Execution (VVE), so that non-cargo activities impacting operations are properly tracked and analyzed.**

Complementary tasks include activities such as inspections, maintenance, cleaning, or safety procedures that may run in parallel with, or temporarily suspend, cargo operations.

---

## 1. Architectural Vision and Responsibility (WHY)

### Objective and Value
The objective of User Story 4.1.15 is to ensure that **non-cargo operational activities** are explicitly modeled, recorded, and associated with a **Vessel Visit Execution (VVE)**.

By capturing complementary tasks, the system can:
- Provide a more accurate representation of real operational conditions;
- Explain deviations between planned and executed activities;
- Improve traceability of delays or idle periods;
- Support operational analysis and reporting.

### Role in the System
This User Story is part of the **Operations & Execution Management (OEM)** module and complements the execution lifecycle managed by VVEs.

Its role is **supportive and descriptive**, enriching execution data without directly driving cargo operations or scheduling decisions.

### Architectural Layer
- **Application Layer**
  - Coordinates creation, association, and retrieval of complementary tasks.
  - Does not perform scheduling or execution control.

### Architectural Principles Applied
- **Separation of Concerns**  
  Complementary tasks are treated as a concept distinct from cargo operations.
- **Layer Isolation**  
  Execution logic is independent of persistence and presentation concerns.
- **Extensibility**  
  New task types and behaviors can be introduced without affecting the core execution model.

---

## 2. Functional Analysis and Business Rules (WHAT)

### Functional Description
The logistics operator records **complementary tasks** associated with a specific VVE.  
Each complementary task represents an activity that:
- Is not a cargo operation;
- May run in parallel with cargo operations;
- May temporarily block or suspend cargo operations.

The system allows the operator to:
- Create a complementary task linked to a VVE;
- Assign the task to a predefined category (defined in US 4.1.14);
- Register start and end times;
- Indicate whether the task blocks cargo operations;
- Query existing complementary tasks for a VVE.

### Business Rules
- A complementary task must always be associated with an existing VVE.
- Each task must belong to a valid **complementary task category**.
- The start time must precede the end time.
- Multiple complementary tasks may overlap in time.
- Blocking tasks may justify delays but do not automatically change the VVE state.
- Modification of tasks for a completed VVE is restricted, unless explicitly authorized.

### Typical Use Cases
- Recording safety or regulatory inspections.
- Logging maintenance or repair activities during berth.
- Tracking cleaning or environmental procedures.
- Explaining idle or blocked periods in execution timelines.

### Exceptional Scenarios
- Non-existent or invalid VVE identifier.
- Invalid complementary task category.
- End time earlier than start time.
- Attempt to update tasks for a completed VVE without authorization.

---

## 3. Technical and Structural Analysis (HOW)

### REST API Exposure
Based on the OEM backend structure, complementary tasks are exposed through dedicated REST endpoints, such as:

- **Create complementary task**

- **List complementary tasks**

Typical request payload fields include:
- `taskCategoryId`
- `startTime`
- `endTime`
- `blocksOperations` (boolean)
- Optional description or notes

### Component Responsibilities

#### Presentation Layer
- **ComplementaryTaskController**
- Validates path parameters and request body.
- Verifies that the referenced VVE exists.
- Delegates processing to the application service.
- Returns standardized HTTP responses.

#### Application Layer
- **ComplementaryTaskService**
- Orchestrates task creation and retrieval.
- Coordinates interactions between task and VVE repositories.
- Applies application-level validations and policies.

#### Domain Layer
- **ComplementaryTask**
- Encapsulates task attributes and invariants.
- **ComplementaryTaskCategory**
- Defines the classification and semantics of tasks.
- **VesselVisitExecution**
- Acts as the aggregate root to which complementary tasks are associated.

Domain rules enforce:
- Temporal consistency of task execution.
- Mandatory association to a valid VVE.
- Restrictions when the VVE is in a completed state.

#### Infrastructure Layer
- **ComplementaryTaskRepository**
- Persists complementary tasks in MongoDB.
- **VesselVisitExecutionRepository**
- Ensures VVE existence and retrieves execution state.

MongoDB is used strictly as a persistence mechanism, with mapping logic isolated from the domain.

### Dependency Management
- Application services depend on **repository interfaces**, not concrete implementations.
- MongoDB-specific code is confined to the infrastructure layer.
- All dependencies point inward, complying with the **Dependency Inversion Principle (DIP)**.

---

## 4. Quality Attributes, SOLID Principles, and Evolution (ROBUSTNESS)

### Architectural Quality

**Single Responsibility Principle (SRP)**  
- Controllers handle HTTP-related concerns.
- Services coordinate use cases.
- Domain entities enforce business rules.
- Repositories manage persistence.

**Dependency Inversion Principle (DIP)**  
- Core business logic is independent of MongoDB and Express.
- Infrastructure components implement interfaces defined in inner layers.

**Testability**  
- Complementary task logic can be unit-tested using mocked repositories.
- Domain invariants can be verified without database dependencies.

### Validation and Auditability
- Input validation is performed at the API boundary.
- Creation and modification of complementary tasks are logged.
- Task timestamps provide temporal traceability of activities.

### Current Limitations
- No persistent audit trail for task modifications (only current state is stored).
- Blocking tasks do not automatically influence cargo operation states.
- No dedicated workflow for authorized corrections after VVE completion.

### Recommended Improvements
- Introduce persistent audit events for task creation and updates.
- Explicitly model the impact of blocking tasks on execution timelines.
- Add role-based correction endpoints for completed VVEs.
- Support aggregated reporting (e.g., total blocking time per VVE).

---

## Conclusion
User Story 4.1.15 enhances the realism and analytical value of the OEM module by explicitly modeling complementary operational activities.

By isolating complementary tasks from cargo operations while associating them with VVEs, the system achieves:
- Improved transparency of execution data;
- Better support for post-operation analysis;
- Strong alignment with layered, clean, and onion architecture principles.

This functionality provides a solid foundation for advanced execution analytics and operational intelligence without compromising architectural integrity.
