# User Story 4.1.6 — Technical Documentation (Levels 1–4)

## US 4.1.6
**As a Logistics Operator, I want to query, for a given period, the total allocation time of a specific resource (e.g., crane, dock, or staff), so that I can assess resource utilization and workload distribution.**

---

## 1. Architectural Vision and Responsibility (WHY)

### Objective
This User Story aims to assess the effective utilization of operational resources, enabling the system to answer questions such as:
- Which resources are overloaded?
- Which resources are underutilized?
- Where do operational imbalances occur?

By providing aggregated utilization metrics, this functionality supports informed decision-making related to capacity planning, workload balancing, and operational optimization.

### Role in the System
US 4.1.6 belongs to the **Operations & Execution Management (OEM)** module and fulfills a strictly **analytical role**. It operates exclusively on **persisted planning data (Operation Plans)** and does not interfere with planning generation or execution processes.

### Architectural Layer
- **Application Layer**
  - Orchestrates data aggregation and analysis.
  - Does not contain persistence or presentation logic.

This User Story respects the **Separation of Concerns** principle:
- It does not recalculate planning.
- It does not execute or modify operations.
- It only analyzes existing, persisted plans.

### Applied Architectural Principles
- Clear isolation of layers (*Layers of Isolation*).
- Independence from user interface and database technologies.
- Dependency exclusively on internal abstractions (repository interfaces).

---

## 2. Functional Analysis and Business Rules (WHAT)

### Functional Description
The logistics operator requests an aggregated utilization report for a specific resource over a defined time interval.

The system performs the following steps:
1. Receives the resource identifier and the requested time period.
2. Selects only **persisted Operation Plans** that fall within the specified interval.
3. Aggregates:
   - The total allocation time of the resource.
   - The number of associated operations.
4. Returns a quantitative summary of resource utilization.

### Business Rules
- Only **persisted Operation Plans** are considered.
- The data analyzed is **planned data**, not execution data.
- The time interval must be valid (start date/time precedes end date/time).
- Non-existent resources result in an empty response or a controlled error.

### Use Cases
- Analysis of crane workload distribution.
- Evaluation of dock occupancy levels.
- Assessment of staff workload allocation.

### Exceptional Scenarios
- Invalid time interval.
- Resource with no allocations in the given period.
- Resource identifier does not exist.

---

## 3. Technical and Structural Analysis (HOW)

### REST API Exposure
- **HTTP Method:** `GET`
- **Endpoint:** 
- `/api/resource-allocations`
- **Query Parameters:**
- `resourceId`
- `resourceType` (`crane` | `dock` | `staff`)
- `from`
- `to`

### Involved Components

#### Presentation Layer
- **Controller**
- Validates input parameters.
- Delegates the request to the application service.
- Returns the appropriate HTTP response.

#### Application Layer
- **ResourceAllocationService**
- Coordinates aggregation logic.
- Is independent of MongoDB and persistence schemas.
- Operates using DTOs and domain entities.

#### Domain Layer
- **Core Concepts:**
- `OperationPlan`
- `PlannedOperation`
- `ResourceReference`
- Calculation rules (e.g., operation duration) are encapsulated within the domain model.

#### Infrastructure Layer
- **OperationPlanRepository**
- Implements queries filtered by:
  - Time period.
  - Associated resource.
- MongoDB is used strictly as a technical persistence mechanism, without leaking infrastructure concerns into the domain.

### Dependency Management
- Dependencies always point inward, toward the domain.
- The application service depends on repository interfaces rather than concrete implementations.
- Concrete implementations are injected at runtime, in accordance with the **Dependency Inversion Principle (DIP)**.

---

## 4. Quality Attributes, SOLID Principles, Auditability, and Evolution (ROBUSTNESS)

### Architectural Quality

**Single Responsibility Principle (SRP)**  
Each layer has a single, well-defined responsibility:
- Controller: HTTP request/response handling.
- Service: Application-level orchestration and aggregation.
- Repository: Data access and persistence concerns.

**Dependency Inversion Principle (DIP)**  
The domain and application layers are independent of MongoDB and external frameworks.

**Testability**  
The application service can be unit-tested using mocked repository implementations, enabling fast and deterministic tests.

### Validation and Auditability
- Input validation is performed at the presentation layer.
- Requests and aggregation results are logged for traceability.
- The operation is strictly read-only and does not alter system state.

### Current Limitations
- Operates exclusively on planned data.
- Does not yet correlate planning data with real execution data (Vessel Visit Executions).
- Does not distinguish utilization peaks within the analyzed period.

### Future Improvements
- Integration with execution data to support *planned vs. actual* analysis.
- Introduction of advanced metrics (e.g., utilization rate, idle time).
- Graphical visualization of results (e.g., heatmaps, timelines).
- Caching of aggregation results for large time intervals to improve performance.

---

## Conclusion
User Story 4.1.6 establishes a solid foundation for operational analytics within the OEM module. It achieves this while maintaining:
- Strong architectural isolation.
- Low coupling between components.
- High extensibility for future analytical and reporting features.

This functionality is well positioned to evolve into comprehensive KPI monitoring, business intelligence, and operational dashboards without compromising the principles of Clean and Onion Architecture.
