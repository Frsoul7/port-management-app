# Architecture Improvements Checklist

**Date:** January 4, 2026  
**Module:** OEM Backend (backend-oem)  
**Current Test Status:** 944 tests passing ✅

---

## Overview

This document tracks architectural improvements to ensure Clean Architecture compliance in the OEM Backend module. Each item includes priority, current state, proposed fix, and implementation status.

---

## 🔴 CRITICAL Priority

### 1. Application Service Depends on Infrastructure (DIP Violation)

**Status:** ⬜ Not Started

**Problem:**  
`OperationPlanService.ts` directly imports infrastructure classes, violating the Dependency Inversion Principle.

**Current Code (Lines 4-5):**
```typescript
import { CoreBackendClient } from '@infrastructure/http-clients/CoreBackendClient';
import { PlanningClient } from '@infrastructure/http-clients/PlanningClient';
```

**Files Affected:**
- `src/application/services/OperationPlanService.ts`

**Proposed Solution:**

1. Create interfaces in domain/application layer:

```typescript
// src/domain/ports/ICoreBackendClient.ts
export interface ICoreBackendClient {
  getVvnsByDateRange(fromDate: Date, toDate: Date): Promise<VvnReference[]>;
  getVvnById(vvnId: string): Promise<VvnReference | null>;
  getVvnsByIds(vvnIds: string[]): Promise<VvnReference[]>;
}

// src/domain/ports/IPlanningClient.ts
export interface IPlanningClient {
  generatePlan(
    vessels: PlanningVesselInput[],
    algorithm: PlanningAlgorithm,
    targetDate: Date
  ): Promise<PlanningResponse>;
}
```

2. Update `CoreBackendClient` and `PlanningClient` to implement these interfaces

3. Update `OperationPlanService` constructor to receive interfaces instead of concrete classes

**Tasks:**
- [ ] Create `src/domain/ports/ICoreBackendClient.ts`
- [ ] Create `src/domain/ports/IPlanningClient.ts`
- [ ] Create `src/domain/ports/index.ts` for exports
- [ ] Update `CoreBackendClient` to implement `ICoreBackendClient`
- [ ] Update `PlanningClient` to implement `IPlanningClient`
- [ ] Update `OperationPlanService` imports to use interfaces
- [ ] Update tests if necessary
- [ ] Verify all 944 tests still pass

---

## 🟠 HIGH Priority

### 2. DTOs Defined Inside Service Files

**Status:** ⬜ Not Started

**Problem:**  
Some services define DTOs inline instead of in dedicated DTO files, creating inconsistency.

**Files with Inline DTOs:**
- `src/application/services/IncidentService.ts` (11 DTOs, lines 11-80)
- `src/application/services/IncidentTypeService.ts` (3 DTOs, lines 9-40)

**Files with Proper DTOs (for reference):**
- `src/application/dtos/OperationPlanDto.ts` ✅
- `src/application/dtos/VesselVisitExecutionDto.ts` ✅
- `src/application/dtos/ComplementaryTaskDto.ts` ✅
- `src/application/dtos/TaskCategoryDto.ts` ✅
- `src/application/dtos/IncidentDto.ts` ✅ (partial - some DTOs here)

**Proposed Solution:**

1. Create `src/application/dtos/IncidentTypeDto.ts` with:
   - `CreateIncidentTypeDto`
   - `UpdateIncidentTypeDto`
   - `IncidentTypeDto`
   - `ListIncidentTypesQuery`

2. Move DTOs from `IncidentService.ts` to `IncidentDto.ts`:
   - `CreateIncidentDto` (if not already there)
   - `UpdateIncidentDto`
   - `StartInvestigationDto`
   - `ResolveIncidentDto`
   - `CloseIncidentDto`
   - `AddNoteDto`
   - `IncidentDto` (if not already there)
   - `IncidentFilterDto`

3. Update `src/application/dtos/index.ts` exports

4. Update service imports

**Tasks:**
- [ ] Create `src/application/dtos/IncidentTypeDto.ts`
- [ ] Move/consolidate DTOs from `IncidentService.ts` to `IncidentDto.ts`
- [ ] Update `src/application/dtos/index.ts`
- [ ] Update `IncidentService.ts` imports
- [ ] Update `IncidentTypeService.ts` imports
- [ ] Update any test file imports
- [ ] Verify all tests pass

---

### 3. Dependency Wiring in Route Files

**Status:** ⬜ Not Started

**Problem:**  
Manual dependency instantiation in route files creates tight coupling and makes testing harder.

**Current Pattern (in each route file):**
```typescript
const operationPlanRepository = new OperationPlanRepository();
const coreBackendClient = getCoreBackendClient();
const planningClient = new PlanningClient();
const operationPlanService = new OperationPlanService(
  operationPlanRepository,
  coreBackendClient,
  planningClient
);
const controller = new OperationPlanController(operationPlanService);
```

**Files Affected:**
- `src/presentation/routes/operationPlan.routes.ts`
- `src/presentation/routes/vesselVisitExecution.routes.ts`
- `src/presentation/routes/incident.routes.ts`
- `src/presentation/routes/incidentType.routes.ts`
- `src/presentation/routes/complementaryTask.routes.ts`
- `src/presentation/routes/taskCategory.routes.ts`

**Proposed Solution:**

Create a composition root / simple factory module:

```typescript
// src/infrastructure/container.ts
import { OperationPlanRepository } from './repositories/OperationPlanRepository';
import { OperationPlanService } from '@application/services/OperationPlanService';
import { OperationPlanController } from '@presentation/controllers/OperationPlanController';
// ... other imports

export class Container {
  // Singleton instances
  private static _operationPlanController: OperationPlanController | null = null;
  
  static getOperationPlanController(): OperationPlanController {
    if (!this._operationPlanController) {
      const repo = new OperationPlanRepository();
      const coreClient = getCoreBackendClient();
      const planningClient = new PlanningClient();
      const service = new OperationPlanService(repo, coreClient, planningClient);
      this._operationPlanController = new OperationPlanController(service);
    }
    return this._operationPlanController;
  }
  
  // ... similar for other controllers
  
  // For testing - reset all instances
  static reset(): void {
    this._operationPlanController = null;
    // ... reset others
  }
}
```

**Tasks:**
- [ ] Create `src/infrastructure/container.ts`
- [ ] Add factory methods for all 6 controllers
- [ ] Update route files to use container
- [ ] Add `reset()` method for testing
- [ ] Verify all tests pass

---

## 🟡 MEDIUM Priority

### 4. Standardize Query/Filter DTO Naming

**Status:** ⬜ Not Started

**Problem:**  
Inconsistent naming for query/filter DTOs across services.

**Current State:**
- ✅ `ListOperationPlansQuery` - Good naming
- ✅ `ListIncidentsQuery` - Good naming  
- ✅ `ListComplementaryTasksQuery` - Good naming
- ❓ `IncidentFilterDto` - Different pattern (in IncidentService.ts)

**Proposed Solution:**

Standardize on `List{Entity}Query` pattern for all list/filter operations.

**Tasks:**
- [ ] Review all filter/query DTOs
- [ ] Rename any inconsistent DTOs
- [ ] Update imports and usages
- [ ] Verify tests pass

---

### 5. Duplicate/Inconsistent Type Definitions

**Status:** ⬜ Not Started

**Problem:**  
Ensure all services use `PaginatedResult<T>` from `@shared/types` and not local definitions.

**Shared Definition (correct):**
```typescript
// src/shared/types/index.ts
export interface PaginatedResult<T> {
  data: T[];
  pagination: PaginationMetadata;
}
```

**Tasks:**
- [ ] Audit all services for local `PaginatedResult` definitions
- [ ] Remove any duplicates
- [ ] Ensure all imports use `@shared/types`
- [ ] Verify tests pass

---

## 🟢 LOW Priority

### 6. Seeding Logic in Connection File

**Status:** ⬜ Not Started

**Problem:**  
Database connection file (`connection.ts`) contains ~100 lines of seeding business logic.

**File:** `src/infrastructure/database/connection.ts` (lines 21-130)

**Proposed Solution:**

1. Create seeders directory structure:
```
src/infrastructure/database/
├── connection.ts (only connection logic)
├── seeders/
│   ├── index.ts
│   ├── operationPlanSeeder.ts
│   ├── vveSeeder.ts
│   └── taskCategorySeeder.ts
```

2. Move seeding functions to appropriate files

3. Update `connection.ts` to optionally call seeders based on environment

**Tasks:**
- [ ] Create `src/infrastructure/database/seeders/` directory
- [ ] Extract `seedVveFromOperationPlanIfNeeded` to seeder file
- [ ] Extract `seedTaskCategories` to seeder file
- [ ] Update `connection.ts` to import and call seeders
- [ ] Verify tests pass

---

## Progress Tracking

| # | Priority | Issue | Status | Date Completed |
|---|----------|-------|--------|----------------|
| 1 | 🔴 CRITICAL | DIP Violation - Infrastructure imports | ⬜ Not Started | - |
| 2 | 🟠 HIGH | DTOs in service files | ⬜ Not Started | - |
| 3 | 🟠 HIGH | Dependency wiring in routes | ⬜ Not Started | - |
| 4 | 🟡 MEDIUM | Query DTO naming | ⬜ Not Started | - |
| 5 | 🟡 MEDIUM | Duplicate type definitions | ⬜ Not Started | - |
| 6 | 🟢 LOW | Seeding in connection file | ⬜ Not Started | - |

**Legend:**
- ⬜ Not Started
- 🔄 In Progress
- ✅ Completed
- ⏸️ Blocked

---

## Architecture Compliance Checklist

### Clean Architecture Rules

- [ ] Domain layer has no external dependencies
- [ ] Application layer only depends on Domain layer
- [ ] Infrastructure implements Domain interfaces
- [ ] Presentation only depends on Application layer
- [ ] Dependencies point inward (toward Domain)

### Current Compliance Status

| Layer | Compliant | Notes |
|-------|-----------|-------|
| Domain | ✅ Yes | No infrastructure imports |
| Application | ⚠️ Partial | Fix CoreBackendClient/PlanningClient |
| Infrastructure | ✅ Yes | Implements domain interfaces |
| Presentation | ✅ Yes | Controllers delegate to services |

---

## Notes

- Always run `npm test` after each change to verify 944 tests pass
- Consider creating feature branches for each major change
- Update this document as items are completed
