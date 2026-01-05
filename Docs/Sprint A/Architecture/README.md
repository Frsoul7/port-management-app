# Architecture Documentation - Port Management System

This folder contains the C4 model architecture diagrams for the Port Management System.

## C4 Model Overview

The C4 model provides a hierarchical set of software architecture diagrams at different levels of abstraction:
- **Level 1 (Context)**: Shows the system in its environment with external actors and systems
- **Level 2 (Container)**: Shows the high-level technical building blocks (applications, data stores)
- **Level 3 (Component)**: Shows the internal structure of containers (specific to user stories)

## Diagrams

### 4.1 - C4 Level 1: Context Diagram
**File**: `C4_Level1_Context.puml`

Shows the Port Management System in its operational context with:

**Actors:**
- **Port Authority Officer**: Manages port operations, approves vessel visits, assigns resources
- **Shipping Agent Representative**: Represents shipping companies, submits vessel visit notifications
- **Port Staff Member**: Executes port operations, uses physical resources

**External Systems:**
- **IMO Database**: International Maritime Organization vessel registry for IMO number validation
- **Customs System**: Integration for cargo clearance and customs inspection
- **Email/SMS Service**: Notification system for VVN status changes

**Key Interactions:**
- Officers register vessels, approve/reject VVNs, assign docks and resources
- Agents create VVNs, manage cargo manifests (loading/unloading), update visit details
- System validates vessel IMO numbers against international registry
- System shares cargo manifest data with customs for clearance
- System sends automated notifications about VVN status changes

### 4.2 - C4 Level 2: Container Diagram
**File**: `C4_Level2_Container.puml`

Shows the internal architecture of the Port Management System:

**Containers:**

1. **Web API** (.NET 9 / ASP.NET Core)
   - RESTful API endpoints for all operations
   - Swagger/Scalar documentation
   - Authentication via headers (X-User-Id, X-Org-Id, X-Role)
   - Controllers: VesselVisitNotifications, Vessels, Organizations, Docks, etc.

2. **Application Layer** (.NET 9)
   - Service classes orchestrating business workflows
   - Policy enforcement (e.g., HazardousRequiresCrewPolicy)
   - Cross-cutting concerns (validation, authorization)
   - Key services: VesselVisitService, VvnIdGenerator

3. **Domain Layer** (.NET 9 / DDD)
   - Core business entities and aggregates:
     - **VesselVisitNotification** (aggregate root)
     - **Vessel** and **VesselType**
     - **CargoManifest** (Loading/Unloading)
     - **Organization** and **Representative**
     - **PhysicalResource** (STS Cranes, Mobile Equipment)
     - **StaffMember** and **Qualification**
     - **Dock** and **StorageArea**
   - Domain services and validators (IMO, ISO 6346)
   - Business rules and invariants

4. **Infrastructure Layer** (EF Core)
   - Repository implementations
   - Entity configurations (Fluent API)
   - Database context (PortDbContext)
   - In-Memory provider for development/testing
   - SQL Server support for production

5. **Database** (In-Memory / SQL Server)
   - Stores all domain entities
   - Configured via EF Core migrations
   - Seeded with test data in development

**Technology Stack:**
- **Framework**: .NET 9
- **API**: ASP.NET Core Web API
- **ORM**: Entity Framework Core
- **Database**: In-Memory (dev), SQL Server (prod)
- **Testing**: xUnit, In-Memory database
- **Documentation**: Swagger UI, Scalar

**Architecture Pattern:**
- **Domain-Driven Design (DDD)**
- **Clean Architecture** (Domain, Application, Infrastructure separation)
- **Repository Pattern** (data access abstraction)
- **Aggregate Pattern** (consistency boundaries)

## Viewing the Diagrams

### Option 1: PlantUML Extension (VS Code)
1. Install the PlantUML extension in VS Code
2. Open any `.puml` file
3. Press `Alt+D` to preview the diagram

### Option 2: Online Viewer
1. Copy the content of the `.puml` file
2. Go to https://www.plantuml.com/plantuml/uml/
3. Paste the content to view the diagram

### Option 3: Export to SVG
Run the following command to generate SVG files:
```bash
# Using PlantUML JAR
java -jar plantuml.jar C4_Level1_Context.puml
java -jar plantuml.jar C4_Level2_Container.puml
```

## Domain Model Highlights

### Key Aggregates

1. **VesselVisitNotification (VVN)**
   - State machine: IN_PROGRESS → SUBMITTED → APPROVED/REJECTED
   - Contains cargo manifests (loading/unloading)
   - Crew summary with captain nationality (ISO 3166-1)
   - ISO 6346:2022 validation for container codes
   - IMO number validation with checksum

2. **Vessel**
   - Identified by IMO number (7-digit with mod-10 check)
   - Has VesselType (container ship, bulk carrier, etc.)
   - Port of registry

3. **Organization**
   - Port Authority or Shipping Agent
   - Has representatives (users)
   - Business identifier (e.g., PA001, SHIPAG01)

4. **Physical Resources**
   - STS Cranes, Mobile Equipment
   - Resource codes with validation
   - Assigned to port operations

5. **Human Resources**
   - Staff members with qualifications
   - Role-based access control
   - Work schedules and assignments

## Business Rules

### VVN Workflow
1. **Creation** (IN_PROGRESS):
   - Shipping agent representative creates VVN
   - Adds loading/unloading cargo manifests
   - Sets crew information (optional at this stage)
   - Can modify all fields

2. **Submission** (SUBMITTED):
   - Validates all required fields
   - Validates IMO number with checksum
   - Validates all ISO 6346 container codes
   - Requires crew info if hazardous goods present
   - VVN becomes immutable

3. **Approval/Rejection** (APPROVED/REJECTED):
   - Port authority officer reviews VVN
   - Assigns dock and resources if approved
   - Can reject with reason
   - Rejected VVNs can be reopened to IN_PROGRESS

### Container Code Validation
- **Standard**: ISO 6346:2022
- **Format**: 3 letters (owner) + 1 letter (category: U/J/Z) + 6 digits + 1 check digit
- **Examples**: ABCU0000017, ABCU0000022
- **Algorithm**: Checksum with positional weights (2^0 to 2^9), modulo 11

### IMO Number Validation
- **Format**: 7 digits with last digit as mod-10 check
- **Examples**: 9319466, 9379466
- **Invalid**: 9999999 (fails checksum)

## References

- **C4 Model**: https://c4model.com/
- **PlantUML**: https://plantuml.com/
- **C4-PlantUML**: https://github.com/plantuml-stdlib/C4-PlantUML
- **ISO 6346**: Container code standard
- **IMO Number**: International Maritime Organization vessel identification
