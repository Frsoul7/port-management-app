# Port Management System

This project implements a Port Visit Notification and Management System using Clean Architecture principles with DDD (Domain-Driven Design) and 3D visualization.

## Project Structure

```
LEI-SEM5-PI-2025-26-3DE-05/
├── backend/                        # Backend .NET solution
│   ├── src/                        # Source code
│   │   ├── Domain/                 # Domain Layer (Core business logic)
│   │   │   ├── Shared/            # Base classes, interfaces
│   │   │   ├── Organizations/     # Organization aggregate
│   │   │   ├── Users/             # User aggregate
│   │   │   ├── Vessels/           # Vessel entities
│   │   │   ├── Visits/            # VVN and ScheduledVisit aggregates
│   │   │   ├── HumanResources/    # Staff, crew management
│   │   │   ├── Resources/         # Physical resources
│   │   │   ├── Docks/             # Dock and storage areas
│   │   │   └── Factory/           # Domain factories
│   │   ├── Application/           # Application Layer (Use cases)
│   │   │   ├── DTOs/              # Data transfer objects
│   │   │   │   └── Visualization/ # 3D visualization DTOs
│   │   │   ├── Interfaces/        # Service interfaces
│   │   │   ├── Services/          # Application services
│   │   │   │   └── VisualizationAppService.cs  # 3D data service
│   │   │   ├── Authorization/     # Authorization handlers
│   │   │   └── Security/          # Security utilities
│   │   ├── Infrastructure/        # Infrastructure Layer (EF Core, persistence)
│   │   │   ├── PortDbContext.cs   # DbContext
│   │   │   ├── Configurations/    # Entity configurations
│   │   │   ├── Repositories/      # Repository implementations
│   │   │   ├── Mappers/           # Entity mappers
│   │   │   └── UnitOfWork.cs      # Unit of Work pattern
│   │   ├── Presentation/          # Presentation Layer (Web API)
│   │   │   └── Controllers/       # REST API controllers
│   │   │       └── VisualizationController.cs  # 3D data API
│   │   ├── Program.cs             # Application entry point
│   │   └── appsettings.json       # Configuration
│   ├── tests/                      # Test projects
│   └── LEI-SEM5-PI-2025-26-3DE-05.sln  # Solution file
│
├── backend-oem/                    # Backend OEM (Operations & Execution Management)
│   ├── src/                        # Source code (TypeScript + Express + MongoDB)
│   │   ├── domain/                 # Domain Layer (Core business logic)
│   │   │   ├── entities/          # Domain entities
│   │   │   │   ├── OperationPlan.ts
│   │   │   │   ├── VesselVisitExecution.ts
│   │   │   │   ├── ComplementaryTask.ts
│   │   │   │   ├── TaskCategory.ts
│   │   │   │   ├── IncidentType.ts
│   │   │   │   └── Incident.ts
│   │   │   ├── ports/             # Port interfaces (DIP - Dependency Inversion)
│   │   │   │   ├── ICoreBackendClient.ts
│   │   │   │   └── IPlanningClient.ts
│   │   │   ├── repositories/      # Repository interfaces
│   │   │   ├── services/          # Domain services
│   │   │   └── value-objects/     # Value objects
│   │   ├── application/           # Application Layer (Use cases)
│   │   │   ├── dtos/              # Data transfer objects
│   │   │   ├── interfaces/        # Service interfaces
│   │   │   └── services/          # Application services
│   │   │       ├── OperationPlanService.ts
│   │   │       ├── VesselVisitExecutionService.ts
│   │   │       ├── ComplementaryTaskService.ts
│   │   │       ├── TaskCategoryService.ts
│   │   │       ├── IncidentTypeService.ts
│   │   │       └── IncidentService.ts
│   │   ├── infrastructure/        # Infrastructure Layer
│   │   │   ├── container.ts       # Dependency Injection container
│   │   │   ├── database/          # MongoDB models and connection
│   │   │   │   ├── models/        # Mongoose schemas
│   │   │   │   ├── seeders/       # Database seeders
│   │   │   │   └── connection.ts  # Database connection
│   │   │   ├── repositories/      # Repository implementations
│   │   │   ├── http/              # HTTP utilities
│   │   │   ├── http-clients/      # External API clients
│   │   │   └── middleware/        # Express middleware
│   │   ├── presentation/          # Presentation Layer (REST API)
│   │   │   ├── controllers/       # Request handlers
│   │   │   │   ├── OperationPlanController.ts
│   │   │   │   ├── VesselVisitExecutionController.ts
│   │   │   │   ├── ComplementaryTaskController.ts
│   │   │   │   ├── TaskCategoryController.ts
│   │   │   │   ├── IncidentTypeController.ts
│   │   │   │   └── IncidentController.ts
│   │   │   └── routes/            # Express routes
│   │   │       ├── operationPlan.routes.ts
│   │   │       ├── vesselVisitExecution.routes.ts
│   │   │       ├── complementaryTask.routes.ts
│   │   │       ├── taskCategory.routes.ts
│   │   │       ├── incidentType.routes.ts
│   │   │       └── incident.routes.ts
│   │   ├── shared/                # Shared utilities
│   │   ├── config/                # Configuration
│   │   └── server.ts              # Application entry point
│   ├── tests/                      # Test projects
│   │   ├── unitTests/             # Unit tests (Jest)
│   │   └── integration/           # Integration tests
│   ├── logs/                       # Application logs
│   ├── package.json               # NPM dependencies
│   ├── tsconfig.json              # TypeScript configuration
│   └── jest.config.js             # Jest test configuration
│
├── frontend/                       # Frontend Angular application
│   ├── src/
│   │   ├── app/
│   │   │   ├── core/              # Core module (auth, guards, interceptors)
│   │   │   │   └── services/
│   │   │   │       └── visualization.service.ts  # 3D data service
│   │   │   ├── features/          # Feature modules (VVN, vessels, users, etc.)
│   │   │   │   └── visualization/ # 3D port visualization
│   │   │   │       └── port-3d.component.ts
│   │   │   ├── three/             # Three.js services
│   │   │   │   └── services/
│   │   │   │       ├── model-loader.service.ts
│   │   │   │       ├── port-assets.service.ts
│   │   │   │       └── port-scene.service.ts
│   │   │   └── shared/            # Shared components
│   │   └── environments/
│   ├── public/models/              # 3D GLB models
│   │   ├── Ocean.glb              # Ocean animation
│   │   ├── dock.glb               # Dock structures
│   │   ├── vessel_container.glb   # Container vessels
│   │   ├── warehouse.glb          # Warehouse buildings
│   │   └── yard.glb               # Yard areas
│   ├── angular.json
│   └── package.json
│
├── Docs/                           # Documentation
│   ├── Sprint A/                   # Sprint A documentation
│   ├── Sprint B/                   # Sprint B documentation (with 3D viz)
│   ├── Sprint C/                   # Sprint C documentation
│   │   ├── Architecture/          # Architecture documentation
│   │   ├── UserStories/           # User stories (folder renamed from 'User Stories')
│   │   ├── USs/                   # US implementation docs
│   │   ├── Misc/                  # Miscellaneous documents
│   │   └── NewDocs/               # New documentation
│   └── GDPR/                       # GDPR compliance reports
│       └── GDPR_Compliance_Report_SprintC.pdf
│
└── README.md                       # This file
```

## Clean Architecture Layers

**1. Domain Layer** (Innermost - No dependencies)
- Pure business logic
- Domain entities, value objects, aggregates
- Domain events and domain services
- Factory interfaces

**2. Application Layer** (Depends only on Domain)
- Use cases and business workflows
- DTOs for data transfer (including 3D visualization data)
- Service interfaces
- Authorization policies

**3. Infrastructure Layer** (Implements interfaces from Application/Domain)
- EF Core DbContext and repositories
- External service integrations
- Data persistence
- Unit of Work implementation

**4. Presentation Layer** (Outermost - Depends on Application)
- REST API controllers (including visualization endpoints)
- Request/response handling
- HTTP-specific concerns

## How to Run

### Backend (.NET API)

1. Navigate to backend directory:
   ```bash
   cd backend/src
   ```

2. Restore dependencies:
   ```bash
   dotnet restore
   ```

3. Apply database migrations:
   ```bash
   dotnet ef database update
   ```

4. Run the application:
   ```bash
   dotnet run
   ```

5. API served at **https://localhost:5001** (or configured port) by default
5. Access API documentation:
   - **Swagger UI**: https://localhost:5001/swagger
   - **Scalar**: https://localhost:5001/scalar/v1
   - **Visualization API**: https://localhost:5001/api/visualization (3D port layout data)

### Backend OEM (Node.js + TypeScript + MongoDB)

1. Navigate to backend-oem directory:
   ```bash
   cd backend-oem
   ```

2. Install dependencies:
   ```bash
   npm install
   ```

3. Configure MongoDB connection:
   - Create `.env` file in backend-oem directory (if not exists)
   - Set MongoDB connection string:
     ```env
     MONGODB_URI=mongodb://localhost:27017/port-oem
     PORT=3000
     ```

4. Run development server:
   ```bash
   npm run dev
   ```

5. API served at **http://localhost:3000** by default
6. Access API documentation:
   - **Swagger UI**: http://localhost:3000/api-docs
   - **Operation Plans API**: http://localhost:3000/api/v1/operation-plans
   - **Vessel Visit Executions API**: http://localhost:3000/api/v1/vessel-visit-executions
   - **Incident Types API**: http://localhost:3000/api/v1/incident-types

### Frontend (Angular)

1. Navigate to frontend directory:
   ```bash
   cd frontend
   ```

2. Install dependencies:
   ```bash
   npm install
   ```

3. Run development server:
   ```bash
   npm start
   ```

4. Access frontend at **http://localhost:4200**
5. Features:
   - Authentication (Google OAuth)
   - VVN Management
   - Vessel Registration
   - **3D Port Visualization** (Three.js with GLB models)

## Database Configuration

- The project uses **InMemory EF provider** for quick testing and development
- To use **SQL Server**, update the connection string in `backend/src/appsettings.json`:
  ```json
  "ConnectionStrings": {
    "DefaultConnection": "Server=localhost;Database=PortManagement;Trusted_Connection=True;"
  }
  ```
- Update `backend/src/Program.cs` to use SQL Server:
  ```csharp
  options.UseSqlServer(builder.Configuration.GetConnectionString("DefaultConnection"))
  ```
- Run migrations from `backend/src`:
  ```bash
  dotnet ef migrations add Initial
  dotnet ef database update
  ```

## Technologies

**Backend (.NET):**
- .NET 9.0
- ASP.NET Core Web API
- Entity Framework Core (InMemory)
- Google OAuth 2.0 Authentication
- JWT Token-based sessions
- Swagger/Scalar API Documentation

**Backend OEM (Node.js):**
- Node.js 18+
- TypeScript 5.x
- Express.js
- MongoDB with Mongoose ODM
- Clean Architecture (Domain-Driven Design)
- Jest (Unit Testing)
- Swagger/OpenAPI Documentation
- Winston (Logging)

**Frontend:**
- Angular 20
- TypeScript
- **Three.js** (3D visualization with WebGL)
- RxJS
- Angular Material (UI components)
- **GLB 3D Models** (Ocean, vessels, docks, warehouses, yard)

## API Endpoints

- `GET /api/organizations` - List all organizations
- `GET /api/organizations/{id}` - Get organization by ID
- `POST /api/organizations` - Create new organization

## API Documentation

- **Swagger UI**: https://localhost:5001/swagger - Traditional Swagger interface
- **Scalar**: https://localhost:5001/scalar/v1 - Modern API documentation interface

## Authentication Headers for Testing

Most endpoints require authentication headers. Use these seeded test credentials:

### Port Authority Officer (for vessel registration, approvals, etc.)
**Simplified authentication - no user tracking required**
```http
X-Org-Id: aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa
X-Role: portauthorityofficer
```
**Organization:** Port Authority of Demo (PA001)

**Note:** Port Authority operations validate only the role and organization type. X-User-Id is optional and defaults to a system placeholder if not provided.

### Shipping Agent Representative (for VVN creation, manifest management, etc.)
**Full user tracking required**
```http
X-User-Id: 22222222-2222-2222-2222-222222222222
X-Org-Id: bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb
X-Role: shippingagentrep
```
**User:** Alice Maritime  
**Email:** alice@globalship.com  
**Organization:** Global Shipping Ltd (SHIPAG01)

### Admin (full access)
```http
X-User-Id: <any-valid-user-id>
X-Org-Id: <any-valid-org-id>
X-Role: admin
```

### Example Requests

**POST Vessel (Port Authority):**
```http
POST /api/vessels
Headers:
  X-Org-Id: aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa
  X-Role: portauthorityofficer
  Content-Type: application/json

Body:
{
  "imoNumber": "9074729",
  "name": "Test Vessel",
  "vesselTypeId": "<vessel-type-id>",
  "organizationId": "bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb",
  "capacityTEU": 5000
}
```

**POST VVN (Shipping Agent):**
```http
POST /vvns
Headers:
  X-User-Id: 22222222-2222-2222-2222-222222222222
  X-Org-Id: bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb
  X-Role: shippingagentrep
  Content-Type: application/json

Body:
{
  "vesselImo": "9074729",
  "purpose": "LOAD",
  "eta": "2025-12-01T10:00:00Z",
  "etd": "2025-12-02T18:00:00Z",
  "captainName": "Captain Smith",
  "captainCitizenId": "ABC123",
  "captainNationality": "PT",
  "crewCount": 25
}
```

