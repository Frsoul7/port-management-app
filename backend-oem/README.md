# OEM Backend - Operations & Execution Management

**Technology Stack:** Node.js 20+ | TypeScript 5+ | Express 4.x | MongoDB | Mongoose

**Architecture:** Clean Architecture with Domain-Driven Design (DDD)

---

## Project Structure

```
backend-oem/
├── src/
│   ├── domain/                    # LAYER 1: Domain (Core - NO dependencies)
│   │   ├── entities/              # Aggregates and entities (pure business logic)
│   │   ├── repositories/          # Repository INTERFACES (IOperationPlanRepository)
│   │   └── value-objects/         # Value objects (immutable)
│   │
│   ├── application/               # LAYER 2: Application (Depends ONLY on Domain)
│   │   ├── dtos/                  # Data Transfer Objects
│   │   ├── services/              # Application services (use repository interfaces)
│   │   └── interfaces/            # Service interfaces
│   │
│   ├── infrastructure/            # LAYER 3: Infrastructure (Implements Domain interfaces)
│   │   ├── database/              # MongoDB connection and models
│   │   ├── http/                  # HTTP clients (Core Backend, Prolog)
│   │   ├── repositories/          # Repository IMPLEMENTATIONS (OperationPlanRepository)
│   │   └── middleware/            # Express middleware (auth, logging, etc.)
│   │
│   ├── presentation/              # LAYER 4: Presentation (Depends on Application)
│   │   ├── controllers/           # REST API controllers
│   │   └── routes/                # Express routes
│   │
│   ├── shared/                    # Shared utilities
│   │   ├── types/                 # Common TypeScript types
│   │   ├── utils/                 # Utility functions
│   │   └── constants/             # Constants and enums
│   │
│   └── server.ts                  # Application entry point
│
├── .env.example                   # Environment variables template
├── .gitignore                     # Git ignore rules
├── package.json                   # NPM dependencies
├── tsconfig.json                  # TypeScript configuration
├── jest.config.js                 # Jest testing configuration
└── README.md                      # This file
```

### Clean Architecture Layers

**Dependency Rule:** Dependencies always point INWARD toward the Domain layer.

```
Presentation → Application → Domain ← Infrastructure
```

1. **Domain Layer (Core)** - NO external dependencies
   - Pure business logic, entities, value objects
   - Repository **INTERFACES** (e.g., `IOperationPlanRepository`)
   - Framework-independent

2. **Application Layer** - Depends ONLY on Domain
   - Use cases and application services
   - Uses repository **interfaces**, not implementations
   - Orchestrates business logic

3. **Infrastructure Layer** - Implements Domain interfaces
   - Repository **IMPLEMENTATIONS** (e.g., `OperationPlanRepository implements IOperationPlanRepository`)
   - Database models (Mongoose schemas)
   - HTTP clients, external services

4. **Presentation Layer** - Depends on Application
   - REST API controllers and routes
   - HTTP-specific concerns

**Key Principle:** Application services depend on repository **interfaces** from Domain, not concrete implementations from Infrastructure. This enables:
- Easy testing with mock repositories
- Swapping database technologies without changing business logic
- Framework independence

📖 **See [CLEAN_ARCHITECTURE_GUIDE.md](CLEAN_ARCHITECTURE_GUIDE.md) for detailed explanation and examples.**

---

## Domain Model

### Aggregate Roots (6 total)

1. **OperationPlan** - Stores planning results from scheduling algorithms
2. **VesselVisitExecution (VVE)** - Tracks actual vessel visit execution
3. **IncidentType** - Catalog of incident types (hierarchical)
4. **Incident** - Records disruptions and unexpected events
5. **TaskCategory** - Catalog of complementary task types
6. **ComplementaryTask** - Non-cargo activities during vessel visits

See [OEM_DOMAIN_MODEL.md](../Docs/Sprint C/OEM_DOMAIN_MODEL.md) for detailed domain model documentation.

---

## Prerequisites

- **Node.js** 20.x or higher
- **npm** 10.x or higher
- **MongoDB** 6.x or higher (running locally or remote)
- **Core Backend** running on port 5001 (.NET 9)

---

## Installation

### 1. Clone and Navigate
```bash
cd backend-oem
```

### 2. Install Dependencies
```bash
npm install
```

### 3. Set Up Environment Variables
```bash
# Copy the example file
cp .env.example .env

# Edit .env with your configuration
# IMPORTANT: Set JWT_SECRET to match Core Backend
```

### 4. Start MongoDB
```bash
# Option 1: Local MongoDB
mongod --dbpath ./data

# Option 2: Docker
docker run -d -p 27017:27017 --name oem-mongo mongo:latest

# Option 3: MongoDB Atlas (update DATABASE_URL in .env)
```

### 5. Run Development Server
```bash
npm run dev
```

Server will start on **http://localhost:3000**

---

## Available Scripts

### Development
```bash
npm run dev          # Start development server with hot reload (tsx watch)
```

### Build
```bash
npm run build        # Compile TypeScript to JavaScript (dist/)
npm start            # Run production build
```

### Testing
```bash
npm test             # Run all tests
npm run test:watch   # Run tests in watch mode
npm run test:coverage # Generate coverage report
```

### Code Quality
```bash
npm run lint         # Run ESLint
npm run lint:fix     # Fix ESLint errors
npm run format       # Format code with Prettier
```

---

## API Endpoints (Sprint C User Stories)

### Operation Plans
```
POST   /api/operationplans/generate     # Generate new operation plan (US 4.1.2)
GET    /api/operationplans               # List/filter/sort plans (US 4.1.3)
GET    /api/operationplans/missing       # Identify VVNs without plans (US 4.1.5)
GET    /api/operationplans/:id           # Get plan by ID
PATCH  /api/operationplans/:id           # Update plan (US 4.1.4)
GET    /api/operationplans/resource-allocation # Query resource usage (US 4.1.6)
```

### Vessel Visit Executions (VVE)
```
POST   /api/vessel-visit-executions      # Create VVE (US 4.1.7)
GET    /api/vessel-visit-executions      # List/filter VVEs (US 4.1.10)
GET    /api/vessel-visit-executions/:id  # Get VVE by ID
PATCH  /api/vessel-visit-executions/:id/berth       # Update berth (US 4.1.8)
PATCH  /api/vessel-visit-executions/:id/operations  # Update operations (US 4.1.9)
PATCH  /api/vessel-visit-executions/:id/complete    # Mark as completed (US 4.1.11)
```

### Incidents
```
GET    /api/incidents                    # List incidents (US 4.1.13)
POST   /api/incidents                    # Create incident (US 4.1.13)
GET    /api/incidents/:id                # Get incident by ID
PATCH  /api/incidents/:id                # Update incident
DELETE /api/incidents/:id                # Delete incident

GET    /api/incident-types               # List incident types (US 4.1.12)
POST   /api/incident-types               # Create incident type
GET    /api/incident-types/:id           # Get type by ID
PATCH  /api/incident-types/:id           # Update type
DELETE /api/incident-types/:id           # Delete type
```

### Complementary Tasks
```
GET    /api/complementary-tasks          # List tasks (US 4.1.15)
POST   /api/complementary-tasks          # Create task (US 4.1.15)
GET    /api/complementary-tasks/:id      # Get task by ID
PATCH  /api/complementary-tasks/:id      # Update task
DELETE /api/complementary-tasks/:id      # Delete task

GET    /api/task-categories              # List categories (US 4.1.14)
POST   /api/task-categories              # Create category
GET    /api/task-categories/:id          # Get category by ID
PATCH  /api/task-categories/:id          # Update category
DELETE /api/task-categories/:id          # Delete category
```

---

## Authentication

All endpoints require JWT authentication (except health check).

### JWT Token Format
Token is issued by Core Backend (.NET 9) during login.

```typescript
// JWT Payload
{
  userId: string;
  email: string;
  name: string;
  role: string;          // "admin" | "logistics_operator" | "shipping_agent" | "port_authority"
  organizationId?: string;
  iat: number;
  exp: number;
}
```

### Request Headers
```http
Authorization: Bearer <jwt-token>
Content-Type: application/json
```

### Example Request
```bash
curl -X GET http://localhost:3000/api/operationplans \
  -H "Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9..." \
  -H "Content-Type: application/json"
```

---

## Integration Points

### 1. Core Backend (.NET 9)
**URL:** http://localhost:5001
**Purpose:** Fetch VVNs, vessels, resources

**Example:**
```typescript
GET /api/vvns?fromDate=2025-11-30&toDate=2025-11-30&status=APPROVED
```

### 2. Prolog Planning Service (Future Integration)
**URL:** http://localhost:5000
**Purpose:** Generate scheduling sequences

**Note:** Integration structure is prepared but actual HTTP calls will be implemented later.

---

## Environment Variables

### Required
```bash
# Server
PORT=3000
NODE_ENV=development

# Database
DATABASE_URL=mongodb://localhost:27017/oem

# JWT (MUST match Core Backend)
JWT_SECRET=your-secret-key-here
JWT_ISSUER=PortManagementSystem

# Core Backend Integration
CORE_BACKEND_URL=http://localhost:5001
```

### Optional
```bash
# Prolog Planning Service (future)
PLANNING_SERVICE_URL=http://localhost:5000
PLANNING_SERVICE_TIMEOUT=30000

# Processing Time Constants
CONTAINER_HANDLING_TIME_MINUTES=2
SETUP_TIME_MINUTES=30

# Logging
LOG_LEVEL=debug
LOG_FILE=logs/oem-backend.log

# CORS
CORS_ORIGIN=http://localhost:4200
```

---

## Testing

### Unit Tests
```bash
npm test -- --testPathPattern=services
```

### Integration Tests
```bash
npm test -- --testPathPattern=integration
```

### E2E Tests
```bash
npm test -- --testPathPattern=e2e
```

### Coverage Report
```bash
npm run test:coverage
```

Coverage reports are generated in `coverage/` directory.

---

## Docker Support (Future)

```dockerfile
# Dockerfile example
FROM node:20-alpine
WORKDIR /app
COPY package*.json ./
RUN npm ci --only=production
COPY dist ./dist
EXPOSE 3000
CMD ["node", "dist/server.js"]
```

```bash
# Build and run
docker build -t oem-backend .
docker run -p 3000:3000 --env-file .env oem-backend
```

---

## Troubleshooting

### MongoDB Connection Error
```
Error: connect ECONNREFUSED 127.0.0.1:27017
```
**Solution:** Ensure MongoDB is running
```bash
# Check if MongoDB is running
mongosh

# Start MongoDB
mongod --dbpath ./data
```

### JWT Verification Failed
```
Error: Invalid token
```
**Solution:** Ensure `JWT_SECRET` matches Core Backend
- Check `.env` file in both backend-oem and backend/src
- Secrets must be identical

### Core Backend Connection Error
```
Error: connect ECONNREFUSED 127.0.0.1:5001
```
**Solution:** Ensure Core Backend is running
```bash
cd backend/src
dotnet run
```

---

## Development Workflow

### 1. Start All Services
```bash
# Terminal 1: MongoDB
mongod --dbpath ./data

# Terminal 2: Core Backend (.NET)
cd backend/src
dotnet run

# Terminal 3: OEM Backend (Node.js)
cd backend-oem
npm run dev

# Terminal 4: Frontend (Angular)
cd frontend
npm start
```

### 2. Access Points
- **Frontend:** http://localhost:4200
- **Core Backend API:** https://localhost:5001/swagger
- **OEM Backend API:** http://localhost:3000/api
- **MongoDB:** mongodb://localhost:27017

---

## Contributing

### Code Style
- Use ESLint and Prettier (configurations provided)
- Follow Clean Architecture layer boundaries
- Write unit tests for all services
- Document complex business logic

### Git Workflow
1. Create feature branch from `main`
2. Implement changes following architecture
3. Write tests (aim for 80%+ coverage)
4. Run `npm run lint:fix` and `npm run format`
5. Commit with descriptive messages
6. Push and create Pull Request

---

## Architecture Principles

### Clean Architecture Layers
1. **Domain** - No dependencies on other layers
2. **Application** - Depends only on Domain
3. **Infrastructure** - Implements interfaces from Domain/Application
4. **Presentation** - Depends on Application

### Dependency Rule
Dependencies point **inward** (Presentation → Application → Domain)

### Key Patterns
- **Repository Pattern** - Data access abstraction
- **Dependency Injection** - Loose coupling
- **Value Objects** - Immutable domain concepts
- **Aggregate Roots** - Consistency boundaries

---

## License

ISC License - 3DE-05 Team

---

## Support

For questions or issues:
- Check [OEM_DOMAIN_MODEL.md](../Docs/Sprint C/OEM_DOMAIN_MODEL.md)
- Review [Sprint C USs.md](../Docs/Sprint C/USs.md)
- Contact team members

---

**Ready to start implementing!** 🚀
