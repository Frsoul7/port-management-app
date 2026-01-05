# Cypress E2E Tests

This directory contains end-to-end tests for the Port Management System frontend using Cypress.

## Prerequisites

- Node.js and npm installed
- Frontend application running on `http://localhost:4200`
- Backend API running on `http://localhost:5174`
- Admin user seeded in the database

## Admin Credentials

The tests use the default admin credentials from the backend seed data:

- **Email**: `admin@portsystem.com`
- **Password**: `Admin@123`

These credentials are configured in `cypress.config.ts` and can be overridden using environment variables.

## Test Structure

```
cypress/
├── e2e/                          # Test specifications
│   ├── admin-login.cy.ts         # Admin authentication tests
│   ├── docks.cy.ts               # Dock management tests
│   ├── navigation.cy.ts          # Navigation and routing tests
│   ├── physical-resources.cy.ts  # Physical resources tests
│   ├── shipping-agents.cy.ts     # Shipping agents tests
│   ├── storage-areas.cy.ts       # Storage areas tests
│   ├── user-management.cy.ts     # User management tests
│   ├── vessels.cy.ts             # Vessel management tests
│   ├── visualization.cy.ts       # 3D visualization tests
│   └── vvns.cy.ts                # Vessel Visit Notifications tests
├── support/
│   ├── commands.ts               # Custom Cypress commands
│   └── e2e.ts                    # Support file
└── cypress.config.ts             # Cypress configuration

```

## Running Tests

### Interactive Mode (Cypress GUI)

Open the Cypress Test Runner for interactive test development:

```bash
npm run cypress:open
```

### Headless Mode (CI/CD)

Run all tests in headless mode:

```bash
npm run cypress:run
```

Run tests in a specific browser:

```bash
npm run cypress:run:chrome
npm run cypress:run:firefox
```

### Run with Application

Start the application and run tests automatically:

```bash
npm run e2e
```

This command:
1. Starts the frontend on `http://localhost:4200`
2. Waits for the server to be ready
3. Runs all Cypress tests
4. Stops the server

## Custom Commands

The test suite includes custom Cypress commands for common operations:

### `cy.loginAsAdmin()`

Login as admin user using the default credentials. Uses session caching for better performance.

```javascript
cy.loginAsAdmin();
cy.visit('/dashboard');
```

### `cy.logout()`

Logout the current user and clear all session data.

```javascript
cy.logout();
```

### `cy.checkAuthentication()`

Check if a user is currently authenticated.

```javascript
cy.checkAuthentication().then((isAuthenticated) => {
  if (!isAuthenticated) {
    cy.loginAsAdmin();
  }
});
```

### `cy.getAuthToken()`

Get the current authentication token from localStorage.

```javascript
cy.getAuthToken().then((token) => {
  cy.log('Current token:', token);
});
```

### `cy.setAuthToken(token)`

Set an authentication token in localStorage.

```javascript
cy.setAuthToken('your-token-here');
```

## Test Coverage

### Authentication & Authorization
- **admin-login.cy.ts**: Admin login form validation, successful login, error handling, session management

### Core Features
- **vessels.cy.ts**: Vessel CRUD operations, search, filtering, validation
- **docks.cy.ts**: Dock management, assignments, capacity information
- **storage-areas.cy.ts**: Storage area management, allocation status
- **vvns.cy.ts**: VVN creation, submission, approval/rejection workflow
- **physical-resources.cy.ts**: Resource management, availability tracking
- **shipping-agents.cy.ts**: Organization management, representative handling

### Admin Features
- **user-management.cy.ts**: User activation, role assignment, status management

### UI & UX
- **navigation.cy.ts**: Routing, menu navigation, browser navigation
- **visualization.cy.ts**: 3D port visualization, camera controls, object interaction

## Configuration

### Base URL

The base URL is configured in `cypress.config.ts`:

```typescript
baseUrl: 'http://localhost:4200'
```

### API URL

The API URL is configured as an environment variable:

```typescript
env: {
  apiUrl: 'http://localhost:5174/api'
}
```

### Timeouts

- **Command Timeout**: 10 seconds
- **Request Timeout**: 10 seconds
- **Response Timeout**: 30 seconds

### Viewport

Default viewport: 1280x720

## Best Practices

1. **Use Admin Login**: Always use `cy.loginAsAdmin()` in `beforeEach()` for protected routes
2. **Wait for API Calls**: Use `cy.intercept()` and `cy.wait()` for API-dependent tests
3. **Handle Dynamic Data**: Check for data existence before testing CRUD operations
4. **Graceful Degradation**: Tests handle empty states and missing data
5. **Error Handling**: All tests include error scenario coverage

## Debugging Tests

### View Test Execution

Run Cypress in headed mode to see tests execute:

```bash
npx cypress run --headed
```

### Debug Specific Test

Run a single test file:

```bash
npx cypress run --spec "cypress/e2e/vessels.cy.ts"
```

### Enable Debug Logs

Set environment variable for verbose logging:

```bash
DEBUG=cypress:* npm run cypress:run
```

## Troubleshooting

### Tests Fail on Login

1. Verify backend is running on `http://localhost:5174`
2. Check admin credentials in database
3. Ensure database is seeded with admin user

### API Timeouts

1. Increase timeout in `cypress.config.ts`
2. Check backend response times
3. Verify network connectivity

### Element Not Found

1. Update selectors to match current implementation
2. Check if elements are hidden or disabled
3. Add appropriate waits for dynamic content

## CI/CD Integration

Example GitHub Actions workflow:

```yaml
- name: Run E2E Tests
  run: |
    npm install
    npm run e2e
  env:
    CYPRESS_adminEmail: admin@portsystem.com
    CYPRESS_adminPassword: Admin@123
```

## Reporting

Test results are displayed in the terminal. Screenshots of failures are saved to:

```
cypress/screenshots/
```

Videos are disabled by default but can be enabled in `cypress.config.ts`:

```typescript
video: true
```

## Contributing

When adding new tests:

1. Follow existing naming conventions
2. Use descriptive test names
3. Group related tests in describe blocks
4. Add comments for complex interactions
5. Handle both success and error scenarios
6. Clean up test data when possible

## Support

For issues or questions about the tests, please refer to:
- [Cypress Documentation](https://docs.cypress.io)
- Project documentation in `/Docs`
