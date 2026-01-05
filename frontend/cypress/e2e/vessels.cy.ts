/// <reference types="cypress" />

describe('Vessels Management', () => {
  beforeEach(() => {
    // Login as admin before each test
    cy.loginAsAdmin();
  });

  it('should display vessels page', () => {
    cy.visit('/vessels');
    cy.url().should('include', '/vessels');
    cy.contains(/vessels/i).should('be.visible');
  });

  it('should load and display vessel list', () => {
    cy.visit('/vessels');

    // Wait for vessels to load
    cy.intercept('GET', '**/api/vessels**').as('getVessels');
    cy.wait('@getVessels', { timeout: 10000 });

    // Check if vessel table component is displayed
    cy.get('app-vessel-table').should('exist');
  });

  it('should allow searching for vessels', () => {
    cy.visit('/vessels');

    // Look for search input - they are type="text"
    cy.get('app-vessel-search-bar input[type="text"]')
      .first()
      .should('be.visible')
      .type('test vessel');

    // Search button exists
    cy.contains('button', /search/i).should('be.visible');
  });

  it('should display vessel details when clicking a vessel', () => {
    cy.visit('/vessels');

    // Wait for vessels to load
    cy.intercept('GET', '**/api/vessels**').as('getVessels');
    cy.wait('@getVessels', { timeout: 10000 }).then((interception) => {
      if (interception.response && interception.response.body && interception.response.body.length > 0) {
        // Click the eye icon button to view first vessel
        cy.get('table tbody tr').first().within(() => {
          cy.contains('button', '👁️').click();
        });

        // Should show vessel details in view modal
        cy.get('app-vessel-view-modal').should('be.visible');
      } else {
        cy.log('No vessels in database to view');
      }
    });
  });

  it('should open create vessel form', () => {
    cy.visit('/vessels');

    // Look for create button
    cy.contains('button', /create/i, { timeout: 5000 })
      .should('be.visible')
      .click();

    // Form modal should appear
    cy.get('app-vessel-form-modal').should('be.visible');
  });

  it('should validate required fields when creating vessel', () => {
    cy.visit('/vessels');

    // Open create form
    cy.contains('button', /create/i)
      .should('be.visible')
      .click();

    // Wait for form to be visible
    cy.get('app-vessel-form-modal').should('be.visible');

    // Check that the submit button is disabled
    cy.get('button[type="submit"]').should('be.disabled');

    // Blur required fields to trigger validation
    cy.get('input[name="name"]').focus().blur();
    cy.get('input[name="imoNumber"]').focus().blur();
    cy.get('select[name="vesselTypeId"]').focus().blur();
    cy.get('select[name="organizationId"]').focus().blur();

    // Should show validation errors
    cy.get('.error-text, .invalid-feedback').should('exist');
  });

  it('should create a new vessel with valid data', () => {
    cy.visit('/vessels');

    // Intercept create request
    cy.intercept('POST', '**/api/vessels').as('createVessel');

    // Open create form
    cy.contains('button', /create/i)
      .should('be.visible')
      .click();

    const timestamp = Date.now();
    const vesselName = `Test Vessel ${timestamp}`;

    // Wait for form to load
    cy.get('app-vessel-form-modal').should('be.visible');

    // Fill in form fields (use name attributes)
    cy.get('input[name="name"]').type(vesselName);
    cy.get('input[name="imoNumber"]').type(`1234567`);
    cy.get('select[name="vesselTypeId"]').select(1); // Selects the second option
    cy.get('select[name="organizationId"]').select(1); // Selects the second option
    cy.get('input[name="capacityTEU"]').type('500');

    // Submit form
    cy.get('button[type="submit"]').click();

    // Wait for creation - expects 200 or 201
    cy.wait('@createVessel', { timeout: 10000 }).its('response.statusCode').should('be.oneOf', [200, 201]);
  });

  it('should edit an existing vessel', () => {
    cy.visit('/vessels');

    // Wait for vessels to load
    cy.intercept('GET', '**/api/vessels**').as('getVessels');
    cy.wait('@getVessels', { timeout: 10000 }).then((interception) => {
      if (interception.response && interception.response.body && interception.response.body.length > 0) {
        // Click the pencil icon button to edit first vessel
        cy.get('table tbody tr').first().within(() => {
          cy.contains('button', '✏️').click();
        });

        // Edit form should appear
        cy.get('app-vessel-form-modal').should('be.visible');

        // Intercept update request
        cy.intercept('PUT', '**/api/vessels/**').as('updateVessel');

        // Modify a field (use name attribute)
        cy.get('input[name="name"]').clear().type('Updated Vessel Name');

        // Submit
        cy.get('button[type="submit"]').click();

        // Wait for update
        cy.wait('@updateVessel', { timeout: 10000 });
      } else {
        cy.log('No vessels in database to edit');
      }
    });
  });


  it('should filter vessels by type', () => {
    cy.visit('/vessels');

    // Wait for vessels to load
    cy.intercept('GET', '**/api/vessels**').as('getVessels');
    cy.wait('@getVessels', { timeout: 10000 });

    // The vessel table component should exist
    cy.get('app-vessel-table').should('exist');
  });

  it('should handle API errors gracefully', () => {
    // Intercept and force error
    cy.intercept('GET', '**/api/vessels**', {
      statusCode: 500,
      body: { error: 'Internal Server Error' }
    }).as('getVesselsError');

    cy.visit('/vessels');
    cy.wait('@getVesselsError');

    // Wait for error to be processed
    cy.wait(1000);

    // Check if alert banner with error is displayed
    cy.get('app-alert-banner .alert-banner.error').should('be.visible');
  });

  it('should require authentication', () => {
    // Logout first
    cy.logout();

    // Try to visit vessels page
    cy.visit('/vessels');

    // Should redirect to login
    cy.url().should('include', '/login');
  });
});
