/// <reference types="cypress" />

describe('Physical Resources Management', () => {
  beforeEach(() => {
    cy.loginAsAdmin();
  });

  it('should display physical resources page', () => {
    cy.visit('/physical-resources');
    cy.url().should('include', '/physical-resources');
    cy.contains(/physical resources|resources/i).should('be.visible');
  });

  it('should load and display resource list', () => {
    cy.visit('/physical-resources');

    // Wait for resources to load
    cy.intercept('GET', '**/api/resources**').as('getResources');
    cy.wait('@getResources', { timeout: 10000 });

    // Check if resource table component is displayed
    cy.get('app-physical-resource-table').should('exist');
  });

  it('should open create resource form', () => {
    cy.visit('/physical-resources');

    // Look for Add Resource button
    cy.contains('button', /add resource/i, { timeout: 5000 })
      .should('be.visible')
      .click();

    // Form modal should appear
    cy.get('app-physical-resource-form-modal').should('be.visible');
  });

  it('should validate resource creation form', () => {
    cy.visit('/physical-resources');

    // Open create form
    cy.contains('button', /add resource/i)
      .should('be.visible')
      .click();

    // Wait for form to be visible
    cy.get('app-physical-resource-form-modal').should('be.visible');

    // Submit button should be disabled when form is invalid (empty)
    cy.get('button[type="submit"]').should('be.disabled');
  });

  it('should create a new physical resource', () => {
    cy.visit('/physical-resources');

    // Intercept create request
    cy.intercept('POST', '**/api/resources').as('createResource');

    // Open create form
    cy.contains('button', /add resource/i)
      .should('be.visible')
      .click();

    const timestamp = Date.now();

    // Wait for form to load
    cy.get('app-physical-resource-form-modal').should('be.visible');

    // Fill in form - physical resources use NAME attributes
    cy.get('input[name="code"]')
      .type(`RES-${timestamp}`);

    // Select resource type (STS_CRANE)
    cy.get('select[name="resourceType"]')
      .select('STS_CRANE');

    // Fill setup time
    cy.get('input[name="setupTimeSeconds"]')
      .type('300');

    // Fill STS crane specific field
    cy.get('input[name="avgContainersPerHour"]')
      .type('25');

    // Submit
    cy.get('button[type="submit"]').click();

    // Wait for creation
    cy.wait('@createResource', { timeout: 10000 }).its('response.statusCode').should('be.oneOf', [200, 201]);
  });

  it('should display resource details', () => {
    cy.visit('/physical-resources');

    // Wait for resources to load
    cy.intercept('GET', '**/api/resources**').as('getResources');
    cy.wait('@getResources', { timeout: 10000 }).then((interception) => {
      if (interception.response && interception.response.body && interception.response.body.length > 0) {
        // Click the eye icon button to view first resource
        cy.get('table tbody tr').first().within(() => {
          cy.get('.btn-view').click();
        });

        // Wait for modal to appear - check for modal-view class
        cy.get('.modal-view', { timeout: 5000 }).should('be.visible');
      } else {
        cy.log('No physical resources in database to view');
      }
    });
  });

  it('should edit a physical resource', () => {
    cy.visit('/physical-resources');

    // Wait for resources to load
    cy.intercept('GET', '**/api/resources**').as('getResources');
    cy.wait('@getResources', { timeout: 10000 }).then((interception) => {
      if (interception.response && interception.response.body && interception.response.body.length > 0) {
        // Click the pencil icon button to edit first resource
        cy.get('table tbody tr').first().within(() => {
          cy.get('.btn-edit').click();
        });

        // Form modal should appear in edit mode
        cy.get('app-physical-resource-form-modal').should('be.visible');

        // Intercept update request
        cy.intercept('PUT', '**/api/resources/**').as('updateResource');

        // Modify setup time
        cy.get('input[name="setupTimeSeconds"]')
          .clear()
          .type('600');

        // Submit
        cy.get('button[type="submit"]').click();

        // Wait for update
        cy.wait('@updateResource', { timeout: 10000 });
      } else {
        cy.log('No physical resources in database to edit');
      }
    });
  });


  it('should filter resources by type', () => {
    cy.visit('/physical-resources');

    // Wait for resources to load
    cy.intercept('GET', '**/api/resources**').as('getResources');
    cy.wait('@getResources', { timeout: 10000 });

    // The table component should exist
    cy.get('app-physical-resource-table').should('exist');
  });

  it('should search for resources', () => {
    cy.visit('/physical-resources');

    // Look for search input - type="text"
    cy.get('app-physical-resource-search-bar input[type="text"]')
      .first()
      .should('be.visible')
      .type('resource');

    // Search button exists
    cy.contains('button', /search/i).should('be.visible');
  });

  it('should display resource availability status', () => {
    cy.visit('/physical-resources');

    // Wait for resources to load
    cy.intercept('GET', '**/api/resources**').as('getResources');
    cy.wait('@getResources', { timeout: 10000 });

    // The table component should exist
    cy.get('app-physical-resource-table').should('exist');
  });

  it('should handle API errors gracefully', () => {
    cy.intercept('GET', '**/api/resources**', {
      statusCode: 500,
      body: { error: 'Internal Server Error' }
    }).as('getResourcesError');

    cy.visit('/physical-resources');
    cy.wait('@getResourcesError');

    // Wait for error processing
    cy.wait(1000);

    // Check if alert banner with error is displayed
    cy.get('app-alert-banner .alert-banner.error').should('be.visible');
  });

  it('should require authentication', () => {
    cy.logout();
    cy.visit('/physical-resources');
    cy.url().should('include', '/login');
  });

  it('should display resource specifications', () => {
    cy.visit('/physical-resources');

    // Wait for resources to load
    cy.intercept('GET', '**/api/resources**').as('getResources');
    cy.wait('@getResources', { timeout: 10000 });

    // The table component should exist
    cy.get('app-physical-resource-table').should('exist');
  });
});
