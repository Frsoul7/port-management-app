/// <reference types="cypress" />

describe('Storage Areas Management', () => {
  beforeEach(() => {
    // Login as admin before each test (admin has access to storage areas)
    cy.loginAsAdmin();
  });

  it('should display storage areas page', () => {
    cy.visit('/storage-areas');
    cy.url().should('include', '/storage-areas');
    cy.contains(/storage areas|storage area management/i).should('be.visible');
  });

  it('should load and display storage area list', () => {
    cy.visit('/storage-areas');

    // Wait for storage areas to load
    cy.intercept('GET', '**/api/StorageAreas**').as('getStorageAreas');
    cy.wait('@getStorageAreas', { timeout: 10000 });

    // Check if storage area table component is displayed
    cy.get('app-storage-area-table').should('exist');
  });

  it('should require proper authorization', () => {
    cy.visit('/storage-areas');

    // Admin should have access
    cy.url().should('include', '/storage-areas');
    cy.contains(/storage/i).should('be.visible');
  });

  it('should open create storage area form', () => {
    cy.visit('/storage-areas');

    // Look for create button
    cy.contains('button', /create/i, { timeout: 5000 })
      .should('be.visible')
      .click();

    // Form modal should appear
    cy.get('app-storage-area-form-modal').should('be.visible');
  });


  it('should create a new storage area', () => {
    cy.visit('/storage-areas');

    // Intercept create request
    cy.intercept('POST', '**/api/StorageAreas').as('createStorageArea');

    // Open create form
    cy.contains(/add storage|create storage|new storage|\+ storage/i)
      .should('be.visible')
      .click();

    const timestamp = Date.now();

    // Fill in form fields (use correct names)
    cy.get('input[name="name"]').type(`Storage Area ${timestamp}`);
    cy.get('input[name="location"]').type('Zone A');
    cy.get('input[name="maxCapacityTEU"]').type('1000');
    cy.get('select[name="type"]').select(1); // Selects the second option

    // Submit
    cy.get('button[type="submit"]').click();

    // Wait for creation
    cy.wait('@createStorageArea', { timeout: 10000 }).its('response.statusCode').should('be.oneOf', [200, 201]);

    // Should show success message
    cy.contains(/success|created/i, { timeout: 5000 }).should('exist');
  });

  it('should display storage area details', () => {
    cy.visit('/storage-areas');

    // Wait for storage areas to load
    cy.intercept('GET', '**/api/StorageAreas**').as('getStorageAreas');
    cy.wait('@getStorageAreas', { timeout: 10000 }).then((interception) => {
      if (interception.response && interception.response.body && interception.response.body.length > 0) {
        // Click the eye icon button to view first storage area
        cy.get('table tbody tr').first().within(() => {
          cy.get('.btn-view').click();
        });

        // Should show storage area details modal
        cy.get('app-storage-area-view-modal, .modal-view').should('be.visible');
      } else {
        cy.log('No storage areas in database');
      }
    });
  });



  it('should search for storage areas', () => {
    cy.visit('/storage-areas');

    // Look for search input - type="text"
    cy.get('app-storage-area-search-bar input[type="text"]')
      .first()
      .should('be.visible')
      .type('SA-');

    // Search button exists
    cy.contains('button', /search/i).should('be.visible');
  });

  it('should filter storage areas by type or zone', () => {
    cy.visit('/storage-areas');

    // Look for filter dropdown
    cy.get('select, mat-select').first().then(($el) => {
      if ($el.is('select')) {
        cy.wrap($el).select(1);
      } else {
        cy.wrap($el).click();
        cy.get('mat-option').first().click();
      }
    });
  });

  
  it('should require authentication', () => {
    cy.logout();
    cy.visit('/storage-areas');
    cy.url().should('include', '/login');
  });

  
});
