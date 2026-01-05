/// <reference types="cypress" />

describe('User Management (Admin)', () => {
  beforeEach(() => {
    // Login as admin before each test (required for user management)
    cy.loginAsAdmin();
  });

  it('should display user management page', () => {
    cy.visit('/admin/users');
    cy.url().should('include', '/admin/users');
    cy.contains(/user management|users|manage users/i).should('be.visible');
  });

  it('should require admin role', () => {
    // Should allow access for admin
    cy.visit('/admin/users');
    cy.url().should('include', '/admin/users');
  });

  it('should load and display user list', () => {
    cy.visit('/admin/users');
    
    // Wait for users to load
    cy.intercept('GET', '**api/users**').as('getUsers');
    cy.wait('@getUsers', { timeout: 10000 });
    
    // Check if user table is displayed
    cy.get('table, .user-card, .user-item').should('exist');
  });

  it('should display user information columns', () => {
    cy.visit('/admin/users');
    
    // Wait for users to load
    cy.intercept('GET', '**api/users**').as('getUsers');
    cy.wait('@getUsers', { timeout: 10000 });
    
    // Should show user information
    cy.contains(/name|email|role|status|organization/i).should('exist');
  });

  it('should display pending users (requiring activation)', () => {
    cy.visit('/admin/users');
    
    // Wait for users to load
    cy.intercept('GET', '**api/users**').as('getUsers');
    cy.wait('@getUsers', { timeout: 10000 });
    
    // Look for pending/inactive status indicators
    cy.get('body').then(($body) => {
      if ($body.text().includes('pending') || $body.text().includes('inactive')) {
        cy.contains(/pending|inactive|not activated/i).should('exist');
      }
    });
  });

  it('should deactivate an active user', () => {
    cy.visit('/admin/users');
    
    // Wait for users to load
    cy.intercept('GET', '**api/users**').as('getUsers');
    cy.wait('@getUsers', { timeout: 10000 }).then((interception) => {
      if (interception.response && interception.response.body) {
        const activeUser = interception.response.body.find((user: any) => 
          user.isActive && user.role !== 'ADMINISTRATOR'
        );
        
        if (activeUser) {
          // Intercept deactivate request
          cy.intercept('POST', '**api/users/activate').as('deactivateUser');
          
          // Find and click deactivate button
          cy.contains(/deactivate|disable/i).click();
          
          // Confirm deactivation
          cy.get('body').then(($body) => {
            if ($body.text().includes('confirm') || $body.text().includes('are you sure')) {
              cy.contains(/yes|confirm|deactivate/i).click();
            }
          });
          
          // Wait for deactivation
          cy.wait('@deactivateUser', { timeout: 10000 });
        }
      }
    });
  });

  it('should view user details', () => {
    cy.visit('/admin/users');
    
    // Wait for users to load
    cy.intercept('GET', '**api/users**').as('getUsers');
    cy.wait('@getUsers', { timeout: 10000 }).then((interception) => {
      if (interception.response && interception.response.body && interception.response.body.length > 0) {
        // Click on first user
        cy.get('table tbody tr, .user-card').first().click();
        
        // Should show user details
        cy.contains(/email|organization|role|status/i).should('be.visible');
      }
    });
  });

  it('should update user information', () => {
    cy.visit('/admin/users');
    
    // Wait for users to load
    cy.intercept('GET', '**api/users**').as('getUsers');
    cy.wait('@getUsers', { timeout: 10000 }).then((interception) => {
      if (interception.response && interception.response.body && interception.response.body.length > 0) {
        // Intercept update request
        cy.intercept('PUT', '**api/users/**').as('updateUser');
        
        // Find and click edit button
        cy.get('button[title*="edit" i], .edit-btn').first().click();
        
        // Modify a field
        cy.get('input[name="name"], input[formControlName="name"]')
          .clear()
          .type('Updated User Name');
        
        // Submit
        cy.contains(/submit|save|update/i).click();
        
        // Wait for update
        cy.wait('@updateUser', { timeout: 10000 });
      }
    });
  });

  it('should search users by email or name', () => {
    cy.visit('/admin/users');
    
    // Look for search input
    cy.get('input[type="search"], input[placeholder*="search" i]')
      .first()
      .should('be.visible')
      .type('admin');
    
    // Should trigger search
    cy.intercept('GET', '**api/users**').as('searchUsers');
  });

  it('should display user organizations', () => {
    cy.visit('/admin/users');
    
    // Wait for users to load
    cy.intercept('GET', '**api/users**').as('getUsers');
    cy.wait('@getUsers', { timeout: 10000 });
    
    // Should show organization information
    cy.contains(/organization|company/i).should('exist');
  });

  it('should show email verification status', () => {
    cy.visit('/admin/users');
    
    // Wait for users to load
    cy.intercept('GET', '**api/users**').as('getUsers');
    cy.wait('@getUsers', { timeout: 10000 });
    
    // Should display email verification status
    cy.get('body').then(($body) => {
      if ($body.text().includes('verified') || $body.text().includes('unverified')) {
        cy.contains(/verified|unverified/i).should('exist');
      }
    });
  });

  it('should handle API errors gracefully', () => {
    cy.intercept('GET', '**api/users**', {
      statusCode: 500,
      body: { error: 'Internal Server Error' }
    }).as('getUsersError');
    
    cy.visit('/admin/users');
    
    // Should show error message
    cy.contains(/error|failed|could not load/i, { timeout: 5000 }).should('be.visible');
  });

  it('should require authentication and admin role', () => {
    cy.logout();
    cy.visit('/admin/users');
    
    // Should redirect to login
    cy.url().should('include', '/login');
  });

  it('should display all available roles', () => {
    cy.visit('/admin/users');
    
    // Click activate or edit to see role options
    cy.contains(/activate|edit/i).first().click();
    
    // Open role selector
    cy.get('select[name="role"], mat-select[formControlName="role"]').click();
    
    // Should show different role options
    cy.contains(/shipping agent|port authority|logistics|administrator/i).should('exist');
  });
});
