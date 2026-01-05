/// <reference types="cypress" />

// ***********************************************
// Custom Cypress Commands
// ***********************************************

declare global {
  namespace Cypress {
    interface Chainable {
      /**
       * Custom command to login as admin
       * @example cy.loginAsAdmin()
       */
      loginAsAdmin(): Chainable<void>;

      /**
       * Custom command to logout
       * @example cy.logout()
       */
      logout(): Chainable<void>;

      /**
       * Custom command to check if user is authenticated
       * @example cy.checkAuthentication()
       */
      checkAuthentication(): Chainable<boolean>;

      /**
       * Custom command to get auth token from localStorage
       * @example cy.getAuthToken()
       */
      getAuthToken(): Chainable<string | null>;

      /**
       * Custom command to set auth token in localStorage
       * @example cy.setAuthToken(token)
       */
      setAuthToken(token: string): Chainable<void>;
    }
  }
}

/**
 * Login as admin using credentials from environment
 */
Cypress.Commands.add('loginAsAdmin', () => {
  cy.session('admin-session', () => {
    cy.intercept('POST', '**/Authentication/admin/login').as('adminLogin');

    cy.visit('/admin/login');
    cy.get('input[name="email"]').type(Cypress.env('adminEmail'));
    cy.get('input[name="password"]').type(Cypress.env('adminPassword'));
    cy.get('button[type="submit"]').click();

    // Wait for login request
    cy.wait('@adminLogin').then((interception) => {
      expect(interception.response?.statusCode).to.equal(200);
    });

    // Wait for redirect to dashboard
    cy.url().should('include', '/dashboard', { timeout: 10000 });

    // Verify token is stored
    cy.window().then((win) => {
      const token = win.localStorage.getItem('access_token');
      expect(token).to.exist;
    });
  });
});

/**
 * Logout current user
 */
Cypress.Commands.add('logout', () => {
  cy.window().then((win) => {
    win.localStorage.clear();
    win.sessionStorage.clear();
  });
  cy.visit('/login');
});

/**
 * Check if user is authenticated
 */
Cypress.Commands.add('checkAuthentication', () => {
  return cy.window().then((win) => {
    const token = win.localStorage.getItem('access_token');
    return !!token;
  });
});

/**
 * Get auth token from localStorage
 */
Cypress.Commands.add('getAuthToken', () => {
  cy.window().then((win) => {
    return win.localStorage.getItem('access_token');
  });
});

/**
 * Set auth token in localStorage
 */
Cypress.Commands.add('setAuthToken', (token: string) => {
  cy.window().then((win) => {
    win.localStorage.setItem('access_token', token);
  });
});

// Prevent TypeScript from reading file as legacy script
export { };
