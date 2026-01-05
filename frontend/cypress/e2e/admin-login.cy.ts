/// <reference types="cypress" />

describe('Admin Login', () => {
  beforeEach(() => {
    // Clear any existing session
    cy.clearLocalStorage();
    cy.clearCookies();
    cy.clearAllSessionStorage();
  });

  it('should display admin login page', () => {
    cy.visit('/admin/login');
    cy.contains('Administrator Login').should('be.visible');
    cy.contains('System administrators only').should('be.visible');
    cy.get('input[name="email"]').should('be.visible');
    cy.get('input[name="password"]').should('be.visible');
    cy.get('button[type="submit"]').should('be.visible');
  });

  it('should show validation errors for empty fields', () => {
    cy.visit('/admin/login');
    cy.get('button[type="submit"]').should('be.disabled');
  });

  it('should show validation error for invalid email', () => {
    cy.visit('/admin/login');
    cy.get('input[name="email"]').type('invalid-email');
    cy.get('input[name="password"]').type('Password123');
    cy.get('button[type="submit"]').should('be.disabled');
  });

  it('should show validation error for short password', () => {
    cy.visit('/admin/login');
    cy.get('input[name="email"]').type('admin@test.com');
    cy.get('input[name="password"]').type('short');
    cy.get('button[type="submit"]').should('be.disabled');
  });

  it('should successfully login with valid admin credentials', () => {
    cy.intercept('POST', '**/Authentication/admin/login').as('adminLogin');

    cy.visit('/admin/login');
    cy.get('input[name="email"]').type('admin@portsystem.com');
    cy.get('input[name="password"]').type('Admin@123');
    cy.get('button[type="submit"]').click();

    // Wait for login request
    cy.wait('@adminLogin').then((interception) => {
      expect(interception.response?.statusCode).to.equal(200);
    });

    // Should redirect to dashboard
    cy.url().should('include', '/dashboard', { timeout: 10000 });

    // Should store auth token (check both possible keys)
    cy.window().then((win) => {
      const token = win.localStorage.getItem('access_token') || win.localStorage.getItem('token');
      expect(token).to.not.be.null;
      if (token) {
        expect(token.length).to.be.greaterThan(0);
      }
    });
  });

  it('should show error for invalid credentials', () => {
    cy.intercept('POST', '**/Authentication/admin/login').as('adminLogin');

    cy.visit('/admin/login');
    cy.get('input[name="email"]').type('wrong@email.com');
    cy.get('input[name="password"]').type('WrongPassword123');
    cy.get('button[type="submit"]').click();

    // Wait for login request
    cy.wait('@adminLogin').then((interception) => {
      expect(interception.response?.statusCode).to.not.equal(200);
    });

    // Should show error message (either in modal or on page)
    cy.contains(/invalid credentials|login failed/i, { timeout: 5000 }).should('exist');
  });

  it('should navigate to Google login page', () => {
    cy.visit('/admin/login');
    cy.contains('Back to Google Login').click();
    cy.url().should('not.include', '/admin/login');
  });

  it('should redirect to dashboard if already logged in', () => {
    // First login
    cy.loginAsAdmin();

    // Try to visit login page
    cy.visit('/admin/login');

    // Should redirect to dashboard
    cy.url().should('include', '/dashboard');
  });

  it('should disable form while submitting', () => {
    cy.intercept('POST', '**/Authentication/admin/login', (req) => {
      req.on('response', (res) => {
        res.setDelay(1000); // Add delay to verify disabled state
      });
    }).as('adminLoginDelayed');

    cy.visit('/admin/login');
    cy.get('input[name="email"]').type(Cypress.env('adminEmail'));
    cy.get('input[name="password"]').type(Cypress.env('adminPassword'));

    cy.get('button[type="submit"]').click();

    // Check disabled state immediately after click
    cy.get('button[type="submit"]').should('be.disabled');
    cy.get('input[name="email"]').should('be.disabled');

    // Wait for login to complete
    cy.wait('@adminLoginDelayed');

    // Should redirect to dashboard after successful login
    cy.url().should('include', '/dashboard', { timeout: 10000 });
  });
});
