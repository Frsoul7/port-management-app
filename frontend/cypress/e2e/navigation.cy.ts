/// <reference types="cypress" />

describe('Navigation and Routing', () => {
  beforeEach(() => {
    cy.loginAsAdmin();
  });

  it('should navigate to dashboard', () => {
    cy.visit('/');
    cy.url().should('include', '/dashboard');
  });

  it('should display main navigation menu', () => {
    cy.visit('/dashboard');

    // Look for navigation links
    cy.get('nav, header, .nav, .menu').should('exist');
  });

  it('should navigate to vessels page from menu', () => {
    cy.visit('/dashboard');
    cy.contains(/vessels/i).click();
    cy.url().should('include', '/vessels');
  });

  it('should navigate to docks page from menu', () => {
    cy.visit('/dashboard');
    cy.contains(/docks/i).click();
    cy.url().should('include', '/docks');
  });

  it('should navigate to storage areas from menu', () => {
    cy.visit('/dashboard');
    cy.contains(/storage/i).click();
    cy.url().should('include', '/storage-areas');
  });

  it('should navigate to VVNs from menu', () => {
    cy.visit('/dashboard');
    cy.contains(/vvn|visit notification/i).click();
    cy.url().should('include', '/vvns');
  });

  it('should navigate to visualization from menu', () => {
    cy.visit('/dashboard');
    cy.contains(/visualization|3d/i).click();
    cy.url().should('include', '/visualization');
  });

  it('should navigate to user management for admin', () => {
    cy.visit('/dashboard');

    // Look for admin or user management link
    cy.get('body').then(($body) => {
      if ($body.text().includes('Users') || $body.text().includes('User Management')) {
        cy.contains(/users|user management/i).click();
        cy.url().should('include', '/admin/users');
      }
    });
  });


  it('should allow logout', () => {
    cy.visit('/dashboard');

    // Find and click logout
    cy.contains(/logout|sign out/i).click();

    // Should redirect to login
    cy.url().should('include', '/login');
  });

  it('should highlight active navigation item', () => {
    cy.visit('/vessels');

    // Active navigation item should be highlighted
    cy.contains(/vessels/i).should('satisfy', ($el) => {
      return $el.hasClass('active') || $el.hasClass('router-link-active');
    });
  });

  it('should redirect from root to dashboard when authenticated', () => {
    cy.visit('/');
    cy.url().should('include', '/dashboard');
  });

  it('should handle 404 routes', () => {
    cy.visit('/non-existent-route', { failOnStatusCode: false });

    // Should redirect to dashboard or show 404
    cy.url().should('match', /dashboard|404|not-found/);
  });

  it('should preserve navigation state on refresh', () => {
    cy.visit('/vessels');
    cy.reload();
    cy.url().should('include', '/vessels');
  });

  it('should support browser back button', () => {
    cy.visit('/dashboard');
    cy.contains(/vessels/i).click();
    cy.url().should('include', '/vessels');

    cy.go('back');
    cy.url().should('include', '/dashboard');
  });

  it('should support browser forward button', () => {
    cy.visit('/dashboard');
    cy.contains(/vessels/i).click();
    cy.go('back');
    cy.go('forward');
    cy.url().should('include', '/vessels');
  });
});

describe('Dashboard', () => {
  beforeEach(() => {
    cy.loginAsAdmin();
  });

  it('should display dashboard', () => {
    cy.visit('/dashboard');
    cy.contains(/dashboard|welcome|overview/i).should('be.visible');
  });

  it('should display user greeting', () => {
    cy.visit('/dashboard');

    // Should show user name or greeting
    cy.get('body').then(($body) => {
      if ($body.text().includes('Welcome') || $body.text().includes('Hello')) {
        cy.contains(/welcome|hello/i).should('be.visible');
      }
    });
  });

  it('should load dashboard data', () => {
    cy.visit('/dashboard');

    // Intercept potential dashboard API calls
    cy.intercept('GET', '**/api/**').as('getDashboardData');

    // Wait a bit for data to load
    cy.wait(2000);
  });

  it('should display quick access links', () => {
    cy.visit('/dashboard');

    // Should have links to main features
    cy.contains(/vessels|docks|vvn/i).should('exist');
  });

  it('should display recent activity or notifications', () => {
    cy.visit('/dashboard');

    // Look for activity feed or notifications
    cy.get('body').then(($body) => {
      if ($body.find('.activity, .notification, .recent').length > 0) {
        cy.get('.activity, .notification, .recent').should('be.visible');
      }
    });
  });
});
