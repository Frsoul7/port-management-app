import { inject } from '@angular/core';
import { Router, CanActivateFn } from '@angular/router';
import { AuthService } from '../services/auth.service';

/**
 * Admin Guard - Ensures only administrators can access admin routes
 */
export const adminGuard: CanActivateFn = (route, state) => {
  const authService = inject(AuthService);
  const router = inject(Router);

  // Check if user is authenticated
  if (!authService.isAuthenticated()) {
    // Store the intended URL for redirecting after login
    sessionStorage.setItem('redirectUrl', state.url);
    router.navigate(['/login']);
    return false;
  }

  // Check if user has administrator role
  const currentUser = authService.getCurrentUser();
  if (currentUser && currentUser.role === 'ADMINISTRATOR') {
    return true;
  }

  // Not an administrator, redirect to access denied
  router.navigate(['/access-denied']);
  return false;
};
