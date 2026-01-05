import { inject } from '@angular/core';
import { Router, CanActivateFn } from '@angular/router';
import { AuthService } from '../services/auth.service';
import { UserRole } from '../models/user.model';

/**
 * Role Guard Factory - Creates a guard that checks if the user has any of the allowed roles
 * @param allowedRoles Array of roles that are allowed to access the route
 * @returns A CanActivateFn that can be used in route configuration
 */
export function roleGuard(allowedRoles: UserRole[]): CanActivateFn {
  const guardFn: CanActivateFn = (route, state) => {
    const authService = inject(AuthService);
    const router = inject(Router);

    if (!authService.isAuthenticated()) {
      // Store the intended URL for redirecting after login
      sessionStorage.setItem('redirectUrl', state.url);
      router.navigate(['/login']);
      return false;
    }

    if (authService.hasAnyRole(allowedRoles)) {
      return true;
    }

    // Redirect to access denied page
    router.navigate(['/access-denied']);
    return false;
  };
  
  return guardFn;
}
