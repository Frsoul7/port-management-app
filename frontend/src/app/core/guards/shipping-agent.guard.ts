import { inject } from '@angular/core';
import { Router, CanActivateFn } from '@angular/router';
import { AuthService } from '../services/auth.service';
import { UserRole } from '../models/user.model';

/**
 * Shipping Agent Guard - Allows Port Authority Officers and Administrators
 */
export const shippingAgentGuard: CanActivateFn = (route, state) => {
  const authService = inject(AuthService);
  const router = inject(Router);

  if (!authService.isAuthenticated()) {
    sessionStorage.setItem('redirectUrl', state.url);
    router.navigate(['/login']);
    return false;
  }

  const allowedRoles = [
    UserRole.PORT_AUTHORITY_OFFICER,
    UserRole.ADMINISTRATOR
  ];

  if (authService.hasAnyRole(allowedRoles)) {
    return true;
  }

  router.navigate(['/access-denied']);
  return false;
};
