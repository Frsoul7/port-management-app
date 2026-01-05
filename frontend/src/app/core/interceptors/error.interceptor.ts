import { HttpInterceptorFn, HttpErrorResponse } from '@angular/common/http';
import { inject } from '@angular/core';
import { Router } from '@angular/router';
import { catchError } from 'rxjs/operators';
import { throwError } from 'rxjs';
import { ErrorService } from '../services/error.service';
import { AuthService } from '../services/auth.service';

/**
 * Error Interceptor (Functional Style - Angular 20)
 * Global error handling with user-friendly messages (US 3.1.4)
 * Handles 401 (authentication) and 403 (authorization) errors specially
 */
export const errorInterceptor: HttpInterceptorFn = (req, next) => {
  const errorService = inject(ErrorService);
  const authService = inject(AuthService);
  const router = inject(Router);

  return next(req).pipe(
    catchError((error: HttpErrorResponse) => {
      // Handle 401 Unauthorized - Authentication required or token expired
      if (error.status === 401) {
        console.error('401 Unauthorized - Token expired or invalid');
        
        // Don't logout on login/register failures - these are expected failed authentication attempts
        const isLoginAttempt = req.url.includes('/login') || req.url.includes('/register');
        
        // If this is not already a refresh request, the auth interceptor will handle it
        // If refresh fails, logout and redirect
        if (req.url.includes('/refresh')) {
          console.log('Token refresh failed, clearing session and redirecting to login');
          authService.logout();
          router.navigate(['/login'], {
            queryParams: { returnUrl: router.url, reason: 'session-expired' }
          });
          
          errorService.handleHttpError({
            ...error,
            error: { 
              message: 'Your session has expired. Please login again.' 
            }
          } as HttpErrorResponse);
        } else if (!isLoginAttempt) {
          // For other 401 errors (not login/register), this means the user needs to authenticate
          console.log('Authentication required, redirecting to login');
          authService.logout();
          router.navigate(['/login'], {
            queryParams: { returnUrl: router.url }
          });
        }
        // If it's a login attempt, let the component handle the error display
      }
      
      // Handle 403 Forbidden - User doesn't have permission
      else if (error.status === 403) {
        console.error('403 Forbidden - Insufficient permissions');
        router.navigate(['/access-denied'], {
          queryParams: { 
            returnUrl: router.url,
            resource: req.url 
          }
        });
        
        errorService.handleHttpError({
          ...error,
          error: { 
            message: 'You do not have permission to access this resource.' 
            }
        } as HttpErrorResponse);
      }
      
      // Handle all other errors
      else {
        errorService.handleHttpError(error);
      }
      
      // Re-throw for component-level handling if needed
      return throwError(() => error);
    })
  );
};
