import { HttpInterceptorFn, HttpErrorResponse } from '@angular/common/http';
import { inject } from '@angular/core';
import { AuthService } from '../services/auth.service';
import { catchError, switchMap, throwError } from 'rxjs';

/**
 * Auth Interceptor (Functional Style - Angular 20)
 * Automatically adds auth headers and organization context to all HTTP requests
 * Handles 401 errors by attempting token refresh
 */
export const authInterceptor: HttpInterceptorFn = (req, next) => {
  const authService = inject(AuthService);
  const token = authService.getToken();

  // Skip auth header for public endpoints
  if (req.url.includes('/Authentication/') || 
      (req.url.includes('/Organizations') && req.method === 'GET' && !req.url.match(/\/Organizations\/[^/]+$/))) {
    return next(req);
  }

  if (token) {
    // Get organization context headers
    const orgHeaders = authService.getOrganizationHeaders();
    
    // Clone request and add all headers
    let headers = req.headers.set('Authorization', `Bearer ${token}`);
    
    // Add organization context headers (X-Org-Id, X-User-Id, X-Role)
    Object.keys(orgHeaders).forEach(key => {
      headers = headers.set(key, orgHeaders[key]);
    });
    
    const clonedRequest = req.clone({ headers });
    
    // Handle 401 errors with token refresh
    return next(clonedRequest).pipe(
      catchError((error: HttpErrorResponse) => {
        // If 401 and not already a refresh request, try to refresh token
        if (error.status === 401 && !req.url.includes('/refresh')) {
          console.log('401 error detected, attempting token refresh');
          
          return authService.refreshToken().pipe(
            switchMap(() => {
              // Retry the original request with new token
              const newToken = authService.getToken();
              if (newToken) {
                const retryHeaders = req.headers.set('Authorization', `Bearer ${newToken}`);
                Object.keys(orgHeaders).forEach(key => {
                  retryHeaders.set(key, orgHeaders[key]);
                });
                const retryRequest = req.clone({ headers: retryHeaders });
                return next(retryRequest);
              }
              return throwError(() => error);
            }),
            catchError((refreshError) => {
              // Token refresh failed, user needs to login again
              console.error('Token refresh failed, logging out');
              authService.logout();
              return throwError(() => refreshError);
            })
          );
        }
        
        return throwError(() => error);
      })
    );
  }

  return next(req);
};