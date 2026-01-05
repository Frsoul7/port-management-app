import { HttpInterceptorFn } from '@angular/common/http';
import { inject } from '@angular/core';
import { finalize } from 'rxjs/operators';
import { LoadingService } from '../services/loading.service';

/**
 * Loading Interceptor (Functional Style - Angular 20)
 * Tracks HTTP requests to show/hide loading indicators
 */
export const loadingInterceptor: HttpInterceptorFn = (req, next) => {
  const loadingService: LoadingService = inject(LoadingService);

  // Increment active requests
  loadingService.show();

  return next(req).pipe(
    finalize(() => {
      // Decrement active requests
      loadingService.hide();
    })
  );
};
