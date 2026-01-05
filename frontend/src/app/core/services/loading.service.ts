import { Injectable, signal } from '@angular/core';

/**
 * Loading service to track HTTP requests
 * Used by loading interceptor
 */
@Injectable({
  providedIn: 'root'
})
export class LoadingService {
  private activeRequests = signal(0);
  public readonly isLoading = signal(false);

  show(): void {
    this.activeRequests.update(count => count + 1);
    this.isLoading.set(true);
  }

  hide(): void {
    this.activeRequests.update(count => Math.max(0, count - 1));
    
    // Only hide if no active requests
    if (this.activeRequests() === 0) {
      this.isLoading.set(false);
    }
  }
}
