import { Injectable } from '@angular/core';
import { Observable, of, delay } from 'rxjs';
import { AuthToken, User, UserRole } from '../models/user.model';

@Injectable({
  providedIn: 'root'
})
export class MockAuthService {
  /**
   * Mock token exchange - use this temporarily if backend isn't ready
   */
  mockTokenExchange(code: string): Observable<AuthToken> {
    console.log('MOCK: Exchanging code for token');
    
    const mockToken: AuthToken = {
      access_token: this.generateMockToken(),
      refresh_token: 'mock_refresh_token_' + Date.now(),
      expires_in: 3600,
      token_type: 'Bearer'
    };
    
    return of(mockToken).pipe(delay(500));
  }

  /**
   * Mock user profile - use this temporarily if backend isn't ready
   */
  mockUserProfile(): Observable<User> {
    console.log('MOCK: Fetching user profile');
    
    const mockUser: User = {
      id: 'mock_user_123',
      username: 'test.user',
      email: 'test@example.com',
      fullName: 'Test User',
      role: UserRole.PORT_AUTHORITY_OFFICER,
      organizationId: 'org_001'
    };
    
    return of(mockUser).pipe(delay(300));
  }

  private generateMockToken(): string {
    const header = btoa(JSON.stringify({ alg: 'HS256', typ: 'JWT' }));
    const payload = btoa(JSON.stringify({
      sub: 'mock_user_123',
      exp: Math.floor(Date.now() / 1000) + 3600,
      iat: Math.floor(Date.now() / 1000)
    }));
    const signature = 'mock_signature';
    
    return `${header}.${payload}.${signature}`;
  }
}
