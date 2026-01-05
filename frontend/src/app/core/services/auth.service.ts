import { Injectable, signal, computed, effect, Injector, inject } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Router } from '@angular/router';
import { Observable, throwError, firstValueFrom } from 'rxjs';
import { tap, catchError } from 'rxjs/operators';
import { environment } from '../../../environments/environment';
import { User, AuthToken, UserRole } from '../models/user.model';

@Injectable({
  providedIn: 'root'
})
export class AuthService {
  private http = inject(HttpClient);
  private router = inject(Router);
  private injector = inject(Injector);
  
  // Signals for reactive state management
  private currentUserSignal = signal<User | null>(null);
  private authStateReadySignal = signal<boolean>(false);
  
  // Public readonly signals
  public readonly currentUser = this.currentUserSignal.asReadonly();
  public readonly authStateReady = this.authStateReadySignal.asReadonly();
  
  // Computed signals for derived state
  public readonly isAuthenticatedSignal = computed(() => {
    const user = this.currentUser();
    if (!user) return false;
    
    const token = this.getToken();
    if (!token) return false;
    
    return !this.isTokenExpired(token);
  });
  
  public readonly userRole = computed(() => this.currentUser()?.role ?? null);
  public readonly organizationId = computed(() => this.currentUser()?.organizationId);
  public readonly isShippingAgent = computed(() => this.userRole() === UserRole.SHIPPING_AGENT_REPRESENTATIVE);
  
  private tokenRefreshTimer?: ReturnType<typeof setTimeout>;

  constructor() {
    // Effect to sync user to localStorage
    effect(() => {
      const user = this.currentUser();
      if (user) {
        localStorage.setItem('currentUser', JSON.stringify(user));
      } else {
        localStorage.removeItem('currentUser');
      }
    });
    
    this.initializeAuthState();
  }
  
  /**
   * Check if user is authenticated
   * Method version for guards and components that need imperative access
   */
  isAuthenticated(): boolean {
    return this.isAuthenticatedSignal();
  }

  private initializeAuthState(): void {
    const token = this.getToken();
    if (token && !this.isTokenExpired(token)) {
      console.log('Valid token found in localStorage');
      
      // Schedule token refresh for existing session
      this.scheduleTokenRefreshFromStorage();
      
      // Restore user from localStorage
      const storedUser = localStorage.getItem('currentUser');
      if (storedUser) {
        try {
          const user = JSON.parse(storedUser) as User;
          this.currentUserSignal.set(user);
          console.log('User restored from localStorage:', user);
        } catch (error) {
          console.error('Failed to parse stored user:', error);
        }
      } else {
        // Token exists but no user data - try to extract from token
        console.log('Token exists but no user data, extracting from JWT...');
        try {
          const user = this.extractUserFromToken(token);
          if (user) {
            this.currentUserSignal.set(user);
            console.log('User extracted from token:', user);
          }
        } catch (error) {
          console.error('Failed to extract user from token:', error);
        }
      }
    } else if (token) {
      console.log('Token expired, clearing session');
      this.clearSession();
    }
    
    // Mark auth state as ready after initialization attempt
    this.authStateReadySignal.set(true);
    console.log('Auth state initialization complete');
  }

  /**
   * Extract user information from JWT token
   */
  private extractUserFromToken(token: string): User | null {
    try {
      const payload = JSON.parse(atob(token.split('.')[1]));
      console.log('Token payload:', payload);
      
      // Extract claims from JWT
      const email = payload.email || payload['http://schemas.xmlsoap.org/ws/2005/05/identity/claims/emailaddress'] || payload.sub;
      const name = payload.name || payload['http://schemas.xmlsoap.org/ws/2005/05/identity/claims/name'] || email;
      const role = payload.role || payload['http://schemas.microsoft.com/ws/2008/06/identity/claims/role'];
      const organizationId = payload.organizationId || payload.org_id;
      const organizationName = payload.organizationName || payload.org_name;
      const userId = payload.userId || payload.sub || payload.nameid;
      const picture = payload.picture;
      
      return {
        id: userId || '',
        username: name,
        email: email,
        fullName: name,
        role: this.mapRoleFromString(role || 'UNKNOWN'),
        organizationId: organizationId,
        organizationName: organizationName,
        profilePictureUrl: picture
      };
    } catch (error) {
      console.error('Error extracting user from token:', error);
      return null;
    }
  }

  /**
   * Redirect to Google OAuth login page
   */
  login(): void {
    if (!this.validateOAuthConfig()) {
      console.error('OAuth configuration is invalid. Please check environment.ts');
      return;
    }

    const state = this.generateRandomState();
    sessionStorage.setItem('oauth_state', state);

    // Dynamically determine redirect URI based on current origin
    const redirectUri = environment.redirectUri || `${window.location.origin}/auth/callback`;

    const params = {
      client_id: environment.clientId.trim(),
      redirect_uri: redirectUri.trim(),
      response_type: 'code',
      scope: environment.scopes.trim(),
      state: state,
      access_type: 'offline',
      prompt: 'consent'
    };

    console.log('Initiating Google OAuth login...');
    console.log('Redirect URI:', params.redirect_uri);

    const queryString = Object.entries(params)
      .map(([key, value]) => `${encodeURIComponent(key)}=${encodeURIComponent(value)}`)
      .join('&');

    const authUrl = `${environment.iamUrl}?${queryString}`;
    window.location.href = authUrl;
  }

  /**
   * Generate random state for CSRF protection
   */
  private generateRandomState(): string {
    const array = new Uint8Array(32);
    crypto.getRandomValues(array);
    return Array.from(array, byte => byte.toString(16).padStart(2, '0')).join('');
  }

  /**
   * Handle Google OAuth callback and exchange code for token
   */
  handleAuthCallback(code: string, state: string): Observable<any> {
    console.log('Processing OAuth callback...');
    
    // Verify state to prevent CSRF attacks
    const savedState = sessionStorage.getItem('oauth_state');
    
    if (state !== savedState) {
      console.error('State mismatch - possible CSRF attack');
      return throwError(() => new Error('Invalid state parameter'));
    }
    sessionStorage.removeItem('oauth_state');

    // Dynamically determine redirect URI based on current origin
    const redirectUri = environment.redirectUri || `${window.location.origin}/auth/callback`;

    // Exchange authorization code for tokens via backend
    console.log('Exchanging authorization code for access token...');
    return this.http.post<any>(`${environment.apiUrl}/Authentication/google`, {
      code,
      redirect_uri: redirectUri
    }).pipe(
      tap(result => {
        console.log('Authentication result:', result);
        
        if (result.status === 'Success' && result.authResponse) {
          // User authenticated successfully
          console.log('Token received from backend');
          const token: AuthToken = {
            access_token: result.authResponse.accessToken,
            refresh_token: result.authResponse.refreshToken,
            expires_in: result.authResponse.expiresIn || environment.accessTokenExpirySeconds,
            token_type: 'Bearer'
          };
          this.setSession(token);
          
          // Create user object from auth response
          const user: User = {
            id: result.authResponse.userId || '',
            username: result.authResponse.name,
            email: result.authResponse.email,
            fullName: result.authResponse.name,
            role: this.mapRoleFromString(result.authResponse.role),
            organizationId: result.authResponse.organizationId,
            organizationName: result.authResponse.organizationName,
            profilePictureUrl: result.authResponse.profilePictureUrl
          };
          
          this.currentUserSignal.set(user);
          console.log('User profile set from auth response:', user);
        } else if (result.status === 'RequiresRegistration' && result.userInfo) {
          // User needs to complete registration
          console.log('User requires registration');
          sessionStorage.setItem('pendingRegistration', JSON.stringify(result.userInfo));
          // Component will handle redirect
        }
      }),
      catchError(error => {
        console.error('Token exchange failed:', error);
        return throwError(() => error);
      })
    );
  }

  /**
   * Admin-only login using email and password (no Google OAuth)
   */
  async loginAdmin(email: string, password: string): Promise<boolean> {
    try {
      const response = await firstValueFrom(
        this.http.post<any>(`${environment.apiUrl}/Authentication/admin/login`, {
          email,
          password
        })
      );

      if (response && response.accessToken) {
        const token: AuthToken = {
          access_token: response.accessToken,
          refresh_token: response.refreshToken,
          expires_in: response.expiresIn || environment.accessTokenExpirySeconds,
          token_type: 'Bearer'
        };
        this.setSession(token);

        // Create user object from response
        const user: User = {
          id: response.userId || '',
          username: response.name,
          email: response.email,
          fullName: response.name,
          role: this.mapRoleFromString(response.role),
          organizationId: response.organizationId,
          organizationName: response.organizationName,
          profilePictureUrl: response.profilePictureUrl
        };

        this.currentUserSignal.set(user);
        
        console.log('Admin logged in successfully');
        return true;
      }

      return false;
    } catch (error) {
      console.error('Admin login failed:', error);
      return false;
    }
  }

  /**
   * Set user session after registration
   * This method should be called after successful registration to authenticate the user
   */
  setUserFromRegistration(token: string, userInfo: any): void {
    console.log('Setting user session from registration...');
    
    // Store token
    localStorage.setItem(environment.tokenStorageKey, token);
    
    // Extract user from token or use provided info
    let user = this.extractUserFromToken(token);
    
    if (!user && userInfo) {
      // Fallback to provided user info if token extraction fails
      user = {
        id: userInfo.id || userInfo.userId || '',
        username: userInfo.username || userInfo.name,
        email: userInfo.email,
        fullName: userInfo.fullName || userInfo.name,
        role: this.mapRoleFromString(userInfo.role),
        organizationId: userInfo.organizationId,
        organizationName: userInfo.organizationName,
        profilePictureUrl: userInfo.profilePictureUrl
      };
    }
    
    if (user) {
      // Set user in state
      this.currentUserSignal.set(user);
      console.log('User authenticated after registration:', user);
    } else {
      console.error('Failed to extract user from registration response');
    }
  }

  /**
   * Load user profile from backend
   */
  private loadUserProfile(): Observable<User> {
    console.log('Fetching user profile from:', `${environment.apiUrl}/auth/profile`);
    
    return this.http.get<User>(`${environment.apiUrl}/auth/profile`).pipe(
      tap(user => {
        console.log('User profile received:', {
          id: user.id,
          username: user.username,
          email: user.email,
          role: user.role
        });
        this.currentUserSignal.set(user);
        localStorage.setItem(environment.roleStorageKey, user.role);
      }),
      catchError(error => {
        console.error('Failed to load user profile:', error);
        this.clearSession();
        return throwError(() => error);
      })
    );
  }

  /**
   * Store authentication token and set up refresh
   */
  private setSession(authToken: AuthToken): void {
    console.log('Storing authentication token...');
    localStorage.setItem(environment.tokenStorageKey, authToken.access_token);
    
    if (authToken.refresh_token) {
      localStorage.setItem('refresh_token', authToken.refresh_token);
    }

    // Store token expiry timestamp for session restoration
    const expiryTimestamp = Date.now() + (authToken.expires_in * 1000);
    localStorage.setItem('token_expires_at', expiryTimestamp.toString());

    // Set up token refresh using environment configuration
    const refreshBuffer = environment.tokenRefreshBufferSeconds || 300; // Default 5 minutes
    const expiresIn = Math.max((authToken.expires_in - refreshBuffer), 60); // Refresh at least 60 seconds before expiry
    console.log(`Token expires in ${authToken.expires_in}s, will refresh in ${expiresIn}s (${refreshBuffer}s buffer)`);
    this.scheduleTokenRefresh(expiresIn * 1000); // Convert to milliseconds
  }

  /**
   * Schedule automatic token refresh
   */
  private scheduleTokenRefresh(expiresInMs: number): void {
    if (this.tokenRefreshTimer) {
      clearTimeout(this.tokenRefreshTimer);
    }

    console.log(`Scheduling token refresh in ${expiresInMs / 1000} seconds`);

    this.tokenRefreshTimer = setTimeout(() => {
      console.log('Automatic token refresh triggered');
      this.refreshToken().subscribe({
        next: () => console.log('Token refreshed successfully'),
        error: (err) => {
          console.error('Failed to refresh token, logging out', err);
          this.logout();
        }
      });
    }, expiresInMs);
  }

  /**
   * Schedule token refresh when restoring session from localStorage
   */
  private scheduleTokenRefreshFromStorage(): void {
    const expiresAtStr = localStorage.getItem('token_expires_at');
    if (!expiresAtStr) {
      console.warn('No token expiry timestamp found, cannot schedule refresh');
      return;
    }

    const expiresAt = parseInt(expiresAtStr, 10);
    const now = Date.now();
    const remainingMs = expiresAt - now;

    if (remainingMs <= 0) {
      console.log('Token already expired, clearing session');
      this.clearSession();
      return;
    }

    // Calculate when to refresh (buffer seconds before expiry)
    const refreshBuffer = (environment.tokenRefreshBufferSeconds || 300) * 1000; // Convert to ms
    const refreshInMs = Math.max(remainingMs - refreshBuffer, 60000); // At least 60 seconds

    console.log(`Token expires in ${remainingMs / 1000}s, scheduling refresh in ${refreshInMs / 1000}s`);
    this.scheduleTokenRefresh(refreshInMs);
  }

  /**
   * Refresh access token
   */
  refreshToken(): Observable<AuthToken> {
    const refreshToken = localStorage.getItem('refresh_token');
    
    if (!refreshToken) {
      console.error('No refresh token available');
      this.logout();
      return throwError(() => new Error('No refresh token available'));
    }

    console.log('Refreshing access token...');
    
    return this.http.post<any>(`${environment.apiUrl}/Authentication/refresh`, {
      refreshToken: refreshToken
    }).pipe(
      tap(response => {
        console.log('Token refresh successful');
        const token: AuthToken = {
          access_token: response.accessToken,
          refresh_token: response.refreshToken,
          expires_in: response.expiresIn || environment.accessTokenExpirySeconds,
          token_type: 'Bearer'
        };
        this.setSession(token);
      }),
      catchError(error => {
        console.error('Token refresh failed:', error);
        this.logout();
        return throwError(() => error);
      })
    );
  }

  /**
   * Validate OAuth configuration
   */
  private validateOAuthConfig(): boolean {
    const issues: string[] = [];

    if (!environment.clientId || environment.clientId === 'YOUR_GOOGLE_CLIENT_ID.apps.googleusercontent.com') {
      issues.push('Client ID not configured');
    } else if (!environment.clientId.includes('.apps.googleusercontent.com')) {
      issues.push('Client ID format appears invalid');
    }

    // Dynamically determine redirect URI based on current origin
    const redirectUri = environment.redirectUri || `${window.location.origin}/auth/callback`;

    if (!redirectUri) {
      issues.push('Redirect URI not configured');
    } else if (!redirectUri.startsWith('http://') && !redirectUri.startsWith('https://')) {
      issues.push('Redirect URI must start with http:// or https://');
    } else if (!redirectUri.includes('/auth/callback')) {
      issues.push('Redirect URI must include /auth/callback path');
    }

    if (!environment.scopes) {
      issues.push('Scopes not configured');
    }

    if (issues.length > 0) {
      console.error('OAuth Configuration Issues:', issues);
      alert('OAuth configuration error:\n' + issues.join('\n'));
      return false;
    }

    return true;
  }


  /**
   * Check if token is expired
   */
  private isTokenExpired(token: string): boolean {
    try {
      const payload = JSON.parse(atob(token.split('.')[1]));
      const expiry = payload.exp * 1000;
      const now = Date.now();
      
      // Add 60 second buffer
      return now >= (expiry - 60000);
    } catch (error) {
      console.error('Error parsing token:', error);
      return true;
    }
  }

  /**
   * Clear session data
   */
  private clearSession(): void {
    localStorage.removeItem(environment.tokenStorageKey);
    localStorage.removeItem('refresh_token');
    localStorage.removeItem('token_expires_at');
    localStorage.removeItem(environment.roleStorageKey);
    localStorage.removeItem('currentUser');
    this.currentUserSignal.set(null);
    
    if (this.tokenRefreshTimer) {
      clearTimeout(this.tokenRefreshTimer);
    }
  }

  /**
   * Logout user and clear session
   */
  logout(): void {
    console.log('Logging out user...');
    this.clearSession();
    // Use replaceUrl to immediately replace the current route without adding to history
    // This makes logout feel instant
    this.router.navigate(['/login'], { replaceUrl: true });
  }

  /**
   * Get current access token
   */
  getToken(): string | null {
    return localStorage.getItem(environment.tokenStorageKey);
  }

  /**
   * Get current user
   */
  getCurrentUser(): User | null {
    return this.currentUser();
  }

  /**
   * Get current user role
   */
  getUserRole(): UserRole | null {
    return this.userRole();
  }

  /**
   * Get current user's organization ID
   */
  getOrganizationId(): string | undefined {
    return this.organizationId();
  }

  /**
   * Check if user belongs to a specific organization
   */
  isFromOrganization(organizationId: string): boolean {
    const userOrgId = this.organizationId();
    return userOrgId === organizationId;
  }

  /**
   * Check if user is a Shipping Agent Representative
   * (organization-scoped role)
   */
  checkIsShippingAgent(): boolean {
    return this.isShippingAgent();
  }

  /**
   * Get organization context for API calls
   * Returns headers object with organization ID if user is a Shipping Agent
   */
  getOrganizationHeaders(): { [key: string]: string } {
    const headers: { [key: string]: string } = {};
    const user = this.getCurrentUser();
    
    if (user?.organizationId) {
      headers['X-Org-Id'] = user.organizationId;
    }
    
    if (user?.id) {
      headers['X-User-Id'] = user.id;
    }
    
    if (user?.role) {
      // Map frontend role to backend role for API calls
      const backendRoleMap: { [key: string]: string } = {
        'PORT_AUTHORITY_OFFICER': 'PortAuthorityOfficer',
        'SHIPPING_AGENT_REPRESENTATIVE': 'ShippingAgentRep',
        'LOGISTICS_OPERATOR': 'LogisticsOperator',
        'ADMINISTRATOR': 'Administrator'
      };
      headers['X-Role'] = backendRoleMap[user.role] || user.role;
    }
    
    return headers;
  }

  /**
   * Check if user has specific role
   */
  hasRole(role: UserRole): boolean {
    return this.getUserRole() === role;
  }

  /**
   * Check if user has any of the specified roles
   */
  hasAnyRole(roles: UserRole[]): boolean {
    const userRole = this.getUserRole();
    return userRole !== null && roles.includes(userRole);
  }

  /**
   * Map various role string formats to UserRole enum
   */
  private mapRoleFromString(roleString: string | string[] | undefined): UserRole {
    if (!roleString) {
      console.warn('No role string provided, defaulting to PORT_AUTHORITY_OFFICER');
      return UserRole.PORT_AUTHORITY_OFFICER;
    }
    
    // Handle array of roles (take the first non-empty one)
    if (Array.isArray(roleString)) {
      roleString = roleString.find(r => r && r.trim()) || '';
      if (!roleString) {
        console.warn('No valid role in array, defaulting to PORT_AUTHORITY_OFFICER');
        return UserRole.PORT_AUTHORITY_OFFICER;
      }
    }
    
    // Normalize the role string
    const normalized = roleString.toUpperCase().trim();
    
    const roleMap: { [key: string]: UserRole } = {
      'PORT_AUTHORITY_OFFICER': UserRole.PORT_AUTHORITY_OFFICER,
      'PORT_AUTHORITY': UserRole.PORT_AUTHORITY_OFFICER,
      'PORTAUTHORITYOFFICER': UserRole.PORT_AUTHORITY_OFFICER,
      'SHIPPING_AGENT_REPRESENTATIVE': UserRole.SHIPPING_AGENT_REPRESENTATIVE,
      'SHIPPING_AGENT': UserRole.SHIPPING_AGENT_REPRESENTATIVE,
      'SHIPPINGAGENTREP': UserRole.SHIPPING_AGENT_REPRESENTATIVE,
      'SHIPPINGAGENTREPRESENTATIVE': UserRole.SHIPPING_AGENT_REPRESENTATIVE,
      'LOGISTICS_OPERATOR': UserRole.LOGISTICS_OPERATOR,
      'LOGISTICS_PLANNER': UserRole.LOGISTICS_OPERATOR,
      'LOGISTICSOPERATOR': UserRole.LOGISTICS_OPERATOR,
      'ADMINISTRATOR': UserRole.ADMINISTRATOR,
      'ADMIN': UserRole.ADMINISTRATOR
    };
    
    const mappedRole = roleMap[normalized];
    if (!mappedRole) {
      console.warn(`Unknown role: ${roleString}, defaulting to PORT_AUTHORITY_OFFICER`);
      return UserRole.PORT_AUTHORITY_OFFICER;
    }
    
    return mappedRole;
  }

  /**
   * Activate user account with token from email
   */
  activateUser(token: string, email: string): Observable<{ success: boolean; message: string; redirectUrl?: string }> {
    const apiUrl = `${environment.apiUrl}/authentication/activate`;
    return this.http.post<{ success: boolean; message: string; redirectUrl?: string }>(
      apiUrl,
      { Token: token, Email: email }
    ).pipe(
      tap(response => {
        console.log('Activation response:', response);
      }),
      catchError(error => {
        console.error('Activation failed:', error);
        return throwError(() => error);
      })
    );
  }
}
