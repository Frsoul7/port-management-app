import { ConfigService } from '../app/core/services/config.service';

// Get the singleton instance of ConfigService
const configService = new ConfigService();

// Helper function to get API URL dynamically
function getApiUrl(): string {
  try {
    return configService.apiUrl;
  } catch {
    // Fallback during initial load before config is loaded
    return 'http://localhost:5174/api';
  }
}

export const environment = {
  production: false,
  // Dynamic API URL from config.json
  get apiUrl() { return getApiUrl(); },
  // Backend OEM API URL
  backendOemUrl: 'http://localhost:3000/api/v1',
  // Google OAuth2 Configuration
  iamUrl: 'https://accounts.google.com/o/oauth2/v2/auth',
  clientId: '882371773427-qlqbmd9pj5mc7rp2rp57dvmcc683raoo.apps.googleusercontent.com',
  redirectUri: '',
  scopes: 'openid profile email',
  tokenStorageKey: 'access_token',
  roleStorageKey: 'user_role',
  apiTimeoutMs: 30000,
  googleTokenInfoUrl: 'https://oauth2.googleapis.com/tokeninfo',
  // Backend authentication endpoint
  get authEndpoint() { return `${getApiUrl()}/Authentication/google`; },
  useMockAuth: false,
  // Token Configuration
  accessTokenExpirySeconds: 120,
  tokenRefreshBufferSeconds: 15
};
