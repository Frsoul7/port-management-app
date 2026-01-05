export const environment = {
  production: true,
  apiUrl: 'http://api.nunoepteixeira.me/api',
  // Google OAuth2 Configuration
  iamUrl: 'https://accounts.google.com/o/oauth2/v2/auth',
  clientId: '882371773427-qlqbmd9pj5mc7rp2rp57dvmcc683raoo.apps.googleusercontent.com',
  redirectUri: '',
  scopes: 'openid profile email',
  tokenStorageKey: 'access_token',
  roleStorageKey: 'user_role',
  apiTimeoutMs: 30000,
  // Google-specific settings
  googleTokenInfoUrl: 'https://oauth2.googleapis.com/tokeninfo',
  authEndpoint: 'http://api.nunoepteixeira.me/api/Authentication/google',
  useMockAuth: false,
  // Token Configuration (should match backend settings)
  // Access token expires after 8 hours (28800 seconds)
  accessTokenExpirySeconds: 28800,
  // Refresh token buffer: refresh this many seconds before expiry (300 = 5 minutes)
  tokenRefreshBufferSeconds: 300
};
