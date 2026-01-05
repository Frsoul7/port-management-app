using DDDNetCore.Application.DTOs.Authentication;
using DDDNetCore.Application.Interfaces;
using DDDNetCore.Domain.Users;
using DDDNetCore.Domain.Organizations;
using DDDNetCore.Domain.IRepository;
using Google.Apis.Auth;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.Logging;
using Microsoft.IdentityModel.Tokens;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.IdentityModel.Tokens.Jwt;
using System.Linq;
using System.Net.Http;
using System.Security.Claims;
using System.Security.Cryptography;
using System.Text;
using System.Threading.Tasks;

namespace DDDNetCore.Application.Services
{
    public class AuthenticationService : IAuthenticationService
    {
        private readonly IUserRepository _userRepository;
        private readonly IOrganizationRepository _organizationRepository;
        private readonly IUnitOfWork _unitOfWork;
        private readonly IConfiguration _configuration;
        private readonly ILogger<AuthenticationService> _logger;
        private readonly HttpClient _httpClient;
        private readonly IEmailService _emailService;
        // In-memory store for refresh tokens (in production, use Redis or database)
        // Using ConcurrentDictionary for thread-safety
        private static readonly ConcurrentDictionary<string, RefreshTokenData> _refreshTokens = new();

        public AuthenticationService(
            IUserRepository userRepository,
            IOrganizationRepository organizationRepository,
            IUnitOfWork unitOfWork,
            IConfiguration configuration,
            ILogger<AuthenticationService> logger,
            IHttpClientFactory httpClientFactory,
            IEmailService emailService)
        {
            _userRepository = userRepository;
            _organizationRepository = organizationRepository;
            _unitOfWork = unitOfWork;
            _configuration = configuration;
            _logger = logger;
            _httpClient = httpClientFactory.CreateClient();
            _emailService = emailService;
        }

        // Helper methods to read token expiry from environment variables or config
        private double GetAccessTokenExpiryHours()
        {
            var envValue = Environment.GetEnvironmentVariable("JWT_ACCESS_TOKEN_EXPIRY_HOURS");
            if (double.TryParse(envValue, System.Globalization.NumberStyles.Any, System.Globalization.CultureInfo.InvariantCulture, out var hours))
                return hours;

            var configValue = _configuration["Jwt:AccessTokenExpiryHours"];
            if (double.TryParse(configValue, System.Globalization.NumberStyles.Any, System.Globalization.CultureInfo.InvariantCulture, out hours))
                return hours;

            return 8; // Default: 8 hours
        }

        private double GetRefreshTokenExpiryDays()
        {
            var envValue = Environment.GetEnvironmentVariable("JWT_REFRESH_TOKEN_EXPIRY_DAYS");
            if (double.TryParse(envValue, System.Globalization.NumberStyles.Any, System.Globalization.CultureInfo.InvariantCulture, out var days))
                return days;

            var configValue = _configuration["Jwt:RefreshTokenExpiryDays"];
            if (double.TryParse(configValue, System.Globalization.NumberStyles.Any, System.Globalization.CultureInfo.InvariantCulture, out days))
                return days;

            return 7; // Default: 7 days
        }

        private int GetAccessTokenExpirySeconds()
        {
            // Parse as double to handle fractional hours like 0.0333 (2 minutes)
            var hours = GetAccessTokenExpiryHours();
            return (int)(hours * 3600); // Convert to seconds
        }

        private record RefreshTokenData(
            string UserId,
            string Email,
            string Name,
            string Role,
            string? OrganizationId,
            string? OrganizationName,
            DateTime ExpiresAt
        );

        public async Task<AuthenticationResult> AuthenticateWithGoogleAsync(string authorizationCode, string redirectUri)
        {
            try
            {
                // Exchange authorization code for Google tokens
                var tokenResponse = await ExchangeCodeForTokensAsync(authorizationCode, redirectUri);

                if (tokenResponse == null)
                {
                    _logger.LogWarning("Failed to exchange authorization code for tokens");
                    return new AuthenticationResult(
                        Status: AuthenticationStatus.Failed,
                        ErrorMessage: "Failed to exchange authorization code"
                    );
                }

                // Validate Google ID token
                var payload = await GoogleJsonWebSignature.ValidateAsync(tokenResponse.IdToken);

                // Find user by email
                var user = await _userRepository.GetByEmailAsync(payload.Email);

                _logger.LogInformation("User lookup: Email={Email}, Found={Found}, OrgId={OrgId}, OrgLoaded={OrgLoaded}", 
                    payload.Email, 
                    user != null, 
                    user?.OrganizationId?.Value, 
                    user?.Organization != null);

                if (user == null)
                {
                    _logger.LogInformation("User with email {Email} not found - requires registration", payload.Email);
                    
                    // Return user info for registration
                    return new AuthenticationResult(
                        Status: AuthenticationStatus.RequiresRegistration,
                        UserInfo: new GoogleUserInfo(
                            Email: payload.Email,
                            Name: payload.Name,
                            GoogleId: payload.Subject,
                            Picture: payload.Picture
                        )
                    );
                }

                // Check if email is verified
                if (!user.EmailVerified)
                {
                    _logger.LogWarning("User {Email} attempted to login but email is not verified", payload.Email);
                    return new AuthenticationResult(
                        Status: AuthenticationStatus.Failed,
                        ErrorMessage: "Please verify your email address first. Check your inbox for the activation link."
                    );
                }

                // Check if user has been activated by admin
                if (!user.IsActive)
                {
                    _logger.LogWarning("User {Email} attempted to login but account is not activated by admin", payload.Email);
                    return new AuthenticationResult(
                        Status: AuthenticationStatus.Failed,
                        ErrorMessage: "Your account is pending administrator approval. You will be notified once your account is activated."
                    );
                }

                // Check if user has a role assigned
                if (!user.Role.HasValue)
                {
                    _logger.LogWarning("User {Email} attempted to login but no role assigned", payload.Email);
                    return new AuthenticationResult(
                        Status: AuthenticationStatus.Failed,
                        ErrorMessage: "Your account does not have a role assigned yet. Please wait for administrator approval."
                    );
                }

                // Use the user's assigned role, not derived from organization type
                var userRole = user.Role.Value.ToString();
                var frontendRole = MapUserRoleToFrontendRole(userRole);
                var organizationName = user.Organization?.LegalName ?? user.Organization?.AlternativeName;

                // Generate internal JWT token
                var (token, refreshToken) = GenerateTokens(
                    user.UserId.ToString(), 
                    payload.Email, 
                    payload.Name, 
                    frontendRole,  // Use user's actual role
                    user.OrganizationId?.ToString(), 
                    organizationName
                );

                return new AuthenticationResult(
                    Status: AuthenticationStatus.Success,
                    AuthResponse: new AuthResponse(
                        AccessToken: token,
                        Email: payload.Email,
                        Name: payload.Name,
                        Role: frontendRole,
                        UserId: user.UserId.ToString(),
                        OrganizationId: user.OrganizationId?.ToString() ?? string.Empty,
                        OrganizationName: organizationName,
                        ProfilePictureUrl: user.ProfilePictureUrl,
                        RefreshToken: refreshToken,
                        ExpiresIn: GetAccessTokenExpirySeconds()
                    )
                );
            }
            catch (InvalidJwtException ex)
            {
                _logger.LogWarning(ex, "Invalid Google token");
                return new AuthenticationResult(
                    Status: AuthenticationStatus.Failed,
                    ErrorMessage: "Invalid Google token"
                );
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Authentication error");
                return new AuthenticationResult(
                    Status: AuthenticationStatus.Failed,
                    ErrorMessage: "Authentication failed"
                );
            }
        }

        public async Task<AuthResponse?> RegisterUserAsync(RegisterUserRequest request)
        {
            try
            {
                // Check if user already exists
                var existingUser = await _userRepository.GetByEmailAsync(request.Email);
                if (existingUser != null)
                {
                    _logger.LogWarning("User with email {Email} already exists", request.Email);
                    return null;
                }

                // Verify organization exists
                var organizationId = new OrganizationId(request.OrganizationId);
                var organization = await _organizationRepository.GetByIdAsync(organizationId.ToString());
                if (organization == null)
                {
                    _logger.LogWarning("Organization with ID {OrgId} not found", request.OrganizationId);
                    return null;
                }

                // Parse and validate role (optional - can be assigned by admin later)
                UserRole? userRole = null;
                if (!string.IsNullOrEmpty(request.Role))
                {
                    if (!Enum.TryParse<UserRole>(request.Role, out var parsedRole))
                    {
                        _logger.LogWarning("Invalid role {Role} provided", request.Role);
                        return null;
                    }
                    userRole = parsedRole;
                }

                // Create new user (inactive by default, role may be null)
                var userId = Guid.NewGuid();
                var user = new User(
                    id: userId,
                    name: request.Name,
                    email: request.Email,
                    organizationId: organizationId,
                    role: userRole,
                    profilePictureUrl: request.ProfilePictureUrl
                );

                // Generate activation token
                var activationToken = GenerateActivationToken();
                var tokenExpiry = DateTime.UtcNow.AddHours(24); // 24 hours validity
                user.SetActivationToken(activationToken, tokenExpiry);

                _logger.LogInformation("Creating user {Email} with OrganizationId {OrgId} (role: {Role}, requires email verification and admin activation)", 
                    request.Email, organizationId.Value, userRole?.ToString() ?? "None");

                await _userRepository.AddAsync(user);
                await _unitOfWork.CommitAsync();

                // Send activation email
                var frontendUrl = Environment.GetEnvironmentVariable("FRONTEND_URL")
                    ?? _configuration["Frontend:Url"]
                    ?? "http://vm.nunoepteixeira.me";
                var activationLink = $"{frontendUrl}/auth/activate?token={activationToken}&email={Uri.EscapeDataString(request.Email)}";
                
                var emailSent = await _emailService.SendActivationEmailAsync(request.Email, request.Name, activationLink);
                
                if (!emailSent)
                {
                    _logger.LogWarning("Failed to send activation email to {Email}, but user was created", request.Email);
                }
                else
                {
                    _logger.LogInformation("Activation email sent to {Email}", request.Email);
                }

                _logger.LogInformation("User {Email} created successfully with OrganizationId {OrgId} - Email verification and admin activation required", request.Email, user.OrganizationId.Value);

                // Return success response (user is not logged in yet, must verify email and be activated by admin)
                // Don't generate JWT tokens until account is fully activated
                return new AuthResponse(
                    AccessToken: string.Empty, // No token until activated by admin
                    Email: request.Email,
                    Name: request.Name,
                    Role: request.Role ?? string.Empty, // May be empty if no role assigned yet
                    UserId: userId.ToString(),
                    OrganizationId: request.OrganizationId.ToString(),
                    OrganizationName: organization?.LegalName ?? organization?.AlternativeName,
                    ProfilePictureUrl: request.ProfilePictureUrl,
                    RefreshToken: string.Empty,
                    ExpiresIn: 0 // No expiry since no token
                );
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Registration error for email {Email}", request.Email);
                return null;
            }
        }

        public async Task<AuthResponse?> AuthenticateAdminAsync(AdminLoginRequest request)
        {
            try
            {
                // Find user by email
                var user = await _userRepository.GetByEmailAsync(request.Email);

                if (user == null)
                {
                    _logger.LogWarning("Admin login failed: User not found with email {Email}", request.Email);
                    return null;
                }

                // Verify password
                if (string.IsNullOrEmpty(user.PasswordHash))
                {
                    _logger.LogWarning("Admin login failed: User {Email} has no password set", request.Email);
                    return null;
                }

                if (!VerifyPassword(request.Password, user.PasswordHash))
                {
                    _logger.LogWarning("Admin login failed: Invalid password for {Email}", request.Email);
                    return null;
                }

                // Check if user is active
                if (!user.IsActive)
                {
                    _logger.LogWarning("Admin login failed: User {Email} is not active", request.Email);
                    return null;
                }

                // Check if user has a role assigned
                if (!user.Role.HasValue)
                {
                    _logger.LogWarning("Admin login failed: User {Email} does not have a role assigned", request.Email);
                    return null;
                }

                // Get organization name
                var organizationName = user.Organization?.LegalName ?? user.Organization?.AlternativeName;

                // Use the user's assigned role
                var userRole = user.Role.Value.ToString();
                var frontendRole = MapUserRoleToFrontendRole(userRole);

                // Generate JWT token (use user's actual role)
                var (token, refreshToken) = GenerateTokens(
                    user.UserId.ToString(),
                    user.Email,
                    user.Name,
                    frontendRole,
                    user.OrganizationId?.ToString(),
                    organizationName
                );

                _logger.LogInformation("User {Email} logged in successfully with role {Role}", request.Email, frontendRole);

                return new AuthResponse(
                    AccessToken: token,
                    Email: user.Email,
                    Name: user.Name,
                    Role: frontendRole,
                    UserId: user.UserId.ToString(),
                    OrganizationId: user.OrganizationId?.ToString() ?? string.Empty,
                    OrganizationName: organizationName,
                    ProfilePictureUrl: user.ProfilePictureUrl,
                    RefreshToken: refreshToken,
                    ExpiresIn: GetAccessTokenExpirySeconds()
                );
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Admin authentication error for {Email}", request.Email);
                return null;
            }
        }

        public Task<TokenValidationResponse> ValidateTokenAsync(string token)
        {
            try
            {
                var tokenHandler = new JwtSecurityTokenHandler();
                var jwtSecret = Environment.GetEnvironmentVariable("JWT_SECRET")
                    ?? _configuration["Jwt:Secret"]
                    ?? "your-secret-key-min-32-chars-long!";
                var key = Encoding.UTF8.GetBytes(jwtSecret);

                tokenHandler.ValidateToken(token, new TokenValidationParameters
                {
                    ValidateIssuerSigningKey = true,
                    IssuerSigningKey = new SymmetricSecurityKey(key),
                    ValidateIssuer = false,
                    ValidateAudience = false,
                    ClockSkew = TimeSpan.Zero
                }, out SecurityToken validatedToken);

                var jwtToken = (JwtSecurityToken)validatedToken;
                var userId = jwtToken.Claims.First(x => x.Type == "sub").Value;
                var email = jwtToken.Claims.First(x => x.Type == "email").Value;
                var name = jwtToken.Claims.First(x => x.Type == "name").Value;
                var role = jwtToken.Claims.First(x => x.Type == ClaimTypes.Role).Value;

                return Task.FromResult(new TokenValidationResponse(
                    Valid: true,
                    UserId: userId,
                    Email: email,
                    Name: name,
                    Role: role
                ));
            }
            catch (Exception ex)
            {
                _logger.LogWarning(ex, "Token validation failed");
                return Task.FromResult(new TokenValidationResponse(Valid: false));
            }
        }

        private async Task<GoogleTokenResponse?> ExchangeCodeForTokensAsync(string code, string redirectUri)
        {
            var clientId = _configuration["Google:ClientId"];
            var clientSecret = _configuration["Google:ClientSecret"];

            var requestBody = new Dictionary<string, string>
            {
                { "code", code },
                { "client_id", clientId! },
                { "client_secret", clientSecret! },
                { "redirect_uri", redirectUri },
                { "grant_type", "authorization_code" }
            };

            var response = await _httpClient.PostAsync(
                "https://oauth2.googleapis.com/token",
                new FormUrlEncodedContent(requestBody)
            );

            if (!response.IsSuccessStatusCode)
            {
                var errorContent = await response.Content.ReadAsStringAsync();
                _logger.LogError("Google token exchange failed: {Error}", errorContent);
                return null;
            }

            return await response.Content.ReadFromJsonAsync<GoogleTokenResponse>();
        }

        /// <summary>
        /// Maps organization types to frontend role names.
        /// Handles the naming differences between backend (CallerContext) and frontend (Angular).
        /// </summary>
        private string MapOrganizationTypeToFrontendRole(string organizationType)
        {
            return organizationType?.ToUpperInvariant() switch
            {
                "SHIPPING_AGENT" => "SHIPPING_AGENT_REPRESENTATIVE",
                "PORT_AUTHORITY" => "PORT_AUTHORITY_OFFICER",
                "LOGISTICS_OPERATOR" => "LOGISTICS_OPERATOR",
                "ADMINISTRATOR" => "ADMINISTRATOR",
                _ => "UNKNOWN"
            };
        }

        private string MapUserRoleToFrontendRole(string userRole)
        {
            return userRole?.ToUpperInvariant() switch
            {
                "SHIPPING_AGENT_REPRESENTATIVE" => "SHIPPING_AGENT_REPRESENTATIVE",
                "PORT_AUTHORITY_OFFICER" => "PORT_AUTHORITY_OFFICER",
                "LOGISTICS_OPERATOR" => "LOGISTICS_OPERATOR",
                "ADMINISTRATOR" => "ADMINISTRATOR",
                _ => "UNKNOWN"
            };
        }

        private (string AccessToken, string RefreshToken) GenerateTokens(string userId, string email, string name, string role, string? organizationId = null, string? organizationName = null)
        {
            var tokenHandler = new JwtSecurityTokenHandler();
            var jwtSecret = Environment.GetEnvironmentVariable("JWT_SECRET")
                ?? _configuration["Jwt:Secret"]
                ?? "your-secret-key-min-32-chars-long!";
            var key = Encoding.UTF8.GetBytes(jwtSecret);

            // Role is already in frontend format from callers, no need to map again
            var claims = new List<Claim>
            {
                new Claim("sub", userId),
                new Claim("email", email),
                new Claim("name", name),
                new Claim(ClaimTypes.Role, role)
            };

            // Add organization claims if available
            if (!string.IsNullOrEmpty(organizationId))
            {
                claims.Add(new Claim("organizationId", organizationId));
                claims.Add(new Claim("org_id", organizationId)); // Alternative claim name
            }

            if (!string.IsNullOrEmpty(organizationName))
            {
                claims.Add(new Claim("organizationName", organizationName));
                claims.Add(new Claim("org_name", organizationName)); // Alternative claim name
            }

            // Generate Access Token
            var accessTokenExpiryHours = GetAccessTokenExpiryHours();
            var tokenDescriptor = new SecurityTokenDescriptor
            {
                Subject = new ClaimsIdentity(claims),
                Expires = DateTime.UtcNow.AddHours(accessTokenExpiryHours),
                SigningCredentials = new SigningCredentials(
                    new SymmetricSecurityKey(key),
                    SecurityAlgorithms.HmacSha256Signature
                )
            };

            var token = tokenHandler.CreateToken(tokenDescriptor);
            var accessToken = tokenHandler.WriteToken(token);

            // Generate Refresh Token
            var refreshTokenExpiryDays = GetRefreshTokenExpiryDays();
            var refreshToken = Convert.ToBase64String(RandomNumberGenerator.GetBytes(64));
            var refreshTokenExpiry = DateTime.UtcNow.AddDays(refreshTokenExpiryDays);

            // Store refresh token data
            _refreshTokens[refreshToken] = new RefreshTokenData(
                UserId: userId,
                Email: email,
                Name: name,
                Role: role,
                OrganizationId: organizationId,
                OrganizationName: organizationName,
                ExpiresAt: refreshTokenExpiry
            );

            _logger.LogInformation("Generated tokens for user {Email}, refresh token expires at {ExpiresAt}", email, refreshTokenExpiry);

            return (accessToken, refreshToken);
        }

        /// <summary>
        /// Refreshes an access token using a valid refresh token
        /// </summary>
        public async Task<RefreshTokenResponse?> RefreshTokenAsync(string refreshToken)
        {
            try
            {
                // Clean up expired refresh tokens periodically
                CleanupExpiredRefreshTokens();

                // Validate refresh token exists and is not expired
                if (!_refreshTokens.TryGetValue(refreshToken, out var tokenData))
                {
                    _logger.LogWarning("Refresh token not found or invalid");
                    return null;
                }

                if (tokenData.ExpiresAt <= DateTime.UtcNow)
                {
                    _logger.LogWarning("Refresh token expired for user {Email}", tokenData.Email);
                    _refreshTokens.TryRemove(refreshToken, out _);
                    return null;
                }

                // Verify user still exists and is active
                var user = await _userRepository.GetByEmailAsync(tokenData.Email);
                if (user == null)
                {
                    _logger.LogWarning("User {Email} no longer exists, invalidating refresh token", tokenData.Email);
                    _refreshTokens.TryRemove(refreshToken, out _);
                    return null;
                }

                // Generate new tokens
                var (newAccessToken, newRefreshToken) = GenerateTokens(
                    tokenData.UserId,
                    tokenData.Email,
                    tokenData.Name,
                    tokenData.Role,
                    tokenData.OrganizationId,
                    tokenData.OrganizationName
                );

                // Remove old refresh token
                _refreshTokens.TryRemove(refreshToken, out _);

                _logger.LogInformation("Successfully refreshed token for user {Email}", tokenData.Email);

                return new RefreshTokenResponse(
                    AccessToken: newAccessToken,
                    RefreshToken: newRefreshToken,
                    ExpiresIn: GetAccessTokenExpirySeconds()
                );
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Error refreshing token");
                return null;
            }
        }

        /// <summary>
        /// Cleanup expired refresh tokens to prevent memory leaks
        /// </summary>
        private void CleanupExpiredRefreshTokens()
        {
            var expiredTokens = _refreshTokens
                .Where(kvp => kvp.Value.ExpiresAt <= DateTime.UtcNow)
                .Select(kvp => kvp.Key)
                .ToList();

            foreach (var token in expiredTokens)
            {
                _refreshTokens.TryRemove(token, out _);
            }

            if (expiredTokens.Any())
            {
                _logger.LogInformation("Cleaned up {Count} expired refresh tokens", expiredTokens.Count);
            }
        }

        /// <summary>
        /// Hashes a password using PBKDF2 with SHA256
        /// </summary>
        public static string HashPassword(string password)
        {
            // Generate a salt
            byte[] salt = RandomNumberGenerator.GetBytes(16);

            // Hash the password with PBKDF2
            var pbkdf2 = new Rfc2898DeriveBytes(password, salt, 10000, HashAlgorithmName.SHA256);
            byte[] hash = pbkdf2.GetBytes(32);

            // Combine salt and hash
            byte[] hashBytes = new byte[48];
            Array.Copy(salt, 0, hashBytes, 0, 16);
            Array.Copy(hash, 0, hashBytes, 16, 32);

            // Convert to base64
            return Convert.ToBase64String(hashBytes);
        }

        /// <summary>
        /// Verifies a password against a hash
        /// </summary>
        private bool VerifyPassword(string password, string storedHash)
        {
            // Extract the bytes
            byte[] hashBytes = Convert.FromBase64String(storedHash);

            // Get the salt
            byte[] salt = new byte[16];
            Array.Copy(hashBytes, 0, salt, 0, 16);

            // Compute the hash on the password the user entered
            var pbkdf2 = new Rfc2898DeriveBytes(password, salt, 10000, HashAlgorithmName.SHA256);
            byte[] hash = pbkdf2.GetBytes(32);

            // Compare the results
            for (int i = 0; i < 32; i++)
            {
                if (hashBytes[i + 16] != hash[i])
                    return false;
            }

            return true;
        }

        /// <summary>
        /// Generate a cryptographically secure activation token
        /// </summary>
        private string GenerateActivationToken()
        {
            return Convert.ToBase64String(RandomNumberGenerator.GetBytes(32))
                .Replace("+", "-")
                .Replace("/", "_")
                .Replace("=", "");
        }

        /// <summary>
        /// Verify user email with activation token (step 1 of 2-step activation)
        /// </summary>
        public async Task<ActivateUserResponse> ActivateUserAsync(ActivateUserRequest request)
        {
            try
            {
                _logger.LogInformation("Attempting to verify email for user {Email} with token", request.Email);

                // Find user by email
                var user = await _userRepository.GetByEmailAsync(request.Email);
                if (user == null)
                {
                    _logger.LogWarning("Email verification failed: User {Email} not found", request.Email);
                    return new ActivateUserResponse(
                        Success: false,
                        Message: "Invalid activation link. User not found."
                    );
                }

                // Check if already verified
                if (user.EmailVerified)
                {
                    _logger.LogInformation("Email for user {Email} is already verified", request.Email);
                    return new ActivateUserResponse(
                        Success: true,
                        Message: "Your email is already verified. Please wait for an administrator to activate your account and assign your role.",
                        RedirectUrl: "/login"
                    );
                }

                // Validate activation token
                if (!user.ValidateActivationToken(request.Token))
                {
                    _logger.LogWarning("Email verification failed: Invalid or expired token for user {Email}", request.Email);
                    return new ActivateUserResponse(
                        Success: false,
                        Message: "Invalid or expired activation link. Please request a new activation email or contact support."
                    );
                }

                // Verify email
                user.VerifyEmail();
                await _userRepository.UpdateAsync(user);
                await _unitOfWork.CommitAsync();

                _logger.LogInformation("Email successfully verified for user {Email}. Awaiting administrator activation.", request.Email);

                return new ActivateUserResponse(
                    Success: true,
                    Message: "Your email has been successfully verified! An administrator will review your account and assign your role. You will be notified once your account is activated.",
                    RedirectUrl: "/login"
                );
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Error verifying email for user {Email}", request.Email);
                return new ActivateUserResponse(
                    Success: false,
                    Message: "An error occurred during email verification. Please try again later."
                );
            }
        }

        private record GoogleTokenResponse(
            string access_token,
            string id_token,
            int expires_in,
            string token_type
        )
        {
            public string IdToken => id_token;
        }
    }
}
