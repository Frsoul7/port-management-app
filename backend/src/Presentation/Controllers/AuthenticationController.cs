using Microsoft.AspNetCore.Mvc;
using DDDNetCore.Application.Interfaces;
using DDDNetCore.Application.DTOs.Authentication;
using System.Threading.Tasks;
using Microsoft.Extensions.Logging;

namespace DDDNetCore.Presentation.Controllers
{
    [ApiController]
    [Route("api/[controller]")]
    public class AuthenticationController : ControllerBase
    {
        private readonly IAuthenticationService _authenticationService;
        private readonly ILogger<AuthenticationController> _logger;

        public AuthenticationController(
            IAuthenticationService authenticationService,
            ILogger<AuthenticationController> logger)
        {
            _authenticationService = authenticationService;
            _logger = logger;
        }

        /// <summary>
        /// Authenticates a user using Google OAuth authorization code
        /// </summary>
        /// <param name="request">Contains the authorization code and redirect URI from Google OAuth flow</param>
        /// <returns>Authentication result (Success, RequiresRegistration, or Failed)</returns>
        [HttpPost("google")]
        [ProducesResponseType(typeof(AuthenticationResult), 200)]
        [ProducesResponseType(400)]
        public async Task<IActionResult> GoogleAuthenticate([FromBody] GoogleAuthRequest request)
        {
            if (!ModelState.IsValid)
            {
                return BadRequest(ModelState);
            }

            var result = await _authenticationService.AuthenticateWithGoogleAsync(
                request.Code, 
                request.RedirectUri
            );

            return Ok(result);
        }

        /// <summary>
        /// Registers a new user after Google OAuth authentication
        /// </summary>
        /// <param name="request">Contains user information and selected organization</param>
        /// <returns>Authentication response with JWT token and user information</returns>
        [HttpPost("register")]
        [ProducesResponseType(typeof(AuthResponse), 200)]
        [ProducesResponseType(400)]
        [ProducesResponseType(409)]
        public async Task<IActionResult> Register([FromBody] RegisterUserRequest request)
        {
            if (!ModelState.IsValid)
            {
                return BadRequest(ModelState);
            }

            var result = await _authenticationService.RegisterUserAsync(request);

            if (result == null)
            {
                return Conflict(new { error = "User registration failed. Email may already be registered or organization not found." });
            }

            return Ok(result);
        }

        /// <summary>
        /// Validates a JWT token and returns user information if valid
        /// </summary>
        /// <param name="request">Contains the JWT token to validate</param>
        /// <returns>Token validation result with user information</returns>
        [HttpPost("validate")]
        [ProducesResponseType(typeof(TokenValidationResponse), 200)]
        public async Task<IActionResult> ValidateToken([FromBody] TokenValidationRequest request)
        {
            if (!ModelState.IsValid)
            {
                return BadRequest(ModelState);
            }

            var result = await _authenticationService.ValidateTokenAsync(request.Token);
            return Ok(result);
        }

        /// <summary>
        /// Admin-only login using email and password (no Google OAuth required)
        /// </summary>
        /// <param name="request">Contains admin email and password</param>
        /// <returns>Authentication response with JWT token if successful</returns>
        [HttpPost("admin/login")]
        [ProducesResponseType(typeof(AuthResponse), 200)]
        [ProducesResponseType(401)]
        public async Task<IActionResult> AdminLogin([FromBody] AdminLoginRequest request)
        {
            if (!ModelState.IsValid)
            {
                return BadRequest(ModelState);
            }

            var result = await _authenticationService.AuthenticateAdminAsync(request);

            if (result == null)
            {
                return Unauthorized(new { error = "Invalid credentials or not an administrator" });
            }

            return Ok(result);
        }

        /// <summary>
        /// Refreshes an access token using a valid refresh token
        /// </summary>
        /// <param name="request">Contains the refresh token</param>
        /// <returns>New access token and refresh token</returns>
        [HttpPost("refresh")]
        [ProducesResponseType(typeof(RefreshTokenResponse), 200)]
        [ProducesResponseType(401)]
        public async Task<IActionResult> RefreshToken([FromBody] RefreshTokenRequest request)
        {
            if (!ModelState.IsValid || string.IsNullOrWhiteSpace(request.RefreshToken))
            {
                return BadRequest(new { error = "Refresh token is required" });
            }

            var result = await _authenticationService.RefreshTokenAsync(request.RefreshToken);

            if (result == null)
            {
                return Unauthorized(new { error = "Invalid or expired refresh token" });
            }

            return Ok(result);
        }

        /// <summary>
        /// Activates a user account using the activation token from email
        /// </summary>
        /// <param name="request">Contains the activation token and user email</param>
        /// <returns>Activation result with success status and message</returns>
        [HttpPost("activate")]
        [ProducesResponseType(typeof(ActivateUserResponse), 200)]
        [ProducesResponseType(400)]
        public async Task<IActionResult> ActivateUser([FromBody] ActivateUserRequest request)
        {
            if (!ModelState.IsValid)
            {
                return BadRequest(ModelState);
            }

            if (string.IsNullOrWhiteSpace(request.Token) || string.IsNullOrWhiteSpace(request.Email))
            {
                return BadRequest(new { error = "Token and email are required" });
            }

            var result = await _authenticationService.ActivateUserAsync(request);

            return Ok(result);
        }
    }
}
