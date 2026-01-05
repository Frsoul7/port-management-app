using DDDNetCore.Application.DTOs.Authentication;
using System.Threading.Tasks;

namespace DDDNetCore.Application.Interfaces
{
    public interface IAuthenticationService
    {
        Task<AuthenticationResult> AuthenticateWithGoogleAsync(string authorizationCode, string redirectUri);
        Task<AuthResponse?> RegisterUserAsync(RegisterUserRequest request);
        Task<TokenValidationResponse> ValidateTokenAsync(string token);
        Task<AuthResponse?> AuthenticateAdminAsync(AdminLoginRequest request);
        Task<RefreshTokenResponse?> RefreshTokenAsync(string refreshToken);
        Task<ActivateUserResponse> ActivateUserAsync(ActivateUserRequest request);
    }
}
