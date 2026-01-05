namespace DDDNetCore.Application.DTOs.Authentication
{
    public record RefreshTokenResponse(
        string AccessToken,
        string RefreshToken,
        int ExpiresIn = 0 // Set dynamically from environment config
    );
}
