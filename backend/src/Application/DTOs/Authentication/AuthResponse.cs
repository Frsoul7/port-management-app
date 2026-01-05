namespace DDDNetCore.Application.DTOs.Authentication
{
    public record AuthResponse(
        string AccessToken,
        string Email,
        string Name,
        string Role,
        string? UserId,
        string? OrganizationId,
        string? OrganizationName,
        string? ProfilePictureUrl = null,
        string? RefreshToken = null,
        int ExpiresIn = 0 // Set dynamically from environment config
    );
}
