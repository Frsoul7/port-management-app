namespace DDDNetCore.Application.DTOs.Authentication
{
    /// <summary>
    /// Google user information extracted from OAuth token
    /// Used for registration when user is not found in system
    /// </summary>
    public record GoogleUserInfo(
        string Email,
        string Name,
        string? GoogleId = null,
        string? Picture = null
    );
}
