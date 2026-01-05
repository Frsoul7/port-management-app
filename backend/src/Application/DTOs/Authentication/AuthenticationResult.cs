namespace DDDNetCore.Application.DTOs.Authentication
{
    /// <summary>
    /// Result of authentication attempt
    /// Success: User authenticated successfully
    /// RequiresRegistration: User needs to complete registration
    /// Failed: Authentication failed
    /// </summary>
    public enum AuthenticationStatus
    {
        Success,
        RequiresRegistration,
        Failed
    }

    public record AuthenticationResult(
        AuthenticationStatus Status,
        AuthResponse? AuthResponse = null,
        GoogleUserInfo? UserInfo = null,
        string? ErrorMessage = null
    );
}
