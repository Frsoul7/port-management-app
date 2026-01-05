namespace DDDNetCore.Application.DTOs.Authentication
{
    public record ActivateUserResponse(
        bool Success,
        string? Message = null,
        string? RedirectUrl = null
    );
}
