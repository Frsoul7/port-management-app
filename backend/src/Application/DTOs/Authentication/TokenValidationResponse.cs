namespace DDDNetCore.Application.DTOs.Authentication
{
    public record TokenValidationResponse(
        bool Valid,
        string? UserId = null,
        string? Email = null,
        string? Name = null,
        string? Role = null
    );
}
