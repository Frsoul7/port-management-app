namespace DDDNetCore.Application.DTOs.Users
{
    public record UserDto(
        string UserId,
        string Name,
        string Email,
        string OrganizationId,
        string OrganizationName,
        string Role,
        string? ProfilePictureUrl,
        bool EmailVerified,
        bool IsActive
    );
}
