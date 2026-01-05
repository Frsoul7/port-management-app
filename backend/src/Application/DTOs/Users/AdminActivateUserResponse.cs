namespace DDDNetCore.Application.DTOs.Users
{
    public record AdminActivateUserResponse(
        bool Success,
        string Message,
        UserDto? User = null
    );
}
