namespace DDDNetCore.Application.DTOs.Authentication
{
    public record ActivateUserRequest(
        string Token,
        string Email
    );
}
