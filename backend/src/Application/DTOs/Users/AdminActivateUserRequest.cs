using System.ComponentModel.DataAnnotations;

namespace DDDNetCore.Application.DTOs.Users
{
    public record AdminActivateUserRequest(
        [Required] string UserId,
        [Required] string Role,
        bool Activate = true
    );
}
