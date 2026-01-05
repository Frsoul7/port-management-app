using System.ComponentModel.DataAnnotations;

namespace DDDNetCore.Application.DTOs.Users
{
    public record UpdateUserDto(
        [Required] string Name,
        [Required] string OrganizationId,
        [Required] string Role
    );
}
