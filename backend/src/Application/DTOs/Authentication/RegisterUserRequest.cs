using System;
using System.ComponentModel.DataAnnotations;

namespace DDDNetCore.Application.DTOs.Authentication
{
    /// <summary>
    /// Request to register a new user after Google OAuth authentication
    /// Role is optional - will be assigned by administrator later
    /// </summary>
    public record RegisterUserRequest(
        [Required] string Email,
        [Required] string Name,
        [Required] Guid OrganizationId,
        string? Role = null, // Optional - assigned by admin
        string? GoogleId = null,
        string? ProfilePictureUrl = null
    );
}
