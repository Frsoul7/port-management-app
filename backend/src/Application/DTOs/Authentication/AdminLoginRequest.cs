using System.ComponentModel.DataAnnotations;

namespace DDDNetCore.Application.DTOs.Authentication
{
    /// <summary>
    /// Request for admin-only username/password login
    /// </summary>
    public record AdminLoginRequest(
        [Required]
        [RegularExpression(@"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$",
            ErrorMessage = "Email must be a valid email address (format: something@something.something)")]
        string Email,

        [Required]
        [MinLength(8)]
        string Password
    );
}
