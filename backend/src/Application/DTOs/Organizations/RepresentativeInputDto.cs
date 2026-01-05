// DTOs/Organizations/RepresentativeInputDto.cs
using System.ComponentModel.DataAnnotations;

namespace DDDNetCore.Application.DTOs.Organizations;

public class RepresentativeInputDto
{
    [Required, MaxLength(150)]
    public string Name { get; init; } = default!;

    [Required, MaxLength(64)]
    public string CitizenId { get; init; } = default!;    
    
    [Required, StringLength(2, MinimumLength = 2, ErrorMessage = "Nationality must be 2 letters.")]
    public string Nationality { get; init; } = default!;

    [Required, MaxLength(254)]
    [RegularExpression(@"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$",
        ErrorMessage = "Email must be a valid email address (format: something@something.something)")]
    public string Email { get; init; } = default!;

    [MaxLength(64)]
    public string? Phone { get; init; }
}
