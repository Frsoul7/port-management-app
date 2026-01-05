// DTOs/Organizations/CreateOrganizationDto.cs
using System.ComponentModel.DataAnnotations;

namespace DDDNetCore.Application.DTOs.Organizations;

public class CreateOrganizationDto
{
    [Required, RegularExpression("^[A-Za-z0-9]{1,10}$", ErrorMessage = "Identifier must be alphanumeric (max 10).")]
    public string Identifier { get; init; } = default!;

    [Required, MaxLength(200)]
    public string LegalName { get; init; } = default!;

    [Required, MaxLength(200)]
    public string? AlternativeName { get; init; }

    [Required, MaxLength(500)]
    public string Address { get; init; } = default!;

    [Required, MaxLength(64)]
    public string TaxNumber { get; init; } = default!;

    // Enum value as int is fine for now
    [Required]
    public DDDNetCore.Domain.Organizations.OrganizationType Type { get; init; }

    // Representatives optional for non-shipping org; required in controller if Type==SHIPPING_AGENT
    public List<RepresentativeInputDto>? Representatives { get; init; }
}
