using System.ComponentModel.DataAnnotations;

namespace DDDNetCore.Application.DTOs.Organizations;

public class AddRepresentativesDto
{
    [Required]
    public List<RepresentativeInputDto> Representatives { get; init; } = new();
}
