namespace DDDNetCore.Application.DTOs.Organizations;

/// <summary>
/// DTO for PATCH operations on representatives, supporting status changes (activate/deactivate)
/// </summary>
public record PatchRepresentativeStatusDto(
    bool? IsActive  // true = activate, false = deactivate
);
