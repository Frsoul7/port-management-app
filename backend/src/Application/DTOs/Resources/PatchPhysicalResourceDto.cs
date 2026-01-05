using DDDNetCore.Domain.Resources;

namespace DDDNetCore.Application.DTOs.Resources;

/// <summary>
/// DTO for PATCH operations on physical resources, supporting status and availability changes
/// </summary>
public record PatchPhysicalResourceDto(
    bool? IsActive,                         // true = activate, false = deactivate
    string? DeactivationReason,             // Required when IsActive = false
    PhysicalResourceAvailability? Availability  // Update availability status
);
