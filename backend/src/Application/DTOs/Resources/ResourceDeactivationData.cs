namespace DDDNetCore.Application.DTOs.Resources;

/// <summary>
/// Data for resource deactivation (renamed from DeactivateResourceDto - Phase 5: DTO naming fix)
/// </summary>
public record ResourceDeactivationData(
    string? Reason
);
