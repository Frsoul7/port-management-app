namespace DDDNetCore.Application.DTOs.VesselTypes;

public record VesselTypeResponseDto(
    string VesselTypeId,
    string Name,
    string? Description,
    int? CapacityTEU,
    int? MaxRows,
    int? MaxBays,
    int? MaxTiers,
    string? OperationalConstraints
);
