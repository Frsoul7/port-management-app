namespace DDDNetCore.Application.DTOs.VesselTypes;

public record CreateVesselTypeDto(
    string Name,
    string? Description,
    int? CapacityTEU,
    int? MaxRows,
    int? MaxBays,
    int? MaxTiers,
    string? OperationalConstraints,
    string? VesselTypeId
);
