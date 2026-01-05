namespace DDDNetCore.Application.DTOs.Vessels;

public record UpdateVesselDto(
    string Name,
    string VesselTypeId,
    string OrganizationId,
    int? CapacityTEU
);

