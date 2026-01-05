namespace DDDNetCore.Application.DTOs.Vessels;

public record CreateVesselDto(
    string ImoNumber,
    string Name,
    string VesselTypeId,
    string OrganizationId,
    int? CapacityTEU
);

