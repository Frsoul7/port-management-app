namespace DDDNetCore.Application.DTOs.Vessels;

public record VesselResponseDto(
    string ImoNumber,
    string Name,
    string VesselTypeId,
    string? VesselTypeName,
    string OrganizationId,
    string? OrganizationName,
    int? CapacityTEU
);

