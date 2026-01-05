using DDDNetCore.Application.DTOs.Vessels;
using DDDNetCore.Domain.Organizations;
using DDDNetCore.Domain.Vessels;

namespace DDDNetCore.Infrastructure.Mappers;

public static class VesselMapper
{
    /// <summary>
    /// Maps a Vessel domain entity to a VesselResponseDto
    /// </summary>
    public static VesselResponseDto ToDto(Vessel vessel)
    {
        return new VesselResponseDto(
            ImoNumber: vessel.ImoNumber,
            Name: vessel.Name,
            VesselTypeId: vessel.VesselTypeId,
            VesselTypeName: vessel.VesselType?.Name,
            OrganizationId: vessel.OwnerOrganizationId.Value.ToString(),
            OrganizationName: vessel.OwnerOrganization?.LegalName,
            CapacityTEU: vessel.CapacityTEU
        );
    }

    /// <summary>
    /// Maps a CreateVesselDto to a Vessel domain entity
    /// </summary>
    public static Vessel ToEntity(CreateVesselDto dto)
    {
        var organizationId = new OrganizationId(Guid.Parse(dto.OrganizationId));
        return new Vessel(
            imoNumber: dto.ImoNumber,
            name: dto.Name,
            vesselTypeId: dto.VesselTypeId,
            organizationId: organizationId,
            capacityTeu: dto.CapacityTEU
        );
    }

    /// <summary>
    /// Maps an UpdateVesselDto to update an existing Vessel entity
    /// </summary>
    public static void UpdateEntity(Vessel vessel, UpdateVesselDto dto)
    {
        var organizationId = new OrganizationId(Guid.Parse(dto.OrganizationId));
        vessel.UpdateBasics(
            name: dto.Name,
            vesselTypeId: dto.VesselTypeId,
            organizationId: organizationId,
            capacityTeu: dto.CapacityTEU
        );
    }
}
