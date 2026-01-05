using DDDNetCore.Application.DTOs.VesselTypes;
using DDDNetCore.Domain.Vessels;

namespace DDDNetCore.Infrastructure.Mappers;

public static class VesselTypeMapper
{
    /// <summary>
    /// Maps a VesselType domain entity to VesselTypeResponseDto
    /// </summary>
    public static VesselTypeResponseDto ToDto(VesselType vesselType)
    {
        return new VesselTypeResponseDto(
            VesselTypeId: vesselType.VesselTypeId,
            Name: vesselType.Name,
            Description: vesselType.Description,
            CapacityTEU: vesselType.CapacityTEU,
            MaxRows: vesselType.MaxRows,
            MaxBays: vesselType.MaxBays,
            MaxTiers: vesselType.MaxTiers,
            OperationalConstraints: vesselType.OperationalConstraints
        );
    }

    /// <summary>
    /// Maps a CreateVesselTypeDto to a VesselType domain entity
    /// </summary>
    public static VesselType ToEntity(CreateVesselTypeDto dto)
    {
        var vesselType = new VesselType(
            id: dto.VesselTypeId ?? Guid.NewGuid().ToString(),
            name: dto.Name
        );

        vesselType.Update(
            name: dto.Name,
            description: dto.Description,
            capacityTeu: dto.CapacityTEU,
            maxRows: dto.MaxRows,
            maxBays: dto.MaxBays,
            maxTiers: dto.MaxTiers,
            operationalConstraints: dto.OperationalConstraints
        );

        return vesselType;
    }

    /// <summary>
    /// Maps an UpdateVesselTypeDto to update an existing VesselType entity
    /// </summary>
    public static void UpdateEntity(VesselType vesselType, UpdateVesselTypeDto dto)
    {
        vesselType.Update(
            name: dto.Name ?? vesselType.Name,
            description: dto.Description,
            capacityTeu: dto.CapacityTEU,
            maxRows: dto.MaxRows,
            maxBays: dto.MaxBays,
            maxTiers: dto.MaxTiers,
            operationalConstraints: dto.OperationalConstraints
        );
    }
}
