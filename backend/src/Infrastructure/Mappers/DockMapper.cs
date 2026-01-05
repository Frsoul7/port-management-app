using DDDNetCore.Application.DTOs.Docks;
using DDDNetCore.Domain.Docks;

namespace DDDNetCore.Infrastructure.Mappers;

public static class DockMapper
{
    /// <summary>
    /// Maps a Dock domain entity to DockResponseDto
    /// </summary>
    public static DockResponseDto ToDto(Dock dock)
    {
        return new DockResponseDto(
            Code: dock.Code,
            Name: dock.Name,
            Location: dock.Location,
            LengthM: dock.LengthM,
            DepthM: dock.DepthM,
            MaxDraftM: dock.MaxDraftM,
            AllowedVesselTypeIds: dock.AllowedVesselTypes.Select(vt => vt.VesselTypeId).ToList(),
            AllowedVesselTypes: dock.AllowedVesselTypes.Select(vt => new VesselTypeInfoDto(
                VesselTypeId: vt.VesselTypeId,
                Name: vt.Name,
                Description: vt.Description
            )).ToList()
        );
    }

    /// <summary>
    /// Maps a CreateDockDto to a Dock domain entity
    /// </summary>
    public static Dock ToEntity(CreateDockDto dto)
    {
        return new Dock(
            code: dto.Code,
            name: dto.Name,
            location: dto.Location,
            lengthM: dto.LengthM,
            depthM: dto.DepthM,
            maxDraftM: dto.MaxDraftM
        );
    }

    /// <summary>
    /// Maps an UpdateDockDto to update an existing Dock entity
    /// </summary>
    public static void UpdateEntity(Dock dock, UpdateDockDto dto)
    {
        dock.Update(
            name: dto.Name,
            location: dto.Location,
            lengthM: dto.LengthM,
            depthM: dto.DepthM,
            maxDraftM: dto.MaxDraftM
        );
    }
}
