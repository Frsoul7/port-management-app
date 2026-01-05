namespace DDDNetCore.Application.DTOs.Docks;

public record DockResponseDto(
    string Code,
    string Name,
    string Location,  // Free-text location within the port
    double LengthM,
    double DepthM,
    double MaxDraftM,
    List<string> AllowedVesselTypeIds,
    List<VesselTypeInfoDto> AllowedVesselTypes
);

public record VesselTypeInfoDto(
    string VesselTypeId,
    string Name,
    string? Description
);
