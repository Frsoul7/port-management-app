using System.ComponentModel.DataAnnotations;

namespace DDDNetCore.Application.DTOs.Docks;

public record UpdateDockDto(
    string Name,
    string Location,  // Free-text location within the port
    double LengthM,
    double DepthM,
    double MaxDraftM,
    List<string>? AllowedVesselTypeIds
);
