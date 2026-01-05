using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using DDDNetCore.Domain.Resources;

namespace DDDNetCore.Application.DTOs.Resources;

/// <summary>
/// Unified DTO for updating physical resources (replaces type-specific endpoints)
/// </summary>
public record UpdatePhysicalResourceDto(
    string? Description,
    [Range(0, int.MaxValue)] int? SetupTimeSeconds,
    
    // STS Crane specific
    int? AvgContainersPerHour,
    string? InstalledAtDockCode,
    
    // Mobile Equipment specific
    MobileEquipmentType? MobileType,
    int? MaxSpeedKph,
    int? ContainersPerTrip,
    
    // Common
    List<string>? RequiredQualificationIds
);
