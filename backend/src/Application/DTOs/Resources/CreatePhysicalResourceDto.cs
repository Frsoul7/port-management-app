using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using DDDNetCore.Domain.Resources;

namespace DDDNetCore.Application.DTOs.Resources;

/// <summary>
/// Unified DTO for creating physical resources (replaces type-specific endpoints)
/// ResourceType discriminator determines which subtype to create
/// </summary>
public record CreatePhysicalResourceDto(
    [Required, MaxLength(20)] string Code,
    string? Description,
    [Range(0, int.MaxValue)] int SetupTimeSeconds,
    [Required] string ResourceType,  // "STS_CRANE" or "MOBILE_EQUIPMENT"
    
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
