using System;
using System.Collections.Generic;
using DDDNetCore.Domain.Resources;
using DDDNetCore.Application.DTOs.HumanResources;

namespace DDDNetCore.Application.DTOs.Resources
{
    public record PhysicalResourceResponseDto(
        string ResourceId,
        string Code,
        string? Description,
        string ResourceType,
        string Availability,
        int SetupTimeSeconds,
        List<QualificationDto> RequiredQualifications,
        DateTime CreatedAt,
        DateTime? DeactivatedAt,
        string? DeactivationReason,
        // STS Crane specific
        int? AvgContainersPerHour,
        string? InstalledAtDockCode,
        // Mobile Equipment specific
        string? MobileEquipmentType,
        int? MaxSpeedKph,
        int? ContainersPerTrip,
        string? CurrentDockCode
    );
}
