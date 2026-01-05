using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;

namespace DDDNetCore.Application.DTOs.Resources
{
    public record UpdateMobileEquipmentDto(
        string? Description,
        [Range(0, int.MaxValue)] int SetupTimeSeconds,
        int? MaxSpeedKph = null,
        int? ContainersPerTrip = null,
        int? AvgContainersPerHour = null,
        List<string>? RequiredQualificationIds = null
    );
}
