using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using DDDNetCore.Domain.Resources;

namespace DDDNetCore.Application.DTOs.Resources
{
    public record CreateMobileEquipmentDto(
        [Required, MaxLength(20)] string Code,
        string? Description,
        [Range(0, int.MaxValue)] int SetupTimeSeconds,
        MobileEquipmentType Type,
        int? MaxSpeedKph = null,
        int? ContainersPerTrip = null,
        int? AvgContainersPerHour = null,
        List<string>? RequiredQualificationIds = null
    );
}
