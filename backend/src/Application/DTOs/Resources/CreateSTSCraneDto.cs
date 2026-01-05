using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;

namespace DDDNetCore.Application.DTOs.Resources
{
    public record CreateSTSCraneDto(
        [Required, MaxLength(20)] string Code,
        string? Description,
        [Range(0, int.MaxValue)] int SetupTimeSeconds,
        [Range(1, int.MaxValue)] int AvgContainersPerHour,
        string? InstalledAtDockCode = null,
        List<string>? RequiredQualificationIds = null
    );
}
