using System;
using System.Collections.Generic;

namespace DDDNetCore.Application.DTOs.HumanResources
{
    public record StaffMemberResponseDto(
        int MecanographicNumber,
        string ShortName,
        string Email,
        string Phone,
        string Status,
        TimeSpan StartTime,
        TimeSpan EndTime,
        List<QualificationDto> Qualifications,
        DateTime CreatedAt,
        DateTime? DeactivatedAt
    );
}
