using DDDNetCore.Application.DTOs.HumanResources;
using DDDNetCore.Domain.Common;
using DDDNetCore.Domain.HumanResources;

namespace DDDNetCore.Infrastructure.Mappers;

public static class StaffMemberMapper
{
    /// <summary>
    /// Maps a StaffMember domain entity to StaffMemberResponseDto
    /// </summary>
    public static StaffMemberResponseDto ToDto(StaffMember staffMember)
    {
        return new StaffMemberResponseDto(
            MecanographicNumber: (int)staffMember.MecanographicNumber,
            ShortName: staffMember.ShortName,
            Email: staffMember.Email,
            Phone: staffMember.Phone,
            Status: staffMember.Status.ToString(),
            StartTime: staffMember.StartHour,
            EndTime: staffMember.EndHour,
            Qualifications: staffMember.Qualifications.Select(q => new QualificationDto(
                QualificationId: q.QualificationId,
                Name: q.Name,
                Description: q.Description
            )).ToList(),
            CreatedAt: DateTime.UtcNow, // Consider adding CreatedAt to domain if needed
            DeactivatedAt: staffMember.ActivityStatus == EntityActiveStatus.INACTIVE ? DateTime.UtcNow : null
        );
    }

    /// <summary>
    /// Maps a CreateStaffMemberDto to a StaffMember domain entity
    /// </summary>
    public static StaffMember ToEntity(CreateStaffMemberDto dto)
    {
        var startHour = TimeSpan.Parse(dto.StartHour);
        var endHour = TimeSpan.Parse(dto.EndHour);

        return new StaffMember(
            mecanographicNumber: dto.MecanographicNumber,
            shortName: dto.ShortName,
            email: dto.Email,
            phone: dto.Phone,
            status: dto.Status,
            startHour: startHour,
            endHour: endHour,
            activityStatus: EntityActiveStatus.ACTIVE
        );
    }

    /// <summary>
    /// Maps an UpdateStaffMemberDto to update an existing StaffMember entity
    /// </summary>
    public static void UpdateEntity(StaffMember staffMember, UpdateStaffMemberDto dto)
    {
        // Update logic would go here when UpdateStaffMemberDto is implemented
        // For now, this is a placeholder
    }
}
