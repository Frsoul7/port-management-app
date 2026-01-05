using DDDNetCore.Application.DTOs.HumanResources;
using DDDNetCore.Domain.HumanResources;

namespace DDDNetCore.Infrastructure.Mappers;

public static class QualificationMapper
{
    /// <summary>
    /// Maps a StaffMemberQualification domain entity to QualificationDto
    /// </summary>
    public static QualificationDto ToDto(StaffMemberQualification qualification)
    {
        return new QualificationDto(
            QualificationId: qualification.QualificationId,
            Name: qualification.Name,
            Description: qualification.Description
        );
    }
}
