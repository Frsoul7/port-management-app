namespace DDDNetCore.Application.DTOs.HumanResources;

using DDDNetCore.Domain.HumanResources;

public record CreateStaffMemberDto(
    long MecanographicNumber,
    string ShortName,
    string Email,
    string Phone,
    HumanResourceStatus Status,
    string StartHour,  
    string EndHour     
);
