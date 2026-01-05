using DDDNetCore.Domain.Common;
using DDDNetCore.Domain.HumanResources;

namespace DDDNetCore.Application.DTOs.HumanResources;

/// <summary>
/// DTO for PATCH operations on staff members, supporting status changes (activate/deactivate)
/// and other partial updates
/// </summary>
public record PatchStaffStatusDto(
    EntityActiveStatus? ActivityStatus,  // Active/Inactive (for activate/deactivate)
    HumanResourceStatus? Status,         // Operational status (Available/Unavailable/etc)
    string? Email,
    string? Phone,
    TimeSpan? StartHour,
    TimeSpan? EndHour
);
