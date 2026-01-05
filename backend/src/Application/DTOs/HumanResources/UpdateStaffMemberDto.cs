namespace DDDNetCore.Application.DTOs.HumanResources;

using DDDNetCore.Domain.HumanResources;

public record UpdateStaffMemberDto(
    string? Email,                   // Email opcional
    string? Phone,                   // Telefone opcional
    HumanResourceStatus? Status,     // Estado opcional
    TimeSpan? StartHour,             // Hora de início opcional
    TimeSpan? EndHour                // Hora de fim opcional
);
