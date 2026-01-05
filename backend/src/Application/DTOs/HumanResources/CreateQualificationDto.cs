namespace DDDNetCore.Application.DTOs.HumanResources;

public record CreateQualificationDto(
    string QualificationId,  // Código único (e.g., "STS_CRANE_OP")
    string Name,             // Nome descritivo (e.g., "STS Crane Operator")
    string? Description      // Descrição opcional
);
