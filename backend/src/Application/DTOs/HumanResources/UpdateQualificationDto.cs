namespace DDDNetCore.Application.DTOs.HumanResources;

public record UpdateQualificationDto(
    string Name,             // Novo nome
    string? Description      // Nova descrição
);
