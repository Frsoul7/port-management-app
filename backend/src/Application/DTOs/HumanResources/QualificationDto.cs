namespace DDDNetCore.Application.DTOs.HumanResources
{
    public record QualificationDto(
        string QualificationId,
        string Name,
        string? Description
    );
}
