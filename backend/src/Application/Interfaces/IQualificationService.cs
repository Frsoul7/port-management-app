using DDDNetCore.Domain.HumanResources;
using DDDNetCore.Application.DTOs.HumanResources;

namespace DDDNetCore.Application.Interfaces;

public interface IQualificationService
{
    Task<QualificationDto> CreateAsync(CreateQualificationDto dto);
    Task<List<QualificationDto>> SearchAsync(string? id, string? name);
    Task<QualificationDto> UpdateAsync(string id, UpdateQualificationDto dto);
    Task DeleteAsync(string id);
}
