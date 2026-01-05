using DDDNetCore.Application.DTOs.Resources;
using DDDNetCore.Domain.Resources;

namespace DDDNetCore.Application.Interfaces;

public interface IPhysicalResourceService
{
    Task<List<PhysicalResourceResponseDto>> GetAllAsync(string? code, string? description, PhysicalResourceAvailability? availability);
    Task<PhysicalResourceResponseDto?> GetByCodeAsync(string code);
    Task<PhysicalResourceResponseDto> CreateResourceAsync(CreatePhysicalResourceDto dto);
    Task<PhysicalResourceResponseDto> CreateSTSCraneAsync(CreateSTSCraneDto dto);
    Task<PhysicalResourceResponseDto> CreateMobileEquipmentAsync(CreateMobileEquipmentDto dto);
    Task<PhysicalResourceResponseDto> UpdateResourceAsync(string code, UpdatePhysicalResourceDto dto);
    Task<PhysicalResourceResponseDto> UpdateSTSCraneAsync(string code, UpdateSTSCraneDto dto);
    Task<PhysicalResourceResponseDto> UpdateMobileEquipmentAsync(string code, UpdateMobileEquipmentDto dto);
    Task<PhysicalResourceResponseDto> PatchResourceAsync(string code, PatchPhysicalResourceDto dto);
}
