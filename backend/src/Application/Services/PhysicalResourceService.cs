using DDDNetCore.Application.DTOs.Resources;
using DDDNetCore.Application.Interfaces;
using DDDNetCore.Domain.HumanResources;
using DDDNetCore.Domain.IRepository;
using DDDNetCore.Domain.Resources;
using DDDNetCore.Infrastructure.Mappers;

namespace DDDNetCore.Application.Services;

public class PhysicalResourceService : IPhysicalResourceService
{
    private readonly IPhysicalResourceRepository _resourceRepo;
    private readonly IUnitOfWork _unitOfWork;

    public PhysicalResourceService(IPhysicalResourceRepository resourceRepo, IUnitOfWork unitOfWork)
    {
        _resourceRepo = resourceRepo;
        _unitOfWork = unitOfWork;
    }

    public async Task<List<PhysicalResourceResponseDto>> GetAllAsync(string? code, string? description, PhysicalResourceAvailability? availability)
    {
        var resources = !string.IsNullOrWhiteSpace(code) || !string.IsNullOrWhiteSpace(description) || availability.HasValue
            ? await _resourceRepo.SearchAsync(code, description, availability)
            : await _resourceRepo.GetAllAsync(includeQualifications: true);

        return resources.Select(PhysicalResourceMapper.ToDto).ToList();
    }

    public async Task<PhysicalResourceResponseDto?> GetByCodeAsync(string code)
    {
        var resource = await _resourceRepo.GetByCodeAsync(code, includeQualifications: true);
        return resource == null ? null : PhysicalResourceMapper.ToDto(resource);
    }

    public async Task<PhysicalResourceResponseDto> CreateResourceAsync(CreatePhysicalResourceDto dto)
    {
        if (await _resourceRepo.ExistsAsync(dto.Code))
            throw new InvalidOperationException($"Resource with code '{dto.Code}' already exists.");

        PhysicalResource resource;

        if (dto.ResourceType.Equals("STS_CRANE", StringComparison.OrdinalIgnoreCase))
        {
            if (!dto.AvgContainersPerHour.HasValue)
                throw new ArgumentException("AvgContainersPerHour is required for STS Cranes.");

            resource = new STSCrane(
                dto.Code,
                dto.Description,
                dto.SetupTimeSeconds,
                dto.AvgContainersPerHour.Value,
                dto.InstalledAtDockCode
            );
        }
        else if (dto.ResourceType.Equals("MOBILE_EQUIPMENT", StringComparison.OrdinalIgnoreCase))
        {
            if (!dto.MobileType.HasValue)
                throw new ArgumentException("MobileType is required for Mobile Equipment.");

            resource = new MobileEquipment(
                dto.Code,
                dto.Description,
                dto.SetupTimeSeconds,
                dto.MobileType.Value,
                dto.MaxSpeedKph,
                dto.ContainersPerTrip,
                dto.AvgContainersPerHour
            );
        }
        else
        {
            throw new ArgumentException($"Invalid ResourceType '{dto.ResourceType}'. Must be 'STS_CRANE' or 'MOBILE_EQUIPMENT'.");
        }

        await SetQualifications(resource, dto.RequiredQualificationIds);
        await _resourceRepo.AddAsync(resource);
        await _unitOfWork.CommitAsync();

        return PhysicalResourceMapper.ToDto(resource);
    }

    public async Task<PhysicalResourceResponseDto> CreateSTSCraneAsync(CreateSTSCraneDto dto)
    {
        if (await _resourceRepo.ExistsAsync(dto.Code))
            throw new InvalidOperationException($"Resource with code '{dto.Code}' already exists.");

        var crane = new STSCrane(
            dto.Code,
            dto.Description,
            dto.SetupTimeSeconds,
            dto.AvgContainersPerHour,
            dto.InstalledAtDockCode
        );

        await SetQualifications(crane, dto.RequiredQualificationIds);
        await _resourceRepo.AddAsync(crane);
        await _unitOfWork.CommitAsync();

        return PhysicalResourceMapper.ToDto(crane);
    }

    public async Task<PhysicalResourceResponseDto> CreateMobileEquipmentAsync(CreateMobileEquipmentDto dto)
    {
        if (await _resourceRepo.ExistsAsync(dto.Code))
            throw new InvalidOperationException($"Resource with code '{dto.Code}' already exists.");

        var equipment = new MobileEquipment(
            dto.Code,
            dto.Description,
            dto.SetupTimeSeconds,
            dto.Type,
            dto.MaxSpeedKph,
            dto.ContainersPerTrip,
            dto.AvgContainersPerHour
        );

        await SetQualifications(equipment, dto.RequiredQualificationIds);
        await _resourceRepo.AddAsync(equipment);
        await _unitOfWork.CommitAsync();

        return PhysicalResourceMapper.ToDto(equipment);
    }

    public async Task<PhysicalResourceResponseDto> UpdateResourceAsync(string code, UpdatePhysicalResourceDto dto)
    {
        var resource = await _resourceRepo.GetByCodeAsync(code, includeQualifications: true);
        if (resource == null)
            throw new KeyNotFoundException($"Resource with code '{code}' not found.");

        if (resource is STSCrane crane)
        {
            crane.Update(
                dto.Description,
                dto.SetupTimeSeconds ?? crane.SetupTimeSeconds,
                dto.AvgContainersPerHour ?? crane.AvgContainersPerHour,
                dto.InstalledAtDockCode ?? crane.InstalledAtDockCode
            );
        }
        else if (resource is MobileEquipment equipment)
        {
            equipment.Update(
                dto.Description,
                dto.SetupTimeSeconds ?? equipment.SetupTimeSeconds,
                dto.MaxSpeedKph ?? equipment.MaxSpeedKph,
                dto.ContainersPerTrip ?? equipment.ContainersPerTrip,
                dto.AvgContainersPerHour ?? equipment.AvgContainersPerHour
            );
        }
        else
        {
            throw new ArgumentException($"Unknown resource type for '{code}'.");
        }

        await SetQualifications(resource, dto.RequiredQualificationIds);
        _resourceRepo.Update(resource);
        await _unitOfWork.CommitAsync();

        return PhysicalResourceMapper.ToDto(resource);
    }

    public async Task<PhysicalResourceResponseDto> UpdateSTSCraneAsync(string code, UpdateSTSCraneDto dto)
    {
        var resource = await _resourceRepo.GetByCodeAsync(code, includeQualifications: true);
        if (resource == null)
            throw new KeyNotFoundException($"Resource with code '{code}' not found.");

        if (resource is not STSCrane crane)
            throw new ArgumentException($"Resource '{code}' is not an STS Crane.");

        crane.Update(dto.Description, dto.SetupTimeSeconds, dto.AvgContainersPerHour, dto.InstalledAtDockCode);
        await SetQualifications(crane, dto.RequiredQualificationIds);

        _resourceRepo.Update(crane);
        await _unitOfWork.CommitAsync();

        return PhysicalResourceMapper.ToDto(crane);
    }

    public async Task<PhysicalResourceResponseDto> UpdateMobileEquipmentAsync(string code, UpdateMobileEquipmentDto dto)
    {
        var resource = await _resourceRepo.GetByCodeAsync(code, includeQualifications: true);
        if (resource == null)
            throw new KeyNotFoundException($"Resource with code '{code}' not found.");

        if (resource is not MobileEquipment equipment)
            throw new ArgumentException($"Resource '{code}' is not Mobile Equipment.");

        equipment.Update(dto.Description, dto.SetupTimeSeconds, dto.MaxSpeedKph, dto.ContainersPerTrip, dto.AvgContainersPerHour);
        await SetQualifications(equipment, dto.RequiredQualificationIds);

        _resourceRepo.Update(equipment);
        await _unitOfWork.CommitAsync();

        return PhysicalResourceMapper.ToDto(equipment);
    }

    public async Task<PhysicalResourceResponseDto> PatchResourceAsync(string code, PatchPhysicalResourceDto dto)
    {
        var resource = await _resourceRepo.GetByCodeAsync(code, includeQualifications: true);
        if (resource == null)
            throw new KeyNotFoundException($"Resource with code '{code}' not found.");

        // Handle activation/deactivation
        if (dto.IsActive.HasValue)
        {
            if (dto.IsActive.Value)
            {
                resource.Activate();
            }
            else
            {
                if (string.IsNullOrWhiteSpace(dto.DeactivationReason))
                    throw new ArgumentException("DeactivationReason is required when deactivating a resource.");

                resource.Deactivate(dto.DeactivationReason);
            }
        }

        // Handle availability change
        if (dto.Availability.HasValue)
        {
            resource.SetAvailability(dto.Availability.Value);
        }

        _resourceRepo.Update(resource);
        await _unitOfWork.CommitAsync();

        return PhysicalResourceMapper.ToDto(resource);
    }

    private async Task SetQualifications(PhysicalResource resource, List<string>? qualificationIds)
    {
        if (qualificationIds != null && qualificationIds.Any())
        {
            var qualifications = new List<StaffMemberQualification>();
            foreach (var qualId in qualificationIds)
            {
                var qual = await _unitOfWork.Qualifications.GetByIdAsync(qualId);
                if (qual == null)
                    throw new ArgumentException($"Qualification not found: {qualId}");
                qualifications.Add(qual);
            }

            resource.SetRequiredQualifications(qualifications);
        }
        else
        {
            resource.SetRequiredQualifications(new List<StaffMemberQualification>());
        }
    }
}
