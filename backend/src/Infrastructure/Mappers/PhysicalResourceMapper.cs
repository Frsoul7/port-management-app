using DDDNetCore.Application.DTOs.Resources;
using DDDNetCore.Application.DTOs.HumanResources;
using DDDNetCore.Domain.Resources;

namespace DDDNetCore.Infrastructure.Mappers;

public static class PhysicalResourceMapper
{
    /// <summary>
    /// Maps a PhysicalResource domain entity to PhysicalResourceResponseDto
    /// </summary>
    public static PhysicalResourceResponseDto ToDto(PhysicalResource resource)
    {
        return resource switch
        {
            STSCrane crane => new PhysicalResourceResponseDto(
                ResourceId: crane.ResourceId,
                Code: crane.Code,
                Description: crane.Description,
                ResourceType: "STS_CRANE",
                Availability: crane.Availability.ToString(),
                SetupTimeSeconds: crane.SetupTimeSeconds,
                RequiredQualifications: crane.RequiredQualifications.Select(q => new QualificationDto(
                    QualificationId: q.QualificationId,
                    Name: q.Name,
                    Description: q.Description
                )).ToList(),
                CreatedAt: crane.CreatedAt,
                DeactivatedAt: crane.DeactivatedAt,
                DeactivationReason: crane.DeactivationReason,
                AvgContainersPerHour: crane.AvgContainersPerHour,
                InstalledAtDockCode: crane.InstalledAtDockCode,
                MobileEquipmentType: null,
                MaxSpeedKph: null,
                ContainersPerTrip: null,
                CurrentDockCode: null
            ),
            MobileEquipment equipment => new PhysicalResourceResponseDto(
                ResourceId: equipment.ResourceId,
                Code: equipment.Code,
                Description: equipment.Description,
                ResourceType: "MOBILE_EQUIPMENT",
                Availability: equipment.Availability.ToString(),
                SetupTimeSeconds: equipment.SetupTimeSeconds,
                RequiredQualifications: equipment.RequiredQualifications.Select(q => new QualificationDto(
                    QualificationId: q.QualificationId,
                    Name: q.Name,
                    Description: q.Description
                )).ToList(),
                CreatedAt: equipment.CreatedAt,
                DeactivatedAt: equipment.DeactivatedAt,
                DeactivationReason: equipment.DeactivationReason,
                AvgContainersPerHour: null,
                InstalledAtDockCode: null,
                MobileEquipmentType: equipment.MobileEquipmentType.ToString(),
                MaxSpeedKph: equipment.MaxSpeedKph,
                ContainersPerTrip: equipment.ContainersPerTrip,
                CurrentDockCode: equipment.CurrentDockCode
            ),
            _ => throw new ArgumentException($"Unknown resource type: {resource.GetType().Name}")
        };
    }

    /// <summary>
    /// Maps a CreatePhysicalResourceDto to a PhysicalResource domain entity
    /// </summary>
    public static PhysicalResource ToEntity(CreatePhysicalResourceDto dto)
    {
        return dto.ResourceType.ToUpperInvariant() switch
        {
            "STS_CRANE" => new STSCrane(
                code: dto.Code,
                description: dto.Description,
                setupTimeSeconds: dto.SetupTimeSeconds,
                avgContainersPerHour: dto.AvgContainersPerHour ?? 0,
                installedAtDockCode: dto.InstalledAtDockCode
            ),
            "MOBILE_EQUIPMENT" => new MobileEquipment(
                code: dto.Code,
                description: dto.Description,
                setupTimeSeconds: dto.SetupTimeSeconds,
                type: dto.MobileType ?? MobileEquipmentType.TRUCK,
                maxSpeedKph: dto.MaxSpeedKph,
                containersPerTrip: dto.ContainersPerTrip,
                avgContainersPerHour: dto.AvgContainersPerHour
            ),
            _ => throw new ArgumentException($"Invalid resource type: {dto.ResourceType}")
        };
    }

    /// <summary>
    /// Maps an UpdatePhysicalResourceDto to update an existing PhysicalResource entity
    /// </summary>
    public static void UpdateEntity(PhysicalResource resource, UpdatePhysicalResourceDto dto)
    {
        if (resource is STSCrane crane)
        {
            crane.Update(
                description: dto.Description,
                setupTimeSeconds: dto.SetupTimeSeconds ?? crane.SetupTimeSeconds,
                avgContainersPerHour: dto.AvgContainersPerHour ?? crane.AvgContainersPerHour,
                installedAtDockCode: dto.InstalledAtDockCode
            );
        }
        else if (resource is MobileEquipment equipment)
        {
            equipment.Update(
                description: dto.Description,
                setupTimeSeconds: dto.SetupTimeSeconds ?? equipment.SetupTimeSeconds,
                maxSpeedKph: dto.MaxSpeedKph ?? equipment.MaxSpeedKph,
                containersPerTrip: dto.ContainersPerTrip ?? equipment.ContainersPerTrip,
                avgContainersPerHour: dto.AvgContainersPerHour ?? equipment.AvgContainersPerHour
            );
        }
    }
}
