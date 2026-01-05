using DDDNetCore.Application.DTOs.StorageAreas;
using DDDNetCore.Domain.StorageAreas;

namespace DDDNetCore.Infrastructure.Mappers;

public static class StorageAreaMapper
{
    /// <summary>
    /// Maps a StorageArea domain entity to StorageAreaResponseDto
    /// </summary>
    public static StorageAreaResponseDto ToDto(StorageArea storageArea)
    {
        return new StorageAreaResponseDto(
            StorageAreaId: storageArea.StorageAreaId,
            Name: storageArea.Name,
            Location: storageArea.Location,
            MaxCapacityTEU: storageArea.MaxCapacityTEU,
            CurrentOccupancyTEU: storageArea.CurrentOccupancyTEU,
            Type: storageArea.Type,
            ServesAllDocks: storageArea.ServesAllDocks,
            ServedDockCodes: storageArea.Docks.Select(d => d.Code).ToList(),
            YardNotes: storageArea.YardSpec?.Notes,
            WarehouseNotes: storageArea.WarehouseSpec?.Notes
        );
    }

    /// <summary>
    /// Maps a CreateStorageAreaDto to a StorageArea domain entity
    /// </summary>
    public static StorageArea ToEntity(CreateStorageAreaDto dto)
    {
        var storageArea = new StorageArea(
            name: dto.Name,
            location: dto.Location,
            maxCapacityTEU: dto.MaxCapacityTEU,
            type: dto.Type,
            servesAllDocks: dto.ServesAllDocks
        );

        // Set type-specific details if provided
        if (dto.Type == StorageAreaType.YARD && !string.IsNullOrWhiteSpace(dto.YardNotes))
        {
            storageArea.SetYardSpec(dto.YardNotes);
        }
        else if (dto.Type == StorageAreaType.WAREHOUSE && !string.IsNullOrWhiteSpace(dto.WarehouseNotes))
        {
            storageArea.SetWarehouseSpec(dto.WarehouseNotes);
        }

        return storageArea;
    }

    /// <summary>
    /// Maps an UpdateStorageAreaDto to update an existing StorageArea entity
    /// </summary>
    public static void UpdateEntity(StorageArea storageArea, UpdateStorageAreaDto dto)
    {
        storageArea.Update(
            name: dto.Name,
            location: dto.Location,
            maxCapacityTEU: dto.MaxCapacityTEU,
            servesAllDocks: storageArea.ServesAllDocks  // Keep existing value or get from dto if needed
        );

        // Update type-specific details if provided
        if (storageArea.Type == StorageAreaType.YARD && !string.IsNullOrWhiteSpace(dto.YardNotes))
        {
            storageArea.SetYardSpec(dto.YardNotes);
        }
        else if (storageArea.Type == StorageAreaType.WAREHOUSE && !string.IsNullOrWhiteSpace(dto.WarehouseNotes))
        {
            storageArea.SetWarehouseSpec(dto.WarehouseNotes);
        }
    }
}
