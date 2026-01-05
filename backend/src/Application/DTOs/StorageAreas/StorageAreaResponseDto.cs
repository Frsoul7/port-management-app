using DDDNetCore.Domain.StorageAreas;

namespace DDDNetCore.Application.DTOs.StorageAreas;

public record StorageAreaResponseDto(
    string StorageAreaId,
    string Name,
    string Location,
    int MaxCapacityTEU,
    int CurrentOccupancyTEU,
    StorageAreaType Type,
    bool ServesAllDocks,
    List<string> ServedDockCodes,
    string? YardNotes,
    string? WarehouseNotes
);
