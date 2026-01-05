using System.ComponentModel.DataAnnotations;

namespace DDDNetCore.Application.DTOs.StorageAreas;

public record UpdateStorageAreaDto(
    [Required] string Name,
    [Required] string Location,
    [Range(1, int.MaxValue)] int MaxCapacityTEU,
    bool ServesAllDocks,
    List<string>? ServedDockCodes = null,
    string? YardNotes = null,
    string? WarehouseNotes = null
);
