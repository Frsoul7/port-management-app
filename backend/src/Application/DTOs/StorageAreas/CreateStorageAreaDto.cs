using System.ComponentModel.DataAnnotations;
using DDDNetCore.Domain.StorageAreas;

namespace DDDNetCore.Application.DTOs.StorageAreas;

public record CreateStorageAreaDto(
    [Required] string Name,
    [Required] string Location,
    [Range(1, int.MaxValue)] int MaxCapacityTEU,
    StorageAreaType Type,
    bool ServesAllDocks = true,
    List<string>? ServedDockCodes = null,
    string? YardNotes = null,
    string? WarehouseNotes = null
);
