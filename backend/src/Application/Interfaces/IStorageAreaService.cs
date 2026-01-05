using DDDNetCore.Application.DTOs.StorageAreas;
using DDDNetCore.Domain.StorageAreas;

namespace DDDNetCore.Application.Interfaces;

public interface IStorageAreaService
{
    /// <summary>
    /// Gets all storage areas with optional filtering
    /// </summary>
    Task<List<StorageAreaResponseDto>> GetAllAsync(
        string? name = null,
        string? location = null,
        StorageAreaType? type = null,
        bool? servesAllDocks = null);

    /// <summary>
    /// Gets a specific storage area by ID
    /// </summary>
    Task<StorageAreaResponseDto?> GetByIdAsync(string id);

    /// <summary>
    /// Creates a new storage area
    /// </summary>
    Task<StorageAreaResponseDto> CreateAsync(CreateStorageAreaDto dto);

    /// <summary>
    /// Updates an existing storage area
    /// </summary>
    Task<StorageAreaResponseDto> UpdateAsync(string id, UpdateStorageAreaDto dto);

    /// <summary>
    /// Updates occupancy of a storage area
    /// </summary>
    Task<StorageAreaResponseDto> UpdateOccupancyAsync(string id, UpdateOccupancyDto dto);

    /// <summary>
    /// Deletes a storage area
    /// </summary>
    Task DeleteAsync(string id);
}
