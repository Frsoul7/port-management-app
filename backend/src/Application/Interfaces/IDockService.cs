using DDDNetCore.Application.DTOs.Docks;

namespace DDDNetCore.Application.Interfaces;

public interface IDockService
{
    /// <summary>
    /// Gets all docks with optional filtering
    /// </summary>
    Task<List<DockResponseDto>> GetAllAsync(string? name = null, string? location = null, string? vesselTypeId = null);

    /// <summary>
    /// Gets a specific dock by code
    /// </summary>
    Task<DockResponseDto?> GetByCodeAsync(string code);

    /// <summary>
    /// Creates a new dock
    /// </summary>
    Task<DockResponseDto> CreateAsync(CreateDockDto dto);

    /// <summary>
    /// Updates an existing dock
    /// </summary>
    Task<DockResponseDto> UpdateAsync(string code, UpdateDockDto dto);

    /// <summary>
    /// Deletes a dock
    /// </summary>
    Task DeleteAsync(string code);
}
