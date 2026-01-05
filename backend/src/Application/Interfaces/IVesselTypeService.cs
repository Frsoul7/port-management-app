using System.Collections.Generic;
using System.Threading.Tasks;
using DDDNetCore.Domain.Vessels;
using DDDNetCore.Application.DTOs.VesselTypes;

namespace DDDNetCore.Application.Interfaces
{
    /// <summary>
    /// Application service interface for vessel type business logic.
    /// Handles vessel type-related operations including CRUD and search.
    /// </summary>
    public interface IVesselTypeService
    {
        /// <summary>
        /// Creates a new vessel type.
        /// </summary>
        /// <param name="dto">The data transfer object containing vessel type creation data.</param>
        /// <returns>The created vessel type DTO.</returns>
        /// <exception cref="System.ArgumentException">Thrown when validation fails.</exception>
        /// <exception cref="System.InvalidOperationException">Thrown when business rules are violated (e.g., duplicate name).</exception>
        Task<VesselTypeResponseDto> CreateVesselTypeAsync(CreateVesselTypeDto dto);

        /// <summary>
        /// Updates an existing vessel type.
        /// </summary>
        /// <param name="id">The ID of the vessel type to update.</param>
        /// <param name="dto">The data transfer object containing updated vessel type data.</param>
        /// <returns>The updated vessel type DTO.</returns>
        /// <exception cref="System.Collections.Generic.KeyNotFoundException">Thrown when vessel type is not found.</exception>
        /// <exception cref="System.ArgumentException">Thrown when validation fails.</exception>
        /// <exception cref="System.InvalidOperationException">Thrown when business rules are violated (e.g., duplicate name).</exception>
        Task<VesselTypeResponseDto> UpdateVesselTypeAsync(string id, UpdateVesselTypeDto dto);

        /// <summary>
        /// Gets a vessel type by its ID.
        /// </summary>
        /// <param name="id">The ID of the vessel type.</param>
        /// <returns>The vessel type DTO if found; otherwise null.</returns>
        Task<VesselTypeResponseDto?> GetVesselTypeByIdAsync(string id);

        /// <summary>
        /// Searches vessel types with filters and pagination.
        /// </summary>
        /// <param name="name">Optional name filter (case-insensitive contains).</param>
        /// <param name="description">Optional description filter (case-insensitive contains).</param>
        /// <param name="page">Page number (1-based).</param>
        /// <param name="pageSize">Number of items per page.</param>
        /// <returns>Tuple containing the list of vessel type DTOs and total count.</returns>
        Task<(List<VesselTypeResponseDto> Items, int Total)> SearchVesselTypesAsync(string? name, string? description, int page, int pageSize);
    }
}
