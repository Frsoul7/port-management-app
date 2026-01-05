using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDNetCore.Domain.Vessels;
using DDDNetCore.Application.DTOs.Vessels;

namespace DDDNetCore.Application.Interfaces
{
    /// <summary>
    /// Application service interface for vessel business logic.
    /// Handles vessel-related operations including creation, updates, and queries.
    /// </summary>
    public interface IVesselService
    {
        /// <summary>
        /// Creates a new vessel with validation.
        /// </summary>
        /// <param name="dto">The data transfer object containing vessel creation data.</param>
        /// <param name="callerRole">The role of the caller (for authorization).</param>
        /// <param name="callerOrgId">The organization ID of the caller (for authorization).</param>
        /// <returns>The created vessel DTO.</returns>
        /// <exception cref="UnauthorizedAccessException">Thrown when caller lacks permission.</exception>
        /// <exception cref="ArgumentException">Thrown when validation fails.</exception>
        /// <exception cref="InvalidOperationException">Thrown when business rules are violated.</exception>
        Task<VesselResponseDto> CreateVesselAsync(CreateVesselDto dto, string callerRole, Guid? callerOrgId);

        /// <summary>
        /// Updates an existing vessel.
        /// </summary>
        /// <param name="imo">The IMO number of the vessel to update.</param>
        /// <param name="dto">The data transfer object containing updated vessel data.</param>
        /// <returns>The updated vessel DTO.</returns>
        /// <exception cref="KeyNotFoundException">Thrown when vessel is not found.</exception>
        /// <exception cref="ArgumentException">Thrown when validation fails.</exception>
        /// <exception cref="InvalidOperationException">Thrown when business rules are violated.</exception>
        Task<VesselResponseDto> UpdateVesselAsync(string imo, UpdateVesselDto dto);

        /// <summary>
        /// Gets a vessel by its IMO number.
        /// </summary>
        /// <param name="imo">The IMO number of the vessel.</param>
        /// <returns>The vessel DTO if found; otherwise null.</returns>
        Task<VesselResponseDto?> GetVesselByImoAsync(string imo);

        /// <summary>
        /// Searches for vessels based on multiple criteria.
        /// </summary>
        /// <param name="imo">Optional IMO number filter.</param>
        /// <param name="name">Optional name filter (partial match).</param>
        /// <param name="organizationName">Optional organization name filter (partial match).</param>
        /// <returns>List of vessel DTOs matching the search criteria.</returns>
        Task<List<VesselResponseDto>> SearchVesselsAsync(
            string? imo,
            string? name,
            string? organizationName);
    }
}
