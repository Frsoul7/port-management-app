using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDNetCore.Domain.Organizations;
using DDDNetCore.Application.DTOs.Organizations;

namespace DDDNetCore.Application.Interfaces
{
    /// <summary>
    /// Application service interface for organization business logic.
    /// Handles organization-related operations including creation, representative management, and queries.
    /// </summary>
    public interface IOrganizationService
    {
        /// <summary>
        /// Gets all organizations.
        /// </summary>
        /// <returns>List of all organization DTOs.</returns>
        Task<List<OrganizationResponseDto>> GetAllOrganizationsAsync();

        /// <summary>
        /// Gets an organization by its GUID identifier.
        /// </summary>
        /// <param name="id">The GUID identifier of the organization.</param>
        /// <returns>The organization DTO if found; otherwise null.</returns>
        Task<OrganizationResponseDto?> GetOrganizationByIdAsync(string id);

        /// <summary>
        /// Creates a new organization with validation.
        /// </summary>
        /// <param name="dto">The data transfer object containing organization creation data.</param>
        /// <returns>The created organization DTO.</returns>
        /// <exception cref="ArgumentException">Thrown when validation fails.</exception>
        /// <exception cref="InvalidOperationException">Thrown when business rules are violated (e.g., duplicate identifier).</exception>
        Task<OrganizationResponseDto> CreateOrganizationAsync(CreateOrganizationDto dto);

        /// <summary>
        /// Adds representatives to an existing organization (by GUID).
        /// </summary>
        /// <param name="organizationId">The GUID of the organization.</param>
        /// <param name="dto">The data transfer object containing representatives to add.</param>
        /// <returns>The updated organization DTO.</returns>
        /// <exception cref="KeyNotFoundException">Thrown when organization is not found.</exception>
        /// <exception cref="ArgumentException">Thrown when validation fails.</exception>
        /// <exception cref="InvalidOperationException">Thrown when business rules are violated.</exception>
        Task<OrganizationResponseDto> AddRepresentativesByIdAsync(Guid organizationId, AddRepresentativesDto dto);

        /// <summary>
        /// Updates a representative's information.
        /// </summary>
        /// <param name="organizationId">The GUID of the organization.</param>
        /// <param name="repId">The GUID of the representative to update.</param>
        /// <param name="dto">The data transfer object containing updated representative data.</param>
        /// <returns>The updated representative DTO.</returns>
        /// <exception cref="KeyNotFoundException">Thrown when organization or representative is not found.</exception>
        /// <exception cref="ArgumentException">Thrown when validation fails.</exception>
        /// <exception cref="InvalidOperationException">Thrown when business rules are violated.</exception>
        Task<RepresentativeResponseDto> UpdateRepresentativeAsync(Guid organizationId, Guid repId, UpdateRepresentativeDto dto);

        /// <summary>
        /// Patches a representative's status (activate/deactivate).
        /// </summary>
        /// <param name="organizationId">The GUID of the organization.</param>
        /// <param name="repId">The GUID of the representative to update.</param>
        /// <param name="dto">The data transfer object containing status update.</param>
        /// <returns>The updated representative DTO.</returns>
        /// <exception cref="KeyNotFoundException">Thrown when organization or representative is not found.</exception>
        Task<RepresentativeResponseDto> PatchRepresentativeStatusAsync(Guid organizationId, Guid repId, PatchRepresentativeStatusDto dto);

        /// <summary>
        /// Adds representatives to an existing organization (by identifier).
        /// </summary>
        /// <param name="identifier">The alphanumeric identifier of the organization.</param>
        /// <param name="dto">The data transfer object containing representatives to add.</param>
        /// <returns>The updated organization DTO.</returns>
        /// <exception cref="KeyNotFoundException">Thrown when organization is not found.</exception>
        /// <exception cref="ArgumentException">Thrown when validation fails.</exception>
        /// <exception cref="InvalidOperationException">Thrown when business rules are violated.</exception>
        Task<OrganizationResponseDto> AddRepresentativesByIdentifierAsync(string identifier, AddRepresentativesDto dto);
    }
}
