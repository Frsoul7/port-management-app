using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDNetCore.Domain.Organizations;
using DDDNetCore.Domain.Visits;
using DDDNetCore.Domain.Visits.Crew;
using DDDNetCore.Domain.Visits.Manifests;
using DDDNetCore.Application.DTOs.Vvns;
using DDDNetCore.API.Contracts;

namespace DDDNetCore.Application.Interfaces
{
    /// <summary>
    /// Application service interface for vessel visit notification business logic.
    /// Handles VVN-related operations including creation, updates, manifest management, and submissions.
    /// </summary>
    public interface IVesselVisitService
    {
        /// <summary>
        /// Updates an existing VVN with new data.
        /// </summary>
        /// <param name="vvnId">The GUID of the VVN to update.</param>
        /// <param name="dto">The data transfer object containing updated VVN data.</param>
        /// <returns>The updated VVN.</returns>
        /// <exception cref="KeyNotFoundException">Thrown when VVN is not found.</exception>
        /// <exception cref="InvalidOperationException">Thrown when VVN is not editable.</exception>
        /// <exception cref="ArgumentException">Thrown when validation fails.</exception>
        Task<VvnSummaryResponse> UpdateVvnAsync(Guid vvnId, UpdateVvnRequest dto);

        /// <summary>
        /// Creates a new vessel visit notification.
        /// </summary>
        /// <param name="dto">The data transfer object containing VVN creation data.</param>
        /// <param name="orgId">The organization ID (must be a shipping agent).</param>
        /// <returns>The created VVN.</returns>
        /// <exception cref="ArgumentException">Thrown when validation fails.</exception>
        /// <exception cref="UnauthorizedAccessException">Thrown when organization is not a shipping agent.</exception>
        Task<VvnSummaryResponse> CreateVvnAsync(CreateVvnRequest dto, OrganizationId orgId);

        /// <summary>
        /// Replaces existing manifests for a VVN (legacy bulk-upsert).
        /// </summary>
        /// <param name="vvnId">The GUID of the VVN.</param>
        /// <param name="loading">The loading manifest (optional).</param>
        /// <param name="unloading">The unloading manifest (optional).</param>
        /// <exception cref="KeyNotFoundException">Thrown when VVN is not found.</exception>
        Task ReplaceManifestsAsync(Guid vvnId, CargoManifest? loading, CargoManifest? unloading);

        /// <summary>
        /// Sets the crew for a VVN.
        /// </summary>
        /// <param name="vvnId">The GUID of the VVN.</param>
        /// <param name="captainName">Name of the captain (optional).</param>
        /// <param name="members">List of crew members.</param>
        /// <exception cref="KeyNotFoundException">Thrown when VVN is not found.</exception>
        /// <exception cref="InvalidOperationException">Thrown when VVN is not editable.</exception>
        Task SetCrewAsync(Guid vvnId, string? captainName, IEnumerable<CrewMember> members);

        /// <summary>
        /// Gets manifest entries for a VVN.
        /// </summary>
        /// <param name="vvnId">The GUID of the VVN.</param>
        /// <param name="type">The type of manifest (optional - null returns all).</param>
        /// <returns>List of manifest entries.</returns>
        Task<IReadOnlyList<VvnEntryResponse>> GetEntriesAsync(Guid vvnId, ManifestType? type);

        /// <summary>
        /// Sets crew summary information for a VVN.
        /// </summary>
        /// <param name="vvnId">The GUID of the VVN.</param>
        /// <param name="captainName">Name of the captain.</param>
        /// <param name="captainCitizenId">Citizen ID of the captain.</param>
        /// <param name="captainNationality">Nationality of the captain.</param>
        /// <param name="crewCount">Number of crew members.</param>
        /// <exception cref="KeyNotFoundException">Thrown when VVN is not found.</exception>
        Task SetCrewSummaryAsync(Guid vvnId, string captainName, string captainCitizenId, string captainNationality, int crewCount);

        /// <summary>
        /// Updates a manifest entry.
        /// </summary>
        /// <param name="vvnId">The GUID of the VVN.</param>
        /// <param name="entryId">The GUID of the manifest entry.</param>
        /// <param name="mutate">Action to mutate the entry.</param>
        /// <exception cref="KeyNotFoundException">Thrown when VVN or entry is not found.</exception>
        Task UpdateManifestEntryAsync(Guid vvnId, Guid entryId, Action<ManifestEntry> mutate);

        /// <summary>
        /// Adds a manifest entry to a VVN.
        /// </summary>
        /// <param name="vvnId">The GUID of the VVN.</param>
        /// <param name="type">The type of manifest (Load or Unload).</param>
        /// <param name="entry">The manifest entry to add.</param>
        /// <returns>The GUID of the created entry.</returns>
        /// <exception cref="KeyNotFoundException">Thrown when VVN is not found.</exception>
        Task<Guid> AddManifestEntryAsync(Guid vvnId, ManifestType type, ManifestEntry entry);

        /// <summary>
        /// Removes a manifest entry from a VVN.
        /// </summary>
        /// <param name="vvnId">The GUID of the VVN.</param>
        /// <param name="type">The type of manifest (Load or Unload).</param>
        /// <param name="entryId">The GUID of the entry to remove.</param>
        /// <exception cref="KeyNotFoundException">Thrown when VVN or entry is not found.</exception>
        Task RemoveManifestEntryAsync(Guid vvnId, ManifestType type, Guid entryId);

        /// <summary>
        /// Submits a VVN for review.
        /// </summary>
        /// <param name="vvnId">The GUID of the VVN.</param>
        /// <param name="submittedByUserGuid">The GUID of the user submitting the VVN.</param>
        /// <returns>The submitted VVN.</returns>
        /// <exception cref="KeyNotFoundException">Thrown when VVN is not found.</exception>
        /// <exception cref="InvalidOperationException">Thrown when validation fails.</exception>
        /// <exception cref="UnauthorizedAccessException">Thrown when organization is not a shipping agent.</exception>
        Task<VesselVisitNotification> SubmitAsync(Guid vvnId, Guid submittedByUserGuid);

        /// <summary>
        /// Gets VVNs for a shipping agent organization with optional filters.
        /// </summary>
        /// <param name="organizationId">The organization ID.</param>
        /// <param name="vesselImo">Optional vessel IMO filter.</param>
        /// <param name="status">Optional status filter.</param>
        /// <param name="submittedById">Optional submitter ID filter.</param>
        /// <param name="fromDate">Optional start date filter.</param>
        /// <param name="toDate">Optional end date filter.</param>
        /// <returns>List of VVNs matching the filters.</returns>
        Task<List<VesselVisitNotification>> GetVvnsForOrganizationAsync(
            OrganizationId organizationId,
            string? vesselImo = null,
            VVNState? status = null,
            Guid? submittedById = null,
            DateTime? fromDate = null,
            DateTime? toDate = null);

        /// <summary>
        /// Gets all VVNs visible to the caller based on role and organization.
        /// Port Authority Officers and Logistics Operators can see all VVNs.
        /// Shipping Agents can only see their own organization's VVNs.
        /// </summary>
        /// <param name="callerRole">The role of the caller.</param>
        /// <param name="callerOrgId">The organization ID of the caller (required for Shipping Agents).</param>
        /// <returns>List of VVN summary responses.</returns>
        Task<List<VvnSummaryResponse>> GetAllVvnsAsync(Application.Security.AppRole callerRole, Guid? callerOrgId);

        /// <summary>
        /// Gets a specific VVN by ID with authorization checks.
        /// </summary>
        /// <param name="vvnId">The GUID of the VVN.</param>
        /// <param name="callerRole">The role of the caller.</param>
        /// <param name="callerOrgId">The organization ID of the caller (required for Shipping Agents).</param>
        /// <returns>The VVN summary response.</returns>
        /// <exception cref="KeyNotFoundException">Thrown when VVN is not found.</exception>
        /// <exception cref="UnauthorizedAccessException">Thrown when caller doesn't have permission to view the VVN.</exception>
        Task<VvnSummaryResponse> GetVvnByIdAsync(Guid vvnId, Application.Security.AppRole callerRole, Guid? callerOrgId);

        /// <summary>
        /// Approves a VVN and assigns it to a dock.
        /// </summary>
        /// <param name="vvnId">The GUID of the VVN to approve.</param>
        /// <param name="request">The approval request containing dock assignment details.</param>
        /// <param name="callerUserId">The user ID of the approver.</param>
        /// <param name="callerOrgId">The organization ID of the approver (optional).</param>
        /// <returns>The approval response with VVN and dock assignment details.</returns>
        /// <exception cref="KeyNotFoundException">Thrown when VVN or dock is not found.</exception>
        /// <exception cref="InvalidOperationException">Thrown when VVN is not in SUBMITTED state or dock is not available.</exception>
        Task<VvnApprovalResponse> ApproveVvnAsync(Guid vvnId, VvnApprovalRequest request, Guid callerUserId, Guid? callerOrgId);

        /// <summary>
        /// Rejects a VVN with a reason.
        /// </summary>
        /// <param name="vvnId">The GUID of the VVN to reject.</param>
        /// <param name="request">The rejection request containing reason and notes.</param>
        /// <param name="callerUserId">The user ID of the rejector.</param>
        /// <param name="callerOrgId">The organization ID of the rejector (optional).</param>
        /// <returns>The rejection response with VVN details.</returns>
        /// <exception cref="KeyNotFoundException">Thrown when VVN is not found.</exception>
        /// <exception cref="InvalidOperationException">Thrown when VVN is not in SUBMITTED state.</exception>
        /// <exception cref="ArgumentException">Thrown when reason is not provided.</exception>
        Task<VvnRejectionResponse> RejectVvnAsync(Guid vvnId, VvnRejectionRequest request, Guid callerUserId, Guid? callerOrgId);

        /// <summary>
        /// Validates if a manifest type is compatible with the VVN's visit purpose.
        /// </summary>
        /// <param name="vvnId">The GUID of the VVN.</param>
        /// <param name="manifestType">The manifest type to validate.</param>
        /// <returns>The VVN entity if validation passes.</returns>
        /// <exception cref="KeyNotFoundException">Thrown when VVN is not found.</exception>
        /// <exception cref="InvalidOperationException">Thrown when manifest type is incompatible with visit purpose.</exception>
        Task<VesselVisitNotification> ValidateManifestTypeAsync(Guid vvnId, ManifestType manifestType);

        /// <summary>
        /// Reopens a rejected VVN back to IN_PROGRESS state.
        /// </summary>
        /// <param name="vvnId">The GUID of the VVN.</param>
        /// <param name="callerOrgId">The organization ID of the caller.</param>
        /// <returns>The reopened VVN.</returns>
        /// <exception cref="KeyNotFoundException">Thrown when VVN is not found.</exception>
        /// <exception cref="UnauthorizedAccessException">Thrown when VVN belongs to different organization.</exception>
        /// <exception cref="InvalidOperationException">Thrown when VVN is not in REJECTED state.</exception>
        Task<VesselVisitNotification> ReopenVvnAsync(Guid vvnId, Guid? callerOrgId);

        /// <summary>
        /// Gets decision logs for a VVN.
        /// </summary>
        /// <param name="vvnId">The GUID of the VVN.</param>
        /// <returns>List of decision logs.</returns>
        Task<List<object>> GetDecisionLogsAsync(Guid vvnId);

        /// <summary>
        /// Validates if a VVN belongs to the caller's organization.
        /// </summary>
        /// <param name="vvnId">The GUID of the VVN.</param>
        /// <param name="callerOrgId">The organization ID of the caller.</param>
        /// <returns>True if VVN belongs to caller's organization, false otherwise.</returns>
        Task<bool> ValidateOrganizationOwnershipAsync(Guid vvnId, Guid? callerOrgId);
    }
}
