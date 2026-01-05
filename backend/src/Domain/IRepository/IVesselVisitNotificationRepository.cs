using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDNetCore.Domain.Visits;
using DDDNetCore.Domain.Visits.Manifests;
using DDDNetCore.Domain.Visits.Crew;
using DDDNetCore.Domain.Organizations;

namespace DDDNetCore.Domain.IRepository
{
    /// <summary>
    /// Repository interface for VesselVisitNotification aggregate.
    /// Handles all data access for VVNs, including manifests and crew members.
    /// </summary>
    public interface IVesselVisitNotificationRepository
    {
        // Basic CRUD
        Task<VesselVisitNotification?> GetByIdAsync(Guid vvnId);
        Task<VesselVisitNotification?> GetByBusinessIdAsync(string vvnBusinessId);
        Task<IEnumerable<VesselVisitNotification>> GetAllAsync();
        Task AddAsync(VesselVisitNotification vvn);
        void Update(VesselVisitNotification vvn);
        void Remove(VesselVisitNotification vvn);

        // VVN with navigation properties
        Task<VesselVisitNotification?> GetByIdWithCrewAsync(Guid vvnId);
        Task<VesselVisitNotification?> GetByIdWithManifestsAsync(Guid vvnId);
        Task<VesselVisitNotification?> GetByIdWithAllNavigationAsync(Guid vvnId);

        // Query methods
        Task<IEnumerable<VesselVisitNotification>> GetByOrganizationAsync(
            OrganizationId organizationId,
            string? vesselImo = null,
            VVNState? status = null,
            Guid? submittedById = null,
            DateTime? fromDate = null,
            DateTime? toDate = null);

        Task<IEnumerable<VesselVisitNotification>> GetByVesselAsync(string vesselImo);
        Task<IEnumerable<VesselVisitNotification>> GetByStateAsync(VVNState state);
        Task<bool> ExistsAsync(Guid vvnId);

        // Business ID generation support
        Task<List<string>> GetBusinessIdsByPrefixAsync(string prefix);

        // Manifest operations
        Task<CargoManifest?> GetManifestByIdAsync(Guid manifestId);
        Task<IEnumerable<CargoManifest>> GetManifestsByVvnIdAsync(Guid vvnId);
        void AddManifest(CargoManifest manifest);
        void UpdateManifest(CargoManifest manifest);
        void RemoveManifest(CargoManifest manifest);
        void RemoveManifests(IEnumerable<CargoManifest> manifests);

        // Manifest entry operations
        Task<ManifestEntry?> GetManifestEntryByIdAsync(Guid entryId);
        void AddManifestEntry(ManifestEntry entry);
        void UpdateManifestEntry(ManifestEntry entry);
        void RemoveManifestEntry(ManifestEntry entry);

        // Crew operations
        Task<IEnumerable<CrewMember>> GetCrewMembersByVvnIdAsync(Guid vvnId);
        void AddCrewMembers(IEnumerable<CrewMember> crewMembers);
        void RemoveCrewMembers(IEnumerable<CrewMember> crewMembers);
    }
}
