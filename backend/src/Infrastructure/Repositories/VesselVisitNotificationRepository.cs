using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.EntityFrameworkCore;
using DDDNetCore.Domain.IRepository;
using DDDNetCore.Domain.Visits;
using DDDNetCore.Domain.Visits.Manifests;
using DDDNetCore.Domain.Visits.Crew;
using DDDNetCore.Domain.Organizations;
using DDDNetCore.Domain.Users;
using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Infrastructure.Repositories
{
    /// <summary>
    /// Repository implementation for VesselVisitNotification aggregate.
    /// Manages VVNs with their associated manifests and crew members.
    /// </summary>
    public class VesselVisitNotificationRepository : IVesselVisitNotificationRepository
    {
        private readonly PortDbContext _context;

        public VesselVisitNotificationRepository(PortDbContext context)
        {
            _context = context ?? throw new ArgumentNullException(nameof(context));
        }

        // ==================== Basic CRUD ====================

        public async Task<VesselVisitNotification?> GetByIdAsync(Guid vvnId)
        {
            return await _context.VesselVisitNotifications
                .FirstOrDefaultAsync(v => v.VvnGuid == vvnId);
        }

        public async Task<VesselVisitNotification?> GetByBusinessIdAsync(string vvnBusinessId)
        {
            if (string.IsNullOrWhiteSpace(vvnBusinessId))
                return null;

            return await _context.VesselVisitNotifications
                .FirstOrDefaultAsync(v => v.VvnBusinessId == vvnBusinessId);
        }

        public async Task<IEnumerable<VesselVisitNotification>> GetAllAsync()
        {
            return await _context.VesselVisitNotifications
                .ToListAsync();
        }

        public async Task AddAsync(VesselVisitNotification vvn)
        {
            await _context.VesselVisitNotifications.AddAsync(vvn);
        }

        public void Update(VesselVisitNotification vvn)
        {
            _context.VesselVisitNotifications.Update(vvn);
        }

        public void Remove(VesselVisitNotification vvn)
        {
            _context.VesselVisitNotifications.Remove(vvn);
        }

        // ==================== With Navigation Properties ====================

        public async Task<VesselVisitNotification?> GetByIdWithCrewAsync(Guid vvnId)
        {
            return await _context.VesselVisitNotifications
                .Include(v => v.CrewMembers)
                .FirstOrDefaultAsync(v => v.VvnGuid == vvnId);
        }

        public async Task<VesselVisitNotification?> GetByIdWithManifestsAsync(Guid vvnId)
        {
            return await _context.VesselVisitNotifications
                .Include(v => v.LoadingManifest)
                    .ThenInclude(m => m!.Entries)
                .Include(v => v.UnloadingManifest)
                    .ThenInclude(m => m!.Entries)
                .FirstOrDefaultAsync(v => v.VvnGuid == vvnId);
        }

        public async Task<VesselVisitNotification?> GetByIdWithAllNavigationAsync(Guid vvnId)
        {
            return await _context.VesselVisitNotifications
                .Include(v => v.Vessel)
                    .ThenInclude(vessel => vessel!.VesselType)
                .Include(v => v.Organization)
                .Include(v => v.DockAssignment)
                .Include(v => v.LoadingManifest)
                    .ThenInclude(m => m!.Entries)
                .Include(v => v.UnloadingManifest)
                    .ThenInclude(m => m!.Entries)
                .Include(v => v.CrewMembers)
                .FirstOrDefaultAsync(v => v.VvnGuid == vvnId);
        }

        // ==================== Query Methods ====================

        public async Task<IEnumerable<VesselVisitNotification>> GetByOrganizationAsync(
            OrganizationId organizationId,
            string? vesselImo = null,
            VVNState? status = null,
            Guid? submittedById = null,
            DateTime? fromDate = null,
            DateTime? toDate = null)
        {
            var query = _context.VesselVisitNotifications
                .Include(v => v.DockAssignment)
                .Where(v => v.OrganizationId == organizationId);

            // Filter by vessel IMO (if provided)
            if (!string.IsNullOrWhiteSpace(vesselImo))
            {
                var normalizedImo = ImoValidator.Normalize(vesselImo);
                query = query.Where(v => v.VesselImo == normalizedImo);
            }

            // Filter by status (if provided)
            if (status.HasValue)
            {
                query = query.Where(v => v.State == status.Value);
            }

            // Filter by representative/submitter (if provided)
            if (submittedById.HasValue)
            {
                var userIdToFind = new UserId(submittedById.Value);
                query = query.Where(v => v.SubmittedById != null && v.SubmittedById == userIdToFind);
            }

            // Filter by time range (using CreatedAt as the default time field)
            if (fromDate.HasValue)
            {
                query = query.Where(v => v.CreatedAt >= fromDate.Value);
            }

            if (toDate.HasValue)
            {
                query = query.Where(v => v.CreatedAt <= toDate.Value);
            }

            // Order by most recent first
            return await query
                .OrderByDescending(v => v.CreatedAt)
                .ToListAsync();
        }

        public async Task<IEnumerable<VesselVisitNotification>> GetByVesselAsync(string vesselImo)
        {
            var normalizedImo = ImoValidator.Normalize(vesselImo);
            return await _context.VesselVisitNotifications
                .Where(v => v.VesselImo == normalizedImo)
                .OrderByDescending(v => v.CreatedAt)
                .ToListAsync();
        }

        public async Task<IEnumerable<VesselVisitNotification>> GetByStateAsync(VVNState state)
        {
            return await _context.VesselVisitNotifications
                .Where(v => v.State == state)
                .OrderByDescending(v => v.CreatedAt)
                .ToListAsync();
        }

        public async Task<bool> ExistsAsync(Guid vvnId)
        {
            return await _context.VesselVisitNotifications
                .AnyAsync(v => v.VvnGuid == vvnId);
        }

        public async Task<List<string>> GetBusinessIdsByPrefixAsync(string prefix)
        {
            return await _context.VesselVisitNotifications
                .Where(v => v.VvnBusinessId != null && v.VvnBusinessId.StartsWith(prefix))
                .Select(v => v.VvnBusinessId!)
                .ToListAsync();
        }

        // ==================== Manifest Operations ====================

        public async Task<CargoManifest?> GetManifestByIdAsync(Guid manifestId)
        {
            return await _context.CargoManifests
                .Include(m => m.Entries)
                .FirstOrDefaultAsync(m => m.Id == manifestId);
        }

        public async Task<IEnumerable<CargoManifest>> GetManifestsByVvnIdAsync(Guid vvnId)
        {
            return await _context.CargoManifests
                .Include(m => m.Entries)
                .Where(m => m.VvnGuid == vvnId)
                .ToListAsync();
        }

        public void AddManifest(CargoManifest manifest)
        {
            _context.CargoManifests.Add(manifest);
        }

        public void UpdateManifest(CargoManifest manifest)
        {
            _context.CargoManifests.Update(manifest);
        }

        public void RemoveManifest(CargoManifest manifest)
        {
            _context.CargoManifests.Remove(manifest);
        }

        public void RemoveManifests(IEnumerable<CargoManifest> manifests)
        {
            _context.CargoManifests.RemoveRange(manifests);
        }

        // ==================== Manifest Entry Operations ====================

        public async Task<ManifestEntry?> GetManifestEntryByIdAsync(Guid entryId)
        {
            return await _context.ManifestEntries
                .FirstOrDefaultAsync(e => e.Id == entryId);
        }

        public void AddManifestEntry(ManifestEntry entry)
        {
            _context.ManifestEntries.Add(entry);
        }

        public void UpdateManifestEntry(ManifestEntry entry)
        {
            _context.ManifestEntries.Update(entry);
        }

        public void RemoveManifestEntry(ManifestEntry entry)
        {
            _context.ManifestEntries.Remove(entry);
        }

        // ==================== Crew Operations ====================

        public async Task<IEnumerable<CrewMember>> GetCrewMembersByVvnIdAsync(Guid vvnId)
        {
            return await _context.Set<CrewMember>()
                .Where(c => c.VesselVisitNotificationId == vvnId)
                .ToListAsync();
        }

        public void AddCrewMembers(IEnumerable<CrewMember> crewMembers)
        {
            _context.AddRange(crewMembers);
        }

        public void RemoveCrewMembers(IEnumerable<CrewMember> crewMembers)
        {
            _context.RemoveRange(crewMembers);
        }
    }
}
