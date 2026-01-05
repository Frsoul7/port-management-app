using System;
using System.Collections.Generic;
using System.Linq;
using DDDNetCore.Domain.Users;
using DDDNetCore.Domain.Vessels;
using DDDNetCore.Domain.Visits.Manifests;
using DDDNetCore.Domain.DockAssignments;
using DDDNetCore.Domain.Visits.Crew;
using DDDNetCore.Domain.Organizations;
using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Domain.Visits
{
    public enum VVNState { IN_PROGRESS, SUBMITTED, APPROVED, REJECTED }
    public enum VisitPurpose { LOAD, UNLOAD, BOTH, MAINTENANCE }
    public enum ManifestType { Load = 1, Unload = 2 }

    public class VesselVisitNotification
    {
        // Identity
        public Guid VvnGuid { get; private set; }
        public string VvnBusinessId { get; private set; } = null!; // 2025-PTLEI-000001
        public string VvnId => VvnBusinessId; // Display-friendly ID

        // Core (UTC)
        public VVNState State { get; private set; } = VVNState.IN_PROGRESS;
        public VisitPurpose VisitPurpose { get; private set; }
        public string VesselImo { get; private set; } = null!;

        public DateTime Eta { get; private set; }
        public DateTime Etd { get; private set; }

        // Timeline
        public DateTime CreatedAt { get; private set; }
        public DateTime? SubmittedAt { get; private set; }
        public DateTime? ApprovedAt { get; private set; }
        public DateTime? RejectedAt { get; private set; }
        public string? RejectionReason { get; private set; }

        // Vessel link
        public Vessel? Vessel { get; private set; }

        // Organization specification
        public OrganizationId OrganizationId { get; private set; } = default!;
        public Organization? Organization { get; private set; }      // optional nav

        // Who did what
        public UserId? SubmittedById { get; private set; }
        public User? SubmittedBy { get; private set; }
        public UserId? ApprovedById { get; private set; }
        public User? ApprovedBy { get; private set; }
        public UserId? RejectedById { get; private set; }
        public User? RejectedBy { get; private set; }

        // Dock assignment
        public Guid? DockAssignmentId { get; private set; }
        public DockAssignment? DockAssignment { get; private set; }

        // Manifests
        public LoadingCargoManifest? LoadingManifest { get; private set; }
        public UnloadingCargoManifest? UnloadingManifest { get; private set; }

        // (Optional) denormalized counters
        public int LoadingCount { get; private set; }
        public int UnloadingCount { get; private set; }

    // Crew — ALWAYS required: captain + crew count
    public string CaptainName { get; private set; } = null!;
    public string CaptainCitizenId { get; private set; } = null!;
    public string CaptainNationality { get; private set; } = null!;  // ISO 3166-1 alpha-2
    public int CrewCount { get; private set; }

    // Hazardous handlers (only required when hazardous cargo exists)
    private readonly List<CrewMember> _crewMembers = new();
    public IReadOnlyCollection<CrewMember> CrewMembers => _crewMembers.AsReadOnly();        // When VVN is rejected, it can also be editable
        private bool CanEdit() => State == VVNState.IN_PROGRESS || State == VVNState.REJECTED;


        private VesselVisitNotification() { }

    public VesselVisitNotification(
        string vvnBusinessId,
        string vesselImo,
        VisitPurpose purpose,
        DateTime etaUtc,
        DateTime etdUtc,
        string captainName,
        string captainCitizenId,
        string captainNationality,
        int crewCount,
        OrganizationId orgId)
    {
        if (string.IsNullOrWhiteSpace(vvnBusinessId)) throw new ArgumentException("VVN Business ID required", nameof(vvnBusinessId));
        if (string.IsNullOrWhiteSpace(vesselImo)) throw new ArgumentException("IMO required", nameof(vesselImo));
        if (etaUtc == default) throw new ArgumentException("ETA required", nameof(etaUtc));
        if (etdUtc == default) throw new ArgumentException("ETD required", nameof(etdUtc));
        if (etdUtc <= etaUtc) throw new ArgumentException("ETD must be after ETA", nameof(etdUtc));
        if (string.IsNullOrWhiteSpace(captainName)) throw new ArgumentException("Captain name required", nameof(captainName));
        if (string.IsNullOrWhiteSpace(captainCitizenId)) throw new ArgumentException("Captain citizen ID required", nameof(captainCitizenId));
        if (string.IsNullOrWhiteSpace(captainNationality)) throw new ArgumentException("Captain nationality required", nameof(captainNationality));
        if (captainNationality.Trim().Length != 2) throw new ArgumentException("Captain nationality must be a 2-letter ISO 3166-1 alpha-2 code", nameof(captainNationality));
        if (crewCount < 0) throw new ArgumentOutOfRangeException(nameof(crewCount));

        VvnGuid = Guid.NewGuid();
        VvnBusinessId = vvnBusinessId.Trim().ToUpperInvariant(); // Immutable business ID
        VesselImo = vesselImo;
        VisitPurpose = purpose;
        Eta = etaUtc;
        Etd = etdUtc;
        CaptainName = captainName.Trim();
        CaptainCitizenId = captainCitizenId.Trim();
        CaptainNationality = captainNationality.Trim().ToUpperInvariant();
        CrewCount = crewCount;
        OrganizationId = orgId ?? throw new ArgumentNullException(nameof(orgId));
        State = VVNState.IN_PROGRESS;
        CreatedAt = DateTime.UtcNow;
    }        // Create if missing (kept optional in EF)

        public void AddLoadingEntry(ManifestEntry entry)
        {
            EnsureEditable();
            EnsureLoadingManifest();
            LoadingManifest!.AddEntry(entry);
            UpdateCounts();
        }

        public void AddUnloadingEntry(ManifestEntry entry)
        {
            EnsureEditable();
            EnsureUnloadingManifest();
            UnloadingManifest!.AddEntry(entry);
            UpdateCounts();
        }

        private void UpdateCounts()
        {
            LoadingCount = LoadingManifest?.Entries.Count ?? 0;
            UnloadingCount = UnloadingManifest?.Entries.Count ?? 0;
        }

        public void SetLoadingManifest(LoadingCargoManifest? manifest)
        {
            EnsureEditable();
            LoadingManifest = manifest;
            UpdateCounts(); // <-- keep counters coherent
        }

        public void SetUnloadingManifest(UnloadingCargoManifest? manifest)
        {
            EnsureEditable();
            UnloadingManifest = manifest;
            UpdateCounts(); // <-- keep counters coherent
        }

        // ========= NEW: manifest helpers + incremental entry commands =========

        public LoadingCargoManifest EnsureLoadingManifest()
        {
            EnsureEditable();
            if (LoadingManifest == null)
                LoadingManifest = CargoManifest.CreateLoading(VvnGuid); // use base factory
            return LoadingManifest!;
        }



        public UnloadingCargoManifest EnsureUnloadingManifest()
        {
            EnsureEditable();
            if (UnloadingManifest == null)
                UnloadingManifest = CargoManifest.CreateUnloading(VvnGuid);
            return UnloadingManifest!;
        }

        public void AddEntry(ManifestType type, ManifestEntry entry)
        {
            EnsureEditable();
            if (type == ManifestType.Load)
                EnsureLoadingManifest().AddEntry(entry);
            else
                EnsureUnloadingManifest().AddEntry(entry);

            UpdateCounts();
        }


        public void RemoveEntry(ManifestType type, Guid entryId)
        {
            EnsureEditable();
            if (type == ManifestType.Load)
            {
                if (LoadingManifest == null) throw new InvalidOperationException("Loading manifest does not exist.");
                LoadingManifest.RemoveEntry(entryId);
            }
            else
            {
                if (UnloadingManifest == null) throw new InvalidOperationException("Unloading manifest does not exist.");
                UnloadingManifest.RemoveEntry(entryId);
            }

            UpdateCounts();
        }

    // Allow editing captain & count while editable
    public void SetCrewSummary(string captainName, string captainCitizenId, string captainNationality, int crewCount)
    {
        EnsureEditable();
        if (string.IsNullOrWhiteSpace(captainName))
            throw new ArgumentException("Captain name is required.", nameof(captainName));
        if (string.IsNullOrWhiteSpace(captainCitizenId))
            throw new ArgumentException("Captain citizen ID is required.", nameof(captainCitizenId));
        if (string.IsNullOrWhiteSpace(captainNationality))
            throw new ArgumentException("Captain nationality is required.", nameof(captainNationality));
        if (captainNationality.Trim().Length != 2)
            throw new ArgumentException("Captain nationality must be a 2-letter ISO 3166-1 alpha-2 code", nameof(captainNationality));
        if (crewCount < 0)
            throw new ArgumentOutOfRangeException(nameof(crewCount), "Crew count must be >= 0.");
        if (!CanEdit()) throw new InvalidOperationException("VVN is not editable in the current state.");

        CaptainName = captainName.Trim();
        CaptainCitizenId = captainCitizenId.Trim();
        CaptainNationality = captainNationality.Trim().ToUpperInvariant();
        CrewCount = crewCount;
    }
        // Hazardous handlers (subset, only when hazardous)
        public void SetCrew(string? captainName, IEnumerable<CrewMember>? members)
        {
            EnsureEditable();

            // optional captain update here, but not required (captain is kept in CaptainName)
            if (!string.IsNullOrWhiteSpace(captainName))
                CaptainName = captainName.Trim();

            _crewMembers.Clear();
            if (members != null) _crewMembers.AddRange(members);
        }

        public void Submit(UserId submittedBy)
        {
            if (State != VVNState.IN_PROGRESS && State != VVNState.REJECTED)
                throw new InvalidOperationException("Only VVNs in progress can be submitted.");

            if (string.IsNullOrWhiteSpace(VesselImo))
                throw new InvalidOperationException("Vessel IMO is required before submission.");

            // 🔒 Do the same normalization/validation you use in the service
            if (!ImoValidator.IsValid(VesselImo))
                throw new InvalidOperationException("Vessel IMO is invalid.");

            // (Optional) require manifests depending on purpose
            // if (VisitPurpose == VisitPurpose.LOAD && (LoadingManifest == null || LoadingManifest.Entries.Count == 0))
            //     throw new InvalidOperationException("Loading manifest is required before submission.");
            // if (VisitPurpose == VisitPurpose.UNLOAD && (UnloadingManifest == null || UnloadingManifest.Entries.Count == 0))
            //     throw new InvalidOperationException("Unloading manifest is required before submission.");

            if (string.IsNullOrWhiteSpace(CaptainName))
                throw new InvalidOperationException("Captain name is required.");
            if (CrewCount < 0)
                throw new InvalidOperationException("Crew count must be non-negative.");

            // clear any previous decision flags
            RejectedAt = null;
            RejectionReason = null;
            ApprovedAt = null;

            SubmittedAt = DateTime.UtcNow;   // (re)set on each submit
            State = VVNState.SUBMITTED;
            SubmittedById = submittedBy;
        }

        public void SetVesselImo(string imo)
        {
            if (string.IsNullOrWhiteSpace(imo)) throw new ArgumentException("IMO required", nameof(imo));
            if (!CanEdit()) throw new InvalidOperationException("VVN is not editable in the current state.");
            VesselImo = imo;
        }

        public void SetEta(DateTime eta)
        {
            if (!CanEdit()) throw new InvalidOperationException("VVN is not editable in the current state.");
            Eta = eta;
        }

        public void SetEtd(DateTime etd)
        {
            if (!CanEdit()) throw new InvalidOperationException("VVN is not editable in the current state.");
            Etd = etd;
        }

        public void Approve(UserId approver, Guid dockAssignmentId)
        {
            if (State != VVNState.SUBMITTED)
                throw new InvalidOperationException("Only SUBMITTED VVN can be approved.");
            if (dockAssignmentId == Guid.Empty)
                throw new ArgumentException("DockAssignmentId is required.", nameof(dockAssignmentId));

            State = VVNState.APPROVED;
            ApprovedAt = DateTime.UtcNow;
            ApprovedById = approver;
            DockAssignmentId = dockAssignmentId;
        }

        public void Reject(UserId rejector, string reason)
        {
            if (State != VVNState.SUBMITTED)
                throw new InvalidOperationException("Only SUBMITTED VVN can be rejected.");
            if (string.IsNullOrWhiteSpace(reason))
                throw new ArgumentException("A rejection reason is required.", nameof(reason));

            State = VVNState.REJECTED;
            RejectedAt = DateTime.UtcNow;
            RejectedById = rejector;
            RejectionReason = reason.Trim();
        }

        public void ReopenToDraft()
        {
            if (State != VVNState.REJECTED)
                throw new InvalidOperationException("Only REJECTED VVN can be reopened to DRAFT.");

            State = VVNState.IN_PROGRESS;
            RejectedAt = null;
            RejectedById = null;
            RejectionReason = null;
            SubmittedAt = null;
            SubmittedById = null;
        }

        private void EnsureEditable()
        {
            if (State != VVNState.IN_PROGRESS && State != VVNState.REJECTED)
                throw new InvalidOperationException("VVN is not editable in the current state.");
        }
    }
}
