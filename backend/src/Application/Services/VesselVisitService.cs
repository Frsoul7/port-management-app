using System;
using System.Linq;
using System.Threading.Tasks;
using System.Collections.Generic;
using DDDNetCore.Domain.Visits;
using DDDNetCore.Domain.Visits.Manifests;
using DDDNetCore.Domain.Visits.Crew;
using DDDNetCore.Domain.Visits.Policies;
using DDDNetCore.Domain.Users;
using Microsoft.EntityFrameworkCore;
using DDDNetCore.Infrastructure;
using DDDNetCore.Domain.Organizations;
using DDDNetCore.Application.DTOs.Vvns;
using DDDNetCore.Domain.Shared; // ImoValidator
using DDDNetCore.Domain.IRepository;
using DDDNetCore.Application.Interfaces;
using DDDNetCore.API.Contracts;
using DDDNetCore.Infrastructure.Mappers;



namespace DDDNetCore.Application.Services
{
    /// <summary>
    /// Application service for vessel visit notification business logic.
    /// Orchestrates domain operations and enforces business rules for VVNs.
    /// </summary>
    public class VesselVisitService : IVesselVisitService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly ICrewCompliancePolicy _crewPolicy;
        private readonly VvnIdGenerator _vvnIdGenerator;

        public VesselVisitService(IUnitOfWork unitOfWork, ICrewCompliancePolicy crewPolicy, VvnIdGenerator vvnIdGenerator)
        {
            _unitOfWork = unitOfWork;
            _crewPolicy = crewPolicy;
            _vvnIdGenerator = vvnIdGenerator;
        }

        public async Task<VvnSummaryResponse> UpdateVvnAsync(Guid vvnId, UpdateVvnRequest dto)
        {
            var vvn = await _unitOfWork.VesselVisitNotifications.GetByIdAsync(vvnId)
                ?? throw new KeyNotFoundException("VVN not found.");

            // EDITABILITY GUARD: only editable while IN PROGRESS (not submitted) Or REJECTED (submitted)
            if (vvn.State != VVNState.IN_PROGRESS && vvn.State != VVNState.REJECTED)
                throw new InvalidOperationException("VVN is not editable in the current state.");


            // Vessel IMO change (allowed): validate format + ensure vessel exists
            if (!string.IsNullOrWhiteSpace(dto.VesselImo))
            {
                var imo = ImoValidator.Normalize(dto.VesselImo);
                if (!ImoValidator.IsValid(imo))
                    throw new ArgumentException("Invalid IMO number.", nameof(dto.VesselImo));

                var vesselExists = await _unitOfWork.Vessels.ExistsAsync(imo);
                if (!vesselExists)
                    throw new ArgumentException($"Vessel with IMO {imo} was not found.", nameof(dto.VesselImo));

                vvn.SetVesselImo(imo); // domain method assigns + (optionally) calls EnsureEditable()
            }

            // Purpose change: keep disallowed for now (can invalidate manifests)
            if (!string.IsNullOrWhiteSpace(dto.Purpose))
                throw new ArgumentException("Changing visit purpose is not supported by this endpoint.", nameof(dto.Purpose));

            // Header fields
            if (dto.Eta.HasValue) vvn.SetEta(dto.Eta.Value);
            if (dto.Etd.HasValue) vvn.SetEtd(dto.Etd.Value);

            // Crew summary: merge optional fields, then call strict domain method (non-null captain)
            if (dto.CaptainName != null || dto.CaptainCitizenId != null || dto.CaptainNationality != null || dto.CrewCount.HasValue)
            {
                var newCaptain = dto.CaptainName ?? vvn.CaptainName; // vvn.CaptainName must be non-null in a valid entity
                var newCitizenId = dto.CaptainCitizenId ?? vvn.CaptainCitizenId;
                var newNationality = dto.CaptainNationality ?? vvn.CaptainNationality;
                var newCrew = dto.CrewCount ?? vvn.CrewCount;

                vvn.SetCrewSummary(newCaptain, newCitizenId, newNationality, newCrew);
            }

            _unitOfWork.VesselVisitNotifications.Update(vvn);
            await _unitOfWork.CommitAsync();
            return VvnMapper.ToSummaryDto(vvn);
        }


        public async Task<VvnSummaryResponse> CreateVvnAsync(CreateVvnRequest dto, OrganizationId orgId)
        {
            // 1) Validate & normalize IMO
            var imo = ImoValidator.Normalize(dto.VesselImo);
            if (!ImoValidator.IsValid(imo))
                throw new ArgumentException("Invalid IMO number.", nameof(dto.VesselImo));

            // 2) Vessel must exist
            var vesselExists = await _unitOfWork.Vessels.ExistsAsync(imo);
            if (!vesselExists)
                throw new ArgumentException($"Vessel with IMO {imo} was not found.", nameof(dto.VesselImo));

            // 3) Organization must be a SHIPPING_AGENT
            var org = await _unitOfWork.Organizations.GetByIdAsync(orgId.Value.ToString());
            if (org == null)
                throw new ArgumentException("Organization not found.", nameof(orgId));

            if (org.Type != OrganizationType.SHIPPING_AGENT)
                throw new UnauthorizedAccessException("Only shipping agent organizations can create VVNs.");

            // 4) Parse purpose
            var purpose = Enum.Parse<VisitPurpose>(dto.Purpose, ignoreCase: true);

            // 5) Generate unique business ID
            var businessId = await _vvnIdGenerator.GenerateNextIdAsync(dto.Eta.Year);

            // 6) Create VVN
            var vvn = new VesselVisitNotification(
                businessId, imo, purpose, dto.Eta, dto.Etd,
                dto.CaptainName, dto.CaptainCitizenId, dto.CaptainNationality, dto.CrewCount, orgId);

            await _unitOfWork.VesselVisitNotifications.AddAsync(vvn);
            await _unitOfWork.CommitAsync();
            return VvnMapper.ToSummaryDto(vvn);
        }
        /// <summary>
        /// Replace existing manifests for a VVN (legacy bulk-upsert).
        /// </summary>
        public async Task ReplaceManifestsAsync(Guid vvnId, CargoManifest? loading, CargoManifest? unloading)
        {
            var vvn = await _unitOfWork.VesselVisitNotifications.GetByIdAsync(vvnId)
                ?? throw new KeyNotFoundException("VVN not found.");

            // Ensure editable (domain guard)
            vvn.SetLoadingManifest(vvn.LoadingManifest);
            vvn.SetUnloadingManifest(vvn.UnloadingManifest);

            // Remove existing manifests
            var existing = await _unitOfWork.VesselVisitNotifications.GetManifestsByVvnIdAsync(vvnId);

            if (existing.Any())
                _unitOfWork.VesselVisitNotifications.RemoveManifests(existing);

            // Add new ones if present
            if (loading is not null)
            {
                loading.GetType().GetProperty(nameof(loading.VvnGuid))!.SetValue(loading, vvnId);
                _unitOfWork.VesselVisitNotifications.AddManifest(loading);
            }

            if (unloading is not null)
            {
                unloading.GetType().GetProperty(nameof(unloading.VvnGuid))!.SetValue(unloading, vvnId);
                _unitOfWork.VesselVisitNotifications.AddManifest(unloading);
            }

            await _unitOfWork.CommitAsync();
        }

        public async Task SetCrewAsync(Guid vvnId, string? captainName, IEnumerable<CrewMember> members)
        {
            var vvn = await _unitOfWork.VesselVisitNotifications.GetByIdWithCrewAsync(vvnId)
                ?? throw new KeyNotFoundException("VVN not found.");

            if (vvn.State != VVNState.IN_PROGRESS && vvn.State != VVNState.REJECTED)
                throw new InvalidOperationException("VVN is not editable in the current state.");

            var oldRows = (vvn.CrewMembers ?? new List<CrewMember>()).ToList();

            vvn.SetCrew(captainName, members);

            if (oldRows.Count > 0)
                _unitOfWork.VesselVisitNotifications.RemoveCrewMembers(oldRows);

            var fkProp = typeof(CrewMember).GetProperty(nameof(CrewMember.VesselVisitNotificationId))!;
            foreach (var m in members)
                fkProp.SetValue(m, vvn.VvnGuid);

            _unitOfWork.VesselVisitNotifications.AddCrewMembers(members);
            _unitOfWork.VesselVisitNotifications.Update(vvn);
            await _unitOfWork.CommitAsync();
        }

        public async Task<IReadOnlyList<VvnEntryResponse>> GetEntriesAsync(Guid vvnId, ManifestType? type)
        {
            // Load both manifests (light projection)
            var manifests = (await _unitOfWork.VesselVisitNotifications.GetManifestsByVvnIdAsync(vvnId)).ToList();

            var list = new List<VvnEntryResponse>(128);

            if (type is null || type == ManifestType.Load)
            {
                foreach (var e in manifests.OfType<LoadingCargoManifest>().SelectMany(m => m.Entries))
                {
                    list.Add(new VvnEntryResponse
                    {
                        Id = e.Id,
                        Type = "Load",
                        ContainerCode = e.ContainerUniqueId,
                        Hazardous = e.HazardousGoods,
                        Bay = e.ContainerBayNr,
                        Row = e.ContainerRowNr,
                        Tier = e.ContainerTierNr,
                        GoodsDescription = e.GoodsDescription
                    });
                }
            }

            if (type is null || type == ManifestType.Unload)
            {
                foreach (var e in manifests.OfType<UnloadingCargoManifest>().SelectMany(m => m.Entries))
                {
                    list.Add(new VvnEntryResponse
                    {
                        Id = e.Id,
                        Type = "Unload",
                        ContainerCode = e.ContainerUniqueId,
                        Hazardous = e.HazardousGoods,
                        Bay = e.ContainerBayNr,
                        Row = e.ContainerRowNr,
                        Tier = e.ContainerTierNr,
                        GoodsDescription = e.GoodsDescription
                    });
                }
            }

            return list;
        }

        public async Task SetCrewSummaryAsync(Guid vvnId, string captainName, string captainCitizenId, string captainNationality, int crewCount)
        {
            var vvn = await _unitOfWork.VesselVisitNotifications.GetByIdAsync(vvnId)
                ?? throw new KeyNotFoundException("VVN not found.");

            vvn.SetCrewSummary(captainName, captainCitizenId, captainNationality, crewCount);
            _unitOfWork.VesselVisitNotifications.Update(vvn);
            await _unitOfWork.CommitAsync();
        }

        // ---------------------------------------------------------------------
        // Manifest helpers
        // ---------------------------------------------------------------------

        private async Task<CargoManifest> GetOrCreateManifestAsync(Guid vvnId, bool loading)
        {
            var manifests = await _unitOfWork.VesselVisitNotifications.GetManifestsByVvnIdAsync(vvnId);
            var manifest = manifests.FirstOrDefault(m => loading ? m is LoadingCargoManifest : m is UnloadingCargoManifest);

            if (manifest != null)
                return manifest;

            // Create new via factory
            manifest = loading
                ? CargoManifest.CreateLoading(vvnId)
                : CargoManifest.CreateUnloading(vvnId);

            _unitOfWork.VesselVisitNotifications.AddManifest(manifest);
            await _unitOfWork.CommitAsync();
            return manifest;
        }

        [Obsolete("Use AddManifestEntryAsync / RemoveManifestEntryAsync instead.")]
        public async Task AddManifestEntriesAsync(Guid vvnId, bool loading, IEnumerable<ManifestEntry> newEntries)
        {
            var vvn = await _unitOfWork.VesselVisitNotifications.GetByIdAsync(vvnId)
                ?? throw new KeyNotFoundException("VVN not found.");

            vvn.SetLoadingManifest(vvn.LoadingManifest); // EnsureEditable

            var manifest = await GetOrCreateManifestAsync(vvnId, loading);

            foreach (var e in newEntries)
                manifest.AddEntry(e);

            _unitOfWork.VesselVisitNotifications.UpdateManifest(manifest);
            await _unitOfWork.CommitAsync();
        }

        public async Task UpdateManifestEntryAsync(Guid vvnId, Guid entryId, Action<ManifestEntry> mutate)
        {
            var vvn = await _unitOfWork.VesselVisitNotifications.GetByIdAsync(vvnId)
                ?? throw new KeyNotFoundException("VVN not found.");
            vvn.SetLoadingManifest(vvn.LoadingManifest);

            var entry = await _unitOfWork.VesselVisitNotifications.GetManifestEntryByIdAsync(entryId)
                ?? throw new KeyNotFoundException("Manifest entry not found.");

            mutate(entry);
            _unitOfWork.VesselVisitNotifications.UpdateManifestEntry(entry);
            await _unitOfWork.CommitAsync();
        }

        [Obsolete("Use RemoveManifestEntryAsync(vvnId, ManifestType, entryId) instead.")]
        public async Task RemoveManifestEntryAsync(Guid vvnId, Guid entryId)
        {
            var vvn = await _unitOfWork.VesselVisitNotifications.GetByIdAsync(vvnId)
                ?? throw new KeyNotFoundException("VVN not found.");
            vvn.SetLoadingManifest(vvn.LoadingManifest);

            var entry = await _unitOfWork.VesselVisitNotifications.GetManifestEntryByIdAsync(entryId)
                ?? throw new KeyNotFoundException("Manifest entry not found.");

            _unitOfWork.VesselVisitNotifications.RemoveManifestEntry(entry);
            await _unitOfWork.CommitAsync();
        }

        public async Task<Guid> AddManifestEntryAsync(Guid vvnId, ManifestType type, ManifestEntry entry)
        {
            // 1) Load VVN with manifests
            var vvn = await _unitOfWork.VesselVisitNotifications.GetByIdWithManifestsAsync(vvnId)
                      ?? throw new KeyNotFoundException("VVN not found.");

            // 2) Domain guard (DRAFT/REJECTED)
            // 3) Get or create the correct manifest (and attach it explicitly if new)
            CargoManifest manifest;
            if (type == ManifestType.Load)
            {
                if (vvn.LoadingManifest == null)
                {
                    var newManifest = CargoManifest.CreateLoading(vvn.VvnGuid);
                    vvn.SetLoadingManifest((LoadingCargoManifest)newManifest); // keep counters coherent
                    _unitOfWork.VesselVisitNotifications.AddManifest(newManifest);
                    manifest = newManifest;
                }
                else
                {
                    manifest = vvn.LoadingManifest;
                }
            }
            else
            {
                if (vvn.UnloadingManifest == null)
                {
                    var newManifest = CargoManifest.CreateUnloading(vvn.VvnGuid);
                    vvn.SetUnloadingManifest((UnloadingCargoManifest)newManifest);
                    _unitOfWork.VesselVisitNotifications.AddManifest(newManifest);
                    manifest = newManifest;
                }
                else
                {
                    manifest = vvn.UnloadingManifest;
                }
            }

            // 4) Domain duplicate guard will run here (same ContainerUniqueId)
            manifest.AddEntry(entry);

            // 5) Make sure EF tracks the new entry clearly (avoid graph "update" confusion)
            _unitOfWork.VesselVisitNotifications.AddManifestEntry(entry);

            // 6) Save (no _db.Update(vvn); avoid accidental "Modified" graph states)
            await _unitOfWork.CommitAsync();

            return entry.Id;
        }


        public async Task RemoveManifestEntryAsync(Guid vvnId, ManifestType type, Guid entryId)
        {
            // Load the VVN with manifests
            var vvn = await _unitOfWork.VesselVisitNotifications.GetByIdWithManifestsAsync(vvnId)
                      ?? throw new KeyNotFoundException("VVN not found.");

            // Domain removal (checks existence & editability, updates counters)
            vvn.RemoveEntry(type, entryId);

            // Find the tracked entry and remove it directly to be explicit
            var manifestBase = (CargoManifest)(type == ManifestType.Load
                ? vvn.LoadingManifest!
                : vvn.UnloadingManifest!);

            var tracked = manifestBase.Entries.FirstOrDefault(e => e.Id == entryId);
            if (tracked != null)
                _unitOfWork.VesselVisitNotifications.RemoveManifestEntry(tracked);

            await _unitOfWork.CommitAsync();

        }



        public async Task<VesselVisitNotification> SubmitAsync(Guid vvnId, Guid submittedByUserGuid)
        {
            var vvn = await _unitOfWork.VesselVisitNotifications.GetByIdWithCrewAsync(vvnId)
                ?? throw new KeyNotFoundException("VVN not found.");

            // Guard: only Shipping Agent organizations can submit VVNs
            var org = await _unitOfWork.Organizations.GetByIdAsync(vvn.OrganizationId.Value.ToString());

            if (org is null)
                throw new InvalidOperationException("The organization associated with this VVN no longer exists.");

            if (org.Type != OrganizationType.SHIPPING_AGENT)
                throw new UnauthorizedAccessException("Only shipping agent organizations can submit VVNs.");

            // Load manifests (for domain checks below)
            var manifests = await _unitOfWork.VesselVisitNotifications.GetManifestsByVvnIdAsync(vvnId);

            var loading = manifests.OfType<LoadingCargoManifest>().SingleOrDefault();
            var unloading = manifests.OfType<UnloadingCargoManifest>().SingleOrDefault();

            vvn.SetLoadingManifest(loading);
            vvn.SetUnloadingManifest(unloading);

            // Domain validation (ETA/ETD, required manifests, captain/crew, etc.)
            vvn.Submit(new UserId(submittedByUserGuid));

            // External policy (e.g., hazardous crew compliance)
            _crewPolicy.EnsureSatisfied(vvn);

            _unitOfWork.VesselVisitNotifications.Update(vvn);
            await _unitOfWork.CommitAsync();
            return vvn;

        }

        /// <summary>
        /// Gets VVNs for a Shipping Agent organization with optional filters (US 2.2.10).
        /// Supports filtering by vessel IMO, status, representative (submitter), and time range.
        /// </summary>
        public async Task<List<VesselVisitNotification>> GetVvnsForOrganizationAsync(
            OrganizationId organizationId,
            string? vesselImo = null,
            VVNState? status = null,
            Guid? submittedById = null,
            DateTime? fromDate = null,
            DateTime? toDate = null)
        {
            var result = await _unitOfWork.VesselVisitNotifications.GetByOrganizationAsync(
                organizationId, vesselImo, status, submittedById, fromDate, toDate);

            // Repository already handles all filtering - just return as List
            return result.ToList();
        }

        public async Task<List<VvnSummaryResponse>> GetAllVvnsAsync(Application.Security.AppRole callerRole, Guid? callerOrgId)
        {
            // Authorization: Port Authority Officers, Logistics Operators, and Administrators can view all VVNs
            if (callerRole != Application.Security.AppRole.PortAuthorityOfficer && 
                callerRole != Application.Security.AppRole.LogisticsOperator && 
                callerRole != Application.Security.AppRole.Administrator)
            {
                throw new UnauthorizedAccessException("Only Port Authority Officers, Logistics Operators, and Administrators can view all VVNs.");
            }

            var vvns = await _unitOfWork.Context.VesselVisitNotifications
                                .Include(v => v.DockAssignment)
                                .OrderByDescending(v => v.CreatedAt)
                                .ToListAsync();

            var vesselImos = vvns.Select(v => v.VesselImo).Distinct().ToList();
            var vessels = await _unitOfWork.Context.Vessels
                                   .Include(v => v.VesselType)
                                   .Where(v => vesselImos.Contains(v.ImoNumber))
                                   .ToDictionaryAsync(v => v.ImoNumber);

            return vvns.Select(v =>
            {
                vessels.TryGetValue(v.VesselImo, out var vessel);
                return new VvnSummaryResponse
                {
                    Id = v.VvnGuid,
                    VvnBusinessId = v.VvnBusinessId,
                    Status = v.State.ToString(),
                    Purpose = v.VisitPurpose.ToString(),
                    Eta = v.Eta,
                    Etd = v.Etd,
                    SubmittedAt = v.SubmittedAt,
                    VesselImo = v.VesselImo,
                    VesselName = vessel?.Name,
                    VesselType = vessel?.VesselType?.Name,
                    AssignedDock = v.DockAssignment?.DockCode
                };
            }).ToList();
        }

        public async Task<VvnSummaryResponse> GetVvnByIdAsync(Guid vvnId, Application.Security.AppRole callerRole, Guid? callerOrgId)
        {
            // Authorization: Port Authority, Logistics, Administrators, and Shipping Agents
            if (callerRole != Application.Security.AppRole.PortAuthorityOfficer && 
                callerRole != Application.Security.AppRole.LogisticsOperator && 
                callerRole != Application.Security.AppRole.Administrator &&
                callerRole != Application.Security.AppRole.ShippingAgentRep)
            {
                throw new UnauthorizedAccessException("You don't have permission to view VVN details.");
            }

            var vvn = await _unitOfWork.Context.VesselVisitNotifications
                               .Include(v => v.DockAssignment)
                               .FirstOrDefaultAsync(v => v.VvnGuid == vvnId);
            
            if (vvn == null) 
                throw new KeyNotFoundException("VVN not found.");

            // Shipping Agents can only view their own organization's VVNs
            if (callerRole == Application.Security.AppRole.ShippingAgentRep)
            {
                if (!callerOrgId.HasValue || vvn.OrganizationId.Value != callerOrgId.Value)
                {
                    throw new UnauthorizedAccessException("You can only view VVNs from your own organization.");
                }
            }

            var vessel = await _unitOfWork.Context.Vessels
                                  .Include(v => v.VesselType)
                                  .FirstOrDefaultAsync(v => v.ImoNumber == vvn.VesselImo);

            return new VvnSummaryResponse
            {
                Id = vvn.VvnGuid,
                VvnBusinessId = vvn.VvnBusinessId,
                Status = vvn.State.ToString(),
                Purpose = vvn.VisitPurpose.ToString(),
                Eta = vvn.Eta,
                Etd = vvn.Etd,
                SubmittedAt = vvn.SubmittedAt,
                VesselImo = vvn.VesselImo,
                VesselName = vessel?.Name,
                VesselType = vessel?.VesselType?.Name,
                AssignedDock = vvn.DockAssignment?.DockCode
            };
        }

        public async Task<VvnApprovalResponse> ApproveVvnAsync(Guid vvnId, VvnApprovalRequest request, Guid callerUserId, Guid? callerOrgId)
        {
            // Validation
            if (request is null || string.IsNullOrWhiteSpace(request.DockCode))
                throw new ArgumentException("DockCode is required.");
            if (request.BerthTo <= request.BerthFrom)
                throw new ArgumentException("BerthTo must be after BerthFrom.");

            // Load VVN
            var vvn = await _unitOfWork.Context.VesselVisitNotifications
                               .Include(v => v.DockAssignment)
                               .SingleOrDefaultAsync(v => v.VvnGuid == vvnId);
            
            if (vvn == null) 
                throw new KeyNotFoundException("VVN not found.");

            // Only SUBMITTED can be approved
            if (vvn.State != VVNState.SUBMITTED)
                throw new InvalidOperationException("Only pending (SUBMITTED) VVNs can be approved.");

            // Ensure dock exists
            var dock = await _unitOfWork.Context.Docks.SingleOrDefaultAsync(d => d.Code == request.DockCode.Trim());
            if (dock == null)
                throw new KeyNotFoundException($"Dock '{request.DockCode}' does not exist.");

            // Overlap check
            var overlaps = await _unitOfWork.Context.DockAssignments.AnyAsync(a =>
                a.DockCode == dock.Code &&
                a.Status != Domain.DockAssignments.DockAssignmentStatus.CANCELLED &&
                a.BerthTo > request.BerthFrom &&
                a.BerthFrom < request.BerthTo);

            if (overlaps)
                throw new InvalidOperationException("The selected dock/time window overlaps an existing assignment.");

            var approverUserId = callerUserId != Guid.Empty 
                ? callerUserId 
                : Guid.Parse("00000000-0000-0000-0000-000000000001"); // System/Port Authority placeholder

            var approvalTimestamp = DateTime.UtcNow;

            // Create DockAssignment
            var assignment = Domain.DockAssignments.DockAssignment.Create(
                vvnId: vvn.VvnGuid,
                dockCode: dock.Code,
                berthFromUtc: request.BerthFrom,
                berthToUtc: request.BerthTo,
                assignedByUserId: approverUserId.ToString(),
                assignedAtUtc: approvalTimestamp
            );

            _unitOfWork.Context.DockAssignments.Add(assignment);

            // Transition domain state
            vvn.Approve(approver: new UserId(approverUserId), dockAssignmentId: assignment.DockAssignmentId);

            // Add decision log
            _unitOfWork.Context.DecisionLogs.Add(Domain.Visits.DecisionLog.Approved(
                vvn.VvnGuid,
                new UserId(approverUserId),
                assignment.DockAssignmentId,
                approvalTimestamp,
                request.Notes));

            await _unitOfWork.CommitAsync();

            // Load organization details for response
            var approverOrg = callerOrgId.HasValue 
                ? await _unitOfWork.Context.Organizations.FindAsync(callerOrgId.Value)
                : null;
            
            var submitterOrg = await _unitOfWork.Context.Organizations.FindAsync(vvn.OrganizationId.Value);

            return new VvnApprovalResponse
            {
                VvnId = vvn.VvnGuid,
                VvnBusinessId = vvn.VvnBusinessId,
                Status = vvn.State.ToString(),
                AssignedDock = assignment.DockCode,
                BerthFrom = assignment.BerthFrom,
                BerthTo = assignment.BerthTo,
                DockAssignmentId = assignment.DockAssignmentId,
                ApprovedByOrgId = approverOrg?.OrganizationId.Value ?? callerOrgId ?? Guid.Empty,
                ApprovedByOrgName = approverOrg?.LegalName ?? "Port Authority",
                ApprovedByUserId = callerUserId != Guid.Empty ? callerUserId : null,
                ApprovedAt = approvalTimestamp,
                SubmittedByOrgId = vvn.OrganizationId.Value,
                SubmittedByOrgName = submitterOrg?.LegalName ?? "Unknown",
                SubmittedByUserId = vvn.SubmittedById?.Value,
                SubmittedAt = vvn.SubmittedAt
            };
        }

        public async Task<VvnRejectionResponse> RejectVvnAsync(Guid vvnId, VvnRejectionRequest request, Guid callerUserId, Guid? callerOrgId)
        {
            // Validation
            if (request is null || string.IsNullOrWhiteSpace(request.Reason))
                throw new ArgumentException("A rejection reason is required.");

            // Load VVN
            var vvn = await _unitOfWork.Context.VesselVisitNotifications
                               .SingleOrDefaultAsync(v => v.VvnGuid == vvnId);
            
            if (vvn == null) 
                throw new KeyNotFoundException("VVN not found.");

            // Only SUBMITTED can be rejected
            if (vvn.State != VVNState.SUBMITTED)
                throw new InvalidOperationException("Only pending (SUBMITTED) VVNs can be rejected.");

            var rejectorUserId = callerUserId != Guid.Empty 
                ? callerUserId 
                : Guid.Parse("00000000-0000-0000-0000-000000000001"); // System/Port Authority placeholder

            var rejectionTimestamp = DateTime.UtcNow;

            // Transition domain state
            vvn.Reject(rejector: new UserId(rejectorUserId), reason: request.Reason);

            // Add decision log
            _unitOfWork.Context.DecisionLogs.Add(Domain.Visits.DecisionLog.Rejected(
                vvn.VvnGuid,
                new UserId(rejectorUserId),
                request.Reason,
                rejectionTimestamp,
                request.Notes));

            await _unitOfWork.CommitAsync();

            // Load organization details for response
            var rejectorOrg = callerOrgId.HasValue 
                ? await _unitOfWork.Context.Organizations.FindAsync(callerOrgId.Value)
                : null;
            
            var submitterOrg = await _unitOfWork.Context.Organizations.FindAsync(vvn.OrganizationId.Value);

            return new VvnRejectionResponse
            {
                VvnId = vvn.VvnGuid,
                VvnBusinessId = vvn.VvnBusinessId,
                Status = vvn.State.ToString(),
                RejectionReason = vvn.RejectionReason!,
                RejectedByOrgId = rejectorOrg?.OrganizationId.Value ?? callerOrgId ?? Guid.Empty,
                RejectedByOrgName = rejectorOrg?.LegalName ?? "Port Authority",
                RejectedByUserId = callerUserId != Guid.Empty ? callerUserId : null,
                RejectedAt = rejectionTimestamp,
                SubmittedByOrgId = vvn.OrganizationId.Value,
                SubmittedByOrgName = submitterOrg?.LegalName ?? "Unknown",
                SubmittedByUserId = vvn.SubmittedById?.Value,
                SubmittedAt = vvn.SubmittedAt
            };
        }

        public async Task<VesselVisitNotification> ValidateManifestTypeAsync(Guid vvnId, ManifestType manifestType)
        {
            var vvn = await _unitOfWork.Context.VesselVisitNotifications.FindAsync(vvnId);
            if (vvn == null)
                throw new KeyNotFoundException("VVN not found.");

            switch (vvn.VisitPurpose)
            {
                case VisitPurpose.LOAD:
                    if (manifestType == ManifestType.Unload)
                        throw new InvalidOperationException("This VVN is for loading only; unloading entries are not allowed.");
                    break;

                case VisitPurpose.UNLOAD:
                    if (manifestType == ManifestType.Load)
                        throw new InvalidOperationException("This VVN is for unloading only; loading entries are not allowed.");
                    break;

                case VisitPurpose.MAINTENANCE:
                    throw new InvalidOperationException("Maintenance visits cannot have cargo manifests.");
            }

            return vvn;
        }

        public async Task<VesselVisitNotification> ReopenVvnAsync(Guid vvnId, Guid? callerOrgId)
        {
            // Validate organization ownership
            if (!await ValidateOrganizationOwnershipAsync(vvnId, callerOrgId))
                throw new UnauthorizedAccessException("VVN belongs to a different organization.");

            // Load VVN
            var vvn = await _unitOfWork.Context.VesselVisitNotifications
                               .SingleOrDefaultAsync(v => v.VvnGuid == vvnId);
            
            if (vvn == null)
                throw new KeyNotFoundException("VVN not found.");

            // Only REJECTED VVNs can be reopened
            if (vvn.State != VVNState.REJECTED)
                throw new InvalidOperationException($"Only rejected VVNs can be reopened. Current state: {vvn.State}");

            // Call domain method to transition state
            vvn.ReopenToDraft();

            _unitOfWork.VesselVisitNotifications.Update(vvn);
            await _unitOfWork.CommitAsync();

            return vvn;
        }

        public async Task<List<object>> GetDecisionLogsAsync(Guid vvnId)
        {
            var logs = await _unitOfWork.Context.DecisionLogs
                .Where(d => d.VvnGuid == vvnId)
                .OrderBy(d => d.AtUtc)
                .Select(d => new
                {
                    d.DecisionLogId,
                    Outcome = d.Outcome.ToString(),
                    OfficerId = d.OfficerUserId.Value,
                    d.AtUtc,
                    d.DockAssignmentId,
                    d.Reason,
                    d.Notes
                })
                .ToListAsync();

            return logs.Cast<object>().ToList();
        }

        public async Task<bool> ValidateOrganizationOwnershipAsync(Guid vvnId, Guid? callerOrgId)
        {
            if (!callerOrgId.HasValue)
                return false;

            var vvn = await _unitOfWork.Context.VesselVisitNotifications
                               .Where(v => v.VvnGuid == vvnId)
                               .FirstOrDefaultAsync();
            
            if (vvn == null)
                return false;

            return vvn.OrganizationId.Value == callerOrgId.Value;
        }

    }

}
