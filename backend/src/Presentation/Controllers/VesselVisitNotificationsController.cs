using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;

using DDDNetCore.API.Contracts;
using DDDNetCore.Application.Security;
using DDDNetCore.Application.Interfaces;
using DDDNetCore.Domain.Visits;
using DDDNetCore.Domain.Visits.Manifests;
using DDDNetCore.Domain.Visits.Crew;
using DDDNetCore.Application.DTOs.Vvns;
using DDDNetCore.Domain.Organizations;

namespace DDDNetCore.Presentation.Controllers
{
    [ApiController]
    [Route("api/vvns")]
    public class VesselVisitNotificationsController : ControllerBase
    {
        private readonly IVesselVisitService _svc;

        public VesselVisitNotificationsController(IVesselVisitService svc)
        {
            _svc = svc;
        }

        // --- simple 403 helper to avoid ASP.NET auth middleware Forbid() requirement ---
        private ActionResult Forbidden(string? detail = null) =>
            Problem(title: "Forbidden", detail: detail ?? "You are not allowed to perform this action.", statusCode: 403);

        /// <summary>
        /// Get all vessel visit notifications (VVNs)
        /// </summary>
        /// <remarks>
        /// **Required Role:** PORT_AUTHORITY_OFFICER, LOGISTICS_OPERATOR, ADMINISTRATOR
        /// 
        /// Returns all VVNs in the system.
        /// - Port Authority Officers can review VVNs for approval/rejection (US 2.2.7)
        /// - Logistics Operators can view VVNs for planning purposes
        /// </remarks>
        /// <response code="200">Returns the list of all VVNs</response>
        /// <response code="401">Unauthorized - No valid token</response>
        /// <response code="403">Forbidden - Insufficient permissions</response>
        [HttpGet]
        [Authorize] // Basic authentication required
        public async Task<ActionResult<IEnumerable<VvnSummaryResponse>>> GetAll()
        {
            try
            {
                var caller = CallerContextFactory.FromHeaders(Request.Headers, requireUserId: false);
                var response = await _svc.GetAllVvnsAsync(caller.Role, caller.OrgId);
                return Ok(response);
            }
            catch (UnauthorizedAccessException ex)
            {
                return Forbidden(ex.Message);
            }
        }

        // GET /vvns/{id}
        [HttpGet("{id:guid}")]
        [Authorize] // Basic authentication required
        public async Task<ActionResult<VvnSummaryResponse>> GetById([FromRoute] Guid id)
        {
            try
            {
                var caller = CallerContextFactory.FromHeaders(Request.Headers, requireUserId: false);
                var response = await _svc.GetVvnByIdAsync(id, caller.Role, caller.OrgId);
                return Ok(response);
            }
            catch (KeyNotFoundException)
            {
                return NotFound();
            }
            catch (UnauthorizedAccessException ex)
            {
                return Forbidden(ex.Message);
            }
        }

        // GET /vvns/my-organization  (US 2.2.10 - Shipping Agent Representatives only)
        /// <summary>
        /// Gets all VVNs for the caller's organization with optional filters
        /// </summary>
        /// <remarks>
        /// **Required Role:** SHIPPING_AGENT_REPRESENTATIVE, ADMINISTRATOR
        /// 
        /// **Organization-Scoped:** Only returns VVNs from the authenticated user's organization.
        /// 
        /// Supports filtering by vessel IMO, status, representative, and time range.
        /// Shipping Agent Representatives can view VVNs submitted by themselves or other 
        /// representatives from the same organization.
        /// </remarks>
        /// <param name="vesselImo">Filter by vessel IMO number</param>
        /// <param name="status">Filter by VVN status (Draft, Submitted, Approved, Rejected, etc.)</param>
        /// <param name="submittedById">Filter by representative who submitted the VVN</param>
        /// <param name="fromDate">Filter VVNs from this date onwards</param>
        /// <param name="toDate">Filter VVNs up to this date</param>
        /// <response code="200">Returns the filtered list of VVNs</response>
        /// <response code="401">Unauthorized - No valid token</response>
        /// <response code="403">Forbidden - User not authorized or missing organization ID</response>
        [HttpGet("my-organization")]
        [Authorize(Policy = "ShippingAgent")] // Phase 6: Declarative authorization
        public async Task<ActionResult<IEnumerable<VvnStatusResponse>>> GetMyOrganizationVvns(
            [FromQuery] string? vesselImo = null,
            [FromQuery] string? status = null,
            [FromQuery] Guid? submittedById = null,
            [FromQuery] DateTime? fromDate = null,
            [FromQuery] DateTime? toDate = null)
        {
            try
            {
                // Get caller context (authorization already verified by policy)
                var caller = CallerContextFactory.FromHeaders(Request.Headers, requireUserId: false);

                // Parse status enum if provided
                VVNState? statusEnum = null;
                if (!string.IsNullOrWhiteSpace(status))
                {
                    if (!Enum.TryParse<VVNState>(status, ignoreCase: true, out var parsed))
                    {
                        return BadRequest(Problem(
                            title: "Invalid status", 
                            detail: $"Status must be one of: {string.Join(", ", Enum.GetNames<VVNState>())}", 
                            statusCode: 400));
                    }
                    statusEnum = parsed;
                }

                // Get VVNs from service with filters
                var vvns = await _svc.GetVvnsForOrganizationAsync(
                    organizationId: new OrganizationId(caller.OrgId!.Value),
                    vesselImo: vesselImo,
                    status: statusEnum,
                    submittedById: submittedById,
                    fromDate: fromDate,
                    toDate: toDate);

                // Map to detailed status response
                var response = vvns.Select(v => new VvnStatusResponse
                {
                    Id = v.VvnGuid,
                    VvnBusinessId = v.VvnBusinessId,
                    Status = v.State.ToString(),
                    VesselImo = v.VesselImo,
                    Purpose = v.VisitPurpose.ToString(),
                    Eta = v.Eta,
                    Etd = v.Etd,
                    CreatedAt = v.CreatedAt,
                    SubmittedAt = v.SubmittedAt,
                    ApprovedAt = v.ApprovedAt,
                    RejectedAt = v.RejectedAt,
                    SubmittedById = v.SubmittedById?.Value,
                    CaptainName = v.CaptainName,
                    CaptainCitizenId = v.CaptainCitizenId,
                    CaptainNationality = v.CaptainNationality,
                    CrewCount = v.CrewCount,
                    CrewMembers = v.CrewMembers?.Select(cm => new CrewMemberDto
                    {
                        Name = cm.Name,
                        CitizenId = cm.CitizenId,
                        Nationality = cm.Nationality
                    }).ToList() ?? new List<CrewMemberDto>(),
                    LoadingCount = v.LoadingCount,
                    UnloadingCount = v.UnloadingCount,
                    
                    // Include dock assignment details for approved VVNs
                    DockAssignment = v.DockAssignment != null ? new DockAssignmentInfo
                    {
                        DockAssignmentId = v.DockAssignment.DockAssignmentId,
                        DockCode = v.DockAssignment.DockCode,
                        BerthFrom = v.DockAssignment.BerthFrom,
                        BerthTo = v.DockAssignment.BerthTo,
                        Status = v.DockAssignment.Status.ToString()
                    } : null,
                    
                    ApprovedById = v.ApprovedById?.Value,
                    
                    // Include rejection details for rejected VVNs
                    RejectionReason = v.RejectionReason,
                    RejectedById = v.RejectedById?.Value
                }).ToList();

                return Ok(response);
            }
            catch (UnauthorizedAccessException ex)
            {
                return Unauthorized(Problem(title: "Unauthorized", detail: ex.Message, statusCode: 401));
            }
            catch (ArgumentException ex)
            {
                return BadRequest(Problem(title: "Invalid request", detail: ex.Message, statusCode: 400));
            }
        }

        // GET /vvns/{id}/entries?type=Load|Unload  (omit 'type' to get both)
        [HttpGet("{id:guid}/entries")]
        public async Task<ActionResult<IEnumerable<VvnEntryResponse>>> GetEntries([FromRoute] Guid id, [FromQuery] string? type = null)
        {
            try
            {
                // optional type parsing
                ManifestType? mt = null;
                if (!string.IsNullOrWhiteSpace(type))
                {
                    if (!Enum.TryParse<ManifestType>(type, true, out var parsed))
                        return BadRequest(new { error = "type must be Load or Unload" });
                    mt = parsed;
                }

                var entries = await _svc.GetEntriesAsync(id, mt);
                return Ok(entries);
            }
            catch (KeyNotFoundException)
            {
                return NotFound();
            }
        }

        // POST /vvns/{id}/entries  (add ONE entry)
        [HttpPost("{id:guid}/entries")]
        [Authorize(Policy = "ShippingAgent")] // Phase 6: Declarative authorization
        public async Task<ActionResult> AddManifestEntry([FromRoute] Guid id, [FromBody] AddManifestEntryRequest req)
        {
            try
            {
                // Get caller context (authorization already verified by policy)
                var caller = CallerContextFactory.FromHeaders(Request.Headers, requireUserId: false);
                if (!await _svc.ValidateOrganizationOwnershipAsync(id, caller.OrgId))
                    return Forbidden("VVN belongs to a different organization.");

                // Validate manifest type compatibility with visit purpose
                await _svc.ValidateManifestTypeAsync(id, req.Type);

                // Build domain entry (maps ContainerCode -> ContainerUniqueId)
                var entry = ManifestEntry.Create(
                    containerUniqueId: req.ContainerCode,
                    hazardous: req.Hazardous,
                    bay: req.Bay,
                    row: req.Row,
                    tier: req.Tier,
                    goods: req.GoodsDescription
                );

                var entryId = await _svc.AddManifestEntryAsync(id, req.Type, entry);

                return Ok(new
                {
                    vvnId = id,
                    type = req.Type.ToString(),
                    entryId
                });
            }
            catch (UnauthorizedAccessException ex)
            {
                return Unauthorized(Problem(title: "Unauthorized", detail: ex.Message, statusCode: 401));
            }
            catch (InvalidOperationException ex)
            {
                // e.g., not editable (must be DRAFT/REJECTED)
                return Conflict(Problem(title: "Invalid state", detail: ex.Message, statusCode: 409));
            }
            catch (ArgumentException ex)
            {
                return UnprocessableEntity(Problem(title: "Validation failed", detail: ex.Message, statusCode: 422));
            }
        }


        // POST /vvns  (AGENT-ONLY)
        [HttpPost]
        [Authorize(Policy = "ShippingAgent")] // Phase 6: Declarative authorization
        public async Task<ActionResult<VvnSummaryResponse>> Create([FromBody] CreateVvnRequest req)
        {
            try
            {
                // Get caller context (authorization already verified by policy)
                var caller = CallerContextFactory.FromHeaders(Request.Headers, requireUserId: false);

                var vvnDto = await _svc.CreateVvnAsync(req, new OrganizationId(caller.OrgId!.Value));

                return CreatedAtAction(nameof(GetById), new { id = vvnDto.Id }, vvnDto);
            }
            catch (UnauthorizedAccessException ex)
            {
                return Unauthorized(Problem(title: "Unauthorized", detail: ex.Message, statusCode: 401));
            }
            catch (ArgumentOutOfRangeException ex)
            {
                return UnprocessableEntity(Problem(title: "Validation failed", detail: ex.Message, statusCode: 422));
            }
            catch (ArgumentException ex)
            {
                return UnprocessableEntity(Problem(title: "Validation failed", detail: ex.Message, statusCode: 422));
            }
        }

        // PUT /vvns/{id}  (AGENT-ONLY) - update VVN header while IN PROGRESS
        [HttpPut("{id:guid}")]
        [Authorize(Policy = "ShippingAgent")] // Phase 6: Declarative authorization
        public async Task<ActionResult<VvnSummaryResponse>> Update([FromRoute] Guid id, [FromBody] UpdateVvnRequest req)
        {
            try
            {
                // Get caller context (authorization already verified by policy)
                var caller = CallerContextFactory.FromHeaders(Request.Headers, requireUserId: false);
                if (!await _svc.ValidateOrganizationOwnershipAsync(id, caller.OrgId))
                    return Forbidden("VVN belongs to a different organization.");

                var vvnDto = await _svc.UpdateVvnAsync(id, req);

                return Ok(vvnDto);
            }
            catch (KeyNotFoundException)
            {
                return NotFound();
            }
            catch (InvalidOperationException ex)
            {
                // not editable after submit
                return Conflict(Problem(title: "Invalid state", detail: ex.Message, statusCode: 409));
            }
            catch (ArgumentException ex)
            {
                return UnprocessableEntity(Problem(title: "Validation failed", detail: ex.Message, statusCode: 422));
            }
        }


        // PUT /vvns/{id}/manifests  (AGENT-ONLY)

        [HttpPut("{id:guid}/manifests")]
        [Authorize(Policy = "ShippingAgent")] // Phase 6: Declarative authorization
        public async Task<ActionResult> UpsertManifests([FromRoute] Guid id, [FromBody] UpsertManifestsRequest req)
        {
            try
            {
                // Get caller context (authorization already verified by policy)
                var caller = CallerContextFactory.FromHeaders(Request.Headers, requireUserId: false);
                if (!await _svc.ValidateOrganizationOwnershipAsync(id, caller.OrgId))
                    return Forbidden("VVN belongs to a different organization.");

                // Validate manifest type based on which section has data
                ManifestType typeToValidate;
                if (req.Loading?.Entries?.Any() ?? false)
                    typeToValidate = ManifestType.Load;
                else if (req.Unloading?.Entries?.Any() ?? false)
                    typeToValidate = ManifestType.Unload;
                else
                    typeToValidate = ManifestType.Load; // Default, service will handle empty case

                await _svc.ValidateManifestTypeAsync(id, typeToValidate);

                // Build manifests
                CargoManifest? loading = null, unloading = null;

                if (req.Loading != null)
                {
                    var lm = CargoManifest.CreateLoading(id);
                    foreach (var e in req.Loading.Entries)
                    {
                        var entry = ManifestEntry.Create(
                            containerUniqueId: e.ContainerCode,
                            hazardous: e.Hazardous,
                            bay: e.Bay,
                            row: e.Row,
                            tier: e.Tier,
                            goods: e.GoodsDescription
                        );
                        lm.AddEntry(entry);
                    }
                    loading = lm;
                }

                if (req.Unloading != null)
                {
                    var um = CargoManifest.CreateUnloading(id);
                    foreach (var e in req.Unloading.Entries)
                    {
                        var entry = ManifestEntry.Create(
                            containerUniqueId: e.ContainerCode,
                            hazardous: e.Hazardous,
                            bay: e.Bay,
                            row: e.Row,
                            tier: e.Tier,
                            goods: e.GoodsDescription
                        );
                        um.AddEntry(entry);
                    }
                    unloading = um;
                }

                await _svc.ReplaceManifestsAsync(id, loading, unloading);

                return Ok(new
                {
                    vvnId = id,
                    loadingCount = loading?.Entries.Count ?? 0,
                    unloadingCount = unloading?.Entries.Count ?? 0
                });
            }
            catch (UnauthorizedAccessException ex)
            {
                return Unauthorized(Problem(title: "Unauthorized", detail: ex.Message, statusCode: 401));
            }
            catch (InvalidOperationException ex)
            {
                return Conflict(Problem(title: "Invalid state", detail: ex.Message, statusCode: 409));
            }
            catch (ArgumentException ex)
            {
                return UnprocessableEntity(Problem(title: "Validation failed", detail: ex.Message, statusCode: 422));
            }
        }


        // PUT /vvns/{id}/crew  (AGENT-ONLY)
        [HttpPost("{id:guid}/crew")]
        [Authorize(Policy = "ShippingAgent")] // Phase 6: Declarative authorization
        public async Task<ActionResult> SetCrew([FromRoute] Guid id, [FromBody] SetCrewRequest req)
        {
            try
            {
                // Get caller context (authorization already verified by policy)
                var caller = CallerContextFactory.FromHeaders(Request.Headers, requireUserId: false);
                if (!await _svc.ValidateOrganizationOwnershipAsync(id, caller.OrgId))
                    return Forbidden("VVN belongs to a different organization.");

                var members = (req.Members ?? new()).Select(m => new CrewMember(m.Name, m.CitizenId, m.Nationality)).ToList();
                await _svc.SetCrewAsync(id, req.CaptainName, members);
                return Ok(new { vvnId = id, crewCount = members.Count });
            }
            catch (UnauthorizedAccessException ex)
            {
                return Unauthorized(Problem(title: "Unauthorized", detail: ex.Message, statusCode: 401));
            }
            catch (InvalidOperationException ex)
            {
                return Conflict(Problem(title: "Invalid state", detail: ex.Message, statusCode: 409));
            }
            catch (ArgumentException ex)
            {
                return UnprocessableEntity(Problem(title: "Validation failed", detail: ex.Message, statusCode: 422));
            }
        }

        // POST /vvns/{id}:submit  (SHIPPING AGENT-ONLY)
        // NOTE: Non-RESTful endpoint with colon syntax and verb in URL
        // RESTful alternative: PATCH /vvns/{id} with { "status": "SUBMITTED" }
        // Kept for backward compatibility
        [HttpPost("{id:guid}:submit")]
        [Authorize(Policy = "ShippingAgent")] // Phase 6: Declarative authorization
        public async Task<ActionResult<VvnSummaryResponse>> Submit([FromRoute] Guid id)
        {
            try
            {
                // Get caller context (authorization already verified by policy)
                var caller = CallerContextFactory.FromHeaders(Request.Headers, requireUserId: false);
                if (!await _svc.ValidateOrganizationOwnershipAsync(id, caller.OrgId))
                    return Forbidden("VVN belongs to a different organization.");

                var vvn = await _svc.SubmitAsync(id, caller.UserId);

                return Ok(new VvnSummaryResponse
                {
                    Id = vvn.VvnGuid,
                    VvnBusinessId = vvn.VvnBusinessId,
                    Status = vvn.State.ToString(),
                    Purpose = vvn.VisitPurpose.ToString(),
                    Eta = vvn.Eta,
                    Etd = vvn.Etd,
                    SubmittedAt = vvn.SubmittedAt
                });
            }
            catch (UnauthorizedAccessException ex)
            {
                return Unauthorized(Problem(title: "Unauthorized", detail: ex.Message, statusCode: 401));
            }
            catch (InvalidOperationException ex)
            {
                return UnprocessableEntity(Problem(title: "Cannot submit", detail: ex.Message, statusCode: 422));
            }
            catch (KeyNotFoundException)
            {
                return NotFound();
            }
        }

        // DELETE /vvns/{id}/entries/{entryId}?type=Load|Unload  (remove ONE entry)
        [HttpDelete("{id:guid}/entries/{entryId:guid}")]
        [Authorize(Policy = "ShippingAgent")] // Phase 6: Declarative authorization
        public async Task<ActionResult> RemoveManifestEntry([FromRoute] Guid id, [FromRoute] Guid entryId, [FromQuery] ManifestType type)
        {
            try
            {
                // Get caller context (authorization already verified by policy)
                var caller = CallerContextFactory.FromHeaders(Request.Headers, requireUserId: false);
                if (!await _svc.ValidateOrganizationOwnershipAsync(id, caller.OrgId))
                    return Forbidden("VVN belongs to a different organization.");

                // Validate manifest type compatibility
                await _svc.ValidateManifestTypeAsync(id, type);

                await _svc.RemoveManifestEntryAsync(id, type, entryId);
                return NoContent();
            }
            catch (UnauthorizedAccessException ex)
            {
                return Unauthorized(Problem(title: "Unauthorized", detail: ex.Message, statusCode: 401));
            }
            catch (KeyNotFoundException)
            {
                return NotFound();
            }
            catch (InvalidOperationException ex)
            {
                return Conflict(Problem(title: "Invalid state", detail: ex.Message, statusCode: 409));
            }
        }


        // POST /vvns/{id}/approve    (PORT AUTHORITY ONLY)
        // NOTE: Non-RESTful endpoint with verb in URL
        // RESTful alternative: PATCH /vvns/{id} with { "status": "APPROVED", "dockCode": "...", ... }
        // OR POST /vvns/{id}/approval (approval as a resource)
        // Kept for backward compatibility and complex business logic
        [HttpPost("{id:guid}/approve")]
        [Authorize(Policy = "PortAuthority")] // Phase 6: Declarative authorization
        public async Task<ActionResult<VvnApprovalResponse>> Approve(
            [FromRoute] Guid id,
            [FromBody] VvnApprovalRequest req,
            CancellationToken ct)
        {
            try
            {
                var caller = CallerContextFactory.FromHeaders(Request.Headers, requireUserId: false);
                var approverUserId = caller.UserId != Guid.Empty 
                    ? caller.UserId 
                    : Guid.Parse("00000000-0000-0000-0000-000000000001"); // System/Port Authority placeholder

                var response = await _svc.ApproveVvnAsync(id, req, approverUserId, caller.OrgId);
                return Ok(response);
            }
            catch (ArgumentException ex)
            {
                return UnprocessableEntity(Problem(title: "Validation failed", detail: ex.Message, statusCode: 422));
            }
            catch (KeyNotFoundException ex)
            {
                return ex.Message.Contains("Dock") 
                    ? UnprocessableEntity(Problem(title: "Invalid dock", detail: ex.Message, statusCode: 422))
                    : NotFound();
            }
            catch (InvalidOperationException ex)
            {
                return ex.Message.Contains("overlap")
                    ? UnprocessableEntity(Problem(title: "Scheduling conflict", detail: ex.Message, statusCode: 422))
                    : Conflict(Problem(title: "Invalid state", detail: ex.Message, statusCode: 409));
            }
            catch (UnauthorizedAccessException ex)
            {
                return Problem(title: "Unauthorized", detail: ex.Message, statusCode: 401);
            }
        }

        // POST /vvns/{id}/reject    (PORT AUTHORITY ONLY)
        // NOTE: Non-RESTful endpoint with verb in URL
        // RESTful alternative: PATCH /vvns/{id} with { "status": "REJECTED", "reason": "..." }
        // OR POST /vvns/{id}/rejection (rejection as a resource)
        // Kept for backward compatibility and complex business logic
        [HttpPost("{id:guid}/reject")]
        [Authorize(Policy = "PortAuthority")] // Phase 6: Declarative authorization
        public async Task<ActionResult<VvnRejectionResponse>> Reject(
            [FromRoute] Guid id,
            [FromBody] VvnRejectionRequest req,
            CancellationToken ct)
        {
            try
            {
                var caller = CallerContextFactory.FromHeaders(Request.Headers, requireUserId: false);
                var rejectorUserId = caller.UserId != Guid.Empty 
                    ? caller.UserId 
                    : Guid.Parse("00000000-0000-0000-0000-000000000001"); // System/Port Authority placeholder

                var response = await _svc.RejectVvnAsync(id, req, rejectorUserId, caller.OrgId);
                return Ok(response);
            }
            catch (ArgumentException ex)
            {
                return UnprocessableEntity(Problem(title: "Validation failed", detail: ex.Message, statusCode: 422));
            }
            catch (KeyNotFoundException)
            {
                return NotFound();
            }
            catch (InvalidOperationException ex)
            {
                return Conflict(Problem(title: "Invalid state", detail: ex.Message, statusCode: 409));
            }
            catch (UnauthorizedAccessException ex)
            {
                return Problem(title: "Unauthorized", detail: ex.Message, statusCode: 401);
            }
        }

        // POST /vvns/{id}:reopen    (SHIPPING AGENT ONLY)
        /// <summary>
        /// Reopens a REJECTED VVN back to IN_PROGRESS state, allowing the shipping agent
        /// to make corrections and resubmit. This clears all rejection-related data.
        /// NOTE: Non-RESTful endpoint with colon syntax and verb in URL
        /// RESTful alternative: PATCH /vvns/{id} with { "status": "IN_PROGRESS" }
        /// OR DELETE /vvns/{id}/submission (delete submission to reopen)
        /// Kept for backward compatibility
        /// </summary>
        [HttpPost("{id:guid}:reopen")]
        [Authorize(Policy = "ShippingAgent")] // Phase 6: Declarative authorization
        public async Task<ActionResult> ReopenToDraft([FromRoute] Guid id)
        {
            try
            {
                // Get caller context (authorization already verified by policy)
                var caller = CallerContextFactory.FromHeaders(Request.Headers, requireUserId: false);

                // Reopen VVN through service
                var vvn = await _svc.ReopenVvnAsync(id, caller.OrgId);

                return Ok(new
                {
                    vvnId = vvn.VvnGuid,
                    status = vvn.State.ToString(),
                    message = "VVN has been reopened and is now in IN_PROGRESS state. You can edit and resubmit it."
                });
            }
            catch (UnauthorizedAccessException ex)
            {
                return Unauthorized(Problem(title: "Unauthorized", detail: ex.Message, statusCode: 401));
            }
            catch (InvalidOperationException ex)
            {
                return Conflict(Problem(title: "Invalid operation", detail: ex.Message, statusCode: 409));
            }
            catch (KeyNotFoundException)
            {
                return NotFound();
            }
        }

        [HttpGet("{id:guid}/decisions")]
        public async Task<ActionResult> GetDecisions([FromRoute] Guid id)
        {
            var logs = await _svc.GetDecisionLogsAsync(id);
            return Ok(logs);
        }

    }
}
