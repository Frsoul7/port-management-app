using System;
using System.Linq;
using System.Threading.Tasks;
using DDDNetCore.Domain.IRepository;
using DDDNetCore.Application.Interfaces;
using DDDNetCore.Domain.Organizations;   // OrganizationId VO
using DDDNetCore.Domain.Vessels;
using DDDNetCore.Application.DTOs.Vessels;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;
using DDDNetCore.Domain.Shared;          // ImoValidator
using DDDNetCore.Application.Security;   // CallerContext & AppRole

namespace DDDNetCore.Presentation.Controllers
{
    [ApiController]
    [Route("api/[controller]")]
    public class VesselsController : ControllerBase
    {
        private readonly IVesselService _service;
        
        public VesselsController(IVesselService service)
        {
            _service = service;
        }

        // POST /api/vessels (US 2.2.2 - Port Authority Only)
        [HttpPost]
        [Authorize(Policy = "PortAuthority")] // Phase 6: Declarative authorization
        public async Task<IActionResult> Create([FromBody] CreateVesselDto dto)
        {
            try
            {
                // Get caller context (authorization already verified by policy)
                var caller = CallerContextFactory.FromHeaders(Request.Headers, requireUserId: false);
                var vesselDto = await _service.CreateVesselAsync(dto, caller.Role.ToString(), caller.OrgId);
                // Return created vessel with 201 status (no Location header since GET by IMO was removed)
                return Created(string.Empty, vesselDto);
            }
            catch (UnauthorizedAccessException ex)
            {
                return Problem(title: "Unauthorized", detail: ex.Message, statusCode: ex.Message.Contains("Forbidden") ? 403 : 401);
            }
            catch (ArgumentException ex)
            {
                return BadRequest(new { error = ex.Message });
            }
            catch (InvalidOperationException ex)
            {
                return Conflict(new { error = ex.Message });
            }
        }

        // PUT /api/vessels/{imo}
        [HttpPut("{imo}")]
        public async Task<IActionResult> Update(string imo, [FromBody] UpdateVesselDto dto)
        {
            try
            {
                var vesselDto = await _service.UpdateVesselAsync(imo, dto);
                return Ok(vesselDto);
            }
            catch (KeyNotFoundException)
            {
                return NotFound();
            }
            catch (ArgumentException ex)
            {
                return BadRequest(new { error = ex.Message });
            }
            catch (InvalidOperationException ex)
            {
                return Conflict(new { error = ex.Message });
            }
        }

        // Removed: GET /api/vessels/{imo} - Redundant with search endpoint (US 2.2.2 allows searching by IMO)

        // GET /api/vessels?imo=&name=&organizationName=
        // US 2.2.2: Search vessels by IMO, name, or organization (operator)
        // US 2.2.8: Shipping Agents need to see vessels to create VVNs
        [HttpGet]
        [Authorize] // All authenticated users can search vessels
        public async Task<IActionResult> Search(
            [FromQuery] string? imo,
            [FromQuery] string? name,
            [FromQuery(Name = "organizationName")] string? organizationName)
        {
            var vesselDtos = await _service.SearchVesselsAsync(imo, name, organizationName);
            return Ok(vesselDtos);
        }
    }
}
