using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Authorization;
using System.ComponentModel.DataAnnotations;
using DDDNetCore.Application.Interfaces;
using DDDNetCore.Application.DTOs.Organizations;
using System.Threading.Tasks;
using System.Linq;
using System;
using System.Collections.Generic;

namespace DDDNetCore.Presentation.Controllers
{
    [ApiController]
    [Route("api/[controller]")]
    public class OrganizationsController : ControllerBase
    {
        private readonly IOrganizationService _service;

        public OrganizationsController(IOrganizationService service)
        {
            _service = service;
        }

        /// <summary>
        /// Get all organizations - Public endpoint for registration
        /// </summary>
        /// <remarks>
        /// This endpoint is public to allow users to select an organization during registration.
        /// No sensitive data is exposed - only organization names and IDs.
        /// </remarks>
        [HttpGet]
        [AllowAnonymous] // Allow unauthenticated access for registration
        public async Task<IActionResult> Get()
        {
            var dtoList = await _service.GetAllOrganizationsAsync();
            // Return simplified version for registration
            var simplified = dtoList.Select(o => new
            {
                Id = o.Id,
                OrganizationId = o.OrganizationId,
                Identifier = o.Identifier,
                LegalName = o.LegalName,
                Type = o.Type
            }).ToList();
            return Ok(simplified);
        }

        [HttpGet("{id}")]
        [Authorize] // Require authentication for individual organization details
        public async Task<IActionResult> Get(string id)
        {
            var orgDto = await _service.GetOrganizationByIdAsync(id);
            if (orgDto == null) return NotFound();
            return Ok(orgDto);
        }

        // -------------------------------
        // CREATE ORGANIZATION  (US 2.2.5)
        // -------------------------------
        [HttpPost]
        [Authorize] // Require authentication for creating organizations
        [ProducesResponseType(typeof(OrganizationResponseDto), 201)]
        [ProducesResponseType(400)]
        [ProducesResponseType(409)]
        public async Task<IActionResult> Post([FromBody] CreateOrganizationDto dto)
        {
            if (!ModelState.IsValid) return ValidationProblem(ModelState);

            try
            {
                var orgDto = await _service.CreateOrganizationAsync(dto);
                return CreatedAtAction(nameof(Get), new { id = orgDto.OrganizationId }, orgDto);
            }
            catch (ArgumentException ex)
            {
                return BadRequest(new { message = ex.Message });
            }
            catch (InvalidOperationException ex)
            {
                return Conflict(new { message = ex.Message });
            }
        }


        // -----------------------------------------------------------
        // ADD REPRESENTATIVES TO AN EXISTING ORG (US 2.2.6 - create)
        //   A) by GUID OrganizationId
        // -----------------------------------------------------------
        [HttpPost("{organizationId:guid}/representatives")]
        [Authorize] // Require authentication for adding representatives
        [ProducesResponseType(typeof(List<RepresentativeResponseDto>), 201)]
        [ProducesResponseType(400)]
        [ProducesResponseType(404)]
        [ProducesResponseType(409)]
        public async Task<IActionResult> AddRepresentativesById(Guid organizationId, [FromBody] AddRepresentativesDto dto)
        {
            if (!ModelState.IsValid) return ValidationProblem(ModelState);

            try
            {
                var orgDto = await _service.AddRepresentativesByIdAsync(organizationId, dto);
                var created = orgDto.Representatives
                    .OrderByDescending(r => r.CreatedAt)
                    .Take(dto.Representatives.Count)
                    .ToList();

                Response.Headers.Location = $"/api/organizations/{orgDto.OrganizationId}/representatives";
                return Created(Response.Headers.Location!, created);
            }
            catch (KeyNotFoundException ex)
            {
                return NotFound(new { message = ex.Message });
            }
            catch (ArgumentException ex)
            {
                return BadRequest(new { message = ex.Message });
            }
            catch (InvalidOperationException ex)
            {
                return Conflict(new { message = ex.Message });
            }
        }

        // -----------------------------------------------------------
        //   B) by public identifier (alphanumeric up to 10)
        // -----------------------------------------------------------
        [HttpPost("by-identifier/{identifier}/representatives")]
        [Authorize] // Require authentication for adding representatives
        [ProducesResponseType(typeof(List<RepresentativeResponseDto>), 201)]
        [ProducesResponseType(400)]
        [ProducesResponseType(404)]
        [ProducesResponseType(409)]
        public async Task<IActionResult> AddRepresentativesByIdentifier(string identifier, [FromBody] AddRepresentativesDto dto)
        {
            if (!ModelState.IsValid) return ValidationProblem(ModelState);

            try
            {
                var orgDto = await _service.AddRepresentativesByIdentifierAsync(identifier, dto);
                var created = orgDto.Representatives
                    .OrderByDescending(r => r.CreatedAt)
                    .Take(dto.Representatives.Count)
                    .ToList();

                Response.Headers.Location = $"/api/organizations/{orgDto.OrganizationId}/representatives";
                return Created(Response.Headers.Location!, created);
            }
            catch (KeyNotFoundException ex)
            {
                return NotFound(new { message = ex.Message });
            }
            catch (ArgumentException ex)
            {
                return BadRequest(new { message = ex.Message });
            }
            catch (InvalidOperationException ex)
            {
                return Conflict(new { message = ex.Message });
            }
        }

        [HttpPut("{organizationId:guid}/representatives/{repId:guid}")]
        [Authorize] // Require authentication for updating representatives
        [ProducesResponseType(typeof(RepresentativeResponseDto), 200)]
        [ProducesResponseType(400)]
        [ProducesResponseType(404)]
        [ProducesResponseType(409)]
        public async Task<IActionResult> UpdateRepresentative(Guid organizationId, Guid repId, [FromBody] UpdateRepresentativeDto dto)
        {
            if (!ModelState.IsValid) return ValidationProblem(ModelState);

            try
            {
                var repDto = await _service.UpdateRepresentativeAsync(organizationId, repId, dto);
                return Ok(repDto);
            }
            catch (KeyNotFoundException ex)
            {
                return NotFound(new { message = ex.Message });
            }
            catch (ArgumentException ex)
            {
                return BadRequest(new { message = ex.Message });
            }
            catch (InvalidOperationException ex)
            {
                return Conflict(new { message = ex.Message });
            }
        }

        // PATCH /api/organizations/{organizationId}/representatives/{repId} - Partial update (including status)
        // Note: This endpoint now handles status changes that were previously at /activate and /deactivate
        [HttpPatch("{organizationId:guid}/representatives/{repId:guid}")]
        [Authorize] // Require authentication for changing representative status
        [ProducesResponseType(typeof(RepresentativeResponseDto), 200)]
        [ProducesResponseType(404)]
        public async Task<IActionResult> PatchRepresentativeStatus(
            Guid organizationId, 
            Guid repId, 
            [FromBody] PatchRepresentativeStatusDto dto)
        {
            try
            {
                var repDto = await _service.PatchRepresentativeStatusAsync(organizationId, repId, dto);
                return Ok(repDto);
            }
            catch (KeyNotFoundException ex)
            {
                return NotFound(new { message = ex.Message });
            }
        }


    }



    // REMOVE THESE - moved to DTOs/Organizations/OrganizationResponseDto.cs
    // public record RepresentativeResponseDto(...);
    // public record OrganizationResponseDto(...) { ... }
}
