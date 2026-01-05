using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Authorization;
using DDDNetCore.Application.Interfaces;
using DDDNetCore.Application.DTOs.Resources;

namespace DDDNetCore.Presentation.Controllers
{
    [ApiController]
    [Route("api/resources")]
    [Authorize] // Require authentication for all physical resource endpoints
    public class PhysicalResourcesController : ControllerBase
    {
        private readonly IPhysicalResourceService _service;

        public PhysicalResourcesController(IPhysicalResourceService service)
        {
            _service = service;
        }

        /// <summary>
        /// Get all physical resources or search/filter by criteria
        /// </summary>
        [HttpGet]
        public async Task<IActionResult> GetAll(
            [FromQuery] string? code,
            [FromQuery] string? description,
            [FromQuery] Domain.Resources.PhysicalResourceAvailability? availability)
        {
            var resources = await _service.GetAllAsync(code, description, availability);
            return Ok(resources);
        }

        /// <summary>
        /// Get a specific resource by code
        /// </summary>
        [HttpGet("{code}")]
        public async Task<IActionResult> GetByCode(string code)
        {
            var resource = await _service.GetByCodeAsync(code);
            if (resource == null)
                return NotFound(new { message = $"Physical resource with code '{code}' not found." });

            return Ok(resource);
        }

        /// <summary>
        /// POST /api/physical-resources - Create any physical resource (unified endpoint)
        /// ResourceType discriminator: "STS_CRANE" or "MOBILE_EQUIPMENT"
        /// </summary>
        [HttpPost]
        public async Task<IActionResult> CreateResource([FromBody] CreatePhysicalResourceDto dto)
        {
            try
            {
                var result = await _service.CreateResourceAsync(dto);
                return CreatedAtAction(nameof(GetByCode), new { code = result.Code }, result);
            }
            catch (InvalidOperationException ex)
            {
                return Conflict(new { message = ex.Message });
            }
            catch (ArgumentException ex)
            {
                return BadRequest(new { message = ex.Message });
            }
        }

        /// <summary>
        /// [DEPRECATED] Use POST /api/physical-resources with ResourceType="STS_CRANE" instead
        /// Register a new STS Crane (status defaults to AVAILABLE)
        /// </summary>
        [HttpPost("sts-cranes")]
        [Obsolete("Use POST /api/physical-resources with ResourceType discriminator instead")]
        public async Task<IActionResult> CreateSTSCrane([FromBody] CreateSTSCraneDto dto)
        {
            try
            {
                var result = await _service.CreateSTSCraneAsync(dto);
                return CreatedAtAction(nameof(GetByCode), new { code = result.Code }, result);
            }
            catch (InvalidOperationException ex)
            {
                return Conflict(new { message = ex.Message });
            }
            catch (ArgumentException ex)
            {
                return BadRequest(new { message = ex.Message });
            }
        }

        /// <summary>
        /// [DEPRECATED] Use POST /api/physical-resources with ResourceType="MOBILE_EQUIPMENT" instead
        /// Register a new Mobile Equipment (status defaults to AVAILABLE)
        /// </summary>
        [HttpPost("mobile-equipment")]
        [Obsolete("Use POST /api/physical-resources with ResourceType discriminator instead")]
        public async Task<IActionResult> CreateMobileEquipment([FromBody] CreateMobileEquipmentDto dto)
        {
            try
            {
                var result = await _service.CreateMobileEquipmentAsync(dto);
                return CreatedAtAction(nameof(GetByCode), new { code = result.Code }, result);
            }
            catch (InvalidOperationException ex)
            {
                return Conflict(new { message = ex.Message });
            }
            catch (ArgumentException ex)
            {
                return BadRequest(new { message = ex.Message });
            }
        }

        /// <summary>
        /// PUT /api/physical-resources/{code} - Update any physical resource (unified endpoint)
        /// </summary>
        [HttpPut("{code}")]
        public async Task<IActionResult> UpdateResource(string code, [FromBody] UpdatePhysicalResourceDto dto)
        {
            try
            {
                var result = await _service.UpdateResourceAsync(code, dto);
                return Ok(result);
            }
            catch (KeyNotFoundException ex)
            {
                return NotFound(new { message = ex.Message });
            }
            catch (ArgumentException ex)
            {
                return BadRequest(new { message = ex.Message });
            }
        }

        /// <summary>
        /// [DEPRECATED] Use PUT /api/physical-resources/{code} instead
        /// Update an existing STS Crane (code cannot be changed)
        /// </summary>
        [HttpPut("sts-cranes/{code}")]
        [Obsolete("Use PUT /api/physical-resources/{code} instead")]
        public async Task<IActionResult> UpdateSTSCrane(string code, [FromBody] UpdateSTSCraneDto dto)
        {
            try
            {
                var result = await _service.UpdateSTSCraneAsync(code, dto);
                return Ok(result);
            }
            catch (KeyNotFoundException ex)
            {
                return NotFound(new { message = ex.Message });
            }
            catch (ArgumentException ex)
            {
                return BadRequest(new { message = ex.Message });
            }
        }

        /// <summary>
        /// [DEPRECATED] Use PUT /api/physical-resources/{code} instead
        /// Update an existing Mobile Equipment (code cannot be changed)
        /// </summary>
        [HttpPut("mobile-equipment/{code}")]
        [Obsolete("Use PUT /api/physical-resources/{code} instead")]
        public async Task<IActionResult> UpdateMobileEquipment(string code, [FromBody] UpdateMobileEquipmentDto dto)
        {
            try
            {
                var result = await _service.UpdateMobileEquipmentAsync(code, dto);
                return Ok(result);
            }
            catch (KeyNotFoundException ex)
            {
                return NotFound(new { message = ex.Message });
            }
            catch (ArgumentException ex)
            {
                return BadRequest(new { message = ex.Message });
            }
        }

        /// <summary>
        /// PATCH /api/physical-resources/{code} - Partial update (activate/deactivate/availability)
        /// Note: This endpoint consolidates /activate, /deactivate, and /availability
        /// </summary>
        [HttpPatch("{code}")]
        public async Task<IActionResult> PatchResource(string code, [FromBody] PatchPhysicalResourceDto dto)
        {
            try
            {
                var result = await _service.PatchResourceAsync(code, dto);
                return Ok(result);
            }
            catch (KeyNotFoundException ex)
            {
                return NotFound(new { message = ex.Message });
            }
            catch (ArgumentException ex)
            {
                return BadRequest(new { message = ex.Message });
            }
        }
    }
}
