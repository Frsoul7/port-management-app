using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Authorization;
using DDDNetCore.Application.Interfaces;
using DDDNetCore.Application.DTOs.Docks;

namespace DDDNetCore.Presentation.Controllers
{
    [ApiController]
    [Route("api/[controller]")]
    public class DocksController : ControllerBase
    {
        private readonly IDockService _service;

        public DocksController(IDockService service)
        {
            _service = service;
        }

        /// <summary>
        /// Get all docks or search/filter by criteria
        /// </summary>
        [HttpGet]
        [Authorize] // All authenticated users can view docks (needed for VVN approval, physical resources, etc.)
        public async Task<IActionResult> GetAll([FromQuery] string? name, [FromQuery] string? location, [FromQuery] string? vesselTypeId)
        {
            var docks = await _service.GetAllAsync(name, location, vesselTypeId);
            return Ok(docks);
        }

        /// <summary>
        /// Get a specific dock by code
        /// </summary>
        [HttpGet("{code}")]
        [Authorize(Policy = "LogisticsPlanner")] // Phase 6.6: LogisticsPlanner can view dock details for planning
        public async Task<IActionResult> GetByCode(string code)
        {
            var dock = await _service.GetByCodeAsync(code);
            if (dock == null)
                return NotFound(new { message = $"Dock with code '{code}' not found." });

            return Ok(dock);
        }

        /// <summary>
        /// Register a new dock
        /// </summary>
        [HttpPost]
        public async Task<IActionResult> Create([FromBody] CreateDockDto dto)
        {
            try
            {
                var result = await _service.CreateAsync(dto);
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
        /// Update an existing dock.
        /// All fields except the Code (ID) can be updated.
        /// </summary>
        [HttpPut("{code}")]
        public async Task<IActionResult> Update(string code, [FromBody] UpdateDockDto dto)
        {
            try
            {
                var result = await _service.UpdateAsync(code, dto);
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
        /// Delete a dock
        /// </summary>
        [HttpDelete("{code}")]
        public async Task<IActionResult> Delete(string code)
        {
            try
            {
                await _service.DeleteAsync(code);
                return NoContent();
            }
            catch (KeyNotFoundException ex)
            {
                return NotFound(new { message = ex.Message });
            }
        }
    }
}
