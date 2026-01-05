using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Authorization;
using DDDNetCore.Application.Interfaces;
using DDDNetCore.Domain.StorageAreas;
using DDDNetCore.Application.DTOs.StorageAreas;

namespace DDDNetCore.Presentation.Controllers
{
    [ApiController]
    [Route("api/[controller]")]
    public class StorageAreasController : ControllerBase
    {
        private readonly IStorageAreaService _service;

        public StorageAreasController(IStorageAreaService service)
        {
            _service = service;
        }

        /// <summary>
        /// Get all storage areas or search/filter by criteria
        /// </summary>
        [HttpGet]
        [Authorize] // All authenticated users can view storage areas (needed for VVN approval, physical resources, etc.)
        public async Task<IActionResult> GetAll(
            [FromQuery] string? name,
            [FromQuery] string? location,
            [FromQuery] StorageAreaType? type,
            [FromQuery] bool? servesAllDocks)
        {
            var storageAreas = await _service.GetAllAsync(name, location, type, servesAllDocks);
            return Ok(storageAreas);
        }

        /// <summary>
        /// Get a specific storage area by ID
        /// </summary>
        [HttpGet("{id}")]
        [Authorize(Policy = "LogisticsPlanner")]
        [Authorize(Policy = "PortAuthorityOfficer")]
        public async Task<IActionResult> GetById(string id)
        {
            var storageArea = await _service.GetByIdAsync(id);
            if (storageArea == null)
                return NotFound(new { message = $"Storage area with ID '{id}' not found." });

            return Ok(storageArea);
        }

        /// <summary>
        /// Register a new storage area
        /// </summary>
        [HttpPost]
        public async Task<IActionResult> Create([FromBody] CreateStorageAreaDto dto)
        {
            try
            {
                var result = await _service.CreateAsync(dto);
                return CreatedAtAction(
                    nameof(GetById),
                    new { id = result.StorageAreaId },
                    result);
            }
            catch (ArgumentException ex)
            {
                return BadRequest(new { message = ex.Message });
            }
        }

        /// <summary>
        /// Update an existing storage area
        /// </summary>
        [HttpPut("{id}")]
        public async Task<IActionResult> Update(string id, [FromBody] UpdateStorageAreaDto dto)
        {
            try
            {
                var result = await _service.UpdateAsync(id, dto);
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
            catch (InvalidOperationException ex)
            {
                return BadRequest(new { message = ex.Message });
            }
        }

        /// <summary>
        /// Update occupancy of a storage area
        /// </summary>
        [HttpPatch("{id}/occupancy")]
        public async Task<IActionResult> UpdateOccupancy(string id, [FromBody] UpdateOccupancyDto dto)
        {
            try
            {
                var result = await _service.UpdateOccupancyAsync(id, dto);
                return Ok(result);
            }
            catch (KeyNotFoundException ex)
            {
                return NotFound(new { message = ex.Message });
            }
            catch (Exception ex) when (ex is ArgumentException || ex is InvalidOperationException)
            {
                return BadRequest(new { message = ex.Message });
            }
        }

        /// <summary>
        /// Delete a storage area
        /// </summary>
        [HttpDelete("{id}")]
        public async Task<IActionResult> Delete(string id)
        {
            try
            {
                await _service.DeleteAsync(id);
                return NoContent();
            }
            catch (KeyNotFoundException ex)
            {
                return NotFound(new { message = ex.Message });
            }
            catch (InvalidOperationException ex)
            {
                return BadRequest(new { message = ex.Message });
            }
        }
    }
}
