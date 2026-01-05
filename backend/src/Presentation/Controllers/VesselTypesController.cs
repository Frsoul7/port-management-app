using DDDNetCore.Application.DTOs.VesselTypes;
using DDDNetCore.Domain.IRepository;
using DDDNetCore.Application.Interfaces;
using DDDNetCore.Domain.Vessels;
using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Authorization;
using System.Linq;

namespace DDDNetCore.Presentation.Controllers;

[ApiController]
[Route("api/[controller]")]
[Authorize] // Require authentication for all vessel type endpoints
public class VesselTypesController : ControllerBase
{
    private readonly IVesselTypeService _service;
    
    public VesselTypesController(IVesselTypeService service)
    {
        _service = service;
    }

    // POST /api/vesseltypes  (Create)
    [HttpPost]
    public async Task<IActionResult> Create([FromBody] CreateVesselTypeDto dto)
    {
        try
        {
            var vesselTypeDto = await _service.CreateVesselTypeAsync(dto);
            // Return created vessel type with 201 status (no Location header since GET by ID was removed)
            return Created(string.Empty, vesselTypeDto);
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

    // PUT /api/vesseltypes/{id}  (Update)
    [HttpPut("{id}")]
    public async Task<IActionResult> Update(string id, [FromBody] UpdateVesselTypeDto dto)
    {
        try
        {
            var vesselTypeDto = await _service.UpdateVesselTypeAsync(id, dto);
            return Ok(vesselTypeDto);
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

    // Removed: GET /api/vesseltypes/{id} - Redundant with search endpoint (can search by name)

    // GET /api/vesseltypes?name=&description=&page=1&pageSize=20  (Search/Filter)
    [HttpGet]
    [Authorize(Policy = "PortAuthority")] // Phase 7: Port Authority can search vessel types (needed for vessel registration)
    public async Task<IActionResult> Search(
        [FromQuery] string? name,
        [FromQuery] string? description,
        [FromQuery] int page = 1,
        [FromQuery] int pageSize = 20)
    {
        var (items, total) = await _service.SearchVesselTypesAsync(name, description, page, pageSize);
        return Ok(new { total, page, pageSize, items });
    }
}
