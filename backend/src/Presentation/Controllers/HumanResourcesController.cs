using DDDNetCore.Domain.HumanResources;
using DDDNetCore.Application.DTOs.HumanResources;
using DDDNetCore.Application.Interfaces;
using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Authorization;

namespace DDDNetCore.Presentation.Controllers;

[ApiController]
[Route("api/staff")]
[Authorize] // Require authentication for all staff endpoints
public class HumanResourcesController : ControllerBase
{
    private readonly IStaffMemberService _service;

    public HumanResourcesController(IStaffMemberService service)
    {
        _service = service;
    }

    // POST /api/staff
    [HttpPost]
    public async Task<IActionResult> Create([FromBody] CreateStaffMemberDto dto)
    {
        try
        {
            var result = await _service.CreateAsync(dto);
            return CreatedAtAction(nameof(Get), 
                new { id = result.MecanographicNumber }, 
                result);
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

    // GET /api/staff?id=xxx&name=xxx&status=xxx&qualification=xxx
    // GET /api/staff (retorna todos)
    [HttpGet]
    public async Task<IActionResult> Get(
        [FromQuery] long? id, 
        [FromQuery] string? name, 
        [FromQuery] HumanResourceStatus? status,
        [FromQuery] string? qualification)
    {
        // If searching by ID, return single member
        if (id.HasValue)
        {
            var member = await _service.GetByIdAsync(id.Value);
            return member == null ? NotFound() : Ok(new[] { member });
        }

        // If searching by status only
        if (status.HasValue && string.IsNullOrWhiteSpace(name) && string.IsNullOrWhiteSpace(qualification))
        {
            var members = await _service.GetByStatusAsync(status.Value);
            return Ok(members);
        }

        // For complex filtering or getting all
        if (!string.IsNullOrWhiteSpace(name) || status.HasValue || !string.IsNullOrWhiteSpace(qualification))
        {
            var filtered = await _service.SearchAsync(name, status, qualification);
            return Ok(filtered);
        }

        // Get all
        var allStaff = await _service.GetAllAsync();
        return Ok(allStaff);
    }

    // PUT /api/staff/{mecanographicNumber}
    [HttpPut("{mecanographicNumber}")]
    public async Task<IActionResult> Update(long mecanographicNumber, [FromBody] UpdateStaffMemberDto dto)
    {
        try
        {
            var result = await _service.UpdateAsync(mecanographicNumber, dto);
            return Ok(result);
        }
        catch (KeyNotFoundException)
        {
            return NotFound();
        }
    }

    // PATCH /api/staff/{mecanographicNumber} - Partial update (including status changes)
    // Note: This endpoint now handles status changes that were previously at /activate and /deactivate
    [HttpPatch("{mecanographicNumber}")]
    public async Task<IActionResult> PatchStatus(long mecanographicNumber, [FromBody] PatchStaffStatusDto dto)
    {
        try
        {
            var result = await _service.PatchAsync(mecanographicNumber, dto);
            return Ok(result);
        }
        catch (KeyNotFoundException)
        {
            return NotFound();
        }
    }

    // POST /api/staff/{mecanographicNumber}/qualifications/{qualificationId}
    [HttpPost("{mecanographicNumber}/qualifications/{qualificationId}")]
    public async Task<IActionResult> AddQualification(long mecanographicNumber, string qualificationId)
    {
        try
        {
            var result = await _service.AddQualificationAsync(mecanographicNumber, qualificationId);
            return Ok(result);
        }
        catch (KeyNotFoundException ex)
        {
            return NotFound(new { error = ex.Message });
        }
        catch (InvalidOperationException ex)
        {
            return BadRequest(new { error = ex.Message });
        }
    }

    // DELETE /api/staff/{mecanographicNumber}/qualifications/{qualificationId}
    [HttpDelete("{mecanographicNumber}/qualifications/{qualificationId}")]
    public async Task<IActionResult> RemoveQualification(long mecanographicNumber, string qualificationId)
    {
        try
        {
            await _service.RemoveQualificationAsync(mecanographicNumber, qualificationId);
            return NoContent();
        }
        catch (KeyNotFoundException ex)
        {
            return NotFound(new { error = ex.Message });
        }
    }
}
