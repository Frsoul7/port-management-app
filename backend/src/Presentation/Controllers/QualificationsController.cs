using DDDNetCore.Application.DTOs.HumanResources;
using DDDNetCore.Application.Interfaces;
using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Authorization;

namespace DDDNetCore.Presentation.Controllers;

[ApiController]
[Route("api/[controller]")]
[Authorize] // Require authentication for all qualification endpoints
public class QualificationsController : ControllerBase
{
    private readonly IQualificationService _service;

    public QualificationsController(IQualificationService service)
    {
        _service = service;
    }

    // POST /api/qualifications
    [HttpPost]
    public async Task<IActionResult> Create([FromBody] CreateQualificationDto dto)
    {
        try
        {
            var qualificationDto = await _service.CreateAsync(dto);
            return CreatedAtAction(nameof(Get), new { id = qualificationDto.QualificationId }, qualificationDto);
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

    // GET /api/qualifications?id=xxx&name=yyy
    // GET /api/qualifications (retorna todas)
    [HttpGet]
    public async Task<IActionResult> Get([FromQuery] string? id, [FromQuery] string? name)
    {
        var qualificationDtos = await _service.SearchAsync(id, name);
        return Ok(qualificationDtos);
    }

    // PUT /api/qualifications/{id}
    [HttpPut("{id}")]
    public async Task<IActionResult> Update(string id, [FromBody] UpdateQualificationDto dto)
    {
        try
        {
            var qualificationDto = await _service.UpdateAsync(id, dto);
            return Ok(qualificationDto);
        }
        catch (KeyNotFoundException)
        {
            return NotFound();
        }
    }

    // DELETE /api/qualifications/{id}
    [HttpDelete("{id}")]
    public async Task<IActionResult> Delete(string id)
    {
        try
        {
            await _service.DeleteAsync(id);
            return NoContent();
        }
        catch (KeyNotFoundException)
        {
            return NotFound();
        }
    }
}
