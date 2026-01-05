using Microsoft.AspNetCore.Mvc;
using DDDNetCore.Application.Services;
using DDDNetCore.Application.DTOs.Visualization;

namespace DDDNetCore.Presentation.Controllers;

[ApiController]
[Route("api/[controller]")]
public sealed class VisualizationController : ControllerBase
{
    private readonly VisualizationAppService _service;

    public VisualizationController(VisualizationAppService service)
    {
        _service = service;
    }

    [HttpGet("layout")]
    public async Task<ActionResult<PortLayoutDto>> GetLayout(CancellationToken ct)
    {
        var layout = await _service.GetPortLayoutAsync(ct);
        return Ok(layout);
    }

    [HttpGet("materials")]
    public ActionResult<MaterialsConfigDto> GetMaterials()
    {
        var baseUrl = $"{Request.Scheme}://{Request.Host}/textures";

        var dto = new MaterialsConfigDto
        {
            Materials =
            {
                new MaterialConfigDto
                {
                    Category = "DOCK",
                    ColorMapUrl = $"{baseUrl}/dock_color.jpg",
                    NormalMapUrl = $"{baseUrl}/dock_normal.jpg",
                    RoughnessMapUrl = $"{baseUrl}/dock_roughness.jpg"
                },
                new MaterialConfigDto
                {
                    Category = "YARD",
                    ColorMapUrl = $"{baseUrl}/yard_color.jpg",
                    NormalMapUrl = $"{baseUrl}/yard_normal.jpg"
                },
                new MaterialConfigDto
                {
                    Category = "WAREHOUSE",
                    ColorMapUrl = $"{baseUrl}/warehouse_color.jpg",
                    NormalMapUrl = $"{baseUrl}/warehouse_normal.jpg"
                }
                // TODO: depois adicionas VESSEL, CRANE, etc.
            }
        };

        return Ok(dto);
    }

    [HttpGet("live-objects")]
    public ActionResult<LiveObjectsDto> GetLiveObjects()
    {
        var dto = _service.GetLiveObjects();
        return Ok(dto);
    }

}
