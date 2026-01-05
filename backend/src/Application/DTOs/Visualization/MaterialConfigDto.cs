namespace DDDNetCore.Application.DTOs.Visualization;

public sealed class MaterialConfigDto
{
    public string Category { get; set; } = default!;  // DOCK, YARD, WAREHOUSE, VESSEL, CRANE...

    public string ColorMapUrl { get; set; } = default!;
    public string? NormalMapUrl { get; set; }
    public string? RoughnessMapUrl { get; set; }
    public string? BumpMapUrl { get; set; }

    public float Metalness { get; set; } = 0.1f;
    public float Roughness { get; set; } = 0.8f;
}

public sealed class MaterialsConfigDto
{
    public IList<MaterialConfigDto> Materials { get; set; } = new List<MaterialConfigDto>();
}
