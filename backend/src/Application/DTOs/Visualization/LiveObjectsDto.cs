namespace DDDNetCore.Application.DTOs.Visualization;

public sealed class Vessel3DDto
{
    public Guid Id { get; set; }
    public string Name { get; set; } = default!;
    public string DockId { get; set; } = default!;
    public float Length { get; set; }
    public float Width { get; set; }
    public float Height { get; set; }
}

public sealed class Resource3DDto
{
    public Guid Id { get; set; }
    public string Type { get; set; } = default!;        // "STS_CRANE", "YARD_GANTRY", etc.
    public string AssignedAreaId { get; set; } = default!;
}

public sealed class LiveObjectsDto
{
    public List<Vessel3DDto> Vessels { get; set; } = new();
    public List<Resource3DDto> Resources { get; set; } = new();
}
