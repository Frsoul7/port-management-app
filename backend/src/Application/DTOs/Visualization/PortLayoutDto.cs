namespace DDDNetCore.Application.DTOs.Visualization;

public sealed class PortLayoutDto
{
    public IList<DockLayoutDto> Docks { get; set; } = new List<DockLayoutDto>();
    public IList<StorageAreaLayoutDto> StorageAreas { get; set; } = new List<StorageAreaLayoutDto>();
}

public sealed class DockLayoutDto
{
    public string DockId { get; set; } = default!;  // <-- ALTERADO
    public string Name { get; set; } = default!;
    
    public float PositionX { get; set; }
    public float PositionY { get; set; }
    public float PositionZ { get; set; }
    public float Length { get; set; }
    public float Width { get; set; }
    public float Height { get; set; }
}


public sealed class StorageAreaLayoutDto
{
    public string StorageAreaId { get; set; } = default!;  // <-- ALTERADO
    public string Name { get; set; } = default!;
    public string Type { get; set; } = default!;   // "YARD" ou "WAREHOUSE"

    public float PositionX { get; set; }
    public float PositionY { get; set; }
    public float PositionZ { get; set; }

    public float Length { get; set; }
    public float Width { get; set; }
    public float Height { get; set; }
}