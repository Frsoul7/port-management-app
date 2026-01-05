namespace DDDNetCore.Application.DTOs.Vvns
{
    public class VvnEntryResponse
    {
        public Guid Id { get; set; }
        public string Type { get; set; } = default!; // "Load" | "Unload"
        public string ContainerCode { get; set; } = default!;
        public bool Hazardous { get; set; }
        public int Bay { get; set; }
        public int Row { get; set; }
        public int Tier { get; set; }
        public string? GoodsDescription { get; set; }
    }
}
