namespace DDDNetCore.API.Contracts
{
    using DDDNetCore.Domain.Visits;

    public class AddManifestEntryRequest
    {
        public ManifestType Type { get; set; }          // "Load" or "Unload"
        public string ContainerCode { get; set; } = default!; // we’ll store it as ContainerUniqueId
        public bool Hazardous { get; set; }
        public int Bay { get; set; }
        public int Row { get; set; }
        public int Tier { get; set; }
        public string? GoodsDescription { get; set; }
    }
}
