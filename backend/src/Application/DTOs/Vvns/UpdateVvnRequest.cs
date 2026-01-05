namespace DDDNetCore.Application.DTOs.Vvns
{
    public class UpdateVvnRequest
    {
        // Keep all optional so clients can send just what they need (PUT with PATCH-like behavior).
        public string? VesselImo { get; set; }      // optional; see notes below
        public string? Purpose { get; set; }        // optional; see notes below: probably keep immutable for now
        public DateTime? Eta { get; set; }
        public DateTime? Etd { get; set; }
        public string? CaptainName { get; set; }
        public string? CaptainCitizenId { get; set; }
        public string? CaptainNationality { get; set; } // 2-letter ISO 3166-1 alpha-2
        public int? CrewCount { get; set; }
    }
}
