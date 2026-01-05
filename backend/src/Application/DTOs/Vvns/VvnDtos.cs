using System;
using System.Collections.Generic;

namespace DDDNetCore.Application.DTOs.Vvns
{
    public class CreateVvnRequest
    {
        public string VesselImo { get; set; } = null!;
        public string Purpose { get; set; } = null!; // LOAD | UNLOAD | BOTH | MAINTENANCE
        public DateTime Eta { get; set; }           // UTC
        public DateTime Etd { get; set; }           // UTC
        public string CaptainName { get; set; } = null!;
        public string CaptainCitizenId { get; set; } = null!;
        public string CaptainNationality { get; set; } = null!; // 2-letter ISO 3166-1 alpha-2
        public int CrewCount { get; set; }
    }

    public class ManifestEntryDto
    {
        public string ContainerCode { get; set; } = null!;
        public int Bay { get; set; }
        public int Row { get; set; }
        public int Tier { get; set; }
        public string? GoodsDescription { get; set; }
        public bool Hazardous { get; set; }
    }

    public class ManifestDto
    {
        public List<ManifestEntryDto> Entries { get; set; } = new();
    }

    public class UpsertManifestsRequest
    {
        public ManifestDto? Loading { get; set; }
        public ManifestDto? Unloading { get; set; }
    }

    public class CrewMemberDto
    {
        public string Name { get; set; } = null!;
        public string CitizenId { get; set; } = null!;
        public string Nationality { get; set; } = null!;
    }

    public class SetCrewRequest
    {
        public string? CaptainName { get; set; }
        public List<CrewMemberDto> Members { get; set; } = new();
    }

    public class VvnSummaryResponse
    {
        public Guid Id { get; set; }
        public string VvnBusinessId { get; set; } = null!;
        public string Status { get; set; } = null!;
        public string Purpose { get; set; } = null!;
        public DateTime Eta { get; set; }
        public DateTime Etd { get; set; }
        public DateTime? SubmittedAt { get; set; }
        
        // Additional context information
        public string? VesselImo { get; set; }
        public string? VesselName { get; set; }
        public string? VesselType { get; set; }
        public string? AssignedDock { get; set; } // Dock code if approved
    }

    public class SetCrewSummaryRequest
    {
        public string CaptainName { get; set; } = null!;
        public string CaptainCitizenId { get; set; } = null!;
        public string CaptainNationality { get; set; } = null!; // 2-letter ISO 3166-1 alpha-2
        public int CrewCount { get; set; }
    }

    public class AddEntriesRequest
    {
        // "loading": true -> Loading manifest; false -> Unloading
        public bool Loading { get; set; }
        public List<ManifestEntryDto> Entries { get; set; } = new();
    }

    public class UpdateEntryRequest
    {
        // Any field you want to allow updating; all optional
        public string? ContainerCode { get; set; }
        public int? Bay { get; set; }
        public int? Row { get; set; }
        public int? Tier { get; set; }
        public string? GoodsDescription { get; set; }
        public bool? Hazardous { get; set; }
        
    }

}
