using DDDNetCore.Domain.Docks;

namespace DDDNetCore.Domain.Vessels;

public class VesselType
{
    // NOTE: keeping string for ID to avoid ripple changes, can switch to Guid later.
    public string VesselTypeId { get; private set; } = default!;
    public string Name { get; private set; } = default!;
    public string? Description { get; private set; }

    // US 2.2.1 attributes
    public int? CapacityTEU { get; private set; }
    public int? MaxRows { get; private set; }
    public int? MaxBays { get; private set; }
    public int? MaxTiers { get; private set; }
    public string? OperationalConstraints { get; private set; }

    // Navigation (optional now; useful later when listing vessels by type)
    public ICollection<Vessel> Vessels { get; private set; } = new List<Vessel>();
    public ICollection<Dock> AllowedDocks { get; private set; } = new List<Dock>();


    private VesselType() { } // EF

    public VesselType(string id, string name)
    {
        if (string.IsNullOrWhiteSpace(id)) throw new ArgumentException("Id is required", nameof(id));
        if (string.IsNullOrWhiteSpace(name)) throw new ArgumentException("Name is required", nameof(name));

        VesselTypeId = id.Trim();
        Name = name.Trim();
    }

    public void Update(
        string name,
        string? description,
        int? capacityTeu,
        int? maxRows,
        int? maxBays,
        int? maxTiers,
        string? operationalConstraints)
    {
        if (string.IsNullOrWhiteSpace(name)) throw new ArgumentException("Name is required");
        if (capacityTeu is < 0) throw new ArgumentException("CapacityTEU cannot be negative");
        if (maxRows is < 0) throw new ArgumentException("MaxRows cannot be negative");
        if (maxBays is < 0) throw new ArgumentException("MaxBays cannot be negative");
        if (maxTiers is < 0) throw new ArgumentException("MaxTiers cannot be negative");

        Name = name.Trim();
        Description = string.IsNullOrWhiteSpace(description) ? null : description.Trim();
        CapacityTEU = capacityTeu;
        MaxRows = maxRows;
        MaxBays = maxBays;
        MaxTiers = maxTiers;
        OperationalConstraints = string.IsNullOrWhiteSpace(operationalConstraints) ? null : operationalConstraints.Trim();
    }
}
