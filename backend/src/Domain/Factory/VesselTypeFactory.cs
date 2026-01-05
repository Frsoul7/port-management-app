using System;
using DDDNetCore.Domain.Vessels;

namespace DDDNetCore.Domain.Factory;

/// <summary>
/// Factory for creating VesselType aggregates.
/// Encapsulates complex creation logic including validation and normalization.
/// </summary>
public class VesselTypeFactory : IVesselTypeFactory
{
    /// <summary>
    /// Creates a new VesselType with validated and normalized data.
    /// </summary>
    /// <param name="id">VesselType ID</param>
    /// <param name="name">VesselType name</param>
    /// <returns>A new VesselType instance with validated data</returns>
    /// <exception cref="ArgumentException">Thrown when required fields are missing or invalid</exception>
    public VesselType Create(string id, string name)
    {
        // Validate required fields
        if (string.IsNullOrWhiteSpace(id))
            throw new ArgumentException("VesselType ID is required", nameof(id));

        if (string.IsNullOrWhiteSpace(name))
            throw new ArgumentException("VesselType name is required", nameof(name));

        // Create VesselType with validated data (constructor performs additional validation)
        return new VesselType(id.Trim(), name.Trim());
    }
}
