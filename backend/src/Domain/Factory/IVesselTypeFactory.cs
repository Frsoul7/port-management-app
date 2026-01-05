using System;
using DDDNetCore.Domain.Vessels;

namespace DDDNetCore.Domain.Factory;

/// <summary>
/// Factory interface for creating VesselType aggregates.
/// Encapsulates VesselType creation logic and enforces business invariants.
/// </summary>
public interface IVesselTypeFactory
{
    /// <summary>
    /// Creates a new VesselType with validated data.
    /// </summary>
    VesselType Create(string id, string name);
}
