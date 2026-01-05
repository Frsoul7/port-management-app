using System;
using DDDNetCore.Domain.Organizations;
using DDDNetCore.Domain.Vessels;

namespace DDDNetCore.Domain.Factory;

/// <summary>
/// Factory interface for creating Vessel aggregates.
/// Encapsulates vessel creation logic and enforces business invariants.
/// </summary>
public interface IVesselFactory
{
    /// <summary>
    /// Creates a new Vessel with validated IMO number and normalized data.
    /// </summary>
    /// <param name="imoNumber">IMO number (will be normalized and validated)</param>
    /// <param name="name">Vessel name</param>
    /// <param name="vesselTypeId">Vessel type ID</param>
    /// <param name="organizationId">Owner organization ID (as value object)</param>
    /// <param name="capacityTeu">Capacity in TEU (optional)</param>
    /// <returns>Valid Vessel instance</returns>
    /// <exception cref="ArgumentException">If IMO number is invalid or required fields are missing</exception>
    Vessel Create(
        string imoNumber,
        string name,
        string vesselTypeId,
        OrganizationId organizationId,
        int? capacityTeu = null);
}
