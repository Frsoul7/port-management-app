using System;
using DDDNetCore.Domain.Organizations;
using DDDNetCore.Domain.Shared;
using DDDNetCore.Domain.Vessels;

namespace DDDNetCore.Domain.Factory;

/// <summary>
/// Factory for creating Vessel aggregates.
/// Encapsulates complex creation logic including IMO validation and normalization.
/// </summary>
public class VesselFactory : IVesselFactory
{
    /// <summary>
    /// Creates a new Vessel with validated and normalized IMO number.
    /// </summary>
    /// <param name="imoNumber">IMO number (will be normalized and validated)</param>
    /// <param name="name">Vessel name</param>
    /// <param name="vesselTypeId">Vessel type ID</param>
    /// <param name="organizationId">Owner organization ID (as value object)</param>
    /// <param name="capacityTeu">Capacity in TEU (optional)</param>
    /// <returns>A new Vessel instance with validated data</returns>
    /// <exception cref="ArgumentException">Thrown when IMO number is invalid or required fields are missing</exception>
    public Vessel Create(
        string imoNumber,
        string name,
        string vesselTypeId,
        OrganizationId organizationId,
        int? capacityTeu = null)
    {
        // Validate and normalize IMO using shared validator
        string normalizedImo = ImoValidator.Normalize(imoNumber);
        
        if (!ImoValidator.IsValid(normalizedImo))
        {
            throw new ArgumentException($"Invalid IMO number format: {imoNumber}", nameof(imoNumber));
        }

        // Create vessel with validated IMO (constructor performs additional validation)
        return new Vessel(normalizedImo, name, vesselTypeId, organizationId, capacityTeu);
    }
}
