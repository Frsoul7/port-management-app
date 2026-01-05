using System;
using System.Collections.Generic;
using DDDNetCore.Domain.Organizations;

namespace DDDNetCore.Domain.Factory;

/// <summary>
/// Factory interface for creating Organization aggregates.
/// Encapsulates organization creation logic and enforces business invariants.
/// </summary>
public interface IOrganizationFactory
{
    /// <summary>
    /// Creates a new Organization with validated and normalized data.
    /// </summary>
    Organization Create(
        Guid id,
        string identifier,
        string legalName,
        string alternativeName,
        string addressLine,
        string taxNumber,
        OrganizationType type,
        IEnumerable<Representative>? representatives = null);
}
