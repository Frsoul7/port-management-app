using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using DDDNetCore.Domain.Organizations;

namespace DDDNetCore.Domain.Factory;

/// <summary>
/// Factory for creating Organization aggregates.
/// Encapsulates complex creation logic including validation and normalization.
/// </summary>
public class OrganizationFactory : IOrganizationFactory
{
    /// <summary>
    /// Creates a new Organization with validated and normalized data.
    /// </summary>
    public Organization Create(
        Guid id,
        string identifier,
        string legalName,
        string alternativeName,
        string addressLine,
        string taxNumber,
        OrganizationType type,
        IEnumerable<Representative>? representatives = null)
    {
        // Validate and normalize identifier
        if (string.IsNullOrWhiteSpace(identifier))
            throw new ArgumentException("Identifier is required", nameof(identifier));

        var normalizedId = identifier.Trim().ToUpperInvariant();

        if (normalizedId.Length > 10)
            throw new ArgumentException("Identifier max length is 10", nameof(identifier));

        if (!Regex.IsMatch(normalizedId, "^[A-Za-z0-9]+$"))
            throw new ArgumentException("Identifier must be alphanumeric", nameof(identifier));

        // Create organization with validated data
        return new Organization(
            id,
            normalizedId,
            legalName,
            alternativeName,
            addressLine,
            taxNumber,
            type,
            representatives);
    }
}
