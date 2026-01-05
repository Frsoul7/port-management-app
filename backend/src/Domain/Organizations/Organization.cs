using DDDNetCore.Domain.Shared;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;


namespace DDDNetCore.Domain.Organizations
{
    public enum OrganizationType { SHIPPING_AGENT, PORT_AUTHORITY, LOGISTICS_OPERATOR, ADMINISTRATOR }

    public class Organization : Entity, IAggregateRoot
    {
        public OrganizationId OrganizationId { get; private set; } = default!;


        // US 2.2.5 fields
        public string Identifier { get; private set; } = default!; // Alphanumeric, max 10, unique
        public string LegalName { get; private set; } = default!; // required
        public string AlternativeName { get; private set; } = string.Empty; // optional
        public string AddressLine { get; private set; } = default!; // required (single line)
        public string TaxNumber { get; private set; } = default!; // required (EU supported – no format check now)


        public OrganizationType Type { get; private set; }


        private readonly List<Representative> _representatives = new();
        public IReadOnlyCollection<Representative> Representatives => _representatives.AsReadOnly();


        private Organization() { }


        public Organization(Guid id, string identifier, string legalName, string alternativeName,
    string addressLine, string taxNumber, OrganizationType type, IEnumerable<Representative>? representatives = null)
        {
            Id = id;
            OrganizationId = new OrganizationId(id);

            SetIdentifier(identifier);
            SetLegalName(legalName);
            SetAlternativeName(alternativeName);
            SetAddressLine(addressLine);
            SetTaxNumber(taxNumber);

            Type = type;

            var reps = (representatives ?? Enumerable.Empty<Representative>()).ToList();
            EnsureRepEmailsUnique(reps); // still avoid duplicate emails in the payload
            _representatives.AddRange(reps);
        }


        public void SetIdentifier(string identifier)
        {
            if (string.IsNullOrWhiteSpace(identifier)) throw new ArgumentException("Identifier is required.");
            var trimmed = identifier.Trim();
            if (trimmed.Length > 10) throw new ArgumentException("Identifier max length is 10.");
            if (!Regex.IsMatch(trimmed, "^[A-Za-z0-9]+$")) throw new ArgumentException("Identifier must be alphanumeric.");
            Identifier = trimmed;
        }


        public void SetLegalName(string legalName)
        {
            if (string.IsNullOrWhiteSpace(legalName)) throw new ArgumentException("Legal name is required.");
            LegalName = legalName.Trim();
        }


        public void SetAlternativeName(string alternativeName)
        {
            AlternativeName = (alternativeName ?? string.Empty).Trim();
        }


        public void SetAddressLine(string addressLine)
        {
            if (string.IsNullOrWhiteSpace(addressLine)) throw new ArgumentException("Address is required.");
            AddressLine = addressLine.Trim();
        }


        public void SetTaxNumber(string taxNumber)
        {
            if (string.IsNullOrWhiteSpace(taxNumber)) throw new ArgumentException("Tax number is required.");
            TaxNumber = taxNumber.Trim();
        }


        private static void EnsureRepEmailsUnique(IEnumerable<Representative> reps)
        {
            // uniqueness NOT required globally; only ensure no duplicates within payload
            var dup = reps
            .Select(r => r.Email.Trim().ToLowerInvariant())
            .GroupBy(e => e)
            .FirstOrDefault(g => g.Count() > 1);
            if (dup != null) throw new ArgumentException("Representative emails must be unique within the organization payload.");
        }


        // US 2.2.6 methods like Add/Update/Deactivate Representative

        public void AddRepresentatives(IEnumerable<Representative> reps)
        {
            if (reps == null) return;
            foreach (var r in reps)
            {
                _representatives.Add(r);
            }
        }
    }
}