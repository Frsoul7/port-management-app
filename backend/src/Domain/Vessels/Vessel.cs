using System;
using System.Collections.Generic;
using DDDNetCore.Domain.Organizations;
using DDDNetCore.Domain.Shared;            // ImoValidator
using DDDNetCore.Domain.Visits;
using DDDNetCore.Domain.Vessels;          // If VesselType lives here; adjust if needed

namespace DDDNetCore.Domain.Vessels
{
    public class Vessel
    {
        public string ImoNumber { get; private set; } = default!;
        public string Name { get; private set; } = default!;
        public int? CapacityTEU { get; private set; }

        public string VesselTypeId { get; private set; } = default!;
        public VesselType? VesselType { get; private set; }

        // Operator/Owner organization (uses VO)
        public OrganizationId OwnerOrganizationId { get; private set; } = default!;
        public Organization? OwnerOrganization { get; private set; }

        public ICollection<VesselVisitNotification> VesselVisitNotifications { get; private set; } = new List<VesselVisitNotification>();

        private Vessel() { } // EF

        public Vessel(string imoNumber, string name, string vesselTypeId, OrganizationId organizationId, int? capacityTeu = null)
        {
            if (string.IsNullOrWhiteSpace(imoNumber))
                throw new ArgumentException("IMO number is required", nameof(imoNumber));
            if (string.IsNullOrWhiteSpace(name))
                throw new ArgumentException("Vessel name is required", nameof(name));
            if (string.IsNullOrWhiteSpace(vesselTypeId))
                throw new ArgumentException("Vessel type is required", nameof(vesselTypeId));
            if (organizationId is null)
                throw new ArgumentNullException(nameof(organizationId));

            // Normalize + validate via shared helper
            var normalizedImo = ImoValidator.Normalize(imoNumber);
            if (!ImoValidator.IsValid(normalizedImo))
                throw new ArgumentException("Invalid IMO number.", nameof(imoNumber));

            ImoNumber = normalizedImo;
            Name = name.Trim();
            VesselTypeId = vesselTypeId.Trim();
            OwnerOrganizationId = organizationId;
            CapacityTEU = capacityTeu;
        }

        /// <summary>
        /// Factory method that validates inputs and returns a new Vessel.
        /// Use this in app/service layers instead of calling the ctor directly.
        /// </summary>
        public static Vessel Create(string imoNumber, string name, string vesselTypeId, OrganizationId organizationId, int? capacityTeu = null)
        {
            var normalized = ImoValidator.Normalize(imoNumber);
            if (!ImoValidator.IsValid(normalized))
                throw new ArgumentException("Invalid IMO number.", nameof(imoNumber));

            if (string.IsNullOrWhiteSpace(name))
                throw new ArgumentException("Vessel name is required", nameof(name));
            if (string.IsNullOrWhiteSpace(vesselTypeId))
                throw new ArgumentException("Vessel type is required", nameof(vesselTypeId));
            if (organizationId is null)
                throw new ArgumentNullException(nameof(organizationId));

            return new Vessel(normalized, name, vesselTypeId, organizationId, capacityTeu);
        }

        public void UpdateBasics(string name, string vesselTypeId, OrganizationId organizationId, int? capacityTeu)
        {
            if (string.IsNullOrWhiteSpace(name))
                throw new ArgumentException("Vessel name is required", nameof(name));
            if (string.IsNullOrWhiteSpace(vesselTypeId))
                throw new ArgumentException("Vessel type is required", nameof(vesselTypeId));
            if (organizationId is null)
                throw new ArgumentNullException(nameof(organizationId));

            Name = name.Trim();
            VesselTypeId = vesselTypeId.Trim();
            OwnerOrganizationId = organizationId;
            CapacityTEU = capacityTeu;
        }
    }
}
