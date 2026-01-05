using System;

namespace DDDNetCore.Domain.Visits.Crew
{
    public class CrewMember
    {
        public Guid CrewMemberId { get; private set; } = Guid.NewGuid();

        public string Name { get; private set; } = null!;
        public string CitizenId { get; private set; } = null!;
        public string Nationality { get; private set; } = null!;

        // FK to VVN
        public Guid VesselVisitNotificationId { get; private set; }

        private CrewMember() { }

        public CrewMember(string name, string citizenId, string nationality)
        {
            if (string.IsNullOrWhiteSpace(name)) throw new ArgumentException("Name is required.", nameof(name));
            if (string.IsNullOrWhiteSpace(citizenId)) throw new ArgumentException("Citizen ID is required.", nameof(citizenId));
            if (string.IsNullOrWhiteSpace(nationality)) throw new ArgumentException("Nationality is required.", nameof(nationality));

            Name = name.Trim();
            CitizenId = citizenId.Trim();
            Nationality = nationality.Trim().ToUpperInvariant();
        }
    }
}
