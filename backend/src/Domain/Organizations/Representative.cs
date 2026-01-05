using System;

namespace DDDNetCore.Domain.Organizations
{
    public class Representative
    {
        public Guid Id { get; private set; } // EF key (technical PK)

        public RepresentativeId RepresentativeId { get; private set; } = default!; // DDD VO ID

        public string Name { get; private set; } = default!;
        public string CitizenId { get; private set; } = default!;
        public string Nationality { get; private set; } = default!; // ISO 3166-1 alpha-2
        public string Email { get; private set; } = default!;
        public string Phone { get; private set; } = default!;
        public bool IsActive { get; private set; } = true; // Used in US 2.2.6
        public DateTime CreatedAt { get; private set; }

        private Representative() { } // EF

        public Representative(string name, string citizenId, string nationality, string email, string phone)
        {
            // Validate email format
            if (string.IsNullOrWhiteSpace(email))
                throw new ArgumentException("Email is required.", nameof(email));

            var trimmedEmail = email.Trim();
            if (!System.Text.RegularExpressions.Regex.IsMatch(trimmedEmail, @"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$"))
                throw new ArgumentException($"Email must be a valid email address (format: something@something.something). Invalid: {email}", nameof(email));

            Id = Guid.NewGuid();                          // EF PK
            RepresentativeId = RepresentativeId.NewId();  // DDD VO
            Name = name.Trim();
            CitizenId = citizenId.Trim();
            Nationality = nationality.Trim().ToUpperInvariant();
            Email = trimmedEmail;
            Phone = phone?.Trim() ?? string.Empty;
            CreatedAt = DateTime.UtcNow;
        }

        public void Deactivate() => IsActive = false; // for US 2.2.6
        public void Activate() => IsActive = true;    // for US 2.2.6
    }
}
