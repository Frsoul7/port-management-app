using DDDNetCore.Domain.Shared;
using DDDNetCore.Domain.Organizations;
using System;

namespace DDDNetCore.Domain.Users
{
    public enum UserRole
    {
        SHIPPING_AGENT_REPRESENTATIVE,
        PORT_AUTHORITY_OFFICER,
        LOGISTICS_OPERATOR,
        ADMINISTRATOR
    }

    public class User : Entity, IAggregateRoot
    {
        public UserId UserId { get; private set; } = default!;
        public string Name { get; private set; } = default!;
        public string Email { get; private set; } = default!;
        public string? ProfilePictureUrl { get; private set; }
        public string? PasswordHash { get; private set; }
        public UserRole? Role { get; private set; }
        public bool IsActive { get; private set; } = false;
        public bool EmailVerified { get; private set; } = false;
        public string? ActivationToken { get; private set; }
        public DateTime? ActivationTokenExpiry { get; private set; }
        public DateTime CreatedAt { get; private set; } = DateTime.UtcNow;

        public OrganizationId OrganizationId { get; private set; } = default!;
        public Organization? Organization { get; private set; }

        private User() { }

        /// <summary>
        /// Main domain constructor (normal user flow).
        /// Users created this way must verify email + be activated by admin.
        /// </summary>
        public User(
            Guid id, 
            string name, 
            string email, 
            OrganizationId organizationId, 
            UserRole? role = null,
            string? profilePictureUrl = null, 
            string? passwordHash = null)
        {
            Id = id;
            UserId = new UserId(id);
            Name = name;
            Email = email;
            OrganizationId = organizationId;
            Role = role;
            ProfilePictureUrl = profilePictureUrl;
            PasswordHash = passwordHash;
            IsActive = false;
            EmailVerified = false;
            CreatedAt = DateTime.UtcNow;
        }

        /// <summary>
        /// Factory method for system-level creation of pre-verified Administrator users.
        /// Used for seeding or system installation.
        /// </summary>
        public static User CreateVerifiedAdmin(
            Guid id,
            string name,
            string email,
            OrganizationId organizationId,
            string? profilePictureUrl = null,
            string? passwordHash = null)
        {
            var user = new User(id, name, email, organizationId,
                                UserRole.ADMINISTRATOR, profilePictureUrl, passwordHash);

            user.IsActive = true;
            user.EmailVerified = true;
            user.ActivationToken = null;
            user.ActivationTokenExpiry = null;

            return user;
        }

        // ----------------------------
        // Domain Behavior
        // ----------------------------

        public void VerifyEmail()
        {
            EmailVerified = true;
            ActivationToken = null;
            ActivationTokenExpiry = null;
        }

        public void Activate()
        {
            IsActive = true;
        }

        public void Deactivate()
        {
            IsActive = false;
        }

        public void AssignRole(UserRole role)
        {
            Role = role;
        }

        public void SetActivationToken(string token, DateTime expiry)
        {
            if (string.IsNullOrWhiteSpace(token))
                throw new ArgumentException("Activation token cannot be empty");
            if (expiry <= DateTime.UtcNow)
                throw new ArgumentException("Expiry must be in the future");

            ActivationToken = token;
            ActivationTokenExpiry = expiry;
        }

        public bool ValidateActivationToken(string token)
        {
            if (string.IsNullOrWhiteSpace(ActivationToken))
                return false;
            if (ActivationTokenExpiry == null || ActivationTokenExpiry <= DateTime.UtcNow)
                return false;
            return ActivationToken == token;
        }

        public void ChangeName(string name)
        {
            if (string.IsNullOrWhiteSpace(name))
                throw new ArgumentException("Name cannot be empty");
            Name = name.Trim();
        }

        public void ChangeOrganization(OrganizationId organizationId)
        {
            OrganizationId = organizationId ?? throw new ArgumentException("Organization ID cannot be null");
        }

        public void ChangeRole(UserRole role)
        {
            Role = role;
        }

        public void ChangeProfilePicture(string? profilePictureUrl)
        {
            ProfilePictureUrl = profilePictureUrl;
        }
    }
}
