using System;
using DDDNetCore.Domain.Shared;
using DDDNetCore.Domain.Users;

namespace DDDNetCore.Domain.PrivacyPolicy
{
    /// <summary>
    /// Records when a user acknowledges a specific privacy policy version.
    /// US 4.5.1: A change in the policy must trigger a system user notification upon next login.
    /// 
    /// This entity tracks which policy version each user has acknowledged,
    /// enabling the system to prompt users when a new policy is published.
    /// </summary>
    public class PrivacyPolicyAcknowledgment : Entity
    {
        /// <summary>
        /// Unique identifier for this acknowledgment record
        /// </summary>
        public PrivacyPolicyAcknowledgmentId AcknowledgmentId { get; private set; } = default!;

        /// <summary>
        /// The user who acknowledged the policy
        /// </summary>
        public UserId UserId { get; private set; } = default!;

        /// <summary>
        /// The policy version that was acknowledged
        /// </summary>
        public PrivacyPolicyId PolicyId { get; private set; } = default!;

        /// <summary>
        /// Policy version string for quick reference
        /// </summary>
        public string PolicyVersion { get; private set; } = default!;

        /// <summary>
        /// Timestamp when the user acknowledged the policy
        /// </summary>
        public DateTime AcknowledgedAt { get; private set; }

        /// <summary>
        /// IP address from which the acknowledgment was made (for audit)
        /// </summary>
        public string? IpAddress { get; private set; }

        /// <summary>
        /// User agent string (browser info) for audit purposes
        /// </summary>
        public string? UserAgent { get; private set; }

        // Navigation property
        public PrivacyPolicy? Policy { get; private set; }

        // EF Core constructor
        private PrivacyPolicyAcknowledgment() { }

        /// <summary>
        /// Create a new acknowledgment record
        /// </summary>
        public PrivacyPolicyAcknowledgment(
            Guid id,
            UserId userId,
            PrivacyPolicyId policyId,
            string policyVersion,
            string? ipAddress = null,
            string? userAgent = null)
        {
            Id = id;
            AcknowledgmentId = new PrivacyPolicyAcknowledgmentId(id);
            UserId = userId ?? throw new ArgumentNullException(nameof(userId));
            PolicyId = policyId ?? throw new ArgumentNullException(nameof(policyId));
            PolicyVersion = policyVersion ?? throw new ArgumentNullException(nameof(policyVersion));
            AcknowledgedAt = DateTime.UtcNow;
            IpAddress = ipAddress;
            UserAgent = userAgent;
        }
    }
}
