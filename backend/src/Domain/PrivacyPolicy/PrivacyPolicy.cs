using System;
using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Domain.PrivacyPolicy
{
    /// <summary>
    /// Privacy Policy entity representing a version of the system's privacy policy.
    /// US 4.5.1: As an Administrator, I want to publish and manage the system's Privacy Policy
    /// 
    /// Each policy version is immutable once published. To update, create a new version.
    /// The system maintains history of all policy versions with timestamps for traceability.
    /// </summary>
    public class PrivacyPolicy : Entity, IAggregateRoot
    {
        /// <summary>
        /// Unique identifier for this policy version
        /// </summary>
        public PrivacyPolicyId PolicyId { get; private set; } = default!;

        /// <summary>
        /// Version string (e.g., "1.0", "1.1", "2.0")
        /// </summary>
        public string Version { get; private set; } = default!;

        /// <summary>
        /// Full content of the privacy policy (Markdown format)
        /// </summary>
        public string Content { get; private set; } = default!;

        /// <summary>
        /// Brief summary of what changed in this version (for admin reference)
        /// </summary>
        public string? ChangeSummary { get; private set; }

        /// <summary>
        /// Date when this policy version becomes effective
        /// </summary>
        public DateTime EffectiveDate { get; private set; }

        /// <summary>
        /// User ID of the administrator who published this version
        /// </summary>
        public Guid CreatedBy { get; private set; }

        /// <summary>
        /// Timestamp when this version was created
        /// </summary>
        public DateTime CreatedAt { get; private set; }

        /// <summary>
        /// Whether this is the currently active policy version
        /// </summary>
        public bool IsActive { get; private set; }

        /// <summary>
        /// Language code (e.g., "pt", "en")
        /// </summary>
        public string LanguageCode { get; private set; } = "pt";

        // EF Core constructor
        private PrivacyPolicy() { }

        /// <summary>
        /// Create a new privacy policy version
        /// </summary>
        public PrivacyPolicy(
            Guid id,
            string version,
            string content,
            DateTime effectiveDate,
            Guid createdBy,
            string? changeSummary = null,
            string languageCode = "pt")
        {
            if (string.IsNullOrWhiteSpace(version))
                throw new ArgumentException("Version is required", nameof(version));
            if (string.IsNullOrWhiteSpace(content))
                throw new ArgumentException("Content is required", nameof(content));

            Id = id;
            PolicyId = new PrivacyPolicyId(id);
            Version = version;
            Content = content;
            EffectiveDate = effectiveDate;
            CreatedBy = createdBy;
            ChangeSummary = changeSummary;
            LanguageCode = languageCode;
            CreatedAt = DateTime.UtcNow;
            IsActive = false; // Not active until explicitly activated
        }

        /// <summary>
        /// Activate this policy version (deactivate others via service)
        /// </summary>
        public void Activate()
        {
            IsActive = true;
        }

        /// <summary>
        /// Deactivate this policy version
        /// </summary>
        public void Deactivate()
        {
            IsActive = false;
        }
    }
}
