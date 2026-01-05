using System;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace DDDNetCore.Domain.IRepository
{
    using DDDNetCore.Domain.PrivacyPolicy;
    using DDDNetCore.Domain.Users;

    /// <summary>
    /// Repository interface for Privacy Policy aggregate.
    /// US 4.5.1: Privacy Policy management with versioning and acknowledgment tracking.
    /// </summary>
    public interface IPrivacyPolicyRepository
    {
        // ===== Privacy Policy Operations =====

        /// <summary>
        /// Get the currently active privacy policy
        /// </summary>
        Task<PrivacyPolicy?> GetCurrentAsync(string languageCode = "pt");

        /// <summary>
        /// Get a specific policy by ID
        /// </summary>
        Task<PrivacyPolicy?> GetByIdAsync(PrivacyPolicyId policyId);

        /// <summary>
        /// Get all policy versions ordered by effective date (most recent first)
        /// </summary>
        Task<List<PrivacyPolicy>> GetHistoryAsync(string? languageCode = null);

        /// <summary>
        /// Add a new privacy policy version
        /// </summary>
        Task AddAsync(PrivacyPolicy policy);

        /// <summary>
        /// Update an existing policy (e.g., activate/deactivate)
        /// </summary>
        Task UpdateAsync(PrivacyPolicy policy);

        /// <summary>
        /// Deactivate all policies (called before activating a new one)
        /// </summary>
        Task DeactivateAllAsync(string languageCode);

        // ===== Acknowledgment Operations =====

        /// <summary>
        /// Get user's acknowledgment for a specific policy
        /// </summary>
        Task<PrivacyPolicyAcknowledgment?> GetUserAcknowledgmentAsync(UserId userId, PrivacyPolicyId policyId);

        /// <summary>
        /// Get user's most recent acknowledgment
        /// </summary>
        Task<PrivacyPolicyAcknowledgment?> GetLatestUserAcknowledgmentAsync(UserId userId);

        /// <summary>
        /// Check if user has acknowledged the current active policy
        /// </summary>
        Task<bool> HasUserAcknowledgedCurrentPolicyAsync(UserId userId, string languageCode = "pt");

        /// <summary>
        /// Add a new acknowledgment record
        /// </summary>
        Task AddAcknowledgmentAsync(PrivacyPolicyAcknowledgment acknowledgment);

        /// <summary>
        /// Get all acknowledgments for a specific policy (for admin reporting)
        /// </summary>
        Task<List<PrivacyPolicyAcknowledgment>> GetAcknowledgmentsForPolicyAsync(PrivacyPolicyId policyId);
    }
}
