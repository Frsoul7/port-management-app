using DDDNetCore.Application.DTOs.PrivacyPolicy;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace DDDNetCore.Application.Interfaces
{
    /// <summary>
    /// Service interface for Privacy Policy operations.
    /// US 4.5.1: Privacy Policy management and acknowledgment tracking.
    /// US 4.5.2: Privacy information access for all users.
    /// </summary>
    public interface IPrivacyPolicyService
    {
        // ===== Public Operations (US 4.5.2) =====

        /// <summary>
        /// Get the currently active privacy policy for display to users.
        /// </summary>
        Task<PrivacyPolicyDto?> GetCurrentPolicyAsync(string languageCode = "pt");

        // ===== Admin Operations (US 4.5.1) =====

        /// <summary>
        /// Get all privacy policy versions (history).
        /// Admin only.
        /// </summary>
        Task<List<PrivacyPolicySummaryDto>> GetPolicyHistoryAsync(string? languageCode = null);

        /// <summary>
        /// Publish a new privacy policy version.
        /// Admin only. Triggers notification requirement for all users.
        /// </summary>
        Task<PrivacyPolicyDto> PublishNewPolicyAsync(PublishPrivacyPolicyRequest request, string adminUserId);

        /// <summary>
        /// Get a specific policy by ID.
        /// Admin only.
        /// </summary>
        Task<PrivacyPolicyDto?> GetPolicyByIdAsync(string policyId);

        // ===== User Acknowledgment Operations =====

        /// <summary>
        /// Check if the current user needs to acknowledge a new policy.
        /// Called on login to determine if modal should be shown.
        /// </summary>
        Task<AcknowledgmentStatusDto> CheckAcknowledgmentRequiredAsync(string userId, string languageCode = "pt");

        /// <summary>
        /// Record user's acknowledgment of a privacy policy.
        /// </summary>
        Task<AcknowledgmentResponseDto> AcknowledgePolicyAsync(
            string userId, 
            AcknowledgePolicyRequest request,
            string? ipAddress = null,
            string? userAgent = null);

        // ===== Admin Reporting =====

        /// <summary>
        /// Get all acknowledgments for a specific policy.
        /// Admin only.
        /// </summary>
        Task<List<PolicyAcknowledgmentDto>> GetPolicyAcknowledgmentsAsync(string policyId);
    }
}
