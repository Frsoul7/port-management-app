using DDDNetCore.Application.DTOs.PrivacyPolicy;
using DDDNetCore.Application.Interfaces;
using DDDNetCore.Domain.PrivacyPolicy;
using DDDNetCore.Domain.Users;
using Microsoft.Extensions.Logging;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace DDDNetCore.Application.Services
{
    /// <summary>
    /// Service implementation for Privacy Policy operations.
    /// US 4.5.1: Privacy Policy management and acknowledgment tracking.
    /// US 4.5.2: Privacy information access for all users.
    /// </summary>
    public class PrivacyPolicyService : IPrivacyPolicyService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly ILogger<PrivacyPolicyService> _logger;

        public PrivacyPolicyService(IUnitOfWork unitOfWork, ILogger<PrivacyPolicyService> logger)
        {
            _unitOfWork = unitOfWork;
            _logger = logger;
        }

        // ===== Public Operations (US 4.5.2) =====

        public async Task<PrivacyPolicyDto?> GetCurrentPolicyAsync(string languageCode = "pt")
        {
            var policy = await _unitOfWork.PrivacyPolicies.GetCurrentAsync(languageCode);
            
            if (policy == null)
            {
                _logger.LogWarning("No active privacy policy found for language: {LanguageCode}", languageCode);
                return null;
            }

            return MapToDto(policy);
        }

        // ===== Admin Operations (US 4.5.1) =====

        public async Task<List<PrivacyPolicySummaryDto>> GetPolicyHistoryAsync(string? languageCode = null)
        {
            var policies = await _unitOfWork.PrivacyPolicies.GetHistoryAsync(languageCode);
            var summaries = new List<PrivacyPolicySummaryDto>();

            foreach (var policy in policies)
            {
                var acknowledgments = await _unitOfWork.PrivacyPolicies
                    .GetAcknowledgmentsForPolicyAsync(policy.PolicyId);

                summaries.Add(new PrivacyPolicySummaryDto(
                    PolicyId: policy.PolicyId.ToString(),
                    Version: policy.Version,
                    ChangeSummary: policy.ChangeSummary,
                    EffectiveDate: policy.EffectiveDate,
                    CreatedAt: policy.CreatedAt,
                    LanguageCode: policy.LanguageCode,
                    IsActive: policy.IsActive,
                    AcknowledgmentCount: acknowledgments.Count
                ));
            }

            return summaries;
        }

        public async Task<PrivacyPolicyDto> PublishNewPolicyAsync(PublishPrivacyPolicyRequest request, string adminUserId)
        {
            if (!Guid.TryParse(adminUserId, out var adminGuid))
            {
                throw new ArgumentException("Invalid admin user ID format", nameof(adminUserId));
            }

            // Validate version format (e.g., "1.0", "2.1")
            if (string.IsNullOrWhiteSpace(request.Version))
            {
                throw new ArgumentException("Version is required", nameof(request.Version));
            }

            if (string.IsNullOrWhiteSpace(request.Content))
            {
                throw new ArgumentException("Content is required", nameof(request.Content));
            }

            // Deactivate any currently active policies for this language
            await _unitOfWork.PrivacyPolicies.DeactivateAllAsync(request.LanguageCode);

            // Create new policy
            var effectiveDate = request.EffectiveDate ?? DateTime.UtcNow;
            var policy = new PrivacyPolicy(
                id: Guid.NewGuid(),
                version: request.Version,
                content: request.Content,
                effectiveDate: effectiveDate,
                createdBy: adminGuid,
                changeSummary: request.ChangeSummary,
                languageCode: request.LanguageCode
            );

            // Activate the new policy
            policy.Activate();

            await _unitOfWork.PrivacyPolicies.AddAsync(policy);
            await _unitOfWork.CommitAsync();

            _logger.LogInformation(
                "New privacy policy published. Version: {Version}, Language: {Language}, Admin: {AdminId}",
                policy.Version, policy.LanguageCode, adminUserId);

            return MapToDto(policy);
        }

        public async Task<PrivacyPolicyDto?> GetPolicyByIdAsync(string policyId)
        {
            if (!Guid.TryParse(policyId, out var policyGuid))
            {
                return null;
            }

            var policy = await _unitOfWork.PrivacyPolicies.GetByIdAsync(new PrivacyPolicyId(policyGuid));
            
            return policy != null ? MapToDto(policy) : null;
        }

        // ===== User Acknowledgment Operations =====

        public async Task<AcknowledgmentStatusDto> CheckAcknowledgmentRequiredAsync(string userId, string languageCode = "pt")
        {
            if (!Guid.TryParse(userId, out var userGuid))
            {
                throw new ArgumentException("Invalid user ID format", nameof(userId));
            }

            var userIdObj = new UserId(userGuid);
            var currentPolicy = await _unitOfWork.PrivacyPolicies.GetCurrentAsync(languageCode);

            if (currentPolicy == null)
            {
                // No active policy - no acknowledgment required
                return new AcknowledgmentStatusDto(
                    AcknowledgmentRequired: false,
                    CurrentPolicyVersion: null,
                    CurrentPolicyId: null,
                    LastAcknowledgedAt: null,
                    LastAcknowledgedVersion: null
                );
            }

            var hasAcknowledged = await _unitOfWork.PrivacyPolicies
                .HasUserAcknowledgedCurrentPolicyAsync(userIdObj, languageCode);

            var latestAcknowledgment = await _unitOfWork.PrivacyPolicies
                .GetLatestUserAcknowledgmentAsync(userIdObj);

            return new AcknowledgmentStatusDto(
                AcknowledgmentRequired: !hasAcknowledged,
                CurrentPolicyVersion: currentPolicy.Version,
                CurrentPolicyId: currentPolicy.PolicyId.ToString(),
                LastAcknowledgedAt: latestAcknowledgment?.AcknowledgedAt,
                LastAcknowledgedVersion: latestAcknowledgment?.PolicyVersion
            );
        }

        public async Task<AcknowledgmentResponseDto> AcknowledgePolicyAsync(
            string userId,
            AcknowledgePolicyRequest request,
            string? ipAddress = null,
            string? userAgent = null)
        {
            if (!Guid.TryParse(userId, out var userGuid))
            {
                throw new ArgumentException("Invalid user ID format", nameof(userId));
            }

            if (!Guid.TryParse(request.PolicyId, out var policyGuid))
            {
                throw new ArgumentException("Invalid policy ID format", nameof(request.PolicyId));
            }

            var userIdObj = new UserId(userGuid);
            var policyIdObj = new PrivacyPolicyId(policyGuid);

            // Verify policy exists
            var policy = await _unitOfWork.PrivacyPolicies.GetByIdAsync(policyIdObj);
            if (policy == null)
            {
                throw new KeyNotFoundException($"Policy with ID {request.PolicyId} not found");
            }

            // Check if already acknowledged
            var existingAcknowledgment = await _unitOfWork.PrivacyPolicies
                .GetUserAcknowledgmentAsync(userIdObj, policyIdObj);

            if (existingAcknowledgment != null)
            {
                // Already acknowledged - return existing acknowledgment
                return new AcknowledgmentResponseDto(
                    AcknowledgmentId: existingAcknowledgment.AcknowledgmentId.ToString(),
                    PolicyId: existingAcknowledgment.PolicyId.ToString(),
                    PolicyVersion: existingAcknowledgment.PolicyVersion,
                    AcknowledgedAt: existingAcknowledgment.AcknowledgedAt
                );
            }

            // Create new acknowledgment
            var acknowledgment = new PrivacyPolicyAcknowledgment(
                id: Guid.NewGuid(),
                userId: userIdObj,
                policyId: policyIdObj,
                policyVersion: policy.Version,
                ipAddress: ipAddress,
                userAgent: userAgent
            );

            await _unitOfWork.PrivacyPolicies.AddAcknowledgmentAsync(acknowledgment);
            await _unitOfWork.CommitAsync();

            _logger.LogInformation(
                "User {UserId} acknowledged privacy policy version {Version}",
                userId, policy.Version);

            return new AcknowledgmentResponseDto(
                AcknowledgmentId: acknowledgment.AcknowledgmentId.ToString(),
                PolicyId: acknowledgment.PolicyId.ToString(),
                PolicyVersion: acknowledgment.PolicyVersion,
                AcknowledgedAt: acknowledgment.AcknowledgedAt
            );
        }

        // ===== Admin Reporting =====

        public async Task<List<PolicyAcknowledgmentDto>> GetPolicyAcknowledgmentsAsync(string policyId)
        {
            if (!Guid.TryParse(policyId, out var policyGuid))
            {
                throw new ArgumentException("Invalid policy ID format", nameof(policyId));
            }

            var acknowledgments = await _unitOfWork.PrivacyPolicies
                .GetAcknowledgmentsForPolicyAsync(new PrivacyPolicyId(policyGuid));

            return acknowledgments.Select(a => new PolicyAcknowledgmentDto(
                AcknowledgmentId: a.AcknowledgmentId.ToString(),
                UserId: a.UserId.ToString(),
                PolicyId: a.PolicyId.ToString(),
                PolicyVersion: a.PolicyVersion,
                AcknowledgedAt: a.AcknowledgedAt,
                IpAddress: a.IpAddress
            )).ToList();
        }

        // ===== Helper Methods =====

        private static PrivacyPolicyDto MapToDto(PrivacyPolicy policy)
        {
            return new PrivacyPolicyDto(
                PolicyId: policy.PolicyId.ToString(),
                Version: policy.Version,
                Content: policy.Content,
                ChangeSummary: policy.ChangeSummary,
                EffectiveDate: policy.EffectiveDate,
                CreatedAt: policy.CreatedAt,
                LanguageCode: policy.LanguageCode,
                IsActive: policy.IsActive
            );
        }
    }
}
