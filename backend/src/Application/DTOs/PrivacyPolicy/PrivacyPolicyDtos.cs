using System;

namespace DDDNetCore.Application.DTOs.PrivacyPolicy
{
    /// <summary>
    /// DTO for returning privacy policy details to clients.
    /// </summary>
    public record PrivacyPolicyDto(
        string PolicyId,
        string Version,
        string Content,
        string? ChangeSummary,
        DateTime EffectiveDate,
        DateTime CreatedAt,
        string LanguageCode,
        bool IsActive
    );

    /// <summary>
    /// DTO for privacy policy list/history (without full content).
    /// </summary>
    public record PrivacyPolicySummaryDto(
        string PolicyId,
        string Version,
        string? ChangeSummary,
        DateTime EffectiveDate,
        DateTime CreatedAt,
        string LanguageCode,
        bool IsActive,
        int AcknowledgmentCount
    );

    /// <summary>
    /// Request DTO for publishing a new privacy policy version.
    /// </summary>
    public record PublishPrivacyPolicyRequest(
        string Version,
        string Content,
        string? ChangeSummary,
        DateTime? EffectiveDate,
        string LanguageCode = "pt"
    );

    /// <summary>
    /// Response DTO for acknowledgment status check.
    /// </summary>
    public record AcknowledgmentStatusDto(
        bool AcknowledgmentRequired,
        string? CurrentPolicyVersion,
        string? CurrentPolicyId,
        DateTime? LastAcknowledgedAt,
        string? LastAcknowledgedVersion
    );

    /// <summary>
    /// Request DTO for recording a policy acknowledgment.
    /// </summary>
    public record AcknowledgePolicyRequest(
        string PolicyId
    );

    /// <summary>
    /// Response DTO for successful acknowledgment.
    /// </summary>
    public record AcknowledgmentResponseDto(
        string AcknowledgmentId,
        string PolicyId,
        string PolicyVersion,
        DateTime AcknowledgedAt
    );

    /// <summary>
    /// DTO for acknowledgment records (admin view).
    /// </summary>
    public record PolicyAcknowledgmentDto(
        string AcknowledgmentId,
        string UserId,
        string PolicyId,
        string PolicyVersion,
        DateTime AcknowledgedAt,
        string? IpAddress
    );
}
