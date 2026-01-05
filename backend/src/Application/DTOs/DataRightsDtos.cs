using System;
using System.Collections.Generic;

namespace DDDNetCore.Application.DTOs
{
    /// <summary>
    /// DTO containing all user data for GDPR export (Article 15 - Right of Access)
    /// US 4.5.3: User Data Rights (SAR)
    /// </summary>
    public class UserDataExportDto
    {
        /// <summary>Export timestamp</summary>
        public DateTime ExportedAt { get; set; } = DateTime.UtcNow;
        
        /// <summary>Export format version</summary>
        public string Version { get; set; } = "1.0";
        
        /// <summary>User profile information</summary>
        public UserProfileDataDto Profile { get; set; } = default!;
        
        /// <summary>Organization membership</summary>
        public OrganizationDataDto? Organization { get; set; }
        
        /// <summary>VVNs created by the user (if representative)</summary>
        public List<VvnSummaryDto> CreatedVvns { get; set; } = new();
        
        /// <summary>Staff assignments (if staff member)</summary>
        public List<StaffAssignmentDto> StaffAssignments { get; set; } = new();
        
        /// <summary>Privacy policy acknowledgments</summary>
        public List<PolicyAcknowledgmentDto> PolicyAcknowledgments { get; set; } = new();
        
        /// <summary>Data requests history</summary>
        public List<DataRequestSummaryDto> DataRequests { get; set; } = new();
        
        /// <summary>Data from OEM backend (operation plans, VVEs, etc.)</summary>
        public OemDataDto? OemData { get; set; }
        
        /// <summary>Note if any data source was unavailable</summary>
        public string? DataSourceNotes { get; set; }
    }

    /// <summary>
    /// User profile data for export
    /// </summary>
    public class UserProfileDataDto
    {
        public string Id { get; set; } = default!;
        public string Name { get; set; } = default!;
        public string Email { get; set; } = default!;
        public string? ProfilePictureUrl { get; set; }
        public string? Role { get; set; }
        public bool IsActive { get; set; }
        public bool EmailVerified { get; set; }
        public DateTime CreatedAt { get; set; }
    }

    /// <summary>
    /// Organization data for export
    /// </summary>
    public class OrganizationDataDto
    {
        public string Id { get; set; } = default!;
        public string Name { get; set; } = default!;
        public string Type { get; set; } = default!;
    }

    /// <summary>
    /// VVN summary for export
    /// </summary>
    public class VvnSummaryDto
    {
        public string Id { get; set; } = default!;
        public string VvnNumber { get; set; } = default!;
        public string Status { get; set; } = default!;
        public DateTime CreatedAt { get; set; }
        public string? VesselName { get; set; }
    }

    /// <summary>
    /// Staff assignment data for export
    /// </summary>
    public class StaffAssignmentDto
    {
        public string StaffMemberId { get; set; } = default!;
        public string Name { get; set; } = default!;
        public List<string> Qualifications { get; set; } = new();
    }

    /// <summary>
    /// Policy acknowledgment data for export
    /// </summary>
    public class PolicyAcknowledgmentDto
    {
        public string PolicyId { get; set; } = default!;
        public string PolicyVersion { get; set; } = default!;
        public DateTime AcknowledgedAt { get; set; }
    }

    /// <summary>
    /// Data request summary for export
    /// </summary>
    public class DataRequestSummaryDto
    {
        public string ReferenceNumber { get; set; } = default!;
        public string RequestType { get; set; } = default!;
        public string Status { get; set; } = default!;
        public DateTime SubmittedAt { get; set; }
        public DateTime? CompletedAt { get; set; }
    }

    /// <summary>
    /// Data from OEM backend
    /// </summary>
    public class OemDataDto
    {
        public List<OperationPlanSummaryDto> OperationPlans { get; set; } = new();
        public List<VveSummaryDto> VesselVisitExecutions { get; set; } = new();
        public List<IncidentSummaryDto> Incidents { get; set; } = new();
        public List<ComplementaryTaskSummaryDto> ComplementaryTasks { get; set; } = new();
    }

    public class OperationPlanSummaryDto
    {
        public string Id { get; set; } = default!;
        public string Name { get; set; } = default!;
        public string Status { get; set; } = default!;
        public DateTime CreatedAt { get; set; }
    }

    public class VveSummaryDto
    {
        public string Id { get; set; } = default!;
        public string VvnReference { get; set; } = default!;
        public string Status { get; set; } = default!;
        public DateTime? CompletedAt { get; set; }
    }

    public class IncidentSummaryDto
    {
        public string Id { get; set; } = default!;
        public string Type { get; set; } = default!;
        public string Status { get; set; } = default!;
        public DateTime ReportedAt { get; set; }
    }

    public class ComplementaryTaskSummaryDto
    {
        public string Id { get; set; } = default!;
        public string Type { get; set; } = default!;
        public string Status { get; set; } = default!;
        public DateTime? CompletedAt { get; set; }
    }

    /// <summary>
    /// Request DTO for data rectification
    /// </summary>
    public class RectificationRequestDto
    {
        /// <summary>Category of data to rectify (e.g., "profile", "contact")</summary>
        public string DataCategory { get; set; } = default!;
        
        /// <summary>Current value that is incorrect</summary>
        public string CurrentValue { get; set; } = default!;
        
        /// <summary>Requested correction</summary>
        public string RequestedCorrection { get; set; } = default!;
        
        /// <summary>Reason for the correction</summary>
        public string Reason { get; set; } = default!;
    }

    /// <summary>
    /// Request DTO for account deletion
    /// </summary>
    public class DeletionRequestDto
    {
        /// <summary>User confirms they understand consequences</summary>
        public bool ConfirmUnderstanding { get; set; }
        
        /// <summary>Optional reason for deletion</summary>
        public string? Reason { get; set; }
    }

    /// <summary>
    /// Response DTO for user data requests
    /// </summary>
    public class UserDataRequestResponseDto
    {
        public string RequestId { get; set; } = default!;
        public string ReferenceNumber { get; set; } = default!;
        public string RequestType { get; set; } = default!;
        public string Status { get; set; } = default!;
        public DateTime SubmittedAt { get; set; }
        public int EstimatedResponseDays { get; set; }
        public string Message { get; set; } = default!;
    }

    /// <summary>
    /// DTO for viewing user's own request history
    /// </summary>
    public class MyDataRequestDto
    {
        public string RequestId { get; set; } = default!;
        public string ReferenceNumber { get; set; } = default!;
        public string RequestType { get; set; } = default!;
        public string Status { get; set; } = default!;
        public string Description { get; set; } = default!;
        public DateTime SubmittedAt { get; set; }
        public DateTime? UpdatedAt { get; set; }
        public DateTime? CompletedAt { get; set; }
    }
}
