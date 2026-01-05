using System;

namespace DDDNetCore.Application.DTOs.Vvns
{
    /// <summary>
    /// Response data after VVN approval (renamed from ApproveVvnResponse - Phase 5: DTO naming fix)
    /// </summary>
    public class VvnApprovalResponse
    {
        // VVN Information
        public Guid VvnId { get; set; }
        public string VvnBusinessId { get; set; } = null!;
        public string Status { get; set; } = null!;
        
        // Dock Assignment Information
        public string AssignedDock { get; set; } = null!;
        public DateTime BerthFrom { get; set; }
        public DateTime BerthTo { get; set; }
        public Guid DockAssignmentId { get; set; }
        
        // Approval Information
        public Guid ApprovedByOrgId { get; set; }
        public string ApprovedByOrgName { get; set; } = null!;
        public Guid? ApprovedByUserId { get; set; } // Null if system/Port Authority placeholder
        public DateTime ApprovedAt { get; set; }
        
        // Submitter Information (who created the VVN)
        public Guid SubmittedByOrgId { get; set; }
        public string SubmittedByOrgName { get; set; } = null!;
        public Guid? SubmittedByUserId { get; set; }
        public DateTime? SubmittedAt { get; set; }
    }
}
