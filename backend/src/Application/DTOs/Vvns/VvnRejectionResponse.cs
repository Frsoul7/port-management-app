using System;

namespace DDDNetCore.Application.DTOs.Vvns
{
    /// <summary>
    /// Response data after VVN rejection (renamed from RejectVvnResponse - Phase 5: DTO naming fix)
    /// </summary>
    public class VvnRejectionResponse
    {
        // VVN Information
        public Guid VvnId { get; set; }
        public string VvnBusinessId { get; set; } = null!;
        public string Status { get; set; } = null!;
        
        // Rejection Information
        public string RejectionReason { get; set; } = null!;
        public Guid RejectedByOrgId { get; set; }
        public string RejectedByOrgName { get; set; } = null!;
        public Guid? RejectedByUserId { get; set; } // Null if system/Port Authority placeholder
        public DateTime RejectedAt { get; set; }
        
        // Submitter Information (who created the VVN)
        public Guid SubmittedByOrgId { get; set; }
        public string SubmittedByOrgName { get; set; } = null!;
        public Guid? SubmittedByUserId { get; set; }
        public DateTime? SubmittedAt { get; set; }
    }
}
