using System;

namespace DDDNetCore.API.Contracts
{
    /// <summary>
    /// Data required for VVN approval (renamed from ApproveVvnRequest - Phase 5: DTO naming fix)
    /// </summary>
    public class VvnApprovalRequest
    {
        // Required
        public string DockCode { get; set; } = null!;   // e.g., "D1"
        public DateTime BerthFrom { get; set; }         // UTC
        public DateTime BerthTo { get; set; }           // UTC

        // Optional notes for future auditing
        public string? Notes { get; set; }
    }
}
