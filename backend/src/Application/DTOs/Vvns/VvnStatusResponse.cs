using System;
using System.Collections.Generic;

namespace DDDNetCore.Application.DTOs.Vvns
{
    /// <summary>
    /// Detailed VVN status response for Shipping Agent Representatives (US 2.2.10)
    /// Includes full status information, dock assignment details, and rejection reasons
    /// </summary>
    public class VvnStatusResponse
    {
        public Guid Id { get; set; }
        
        // Business identifier (e.g., 2025-PTLEI-000001)
        public string VvnBusinessId { get; set; } = null!;
        
        // Core VVN information
        public string Status { get; set; } = null!; // IN_PROGRESS, SUBMITTED, APPROVED, REJECTED
        public string VesselImo { get; set; } = null!;
        public string Purpose { get; set; } = null!;
        
        // Timeline
        public DateTime Eta { get; set; }
        public DateTime Etd { get; set; }
        public DateTime CreatedAt { get; set; }
        public DateTime? SubmittedAt { get; set; }
        public DateTime? ApprovedAt { get; set; }
        public DateTime? RejectedAt { get; set; }
        
        // Who submitted this VVN
        public Guid? SubmittedById { get; set; }
        
        // Crew information
        public string CaptainName { get; set; } = null!;
        public string CaptainCitizenId { get; set; } = null!;
        public string CaptainNationality { get; set; } = null!;
        public int CrewCount { get; set; }

        // Hazardous cargo handlers (only required when hazardous goods exist)
        public List<CrewMemberDto> CrewMembers { get; set; } = new();

        // Manifest counts
        public int LoadingCount { get; set; }
        public int UnloadingCount { get; set; }
        
        // Approval details (only when APPROVED)
        public DockAssignmentInfo? DockAssignment { get; set; }
        public Guid? ApprovedById { get; set; }
        
        // Rejection details (only when REJECTED)
        public string? RejectionReason { get; set; }
        public Guid? RejectedById { get; set; }
    }
    
    /// <summary>
    /// Dock assignment information for approved VVNs
    /// </summary>
    public class DockAssignmentInfo
    {
        public Guid DockAssignmentId { get; set; }
        public string DockCode { get; set; } = null!;
        public DateTime BerthFrom { get; set; }
        public DateTime BerthTo { get; set; }
        public string Status { get; set; } = null!; // ASSIGNED, IN_PROGRESS, COMPLETED, CANCELLED
    }
}
