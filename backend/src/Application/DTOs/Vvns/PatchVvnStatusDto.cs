using DDDNetCore.Domain.Visits;

namespace DDDNetCore.Application.DTOs.Vvns;

/// <summary>
/// DTO for PATCH operations on VVNs, supporting status transitions
/// </summary>
public record PatchVvnStatusDto(
    string? Status,  // "SUBMITTED", "APPROVED", "REJECTED", "IN_PROGRESS"
    
    // For APPROVED status
    string? DockCode,
    DateTime? BerthFrom,
    DateTime? BerthTo,
    
    // For REJECTED status
    string? RejectionReason,
    string? RejectionNotes
);
