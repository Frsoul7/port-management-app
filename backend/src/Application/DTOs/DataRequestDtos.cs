using System;
using DDDNetCore.Domain.DataRequests;

namespace DDDNetCore.Application.DTOs
{
    /// <summary>
    /// DTOs for Data Request operations
    /// US 4.5.4: Non-User Data Rights
    /// </summary>

    /// <summary>
    /// Request DTO for submitting a non-user data request
    /// </summary>
    public class NonUserDataRequestDto
    {
        public string FullName { get; set; } = string.Empty;
        public string Email { get; set; } = string.Empty;
        public string? Phone { get; set; }
        public DataRequestType RequestType { get; set; }
        public string? VesselReference { get; set; }
        public string? VvnReference { get; set; }
        public string Description { get; set; } = string.Empty;
        public bool ConsentGiven { get; set; }
    }

    /// <summary>
    /// Response DTO for data request submission
    /// </summary>
    public class DataRequestResponseDto
    {
        public string RequestId { get; set; } = string.Empty;
        public DataRequestStatus Status { get; set; }
        public DateTime SubmittedAt { get; set; }
        public string ReferenceNumber { get; set; } = string.Empty;
        public int EstimatedResponseDays { get; set; }
        public string Message { get; set; } = string.Empty;
    }

    /// <summary>
    /// DTO for viewing data request details (admin)
    /// </summary>
    public class DataRequestDetailsDto
    {
        public string RequestId { get; set; } = string.Empty;
        public string FullName { get; set; } = string.Empty;
        public string Email { get; set; } = string.Empty;
        public string? Phone { get; set; }
        public string RequestType { get; set; } = string.Empty;
        public string Status { get; set; } = string.Empty;
        public string? VesselReference { get; set; }
        public string? VvnReference { get; set; }
        public string Description { get; set; } = string.Empty;
        public string ReferenceNumber { get; set; } = string.Empty;
        public DateTime ConsentGivenAt { get; set; }
        public DateTime SubmittedAt { get; set; }
        public DateTime? UpdatedAt { get; set; }
        public DateTime? CompletedAt { get; set; }
        public string? AdminNotes { get; set; }
        public string? ProcessedBy { get; set; }
    }

    /// <summary>
    /// Request DTO for updating a data request status (admin)
    /// </summary>
    public class UpdateDataRequestStatusDto
    {
        public DataRequestStatus NewStatus { get; set; }
        public string? AdminNotes { get; set; }
    }

    /// <summary>
    /// Request DTO for checking request status
    /// </summary>
    public class CheckRequestStatusDto
    {
        public string ReferenceNumber { get; set; } = string.Empty;
        public string Email { get; set; } = string.Empty;
    }
}
