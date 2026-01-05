using System;
using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Domain.DataRequests
{
    /// <summary>
    /// Enum for data request types (GDPR rights)
    /// US 4.5.3 & 4.5.4: User and Non-User Data Rights
    /// </summary>
    public enum DataRequestType
    {
        /// <summary>Right of Access (Article 15)</summary>
        ACCESS,
        
        /// <summary>Right to Rectification (Article 16)</summary>
        RECTIFICATION,
        
        /// <summary>Right to Erasure / Right to be Forgotten (Article 17)</summary>
        DELETION,
        
        /// <summary>Right to Object (Article 21)</summary>
        OBJECTION,
        
        /// <summary>Right to Data Portability (Article 20)</summary>
        PORTABILITY,
        
        /// <summary>Other data-related request</summary>
        OTHER
    }

    /// <summary>
    /// Enum for data request status
    /// </summary>
    public enum DataRequestStatus
    {
        /// <summary>Request submitted and pending review</summary>
        SUBMITTED,
        
        /// <summary>Request is being reviewed</summary>
        PENDING,
        
        /// <summary>Request is being processed</summary>
        IN_PROGRESS,
        
        /// <summary>Request has been completed</summary>
        COMPLETED,
        
        /// <summary>Request was rejected (with reason)</summary>
        REJECTED
    }

    /// <summary>
    /// Enum to distinguish between user and non-user requests
    /// </summary>
    public enum DataRequestSource
    {
        /// <summary>Request from a registered system user (US 4.5.3)</summary>
        USER,
        
        /// <summary>Request from a non-user like crew member (US 4.5.4)</summary>
        EXTERNAL
    }

    /// <summary>
    /// Entity representing a GDPR data request
    /// US 4.5.3: User Data Rights (SAR)
    /// US 4.5.4: Non-User Data Rights
    /// 
    /// Supports both registered users and external individuals (crew, captains, representatives)
    /// </summary>
    public class DataRequest : Entity, IAggregateRoot
    {
        /// <summary>Strongly-typed identifier</summary>
        public DataRequestId RequestId { get; private set; } = default!;
        
        /// <summary>Source of the request (USER or EXTERNAL)</summary>
        public DataRequestSource Source { get; private set; }
        
        /// <summary>User ID if request is from a registered user (null for external)</summary>
        public Guid? UserId { get; private set; }
        
        /// <summary>Full name of the requester</summary>
        public string FullName { get; private set; } = default!;
        
        /// <summary>Email address of the requester</summary>
        public string Email { get; private set; } = default!;
        
        /// <summary>Phone number (optional)</summary>
        public string? Phone { get; private set; }
        
        /// <summary>Type of GDPR request</summary>
        public DataRequestType RequestType { get; private set; }
        
        /// <summary>Status of the request</summary>
        public DataRequestStatus Status { get; private set; }
        
        /// <summary>Vessel name or reference (optional, mainly for external)</summary>
        public string? VesselReference { get; private set; }
        
        /// <summary>VVN reference number (optional, mainly for external)</summary>
        public string? VvnReference { get; private set; }
        
        /// <summary>Detailed description of the request</summary>
        public string Description { get; private set; } = default!;
        
        /// <summary>System-generated reference number for tracking</summary>
        public string ReferenceNumber { get; private set; } = default!;
        
        /// <summary>JSON data for rectification requests (fields to change)</summary>
        public string? RequestData { get; private set; }
        
        /// <summary>When consent was given</summary>
        public DateTime ConsentGivenAt { get; private set; }
        
        /// <summary>When the request was submitted</summary>
        public DateTime SubmittedAt { get; private set; }
        
        /// <summary>When the request was last updated</summary>
        public DateTime? UpdatedAt { get; private set; }
        
        /// <summary>When the request was completed/resolved</summary>
        public DateTime? CompletedAt { get; private set; }
        
        /// <summary>Admin notes (internal)</summary>
        public string? AdminNotes { get; private set; }
        
        /// <summary>ID of admin who processed the request</summary>
        public Guid? ProcessedBy { get; private set; }
        
        /// <summary>IP address of the submitter (for audit)</summary>
        public string? IpAddress { get; private set; }

        /// <summary>
        /// For EF Core
        /// </summary>
        private DataRequest() { }

        /// <summary>
        /// Create a new data request from an external (non-user) requester
        /// US 4.5.4: Non-User Data Rights
        /// </summary>
        public static DataRequest CreateExternalRequest(
            Guid id,
            string fullName,
            string email,
            string? phone,
            DataRequestType requestType,
            string? vesselReference,
            string? vvnReference,
            string description,
            string? ipAddress = null)
        {
            if (string.IsNullOrWhiteSpace(fullName))
                throw new ArgumentException("Full name is required", nameof(fullName));
            if (string.IsNullOrWhiteSpace(email))
                throw new ArgumentException("Email is required", nameof(email));
            if (string.IsNullOrWhiteSpace(description))
                throw new ArgumentException("Description is required", nameof(description));

            return new DataRequest
            {
                Id = id,
                RequestId = new DataRequestId(id),
                Source = DataRequestSource.EXTERNAL,
                UserId = null,
                FullName = fullName.Trim(),
                Email = email.Trim().ToLowerInvariant(),
                Phone = phone?.Trim(),
                RequestType = requestType,
                Status = DataRequestStatus.SUBMITTED,
                VesselReference = vesselReference?.Trim(),
                VvnReference = vvnReference?.Trim(),
                Description = description.Trim(),
                ReferenceNumber = GenerateReferenceNumber(),
                ConsentGivenAt = DateTime.UtcNow,
                SubmittedAt = DateTime.UtcNow,
                IpAddress = ipAddress
            };
        }

        /// <summary>
        /// Create a new data request from a registered user
        /// US 4.5.3: User Data Rights (SAR)
        /// </summary>
        public static DataRequest CreateUserRequest(
            Guid id,
            Guid userId,
            string fullName,
            string email,
            DataRequestType requestType,
            string description,
            string? requestData = null,
            string? ipAddress = null)
        {
            if (string.IsNullOrWhiteSpace(fullName))
                throw new ArgumentException("Full name is required", nameof(fullName));
            if (string.IsNullOrWhiteSpace(email))
                throw new ArgumentException("Email is required", nameof(email));
            if (string.IsNullOrWhiteSpace(description))
                throw new ArgumentException("Description is required", nameof(description));

            return new DataRequest
            {
                Id = id,
                RequestId = new DataRequestId(id),
                Source = DataRequestSource.USER,
                UserId = userId,
                FullName = fullName.Trim(),
                Email = email.Trim().ToLowerInvariant(),
                Phone = null,
                RequestType = requestType,
                Status = DataRequestStatus.SUBMITTED,
                VesselReference = null,
                VvnReference = null,
                Description = description.Trim(),
                RequestData = requestData,
                ReferenceNumber = GenerateReferenceNumber(),
                ConsentGivenAt = DateTime.UtcNow,
                SubmittedAt = DateTime.UtcNow,
                IpAddress = ipAddress
            };
        }

        /// <summary>
        /// Legacy constructor for backward compatibility
        /// Creates an external request
        /// </summary>
        public DataRequest(
            Guid id,
            string fullName,
            string email,
            string? phone,
            DataRequestType requestType,
            string? vesselReference,
            string? vvnReference,
            string description,
            string? ipAddress = null)
        {
            if (string.IsNullOrWhiteSpace(fullName))
                throw new ArgumentException("Full name is required", nameof(fullName));
            if (string.IsNullOrWhiteSpace(email))
                throw new ArgumentException("Email is required", nameof(email));
            if (string.IsNullOrWhiteSpace(description))
                throw new ArgumentException("Description is required", nameof(description));

            Id = id;
            RequestId = new DataRequestId(id);
            Source = DataRequestSource.EXTERNAL;
            UserId = null;
            FullName = fullName.Trim();
            Email = email.Trim().ToLowerInvariant();
            Phone = phone?.Trim();
            RequestType = requestType;
            Status = DataRequestStatus.SUBMITTED;
            VesselReference = vesselReference?.Trim();
            VvnReference = vvnReference?.Trim();
            Description = description.Trim();
            ReferenceNumber = GenerateReferenceNumber();
            ConsentGivenAt = DateTime.UtcNow;
            SubmittedAt = DateTime.UtcNow;
            IpAddress = ipAddress;
        }

        /// <summary>
        /// Generate a unique reference number for tracking
        /// Format: DR-YYYY-XXXXXX (e.g., DR-2026-A1B2C3)
        /// </summary>
        private static string GenerateReferenceNumber()
        {
            var year = DateTime.UtcNow.Year;
            var randomPart = Guid.NewGuid().ToString("N").Substring(0, 6).ToUpper();
            return $"DR-{year}-{randomPart}";
        }

        /// <summary>
        /// Update the status of the request
        /// </summary>
        public void UpdateStatus(DataRequestStatus newStatus, string? adminNotes = null, Guid? processedBy = null)
        {
            Status = newStatus;
            UpdatedAt = DateTime.UtcNow;
            
            if (!string.IsNullOrWhiteSpace(adminNotes))
            {
                AdminNotes = string.IsNullOrEmpty(AdminNotes) 
                    ? adminNotes 
                    : $"{AdminNotes}\n\n[{DateTime.UtcNow:yyyy-MM-dd HH:mm}]\n{adminNotes}";
            }

            if (processedBy.HasValue)
            {
                ProcessedBy = processedBy;
            }

            if (newStatus == DataRequestStatus.COMPLETED || newStatus == DataRequestStatus.REJECTED)
            {
                CompletedAt = DateTime.UtcNow;
            }
        }

        /// <summary>
        /// Get estimated response days based on GDPR requirements (30 days)
        /// </summary>
        public int GetEstimatedResponseDays()
        {
            // GDPR requires response within 30 days
            return 30;
        }
    }
}
