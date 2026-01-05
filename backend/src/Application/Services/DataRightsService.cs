using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.Json;
using System.Threading.Tasks;
using DDDNetCore.Application.DTOs;
using DDDNetCore.Application.Interfaces;
using DDDNetCore.Domain.DataRequests;
using DDDNetCore.Domain.IRepository;
using DDDNetCore.Domain.Users;

namespace DDDNetCore.Application.Services
{
    /// <summary>
    /// Service implementation for User Data Rights operations (GDPR Subject Access Requests)
    /// US 4.5.3: User Data Rights (SAR)
    /// </summary>
    public class DataRightsService : IDataRightsService
    {
        private readonly IUserRepository _userRepository;
        private readonly IOrganizationRepository _organizationRepository;
        private readonly IVesselVisitNotificationRepository _vvnRepository;
        private readonly IPrivacyPolicyRepository _privacyPolicyRepository;
        private readonly IDataRequestRepository _dataRequestRepository;
        private readonly IUnitOfWork _unitOfWork;

        public DataRightsService(
            IUserRepository userRepository,
            IOrganizationRepository organizationRepository,
            IVesselVisitNotificationRepository vvnRepository,
            IPrivacyPolicyRepository privacyPolicyRepository,
            IDataRequestRepository dataRequestRepository,
            IUnitOfWork unitOfWork)
        {
            _userRepository = userRepository;
            _organizationRepository = organizationRepository;
            _vvnRepository = vvnRepository;
            _privacyPolicyRepository = privacyPolicyRepository;
            _dataRequestRepository = dataRequestRepository;
            _unitOfWork = unitOfWork;
        }

        public async Task<UserDataExportDto> ExportUserDataAsync(Guid userId)
        {
            var user = await _userRepository.GetByIdAsync(new UserId(userId));
            if (user == null)
            {
                throw new InvalidOperationException("User not found");
            }

            var export = new UserDataExportDto
            {
                ExportedAt = DateTime.UtcNow,
                Version = "1.0",
                Profile = new UserProfileDataDto
                {
                    Id = user.Id.ToString(),
                    Name = user.Name,
                    Email = user.Email,
                    ProfilePictureUrl = user.ProfilePictureUrl,
                    Role = user.Role?.ToString(),
                    IsActive = user.IsActive,
                    EmailVerified = user.EmailVerified,
                    CreatedAt = user.CreatedAt
                }
            };

            // Get organization data
            if (user.OrganizationId != null)
            {
                var org = await _organizationRepository.GetByIdAsync(user.OrganizationId.ToString());
                if (org != null)
                {
                    export.Organization = new OrganizationDataDto
                    {
                        Id = org.Id.ToString(),
                        Name = org.LegalName,
                        Type = org.Type.ToString()
                    };
                }
            }

            // Get VVNs created by user (if representative)
            try
            {
                var vvns = await _vvnRepository.GetByOrganizationAsync(
                    user.OrganizationId, 
                    submittedById: userId);
                
                export.CreatedVvns = vvns.Select(v => new VvnSummaryDto
                {
                    Id = v.VvnGuid.ToString(),
                    VvnNumber = v.VvnBusinessId,
                    Status = v.State.ToString(),
                    CreatedAt = v.SubmittedAt ?? v.CreatedAt,
                    VesselName = v.VesselImo // Using IMO as vessel reference
                }).ToList();
            }
            catch
            {
                export.DataSourceNotes = "Some VVN data may be unavailable.";
            }

            // Get policy acknowledgments
            // Note: GetUserAcknowledgmentsAsync needs to be added to IPrivacyPolicyRepository
            // For now, we skip this section
            export.PolicyAcknowledgments = new List<PolicyAcknowledgmentDto>();

            // Get user's data requests
            var requests = await _dataRequestRepository.GetByUserIdAsync(userId);
            export.DataRequests = requests.Select(r => new DataRequestSummaryDto
            {
                ReferenceNumber = r.ReferenceNumber,
                RequestType = r.RequestType.ToString(),
                Status = r.Status.ToString(),
                SubmittedAt = r.SubmittedAt,
                CompletedAt = r.CompletedAt
            }).ToList();

            // TODO: Add OEM data when OEM backend integration is implemented
            // This would call the OEM backend to get operation plans, VVEs, etc.
            export.OemData = null;
            if (string.IsNullOrEmpty(export.DataSourceNotes))
            {
                export.DataSourceNotes = "OEM backend data will be included when integration is complete.";
            }
            else
            {
                export.DataSourceNotes += " OEM backend data will be included when integration is complete.";
            }

            return export;
        }

        public async Task<byte[]> ExportUserDataAsPdfAsync(Guid userId)
        {
            var data = await ExportUserDataAsync(userId);
            
            // Generate PDF using a simple approach
            // In production, use QuestPDF or similar library
            var pdfContent = GenerateSimplePdf(data);
            return pdfContent;
        }

        private byte[] GenerateSimplePdf(UserDataExportDto data)
        {
            // Simple text-based PDF generation
            // In production, replace with QuestPDF for proper formatting
            var content = $@"
GDPR DATA EXPORT REPORT
=======================
Generated: {data.ExportedAt:yyyy-MM-dd HH:mm:ss} UTC
Version: {data.Version}

USER PROFILE
------------
ID: {data.Profile.Id}
Name: {data.Profile.Name}
Email: {data.Profile.Email}
Role: {data.Profile.Role ?? "N/A"}
Active: {data.Profile.IsActive}
Email Verified: {data.Profile.EmailVerified}
Created: {data.Profile.CreatedAt:yyyy-MM-dd}

ORGANIZATION
------------
{(data.Organization != null ? $@"ID: {data.Organization.Id}
Name: {data.Organization.Name}
Type: {data.Organization.Type}" : "No organization data")}

VVNs CREATED ({data.CreatedVvns.Count})
--------------------
{string.Join("\n", data.CreatedVvns.Select(v => $"- {v.VvnNumber} ({v.Status}) - {v.CreatedAt:yyyy-MM-dd}"))}

POLICY ACKNOWLEDGMENTS ({data.PolicyAcknowledgments.Count})
----------------------------
{string.Join("\n", data.PolicyAcknowledgments.Select(p => $"- Version {p.PolicyVersion} on {p.AcknowledgedAt:yyyy-MM-dd}"))}

DATA REQUESTS ({data.DataRequests.Count})
-----------------
{string.Join("\n", data.DataRequests.Select(r => $"- {r.ReferenceNumber}: {r.RequestType} ({r.Status})"))}

NOTES
-----
{data.DataSourceNotes ?? "No additional notes."}
";

            // Return as UTF-8 bytes (in production, generate actual PDF)
            return System.Text.Encoding.UTF8.GetBytes(content);
        }

        public async Task<UserDataRequestResponseDto> RequestRectificationAsync(Guid userId, RectificationRequestDto request)
        {
            var user = await _userRepository.GetByIdAsync(new UserId(userId));
            if (user == null)
            {
                throw new InvalidOperationException("User not found");
            }

            // Serialize rectification data as JSON
            var requestData = JsonSerializer.Serialize(new
            {
                request.DataCategory,
                request.CurrentValue,
                request.RequestedCorrection,
                request.Reason
            });

            var dataRequest = DataRequest.CreateUserRequest(
                id: Guid.NewGuid(),
                userId: userId,
                fullName: user.Name,
                email: user.Email,
                requestType: DataRequestType.RECTIFICATION,
                description: $"Rectification request for {request.DataCategory}: {request.Reason}",
                requestData: requestData
            );

            await _dataRequestRepository.AddAsync(dataRequest);
            await _unitOfWork.CommitAsync();

            return new UserDataRequestResponseDto
            {
                RequestId = dataRequest.Id.ToString(),
                ReferenceNumber = dataRequest.ReferenceNumber,
                RequestType = DataRequestType.RECTIFICATION.ToString(),
                Status = dataRequest.Status.ToString(),
                SubmittedAt = dataRequest.SubmittedAt,
                EstimatedResponseDays = dataRequest.GetEstimatedResponseDays(),
                Message = "Your data rectification request has been submitted. We will review it within 30 days as required by GDPR."
            };
        }

        public async Task<UserDataRequestResponseDto> RequestDeletionAsync(Guid userId, DeletionRequestDto request)
        {
            if (!request.ConfirmUnderstanding)
            {
                throw new InvalidOperationException("You must confirm that you understand the consequences of account deletion.");
            }

            var user = await _userRepository.GetByIdAsync(new UserId(userId));
            if (user == null)
            {
                throw new InvalidOperationException("User not found");
            }

            // Check for existing pending deletion request
            var existingRequests = await _dataRequestRepository.GetByUserIdAsync(userId);
            var pendingDeletion = existingRequests.FirstOrDefault(r => 
                r.RequestType == DataRequestType.DELETION && 
                r.Status != DataRequestStatus.COMPLETED && 
                r.Status != DataRequestStatus.REJECTED);

            if (pendingDeletion != null)
            {
                throw new InvalidOperationException($"You already have a pending deletion request (Reference: {pendingDeletion.ReferenceNumber}).");
            }

            var dataRequest = DataRequest.CreateUserRequest(
                id: Guid.NewGuid(),
                userId: userId,
                fullName: user.Name,
                email: user.Email,
                requestType: DataRequestType.DELETION,
                description: request.Reason ?? "User requested account deletion (Right to Erasure - GDPR Article 17)"
            );

            await _dataRequestRepository.AddAsync(dataRequest);
            await _unitOfWork.CommitAsync();

            return new UserDataRequestResponseDto
            {
                RequestId = dataRequest.Id.ToString(),
                ReferenceNumber = dataRequest.ReferenceNumber,
                RequestType = DataRequestType.DELETION.ToString(),
                Status = dataRequest.Status.ToString(),
                SubmittedAt = dataRequest.SubmittedAt,
                EstimatedResponseDays = dataRequest.GetEstimatedResponseDays(),
                Message = "Your account deletion request has been submitted and will be reviewed by an administrator. You will receive an email once it has been processed."
            };
        }

        public async Task<List<MyDataRequestDto>> GetMyRequestsAsync(Guid userId)
        {
            var requests = await _dataRequestRepository.GetByUserIdAsync(userId);
            
            return requests.Select(r => new MyDataRequestDto
            {
                RequestId = r.Id.ToString(),
                ReferenceNumber = r.ReferenceNumber,
                RequestType = r.RequestType.ToString(),
                Status = r.Status.ToString(),
                Description = r.Description,
                SubmittedAt = r.SubmittedAt,
                UpdatedAt = r.UpdatedAt,
                CompletedAt = r.CompletedAt
            }).ToList();
        }

        public async Task<DataRequestDetailsDto?> ProcessDeletionRequestAsync(
            Guid requestId, 
            Guid adminId, 
            bool approved, 
            string? notes)
        {
            var request = await _dataRequestRepository.GetByIdAsync(requestId);
            if (request == null)
            {
                return null;
            }

            if (request.RequestType != DataRequestType.DELETION)
            {
                throw new InvalidOperationException("This endpoint is only for processing deletion requests.");
            }

            if (approved)
            {
                // Process the deletion
                if (request.UserId.HasValue)
                {
                    var user = await _userRepository.GetByIdAsync(new UserId(request.UserId.Value));
                    if (user != null)
                    {
                        // Soft delete: deactivate and anonymize
                        user.Deactivate();
                        // In production: also anonymize user data and call OEM backend
                        await _userRepository.UpdateAsync(user);
                    }
                }

                request.UpdateStatus(DataRequestStatus.COMPLETED, notes ?? "Deletion request approved and processed.", adminId);
            }
            else
            {
                request.UpdateStatus(DataRequestStatus.REJECTED, notes ?? "Deletion request rejected.", adminId);
            }

            await _dataRequestRepository.UpdateAsync(request);
            await _unitOfWork.CommitAsync();

            return new DataRequestDetailsDto
            {
                RequestId = request.Id.ToString(),
                FullName = request.FullName,
                Email = request.Email,
                Phone = request.Phone,
                RequestType = request.RequestType.ToString(),
                Status = request.Status.ToString(),
                VesselReference = request.VesselReference,
                VvnReference = request.VvnReference,
                Description = request.Description,
                ReferenceNumber = request.ReferenceNumber,
                ConsentGivenAt = request.ConsentGivenAt,
                SubmittedAt = request.SubmittedAt,
                UpdatedAt = request.UpdatedAt,
                CompletedAt = request.CompletedAt,
                AdminNotes = request.AdminNotes,
                ProcessedBy = request.ProcessedBy?.ToString()
            };
        }
    }
}
