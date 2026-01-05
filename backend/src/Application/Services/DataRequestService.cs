using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDNetCore.Application.DTOs;
using DDDNetCore.Application.Interfaces;
using DDDNetCore.Domain.DataRequests;
using DDDNetCore.Domain.IRepository;

namespace DDDNetCore.Application.Services
{
    /// <summary>
    /// Service implementation for Data Request operations
    /// US 4.5.4: Non-User Data Rights
    /// </summary>
    public class DataRequestService : IDataRequestService
    {
        private readonly IDataRequestRepository _repository;
        private readonly IUnitOfWork _unitOfWork;

        public DataRequestService(IDataRequestRepository repository, IUnitOfWork unitOfWork)
        {
            _repository = repository;
            _unitOfWork = unitOfWork;
        }

        public async Task<DataRequestResponseDto> SubmitNonUserRequestAsync(NonUserDataRequestDto dto, string? ipAddress = null)
        {
            // Validate consent
            if (!dto.ConsentGiven)
            {
                throw new InvalidOperationException("Consent must be given to process the data request.");
            }

            // Create the data request entity
            var request = new DataRequest(
                id: Guid.NewGuid(),
                fullName: dto.FullName,
                email: dto.Email,
                phone: dto.Phone,
                requestType: dto.RequestType,
                vesselReference: dto.VesselReference,
                vvnReference: dto.VvnReference,
                description: dto.Description,
                ipAddress: ipAddress
            );

            // Persist
            await _repository.AddAsync(request);
            await _unitOfWork.CommitAsync();

            // Return response
            return new DataRequestResponseDto
            {
                RequestId = request.Id.ToString(),
                Status = request.Status,
                SubmittedAt = request.SubmittedAt,
                ReferenceNumber = request.ReferenceNumber,
                EstimatedResponseDays = request.GetEstimatedResponseDays(),
                Message = GetSubmissionMessage(request.RequestType)
            };
        }

        public async Task<DataRequestResponseDto?> CheckRequestStatusAsync(string referenceNumber, string email)
        {
            var request = await _repository.GetByReferenceAndEmailAsync(referenceNumber, email);
            
            if (request == null)
            {
                return null;
            }

            return new DataRequestResponseDto
            {
                RequestId = request.Id.ToString(),
                Status = request.Status,
                SubmittedAt = request.SubmittedAt,
                ReferenceNumber = request.ReferenceNumber,
                EstimatedResponseDays = request.GetEstimatedResponseDays(),
                Message = GetStatusMessage(request.Status)
            };
        }

        public async Task<List<DataRequestDetailsDto>> GetAllRequestsAsync()
        {
            var requests = await _repository.GetAllAsync();
            return requests.Select(MapToDetailsDto).ToList();
        }

        public async Task<List<DataRequestDetailsDto>> GetPendingRequestsAsync()
        {
            var pendingStatuses = new[] { DataRequestStatus.SUBMITTED, DataRequestStatus.PENDING, DataRequestStatus.IN_PROGRESS };
            var allRequests = await _repository.GetAllAsync();
            return allRequests
                .Where(r => pendingStatuses.Contains(r.Status))
                .Select(MapToDetailsDto)
                .ToList();
        }

        public async Task<DataRequestDetailsDto?> GetRequestByIdAsync(Guid requestId)
        {
            var request = await _repository.GetByIdAsync(requestId);
            return request == null ? null : MapToDetailsDto(request);
        }

        public async Task<DataRequestDetailsDto?> UpdateRequestStatusAsync(Guid requestId, UpdateDataRequestStatusDto updateDto, Guid adminId)
        {
            var request = await _repository.GetByIdAsync(requestId);
            
            if (request == null)
            {
                return null;
            }

            request.UpdateStatus(updateDto.NewStatus, updateDto.AdminNotes, adminId);
            
            await _repository.UpdateAsync(request);
            await _unitOfWork.CommitAsync();

            return MapToDetailsDto(request);
        }

        private static DataRequestDetailsDto MapToDetailsDto(DataRequest request)
        {
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

        private static string GetSubmissionMessage(DataRequestType requestType)
        {
            return requestType switch
            {
                DataRequestType.ACCESS => "Your data access request has been submitted. We will compile your data and respond within 30 days as required by GDPR.",
                DataRequestType.RECTIFICATION => "Your data rectification request has been submitted. We will review and correct any inaccurate data within 30 days.",
                DataRequestType.DELETION => "Your data deletion request has been submitted. We will review and process your request within 30 days.",
                DataRequestType.OBJECTION => "Your objection to data processing has been submitted. We will review your request within 30 days.",
                _ => "Your request has been submitted. We will process it within 30 days as required by GDPR."
            };
        }

        private static string GetStatusMessage(DataRequestStatus status)
        {
            return status switch
            {
                DataRequestStatus.SUBMITTED => "Your request has been received and is awaiting review.",
                DataRequestStatus.PENDING => "Your request is pending and will be reviewed shortly.",
                DataRequestStatus.IN_PROGRESS => "Your request is currently being processed.",
                DataRequestStatus.COMPLETED => "Your request has been completed. Please check your email for details.",
                DataRequestStatus.REJECTED => "Your request has been reviewed but could not be processed. Please check your email for more information.",
                _ => "Request status unknown."
            };
        }
    }
}
