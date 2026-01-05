using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDNetCore.Application.DTOs;

namespace DDDNetCore.Application.Interfaces
{
    /// <summary>
    /// Service interface for Data Request operations
    /// US 4.5.4: Non-User Data Rights
    /// </summary>
    public interface IDataRequestService
    {
        /// <summary>
        /// Submit a new data request from a non-user
        /// </summary>
        Task<DataRequestResponseDto> SubmitNonUserRequestAsync(NonUserDataRequestDto request, string? ipAddress = null);

        /// <summary>
        /// Check the status of a data request by reference number and email
        /// </summary>
        Task<DataRequestResponseDto?> CheckRequestStatusAsync(string referenceNumber, string email);

        /// <summary>
        /// Get all data requests (admin only)
        /// </summary>
        Task<List<DataRequestDetailsDto>> GetAllRequestsAsync();

        /// <summary>
        /// Get pending data requests (admin only)
        /// </summary>
        Task<List<DataRequestDetailsDto>> GetPendingRequestsAsync();

        /// <summary>
        /// Get a specific data request by ID (admin only)
        /// </summary>
        Task<DataRequestDetailsDto?> GetRequestByIdAsync(Guid requestId);

        /// <summary>
        /// Update the status of a data request (admin only)
        /// </summary>
        Task<DataRequestDetailsDto?> UpdateRequestStatusAsync(Guid requestId, UpdateDataRequestStatusDto updateDto, Guid adminId);
    }
}
