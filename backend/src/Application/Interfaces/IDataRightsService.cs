using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using DDDNetCore.Application.DTOs;

namespace DDDNetCore.Application.Interfaces
{
    /// <summary>
    /// Service interface for User Data Rights operations (GDPR Subject Access Requests)
    /// US 4.5.3: User Data Rights (SAR)
    /// </summary>
    public interface IDataRightsService
    {
        /// <summary>
        /// Export all user data as JSON (GDPR Article 15 - Right of Access)
        /// </summary>
        /// <param name="userId">The ID of the user requesting their data</param>
        /// <returns>Complete user data export</returns>
        Task<UserDataExportDto> ExportUserDataAsync(Guid userId);

        /// <summary>
        /// Export user data as PDF (GDPR Article 15 - Right of Access)
        /// </summary>
        /// <param name="userId">The ID of the user requesting their data</param>
        /// <returns>PDF file bytes</returns>
        Task<byte[]> ExportUserDataAsPdfAsync(Guid userId);

        /// <summary>
        /// Submit a data rectification request (GDPR Article 16)
        /// </summary>
        /// <param name="userId">The ID of the user</param>
        /// <param name="request">The rectification request details</param>
        /// <returns>The created request response</returns>
        Task<UserDataRequestResponseDto> RequestRectificationAsync(Guid userId, RectificationRequestDto request);

        /// <summary>
        /// Submit an account deletion request (GDPR Article 17 - Right to Erasure)
        /// </summary>
        /// <param name="userId">The ID of the user</param>
        /// <param name="request">The deletion request details</param>
        /// <returns>The created request response</returns>
        Task<UserDataRequestResponseDto> RequestDeletionAsync(Guid userId, DeletionRequestDto request);

        /// <summary>
        /// Get the user's data request history
        /// </summary>
        /// <param name="userId">The ID of the user</param>
        /// <returns>List of the user's data requests</returns>
        Task<List<MyDataRequestDto>> GetMyRequestsAsync(Guid userId);

        /// <summary>
        /// Process a deletion request (admin only)
        /// </summary>
        /// <param name="requestId">The data request ID</param>
        /// <param name="adminId">The admin processing the request</param>
        /// <param name="approved">Whether to approve or reject</param>
        /// <param name="notes">Admin notes</param>
        /// <returns>Updated request details</returns>
        Task<DataRequestDetailsDto?> ProcessDeletionRequestAsync(Guid requestId, Guid adminId, bool approved, string? notes);
    }
}
