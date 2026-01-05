using System;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace DDDNetCore.Domain.IRepository
{
    using DDDNetCore.Domain.DataRequests;

    /// <summary>
    /// Repository interface for DataRequest aggregate.
    /// US 4.5.3: User Data Rights (SAR)
    /// US 4.5.4: Non-User Data Rights
    /// </summary>
    public interface IDataRequestRepository
    {
        /// <summary>
        /// Get a data request by ID
        /// </summary>
        Task<DataRequest?> GetByIdAsync(Guid id);

        /// <summary>
        /// Get a data request by reference number and email (for status check)
        /// </summary>
        Task<DataRequest?> GetByReferenceAndEmailAsync(string referenceNumber, string email);

        /// <summary>
        /// Get all data requests (for admin management)
        /// </summary>
        Task<List<DataRequest>> GetAllAsync();

        /// <summary>
        /// Get data requests by status
        /// </summary>
        Task<List<DataRequest>> GetByStatusAsync(DataRequestStatus status);

        /// <summary>
        /// Get data requests by email (to check for existing requests)
        /// </summary>
        Task<List<DataRequest>> GetByEmailAsync(string email);

        /// <summary>
        /// Get data requests by user ID (for authenticated users to see their requests)
        /// </summary>
        Task<List<DataRequest>> GetByUserIdAsync(Guid userId);

        /// <summary>
        /// Get data requests by source (USER or EXTERNAL)
        /// </summary>
        Task<List<DataRequest>> GetBySourceAsync(DataRequestSource source);

        /// <summary>
        /// Add a new data request
        /// </summary>
        Task AddAsync(DataRequest request);

        /// <summary>
        /// Update an existing data request
        /// </summary>
        Task UpdateAsync(DataRequest request);
    }
}
