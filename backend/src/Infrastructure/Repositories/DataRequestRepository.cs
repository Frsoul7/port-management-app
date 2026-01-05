using DDDNetCore.Domain.IRepository;
using DDDNetCore.Domain.DataRequests;
using Microsoft.EntityFrameworkCore;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace DDDNetCore.Infrastructure.Repositories
{
    /// <summary>
    /// Repository implementation for DataRequest aggregate.
    /// US 4.5.3: User Data Rights (SAR)
    /// US 4.5.4: Non-User Data Rights
    /// </summary>
    public class DataRequestRepository : BaseRepository<DataRequest>, IDataRequestRepository
    {
        public DataRequestRepository(PortDbContext ctx) : base(ctx)
        {
        }

        public async Task<DataRequest?> GetByIdAsync(Guid id)
        {
            return await _dbSet
                .FirstOrDefaultAsync(r => r.Id == id);
        }

        public async Task<DataRequest?> GetByReferenceAndEmailAsync(string referenceNumber, string email)
        {
            var normalizedEmail = email.Trim().ToLowerInvariant();
            return await _dbSet
                .FirstOrDefaultAsync(r => 
                    r.ReferenceNumber == referenceNumber && 
                    r.Email == normalizedEmail);
        }

        public async Task<List<DataRequest>> GetAllAsync()
        {
            return await _dbSet
                .OrderByDescending(r => r.SubmittedAt)
                .ToListAsync();
        }

        public async Task<List<DataRequest>> GetByStatusAsync(DataRequestStatus status)
        {
            return await _dbSet
                .Where(r => r.Status == status)
                .OrderByDescending(r => r.SubmittedAt)
                .ToListAsync();
        }

        public async Task<List<DataRequest>> GetByEmailAsync(string email)
        {
            var normalizedEmail = email.Trim().ToLowerInvariant();
            return await _dbSet
                .Where(r => r.Email == normalizedEmail)
                .OrderByDescending(r => r.SubmittedAt)
                .ToListAsync();
        }

        public async Task<List<DataRequest>> GetByUserIdAsync(Guid userId)
        {
            return await _dbSet
                .Where(r => r.UserId == userId)
                .OrderByDescending(r => r.SubmittedAt)
                .ToListAsync();
        }

        public async Task<List<DataRequest>> GetBySourceAsync(DataRequestSource source)
        {
            return await _dbSet
                .Where(r => r.Source == source)
                .OrderByDescending(r => r.SubmittedAt)
                .ToListAsync();
        }

        public new async Task AddAsync(DataRequest request)
        {
            await base.AddAsync(request);
        }

        public async Task UpdateAsync(DataRequest request)
        {
            _dbSet.Update(request);
            await Task.CompletedTask;
        }
    }
}
