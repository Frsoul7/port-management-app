using DDDNetCore.Domain.IRepository;
using DDDNetCore.Domain.Resources;
using Microsoft.EntityFrameworkCore;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace DDDNetCore.Infrastructure.Repositories
{
    /// <summary>
    /// Repository implementation for PhysicalResource aggregate.
    /// Layer 4 (Frameworks & Drivers) - Implements data access using EF Core.
    /// </summary>
    public class PhysicalResourceRepository : BaseRepository<PhysicalResource>, IPhysicalResourceRepository
    {
        public PhysicalResourceRepository(PortDbContext ctx) : base(ctx) { }

        public async Task<PhysicalResource?> GetByCodeAsync(string code, bool includeQualifications = false)
        {
            var query = _dbSet.AsQueryable();

            if (includeQualifications)
            {
                query = query.Include("RequiredQualifications");
            }

            return await query.FirstOrDefaultAsync(r => r.Code == code.ToUpper());
        }

        public async Task<List<PhysicalResource>> GetAllAsync(bool includeQualifications = false)
        {
            var query = _dbSet.AsQueryable();

            if (includeQualifications)
            {
                query = query.Include("RequiredQualifications");
            }

            return await query.ToListAsync();
        }

        public async Task<List<PhysicalResource>> SearchAsync(
            string? code,
            string? description,
            PhysicalResourceAvailability? availability)
        {
            var query = _dbSet.AsQueryable();

            if (!string.IsNullOrWhiteSpace(code))
            {
                var codeLower = code.Trim().ToLower();
                query = query.Where(r => r.Code.ToLower().Contains(codeLower));
            }

            if (!string.IsNullOrWhiteSpace(description))
            {
                var descLower = description.Trim().ToLower();
                query = query.Where(r => r.Description != null && r.Description.ToLower().Contains(descLower));
            }

            if (availability.HasValue)
            {
                query = query.Where(r => r.Availability == availability.Value);
            }

            return await query.ToListAsync();
        }

        public async Task<bool> ExistsAsync(string code)
        {
            return await _dbSet.AnyAsync(r => r.Code == code.ToUpper());
        }
    }
}
