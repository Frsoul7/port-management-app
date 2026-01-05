using DDDNetCore.Domain.IRepository;
using DDDNetCore.Domain.Vessels;
using Microsoft.EntityFrameworkCore;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace DDDNetCore.Infrastructure.Repositories
{
    /// <summary>
    /// Repository implementation for VesselType aggregate.
    /// Layer 4 (Frameworks & Drivers) - Implements data access using EF Core.
    /// </summary>
    public class VesselTypeRepository : BaseRepository<VesselType>, IVesselTypeRepository
    {
        public VesselTypeRepository(PortDbContext ctx) : base(ctx) { }

        public async Task<VesselType?> GetByIdAsync(string id)
        {
            return await _dbSet.FirstOrDefaultAsync(vt => vt.VesselTypeId == id);
        }

        public async Task<List<VesselType>> GetByIdsAsync(IEnumerable<string> ids)
        {
            return await _dbSet.Where(vt => ids.Contains(vt.VesselTypeId)).ToListAsync();
        }

        public async Task<bool> ExistsAsync(string id)
        {
            return await _dbSet.AnyAsync(vt => vt.VesselTypeId == id);
        }

        public async Task<bool> ExistsByNameAsync(string name, string? excludeId = null)
        {
            var normalized = name.Trim().ToLower();
            var query = _dbSet.Where(vt => vt.Name.ToLower() == normalized);
            
            if (!string.IsNullOrWhiteSpace(excludeId))
            {
                query = query.Where(vt => vt.VesselTypeId != excludeId);
            }
            
            return await query.AnyAsync();
        }

        public async Task<(List<VesselType> Items, int Total)> SearchAsync(string? name, string? description, int page, int pageSize)
        {
            var query = _dbSet.AsNoTracking().AsQueryable();

            if (!string.IsNullOrWhiteSpace(name))
            {
                var n = name.Trim().ToLower();
                query = query.Where(v => v.Name.ToLower().Contains(n));
            }
            
            if (!string.IsNullOrWhiteSpace(description))
            {
                var d = description.Trim().ToLower();
                query = query.Where(v => (v.Description ?? "").ToLower().Contains(d));
            }

            var total = await query.CountAsync();
            var items = await query
                .OrderBy(v => v.Name)
                .Skip((page - 1) * pageSize)
                .Take(pageSize)
                .ToListAsync();

            return (items, total);
        }
    }
}
