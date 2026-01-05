using DDDNetCore.Domain.IRepository;
using DDDNetCore.Domain.Docks;
using DDDNetCore.Domain.Vessels;
using Microsoft.EntityFrameworkCore;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace DDDNetCore.Infrastructure.Repositories
{
    /// <summary>
    /// Repository implementation for Dock aggregate.
    /// Layer 4 (Frameworks & Drivers) - Implements data access using EF Core.
    /// </summary>
    public class DockRepository : BaseRepository<Dock>, IDockRepository
    {
        public DockRepository(PortDbContext ctx) : base(ctx) { }

        public async Task<Dock?> GetByCodeAsync(string code, bool includeVesselTypes = false)
        {
            var query = _dbSet.AsQueryable();
            
            if (includeVesselTypes)
            {
                query = query.Include(d => d.AllowedVesselTypes);
            }
            
            return await query.FirstOrDefaultAsync(d => d.Code == code);
        }

        public async Task<List<Dock>> GetAllAsync(bool includeVesselTypes = false)
        {
            var query = _dbSet.AsQueryable();
            
            if (includeVesselTypes)
            {
                query = query.Include(d => d.AllowedVesselTypes);
            }
            
            return await query.ToListAsync();
        }

        public async Task<List<Dock>> SearchAsync(string? name, string? location, string? vesselTypeId)
        {
            var query = _dbSet.Include(d => d.AllowedVesselTypes).AsQueryable();

            if (!string.IsNullOrWhiteSpace(name))
            {
                query = query.Where(d => d.Name.Contains(name) || d.Code.Contains(name));
            }

            if (!string.IsNullOrWhiteSpace(location))
            {
                query = query.Where(d => d.Location.Contains(location));
            }

            if (!string.IsNullOrWhiteSpace(vesselTypeId))
            {
                query = query.Where(d => d.AllowedVesselTypes.Any(vt => vt.VesselTypeId == vesselTypeId));
            }

            return await query.ToListAsync();
        }

        public async Task<bool> ExistsAsync(string code)
        {
            return await _dbSet.AnyAsync(d => d.Code == code);
        }
    }
}
