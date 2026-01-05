using DDDNetCore.Domain.IRepository;
using DDDNetCore.Domain.StorageAreas;
using Microsoft.EntityFrameworkCore;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace DDDNetCore.Infrastructure.Repositories
{
    /// <summary>
    /// Repository implementation for StorageArea aggregate.
    /// Layer 4 (Frameworks & Drivers) - Implements data access using EF Core.
    /// </summary>
    public class StorageAreaRepository : BaseRepository<StorageArea>, IStorageAreaRepository
    {
        public StorageAreaRepository(PortDbContext ctx) : base(ctx) { }

        public async Task<StorageArea?> GetByIdAsync(string id, bool includeDocks = false)
        {
            var query = _dbSet.AsQueryable();

            if (includeDocks)
            {
                query = query.Include(sa => sa.Docks);
            }

            return await query.FirstOrDefaultAsync(sa => sa.StorageAreaId == id);
        }

        public async Task<List<StorageArea>> GetAllAsync(bool includeDocks = false)
        {
            var query = _dbSet.AsQueryable();

            if (includeDocks)
            {
                query = query.Include(sa => sa.Docks);
            }

            return await query.ToListAsync();
        }

        public async Task<List<StorageArea>> SearchAsync(
            string? name,
            string? location,
            StorageAreaType? type,
            bool? servesAllDocks)
        {
            var query = _dbSet.Include(sa => sa.Docks).AsQueryable();

            if (!string.IsNullOrWhiteSpace(name))
            {
                query = query.Where(sa => sa.Name.Contains(name));
            }

            if (!string.IsNullOrWhiteSpace(location))
            {
                query = query.Where(sa => sa.Location.Contains(location));
            }

            if (type.HasValue)
            {
                query = query.Where(sa => sa.Type == type.Value);
            }

            if (servesAllDocks.HasValue)
            {
                query = query.Where(sa => sa.ServesAllDocks == servesAllDocks.Value);
            }

            return await query.ToListAsync();
        }

        public async Task<bool> ExistsAsync(string id)
        {
            return await _dbSet.AnyAsync(sa => sa.StorageAreaId == id);
        }
    }
}
