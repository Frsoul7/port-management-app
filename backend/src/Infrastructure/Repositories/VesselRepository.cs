using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDNetCore.Domain.IRepository;
using DDDNetCore.Domain.Vessels;
using Microsoft.EntityFrameworkCore;

namespace DDDNetCore.Infrastructure.Repositories;

/// <summary>
/// Repository implementation for Vessel aggregate root.
/// Handles all data access operations for vessels.
/// </summary>
public class VesselRepository : BaseRepository<Vessel>, IVesselRepository
{
    public VesselRepository(PortDbContext ctx) : base(ctx)
    {
    }

    public async Task<Vessel?> GetByIdAsync(string imoNumber)
    {
        return await _dbSet
            .Include(v => v.VesselType)
            .Include(v => v.OwnerOrganization)
            .FirstOrDefaultAsync(v => v.ImoNumber == imoNumber);
    }

    public async Task<Vessel?> GetByImoNumberAsync(string imoNumber)
    {
        return await GetByIdAsync(imoNumber);
    }

    public async Task<IEnumerable<Vessel>> GetAllAsync()
    {
        return await _dbSet
            .Include(v => v.VesselType)
            .Include(v => v.OwnerOrganization)
            .ToListAsync();
    }

    public async Task<IEnumerable<Vessel>> SearchAsync(string? searchTerm = null, string? vesselTypeId = null, string? organizationId = null)
    {
        var query = _dbSet
            .Include(v => v.VesselType)
            .Include(v => v.OwnerOrganization)
            .AsQueryable();

        if (!string.IsNullOrWhiteSpace(searchTerm))
        {
            var term = searchTerm.Trim().ToUpperInvariant();
            query = query.Where(v => 
                v.ImoNumber.Contains(term) || 
                v.Name.ToUpper().Contains(term));
        }

        if (!string.IsNullOrWhiteSpace(vesselTypeId))
        {
            query = query.Where(v => v.VesselTypeId == vesselTypeId);
        }

        if (!string.IsNullOrWhiteSpace(organizationId))
        {
            query = query.Where(v => v.OwnerOrganizationId.Value.ToString() == organizationId);
        }

        return await query.ToListAsync();
    }

    public async Task<bool> ExistsAsync(string imoNumber)
    {
        return await _dbSet.AnyAsync(v => v.ImoNumber == imoNumber);
    }
}
