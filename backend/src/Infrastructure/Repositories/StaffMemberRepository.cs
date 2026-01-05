using DDDNetCore.Domain.IRepository;
using DDDNetCore.Domain.HumanResources;
using Microsoft.EntityFrameworkCore;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace DDDNetCore.Infrastructure.Repositories
{
    /// <summary>
    /// Repository implementation for StaffMember aggregate.
    /// Layer 4 (Frameworks & Drivers) - Implements data access using EF Core.
    /// </summary>
    public class StaffMemberRepository : BaseRepository<StaffMember>, IStaffMemberRepository
    {
        public StaffMemberRepository(PortDbContext ctx) : base(ctx) { }

        public async Task<StaffMember?> GetByMecanographicNumberAsync(long mecanographicNumber)
        {
            return await _dbSet.FirstOrDefaultAsync(sm => sm.MecanographicNumber == mecanographicNumber);
        }

        public async Task<List<StaffMember>> GetByStatusAsync(HumanResourceStatus status)
        {
            return await _dbSet.Where(sm => sm.Status == status).ToListAsync();
        }

        public async Task<bool> ExistsAsync(long mecanographicNumber)
        {
            return await _dbSet.AnyAsync(sm => sm.MecanographicNumber == mecanographicNumber);
        }
    }
}