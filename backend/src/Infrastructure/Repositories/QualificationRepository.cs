using DDDNetCore.Domain.IRepository;
using DDDNetCore.Domain.HumanResources;
using Microsoft.EntityFrameworkCore;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace DDDNetCore.Infrastructure.Repositories
{
    /// <summary>
    /// Repository implementation for StaffMemberQualification aggregate.
    /// Layer 4 (Frameworks & Drivers) - Implements data access using EF Core.
    /// </summary>
    public class QualificationRepository : BaseRepository<StaffMemberQualification>, IQualificationRepository
    {
        public QualificationRepository(PortDbContext ctx) : base(ctx) { }

        public async Task<StaffMemberQualification?> GetByIdAsync(string qualificationId)
        {
            return await _dbSet.FirstOrDefaultAsync(q => q.QualificationId == qualificationId);
        }

        public async Task<List<StaffMemberQualification>> SearchAsync(string? code = null, string? name = null)
        {
            var query = _dbSet.AsQueryable();

            if (!string.IsNullOrWhiteSpace(code))
            {
                query = query.Where(q => q.QualificationId.Contains(code.ToUpperInvariant()));
            }

            if (!string.IsNullOrWhiteSpace(name))
            {
                query = query.Where(q => q.Name.Contains(name));
            }

            return await query.ToListAsync();
        }

        public async Task<bool> ExistsAsync(string qualificationId)
        {
            return await _dbSet.AnyAsync(q => q.QualificationId == qualificationId);
        }
    }
}