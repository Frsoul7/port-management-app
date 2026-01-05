using DDDNetCore.Domain.IRepository;
using DDDNetCore.Domain.Organizations;
using Microsoft.EntityFrameworkCore;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace DDDNetCore.Infrastructure.Repositories
{
    /// <summary>
    /// Repository implementation for Organization aggregate.
    /// Layer 4 (Frameworks & Drivers) - Implements data access using EF Core.
    /// </summary>
    public class OrganizationRepository : IOrganizationRepository
    {
        private readonly PortDbContext _ctx;

        public OrganizationRepository(PortDbContext ctx)
        {
            _ctx = ctx;
        }

        public async Task<List<Organization>> ListAsync()
        {
            return await _ctx.Organizations
                .Include(o => o.Representatives) // safe include
                .ToListAsync();
        }

        // Lookup only by GUID (OrganizationId.Value)
        public async Task<Organization?> GetByIdAsync(string id)
        {
            if (!Guid.TryParse(id, out var guid))
                return null;

            // Create OrganizationId value object for comparison
            // EF Core will translate this correctly with the configured HasConversion
            var organizationId = new OrganizationId(guid);

            return await _ctx.Organizations
                .Include(o => o.Representatives) // eager load reps
                .FirstOrDefaultAsync(o => o.OrganizationId == organizationId);
        }

        public async Task AddAsync(Organization org)
        {
            await _ctx.Organizations.AddAsync(org);
        }

        public async Task<bool> ExistsWithIdentifierAsync(string identifier)
        {
            return await _ctx.Organizations.AnyAsync(o => o.Identifier.ToLower() == identifier.ToLower());
        }

        public async Task<Organization?> GetByIdWithRepresentativesAsync(string id)
        {
            if (!Guid.TryParse(id, out var guid))
                return null;

            // Create OrganizationId value object for comparison
            var organizationId = new OrganizationId(guid);

            return await _ctx.Organizations
                .Include(o => o.Representatives)
                .FirstOrDefaultAsync(o => o.OrganizationId == organizationId);
        }

        public async Task<Organization?> GetByIdentifierWithRepresentativesAsync(string identifier)
        {
            return await _ctx.Organizations
                .Include(o => o.Representatives)
                .FirstOrDefaultAsync(o => o.Identifier.ToLower() == identifier.ToLower());
        }

        public void Update(Organization org)
        {
            _ctx.Organizations.Update(org);
        }
    }
}
