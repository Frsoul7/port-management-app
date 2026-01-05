using DDDNetCore.Domain.Organizations;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace DDDNetCore.Domain.IRepository
{
    /// <summary>
    /// Repository interface for Organization aggregate.
    /// Layer 2 (Application Business Rules) - Defines contract for data access.
    /// </summary>
    public interface IOrganizationRepository
    {
        Task<List<Organization>> ListAsync();
        Task<Organization?> GetByIdAsync(string id);
        Task<Organization?> GetByIdWithRepresentativesAsync(string id);
        Task<Organization?> GetByIdentifierWithRepresentativesAsync(string identifier);
        Task AddAsync(Organization org);
        void Update(Organization org);
        Task<bool> ExistsWithIdentifierAsync(string identifier);
    }
}
