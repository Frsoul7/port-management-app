using DDDNetCore.Domain.Resources;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace DDDNetCore.Domain.IRepository
{
    /// <summary>
    /// Repository interface for PhysicalResource aggregate.
    /// Layer 2 (Application Business Rules) - Defines contract for data access.
    /// </summary>
    public interface IPhysicalResourceRepository
    {
        Task<PhysicalResource?> GetByCodeAsync(string code, bool includeQualifications = false);
        Task<List<PhysicalResource>> GetAllAsync(bool includeQualifications = false);
        Task<List<PhysicalResource>> SearchAsync(string? code, string? description, PhysicalResourceAvailability? availability);
        Task<bool> ExistsAsync(string code);
        Task AddAsync(PhysicalResource entity);
        void Update(PhysicalResource entity);
        void Remove(PhysicalResource entity);
        Task<List<PhysicalResource>> ListAsync();
    }
}
