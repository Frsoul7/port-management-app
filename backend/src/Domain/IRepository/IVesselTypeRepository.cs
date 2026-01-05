using DDDNetCore.Domain.Vessels;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace DDDNetCore.Domain.IRepository
{
    /// <summary>
    /// Repository interface for VesselType aggregate.
    /// Layer 2 (Application Business Rules) - Defines contract for data access.
    /// </summary>
    public interface IVesselTypeRepository
    {
        Task<VesselType?> GetByIdAsync(string id);
        Task<List<VesselType>> GetByIdsAsync(IEnumerable<string> ids);
        Task<bool> ExistsAsync(string id);
        Task<bool> ExistsByNameAsync(string name, string? excludeId = null);
        Task AddAsync(VesselType entity);
        void Update(VesselType entity);
        void Remove(VesselType entity);
        Task<List<VesselType>> ListAsync();
        Task<(List<VesselType> Items, int Total)> SearchAsync(string? name, string? description, int page, int pageSize);
    }
}
