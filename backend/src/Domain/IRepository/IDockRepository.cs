using DDDNetCore.Domain.Docks;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace DDDNetCore.Domain.IRepository
{
    /// <summary>
    /// Repository interface for Dock aggregate.
    /// Layer 2 (Application Business Rules) - Defines contract for data access.
    /// </summary>
    public interface IDockRepository
    {
        Task<Dock?> GetByCodeAsync(string code, bool includeVesselTypes = false);
        Task<List<Dock>> GetAllAsync(bool includeVesselTypes = false);
        Task<List<Dock>> SearchAsync(string? name, string? location, string? vesselTypeId);
        Task<bool> ExistsAsync(string code);
        Task AddAsync(Dock entity);
        void Update(Dock entity);
        void Remove(Dock entity);
        Task<List<Dock>> ListAsync();
    }
}
