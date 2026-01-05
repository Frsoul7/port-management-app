using System.Collections.Generic;
using System.Threading.Tasks;
using DDDNetCore.Domain.Vessels;

namespace DDDNetCore.Domain.IRepository;

/// <summary>
/// Repository interface for Vessel aggregate root.
/// Provides data access operations for vessels.
/// </summary>
public interface IVesselRepository
{
    Task<Vessel?> GetByIdAsync(string imoNumber);
    Task<Vessel?> GetByImoNumberAsync(string imoNumber);
    Task<IEnumerable<Vessel>> GetAllAsync();
    Task<IEnumerable<Vessel>> SearchAsync(string? searchTerm = null, string? vesselTypeId = null, string? organizationId = null);
    Task<bool> ExistsAsync(string imoNumber);
    Task AddAsync(Vessel vessel);
    void Update(Vessel vessel);
    void Remove(Vessel vessel);
}
