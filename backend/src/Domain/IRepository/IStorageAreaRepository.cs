using DDDNetCore.Domain.StorageAreas;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace DDDNetCore.Domain.IRepository
{
    /// <summary>
    /// Repository interface for StorageArea aggregate.
    /// Layer 2 (Application Business Rules) - Defines contract for data access.
    /// </summary>
    public interface IStorageAreaRepository
    {
        Task<StorageArea?> GetByIdAsync(string id, bool includeDocks = false);
        Task<List<StorageArea>> GetAllAsync(bool includeDocks = false);
        Task<List<StorageArea>> SearchAsync(string? name, string? location, StorageAreaType? type, bool? servesAllDocks);
        Task AddAsync(StorageArea entity);
        void Update(StorageArea entity);
        void Remove(StorageArea entity);
        Task<List<StorageArea>> ListAsync();
    }
}
