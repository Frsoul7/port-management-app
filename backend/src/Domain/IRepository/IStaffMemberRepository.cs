using DDDNetCore.Domain.HumanResources;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace DDDNetCore.Domain.IRepository
{
    /// <summary>
    /// Repository interface for StaffMember aggregate.
    /// Layer 2 (Application Business Rules) - Defines contract for data access.
    /// </summary>
    public interface IStaffMemberRepository
    {
        Task<StaffMember?> GetByMecanographicNumberAsync(long mecanographicNumber);
        Task<List<StaffMember>> GetByStatusAsync(HumanResourceStatus status);
        Task<bool> ExistsAsync(long mecanographicNumber);
        Task AddAsync(StaffMember entity);
        void Update(StaffMember entity);
        void Remove(StaffMember entity);
        Task<List<StaffMember>> ListAsync();
    }
}
