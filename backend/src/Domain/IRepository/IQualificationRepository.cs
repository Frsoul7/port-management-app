using DDDNetCore.Domain.HumanResources;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace DDDNetCore.Domain.IRepository
{
    /// <summary>
    /// Repository interface for StaffMemberQualification aggregate.
    /// Layer 2 (Application Business Rules) - Defines contract for data access.
    /// </summary>
    public interface IQualificationRepository
    {
        Task<StaffMemberQualification?> GetByIdAsync(string qualificationId);
        Task<List<StaffMemberQualification>> SearchAsync(string? code = null, string? name = null);
        Task<bool> ExistsAsync(string qualificationId);
        Task AddAsync(StaffMemberQualification entity);
        void Update(StaffMemberQualification entity);
        void Remove(StaffMemberQualification entity);
        Task<List<StaffMemberQualification>> ListAsync();
    }
}
