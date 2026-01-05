using DDDNetCore.Application.DTOs.HumanResources;
using DDDNetCore.Domain.HumanResources;

namespace DDDNetCore.Application.Interfaces;

public interface IStaffMemberService
{
    /// <summary>
    /// Creates a new staff member
    /// </summary>
    Task<StaffMemberResponseDto> CreateAsync(CreateStaffMemberDto dto);

    /// <summary>
    /// Gets all staff members
    /// </summary>
    Task<List<StaffMemberResponseDto>> GetAllAsync();

    /// <summary>
    /// Gets a staff member by mecanographic number
    /// </summary>
    Task<StaffMemberResponseDto?> GetByIdAsync(long mecanographicNumber);

    /// <summary>
    /// Gets staff members by status
    /// </summary>
    Task<List<StaffMemberResponseDto>> GetByStatusAsync(HumanResourceStatus status);

    /// <summary>
    /// Searches staff members by name, status, and/or qualification
    /// </summary>
    Task<List<StaffMemberResponseDto>> SearchAsync(string? name, HumanResourceStatus? status, string? qualification);

    /// <summary>
    /// Updates a staff member
    /// </summary>
    Task<StaffMemberResponseDto> UpdateAsync(long mecanographicNumber, UpdateStaffMemberDto dto);

    /// <summary>
    /// Partially updates a staff member (including status changes)
    /// </summary>
    Task<StaffMemberResponseDto> PatchAsync(long mecanographicNumber, PatchStaffStatusDto dto);

    /// <summary>
    /// Adds a qualification to a staff member
    /// </summary>
    Task<StaffMemberResponseDto> AddQualificationAsync(long mecanographicNumber, string qualificationId);

    /// <summary>
    /// Removes a qualification from a staff member
    /// </summary>
    Task RemoveQualificationAsync(long mecanographicNumber, string qualificationId);
}
