using DDDNetCore.Application.DTOs.Users;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace DDDNetCore.Application.Interfaces
{
    public interface IUserService
    {
        Task<List<UserDto>> GetAllUsersAsync();
        Task<UserDto?> GetUserByIdAsync(string userId);
        Task<UserDto> UpdateUserAsync(string userId, UpdateUserDto dto);
        Task<AdminActivateUserResponse> AdminActivateUserAsync(AdminActivateUserRequest request);
    }
}
