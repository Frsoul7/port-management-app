using DDDNetCore.Application.DTOs.Users;
using DDDNetCore.Application.Interfaces;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;
using System;
using System.Threading.Tasks;

namespace DDDNetCore.Presentation.Controllers
{
    [ApiController]
    [Route("api/[controller]")]
    [Authorize] // Require authentication
    public class UsersController : ControllerBase
    {
        private readonly IUserService _userService;

        public UsersController(IUserService userService)
        {
            _userService = userService;
        }

        /// <summary>
        /// Get all users (Admin only)
        /// </summary>
        [HttpGet]
        public async Task<IActionResult> GetAll()
        {
            // Only administrators can view all users
            if (!User.IsInRole("ADMINISTRATOR"))
            {
                return Forbid();
            }

            var users = await _userService.GetAllUsersAsync();
            return Ok(users);
        }

        /// <summary>
        /// Get user by ID (Admin only)
        /// </summary>
        [HttpGet("{id}")]
        public async Task<IActionResult> GetById(string id)
        {
            // Only administrators can view user details
            if (!User.IsInRole("ADMINISTRATOR"))
            {
                return Forbid();
            }

            var user = await _userService.GetUserByIdAsync(id);
            if (user == null)
            {
                return NotFound();
            }

            return Ok(user);
        }

        /// <summary>
        /// Update user (Admin only)
        /// </summary>
        [HttpPut("{id}")]
        public async Task<IActionResult> Update(string id, [FromBody] UpdateUserDto dto)
        {
            // Only administrators can update users
            if (!User.IsInRole("ADMINISTRATOR"))
            {
                return Forbid();
            }

            try
            {
                var user = await _userService.UpdateUserAsync(id, dto);
                return Ok(user);
            }
            catch (ArgumentException ex)
            {
                return BadRequest(new { error = ex.Message });
            }
            catch (Exception ex) when (ex is KeyNotFoundException)
            {
                return NotFound(new { error = ex.Message });
            }
        }

        /// <summary>
        /// Activate user and assign role (Admin only) - Sends email notification
        /// </summary>
        [HttpPost("activate")]
        public async Task<IActionResult> AdminActivateUser([FromBody] AdminActivateUserRequest request)
        {
            // Only administrators can activate users
            if (!User.IsInRole("ADMINISTRATOR"))
            {
                return Forbid();
            }

            var result = await _userService.AdminActivateUserAsync(request);
            
            if (!result.Success)
            {
                return BadRequest(new { error = result.Message });
            }

            return Ok(result);
        }
    }
}
