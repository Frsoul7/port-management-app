using DDDNetCore.Application.DTOs.Users;
using DDDNetCore.Application.Interfaces;
using DDDNetCore.Domain.Organizations;
using DDDNetCore.Domain.Users;
using Microsoft.Extensions.Logging;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace DDDNetCore.Application.Services
{
    public class UserService : IUserService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly ILogger<UserService> _logger;
        private readonly IEmailService _emailService;

        public UserService(IUnitOfWork unitOfWork, ILogger<UserService> logger, IEmailService emailService)
        {
            _unitOfWork = unitOfWork;
            _logger = logger;
            _emailService = emailService;
        }

        public async Task<List<UserDto>> GetAllUsersAsync()
        {
            var users = await _unitOfWork.Users.GetAllAsync();
            
            return users.Select(u => new UserDto(
                UserId: u.UserId.Value.ToString(),
                Name: u.Name,
                Email: u.Email,
                OrganizationId: u.OrganizationId.Value.ToString(),
                OrganizationName: u.Organization?.LegalName ?? "Unknown",
                Role: u.Role?.ToString() ?? "None",
                ProfilePictureUrl: u.ProfilePictureUrl,
                EmailVerified: u.EmailVerified,
                IsActive: u.IsActive
            )).ToList();
        }

        public async Task<UserDto?> GetUserByIdAsync(string userId)
        {
            if (!Guid.TryParse(userId, out var guid))
            {
                return null;
            }

            var user = await _unitOfWork.Users.GetByIdAsync(new UserId(guid));
            if (user == null)
            {
                return null;
            }

            return new UserDto(
                UserId: user.UserId.Value.ToString(),
                Name: user.Name,
                Email: user.Email,
                OrganizationId: user.OrganizationId.Value.ToString(),
                OrganizationName: user.Organization?.LegalName ?? "Unknown",
                Role: user.Role?.ToString() ?? "None",
                ProfilePictureUrl: user.ProfilePictureUrl,
                EmailVerified: user.EmailVerified,
                IsActive: user.IsActive
            );
        }

        public async Task<UserDto> UpdateUserAsync(string userId, UpdateUserDto dto)
        {
            if (!Guid.TryParse(userId, out var userGuid))
            {
                throw new ArgumentException("Invalid user ID format");
            }

            if (!Guid.TryParse(dto.OrganizationId, out var orgGuid))
            {
                throw new ArgumentException("Invalid organization ID format");
            }

            // Parse and validate role
            if (!Enum.TryParse<UserRole>(dto.Role, out var role))
            {
                throw new ArgumentException($"Invalid role: {dto.Role}");
            }

            var user = await _unitOfWork.Users.GetByIdAsync(new UserId(userGuid));
            if (user == null)
            {
                throw new KeyNotFoundException($"User with ID {userId} not found");
            }

            // Verify organization exists
            var organization = await _unitOfWork.Organizations.GetByIdAsync(dto.OrganizationId);
            if (organization == null)
            {
                throw new ArgumentException($"Organization with ID {dto.OrganizationId} not found");
            }

            // Update user
            user.ChangeName(dto.Name);
            user.ChangeOrganization(new OrganizationId(orgGuid));
            user.ChangeRole(role);

            await _unitOfWork.Users.UpdateAsync(user);
            await _unitOfWork.CommitAsync();

            _logger.LogInformation("User {UserId} updated: Name={Name}, OrganizationId={OrgId}, Role={Role}", 
                userId, dto.Name, dto.OrganizationId, dto.Role);

            // Fetch updated user with organization
            var updatedUser = await _unitOfWork.Users.GetByIdAsync(new UserId(userGuid));
            
            return new UserDto(
                UserId: updatedUser!.UserId.Value.ToString(),
                Name: updatedUser.Name,
                Email: updatedUser.Email,
                OrganizationId: updatedUser.OrganizationId.Value.ToString(),
                OrganizationName: updatedUser.Organization?.LegalName ?? "Unknown",
                Role: updatedUser.Role?.ToString() ?? "None",
                ProfilePictureUrl: updatedUser.ProfilePictureUrl,
                EmailVerified: updatedUser.EmailVerified,
                IsActive: updatedUser.IsActive
            );
        }

        public async Task<AdminActivateUserResponse> AdminActivateUserAsync(AdminActivateUserRequest request)
        {
            try
            {
                if (!Guid.TryParse(request.UserId, out var userGuid))
                {
                    return new AdminActivateUserResponse(
                        Success: false,
                        Message: "Invalid user ID format"
                    );
                }

                var user = await _unitOfWork.Users.GetByIdAsync(new UserId(userGuid));
                if (user == null)
                {
                    return new AdminActivateUserResponse(
                        Success: false,
                        Message: $"User with ID {request.UserId} not found"
                    );
                }

                // Check if email is verified
                if (!user.EmailVerified)
                {
                    return new AdminActivateUserResponse(
                        Success: false,
                        Message: "User must verify their email before activation"
                    );
                }

                // Parse and validate role
                if (!Enum.TryParse<UserRole>(request.Role, out var role))
                {
                    return new AdminActivateUserResponse(
                        Success: false,
                        Message: $"Invalid role: {request.Role}"
                    );
                }

                // Assign role
                user.AssignRole(role);
                _logger.LogInformation("Admin assigned role {Role} to user {UserId}", role, request.UserId);

                // Activate or deactivate user
                if (request.Activate)
                {
                    user.Activate();
                    _logger.LogInformation("Admin activated user {UserId}", request.UserId);
                }
                else
                {
                    user.Deactivate();
                    _logger.LogInformation("Admin deactivated user {UserId}", request.UserId);
                }

                await _unitOfWork.Users.UpdateAsync(user);
                await _unitOfWork.CommitAsync();

                // Send email notification if activating
                if (request.Activate)
                {
                    try
                    {
                        var subject = "Your Account Has Been Activated";
                        var body = $@"
                            <h2>Welcome to the Port Management System!</h2>
                            <p>Dear {user.Name},</p>
                            <p>Great news! An administrator has reviewed and activated your account.</p>
                            <p><strong>Your assigned role:</strong> {role}</p>
                            <p>You can now log in and access the system with your Google account.</p>
                            <p>If you have any questions, please contact the administrator.</p>
                            <br>
                            <p>Best regards,<br>Port Management Team</p>
                        ";

                        await _emailService.SendEmailAsync(user.Email, subject, body);
                        _logger.LogInformation("Activation email sent to {Email}", user.Email);
                    }
                    catch (Exception emailEx)
                    {
                        _logger.LogError(emailEx, "Failed to send activation email to {Email}", user.Email);
                        // Don't fail the activation if email fails
                    }
                }

                // Return updated user
                var updatedUser = await _unitOfWork.Users.GetByIdAsync(new UserId(userGuid));
                return new AdminActivateUserResponse(
                    Success: true,
                    Message: request.Activate 
                        ? "User successfully activated and notified via email"
                        : "User successfully deactivated",
                    User: new UserDto(
                        UserId: updatedUser!.UserId.Value.ToString(),
                        Name: updatedUser.Name,
                        Email: updatedUser.Email,
                        OrganizationId: updatedUser.OrganizationId.Value.ToString(),
                        OrganizationName: updatedUser.Organization?.LegalName ?? "Unknown",
                        Role: updatedUser.Role?.ToString() ?? "None",
                        ProfilePictureUrl: updatedUser.ProfilePictureUrl,
                        EmailVerified: updatedUser.EmailVerified,
                        IsActive: updatedUser.IsActive
                    )
                );
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Error during admin activation of user {UserId}", request.UserId);
                return new AdminActivateUserResponse(
                    Success: false,
                    Message: $"An error occurred: {ex.Message}"
                );
            }
        }
    }
}
