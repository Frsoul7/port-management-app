using System;
using System.Linq;
using System.Threading.Tasks;
using DDDNetCore.Application.Security;
using DDDNetCore.Domain.Organizations;
using DDDNetCore.Infrastructure;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Http;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.Logging;

namespace DDDNetCore.Application.Authorization
{
    /// <summary>
    /// Authorization handler for Port Authority access.
    /// Validates that:
    /// 1. User has PortAuthorityOfficer or Admin role
    /// 2. If X-Org-Id is provided, it references a PORT_AUTHORITY organization
    /// Note: Administrator role bypasses organization type checks
    /// </summary>
    public class PortAuthorityHandler : AuthorizationHandler<PortAuthorityRequirement>
    {
        private readonly IHttpContextAccessor _httpContextAccessor;
        private readonly PortDbContext _db;
        private readonly ILogger<PortAuthorityHandler> _logger;

        public PortAuthorityHandler(
            IHttpContextAccessor httpContextAccessor, 
            PortDbContext db,
            ILogger<PortAuthorityHandler> logger)
        {
            _httpContextAccessor = httpContextAccessor;
            _db = db;
            _logger = logger;
        }

        protected override async Task HandleRequirementAsync(
            AuthorizationHandlerContext context, 
            PortAuthorityRequirement requirement)
        {
            var httpContext = _httpContextAccessor.HttpContext;
            if (httpContext == null)
            {
                _logger.LogWarning("Authorization failed: HttpContext is null");
                context.Fail();
                return;
            }

            try
            {
                // Parse caller context from JWT claims and headers
                var caller = CallerContextFactory.FromHttpContext(httpContext, requireUserId: false);

                // Check role: Must be PortAuthorityOfficer or Administrator
                if (caller.Role != AppRole.PortAuthorityOfficer && 
                    caller.Role != AppRole.Administrator)
                {
                    _logger.LogWarning(
                        "Unauthorized access attempt to PortAuthority resource. " +
                        "User: {UserId}, Role: {Role}, OrgId: {OrgId}, Endpoint: {Endpoint}, Timestamp: {Timestamp}",
                        caller.UserId, caller.Role, caller.OrgId, 
                        httpContext.Request.Path, DateTime.UtcNow);
                    context.Fail();
                    return;
                }

                // Administrator role bypasses organization checks
                if (caller.Role == AppRole.Administrator)
                {
                    _logger.LogInformation(
                        "Administrator access granted. User: {UserId}, Endpoint: {Endpoint}",
                        caller.UserId, httpContext.Request.Path);
                    context.Succeed(requirement);
                    return;
                }

                // If X-Org-Id is provided for Port Authority Officer, verify it's a Port Authority organization
                if (caller.OrgId.HasValue)
                {
                    var org = await _db.Organizations.FindAsync(caller.OrgId.Value);
                    if (org == null || org.Type != OrganizationType.PORT_AUTHORITY)
                    {
                        _logger.LogWarning(
                            "Unauthorized access attempt: Invalid organization type. " +
                            "User: {UserId}, Role: {Role}, OrgId: {OrgId}, OrgType: {OrgType}, Endpoint: {Endpoint}, Timestamp: {Timestamp}",
                            caller.UserId, caller.Role, caller.OrgId, org?.Type, 
                            httpContext.Request.Path, DateTime.UtcNow);
                        context.Fail();
                        return;
                    }
                }

                // All checks passed
                _logger.LogDebug(
                    "Port Authority access granted. User: {UserId}, OrgId: {OrgId}, Endpoint: {Endpoint}",
                    caller.UserId, caller.OrgId, httpContext.Request.Path);
                context.Succeed(requirement);
            }
            catch (UnauthorizedAccessException ex)
            {
                _logger.LogWarning(ex,
                    "Unauthorized access attempt: {Message}. Endpoint: {Endpoint}, Timestamp: {Timestamp}",
                    ex.Message, httpContext.Request.Path, DateTime.UtcNow);
                context.Fail();
            }
        }
    }
}
