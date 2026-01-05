using System;
using System.Threading.Tasks;
using DDDNetCore.Application.Security;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.Logging;

namespace DDDNetCore.Application.Authorization
{
    /// <summary>
    /// Authorization handler for Shipping Agent access.
    /// Validates that:
    /// 1. User has ShippingAgentRep or Admin role
    /// 2. X-Org-Id header is provided (organization context required)
    /// </summary>
    public class ShippingAgentHandler : AuthorizationHandler<ShippingAgentRequirement>
    {
        private readonly IHttpContextAccessor _httpContextAccessor;
        private readonly ILogger<ShippingAgentHandler> _logger;

        public ShippingAgentHandler(
            IHttpContextAccessor httpContextAccessor,
            ILogger<ShippingAgentHandler> logger)
        {
            _httpContextAccessor = httpContextAccessor;
            _logger = logger;
        }

        protected override Task HandleRequirementAsync(
            AuthorizationHandlerContext context, 
            ShippingAgentRequirement requirement)
        {
            var httpContext = _httpContextAccessor.HttpContext;
            if (httpContext == null)
            {
                _logger.LogWarning("Authorization failed: HttpContext is null");
                context.Fail();
                return Task.CompletedTask;
            }

            try
            {
                // Parse caller context from JWT claims and headers
                // requireUserId=false because we'll check it manually with better error handling
                var caller = CallerContextFactory.FromHttpContext(httpContext, requireUserId: false);

                // Check role: Must be ShippingAgentRep or Administrator
                if (caller.Role != AppRole.ShippingAgentRep && 
                    caller.Role != AppRole.Administrator)
                {
                    _logger.LogWarning(
                        "Unauthorized access attempt to ShippingAgent resource. " +
                        "User: {UserId}, Role: {Role}, OrgId: {OrgId}, Endpoint: {Endpoint}, Timestamp: {Timestamp}",
                        caller.UserId, caller.Role, caller.OrgId, 
                        httpContext.Request.Path, DateTime.UtcNow);
                    context.Fail();
                    return Task.CompletedTask;
                }

                // Administrator role bypasses organization requirements
                if (caller.Role == AppRole.Administrator)
                {
                    _logger.LogInformation(
                        "Administrator access granted to ShippingAgent resource. User: {UserId}, Endpoint: {Endpoint}",
                        caller.UserId, httpContext.Request.Path);
                    context.Succeed(requirement);
                    return Task.CompletedTask;
                }

                // For Shipping Agents: Organization ID is required
                if (!caller.OrgId.HasValue)
                {
                    _logger.LogWarning(
                        "Unauthorized access attempt: Missing organization ID. " +
                        "User: {UserId}, Role: {Role}, Endpoint: {Endpoint}, Timestamp: {Timestamp}",
                        caller.UserId, caller.Role, httpContext.Request.Path, DateTime.UtcNow);
                    context.Fail();
                    return Task.CompletedTask;
                }

                // All checks passed
                _logger.LogDebug(
                    "Shipping Agent access granted. User: {UserId}, OrgId: {OrgId}, Endpoint: {Endpoint}",
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

            return Task.CompletedTask;
        }
    }
}
