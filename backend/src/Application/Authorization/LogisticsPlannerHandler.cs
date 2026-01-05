using System;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Http;
using DDDNetCore.Application.Security;
using Microsoft.Extensions.Logging;

namespace DDDNetCore.Application.Authorization
{
    /// <summary>
    /// Authorization handler for LogisticsPlanner role.
    /// Validates that the user has LogisticsPlanner or Admin role for read-only operations.
    /// No organization validation required - LogisticsPlanner can view all resources for planning.
    /// </summary>
    public class LogisticsPlannerHandler : AuthorizationHandler<LogisticsPlannerRequirement>
    {
        private readonly IHttpContextAccessor _httpContextAccessor;
        private readonly ILogger<LogisticsPlannerHandler> _logger;

        public LogisticsPlannerHandler(
            IHttpContextAccessor httpContextAccessor,
            ILogger<LogisticsPlannerHandler> logger)
        {
            _httpContextAccessor = httpContextAccessor;
            _logger = logger;
        }

        protected override Task HandleRequirementAsync(
            AuthorizationHandlerContext context,
            LogisticsPlannerRequirement requirement)
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
                var caller = CallerContextFactory.FromHttpContext(httpContext, requireUserId: false);

                // Allow LogisticsOperator or Administrator roles
                if (caller.Role == AppRole.LogisticsOperator || 
                    caller.Role == AppRole.Administrator)
                {
                    _logger.LogDebug(
                        "Logistics access granted. User: {UserId}, Role: {Role}, Endpoint: {Endpoint}",
                        caller.UserId, caller.Role, httpContext.Request.Path);
                    context.Succeed(requirement);
                }
                else
                {
                    _logger.LogWarning(
                        "Unauthorized access attempt to LogisticsPlanner resource. " +
                        "User: {UserId}, Role: {Role}, Endpoint: {Endpoint}, Timestamp: {Timestamp}",
                        caller.UserId, caller.Role, httpContext.Request.Path, DateTime.UtcNow);
                    context.Fail();
                }
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
