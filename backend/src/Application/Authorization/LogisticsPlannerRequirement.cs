using Microsoft.AspNetCore.Authorization;

namespace DDDNetCore.Application.Authorization
{
    /// <summary>
    /// Authorization requirement for LogisticsPlanner role.
    /// Allows read-only access to resources across all organizations for planning purposes.
    /// </summary>
    public class LogisticsPlannerRequirement : IAuthorizationRequirement
    {
        // No specific properties needed - just validates role
    }
}
