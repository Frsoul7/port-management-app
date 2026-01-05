using System;
using Microsoft.AspNetCore.Http;

namespace DDDNetCore.Application.Security
{
    public enum AppRole
    {
        Unknown = 0,
        ShippingAgentRep = 1,
        PortAuthorityOfficer = 2,
        LogisticsOperator = 3,
        Administrator = 4
    }

    public sealed class CallerContext
    {
        public Guid UserId { get; }
        public Guid? OrgId { get; }   // <-- Guid now
        public AppRole Role { get; }

        public CallerContext(Guid userId, Guid? orgId, AppRole role)
        {
            UserId = userId;
            OrgId = orgId;
            Role = role;
        }
    }

    public static class CallerContextFactory
    {
        /// <summary>
        /// Creates CallerContext from JWT claims and HTTP headers.
        /// Prefers JWT claims but falls back to X-* headers for backward compatibility.
        /// </summary>
        public static CallerContext FromHttpContext(HttpContext httpContext, bool requireUserId = true)
        {
            var user = httpContext.User;
            
            // Try to get from JWT claims first
            if (user.Identity?.IsAuthenticated == true)
            {
                // Extract user ID from JWT 'sub' claim
                var userIdClaim = user.FindFirst("sub")?.Value;
                Guid userId = Guid.Empty;
                if (!string.IsNullOrWhiteSpace(userIdClaim))
                {
                    Guid.TryParse(userIdClaim, out userId);
                }

                // Extract role from JWT claims
                var roleClaim = user.FindFirst(System.Security.Claims.ClaimTypes.Role)?.Value 
                             ?? user.FindFirst("role")?.Value;
                var role = MapFrontendRoleToAppRole(roleClaim);

                // Try to get organization ID from headers (X-Org-Id) - needed for organization-scoped operations
                Guid? orgId = null;
                var orgRaw = httpContext.Request.Headers["X-Org-Id"].ToString();
                if (!string.IsNullOrWhiteSpace(orgRaw))
                {
                    if (Guid.TryParse(orgRaw, out var parsed))
                    {
                        orgId = parsed;
                    }
                }

                // Validate userId requirement
                if (requireUserId && userId == Guid.Empty)
                {
                    throw new UnauthorizedAccessException("User ID not found in JWT token.");
                }

                return new CallerContext(userId, orgId, role);
            }

            // Fall back to headers for backward compatibility (development/testing)
            return FromHeaders(httpContext.Request.Headers, requireUserId);
        }

        /// <summary>
        /// Maps frontend role names to AppRole enum.
        /// Handles both frontend format (SHIPPING_AGENT_REPRESENTATIVE) and backend format (ShippingAgentRep).
        /// </summary>
        private static AppRole MapFrontendRoleToAppRole(string? roleString)
        {
            if (string.IsNullOrWhiteSpace(roleString))
                return AppRole.Unknown;

            return roleString.Trim().ToUpperInvariant() switch
            {
                "SHIPPING_AGENT_REPRESENTATIVE" => AppRole.ShippingAgentRep,
                "SHIPPINGAGENTREP" => AppRole.ShippingAgentRep,
                "PORT_AUTHORITY_OFFICER" => AppRole.PortAuthorityOfficer,
                "PORTAUTHORITYOFFICER" => AppRole.PortAuthorityOfficer,
                "LOGISTICS_OPERATOR" => AppRole.LogisticsOperator,
                "LOGISTICSOPERATOR" => AppRole.LogisticsOperator,
                "ADMINISTRATOR" => AppRole.Administrator,
                "ADMIN" => AppRole.Administrator,
                // Legacy mappings
                "LOGISTICSPLANNER" => AppRole.LogisticsOperator,
                "LOGISTICS_PLANNER" => AppRole.LogisticsOperator,
                _ => AppRole.Unknown
            };
        }

        /// <summary>
        /// Creates CallerContext from HTTP headers.
        /// For Port Authority: Only X-Role and X-Org-Id are required (user tracking optional)
        /// For Shipping Agents: X-User-Id, X-Org-Id, and X-Role are all required
        /// </summary>
        public static CallerContext FromHeaders(IHeaderDictionary headers, bool requireUserId = true)
        {
            // X-Role (always required)
            var roleRaw = headers["X-Role"].ToString();
            var role = roleRaw?.Trim().ToLowerInvariant() switch
            {
                "shippingagentrep" => AppRole.ShippingAgentRep,
                "portauthorityofficer" => AppRole.PortAuthorityOfficer,
                "logisticsoperator" => AppRole.LogisticsOperator,
                "administrator" => AppRole.Administrator,
                "admin" => AppRole.Administrator,
                // Legacy mappings
                "logisticsplanner" => AppRole.LogisticsOperator,
                _ => AppRole.Unknown
            };

            // X-User-Id (required for Shipping Agents, optional for Port Authority)
            Guid userId = Guid.Empty;
            var userRaw = headers["X-User-Id"].ToString();
            
            if (requireUserId || role == AppRole.ShippingAgentRep)
            {
                if (!Guid.TryParse(userRaw, out userId))
                    throw new UnauthorizedAccessException("Missing or invalid X-User-Id header.");
            }
            else if (!string.IsNullOrWhiteSpace(userRaw))
            {
                // Parse if provided, but don't fail if missing
                Guid.TryParse(userRaw, out userId);
            }

            // X-Org-Id (required for organization-specific operations)
            Guid? orgId = null;
            var orgRaw = headers["X-Org-Id"].ToString();
            if (!string.IsNullOrWhiteSpace(orgRaw))
            {
                if (!Guid.TryParse(orgRaw, out var parsed))
                    throw new UnauthorizedAccessException("Invalid X-Org-Id header (must be a GUID).");
                orgId = parsed;
            }

            return new CallerContext(userId, orgId, role);
        }
    }
}
