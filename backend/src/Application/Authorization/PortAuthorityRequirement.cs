using Microsoft.AspNetCore.Authorization;

namespace DDDNetCore.Application.Authorization
{
    /// <summary>
    /// Requirement for Port Authority access.
    /// Used for operations that only Port Authority Officers or Admins can perform.
    /// </summary>
    public class PortAuthorityRequirement : IAuthorizationRequirement
    {
    }
}
