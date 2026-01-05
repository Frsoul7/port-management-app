using Microsoft.AspNetCore.Authorization;

namespace DDDNetCore.Application.Authorization
{
    /// <summary>
    /// Requirement for Shipping Agent access.
    /// Used for operations that only Shipping Agent Representatives or Admins can perform.
    /// </summary>
    public class ShippingAgentRequirement : IAuthorizationRequirement
    {
    }
}
