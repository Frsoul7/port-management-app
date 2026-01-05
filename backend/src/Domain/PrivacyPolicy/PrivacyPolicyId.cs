using System;

namespace DDDNetCore.Domain.PrivacyPolicy
{
    /// <summary>
    /// Strongly-typed ID for PrivacyPolicy aggregate.
    /// </summary>
    public record PrivacyPolicyId(Guid Value)
    {
        public static PrivacyPolicyId NewId() => new PrivacyPolicyId(Guid.NewGuid());
        
        public static PrivacyPolicyId FromString(string value) => new PrivacyPolicyId(Guid.Parse(value));
        
        public override string ToString() => Value.ToString();
    }
}
