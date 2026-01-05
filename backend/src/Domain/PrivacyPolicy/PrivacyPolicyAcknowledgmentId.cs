using System;

namespace DDDNetCore.Domain.PrivacyPolicy
{
    /// <summary>
    /// Strongly-typed ID for PrivacyPolicyAcknowledgment entity.
    /// </summary>
    public record PrivacyPolicyAcknowledgmentId(Guid Value)
    {
        public static PrivacyPolicyAcknowledgmentId NewId() => new PrivacyPolicyAcknowledgmentId(Guid.NewGuid());
        
        public static PrivacyPolicyAcknowledgmentId FromString(string value) => new PrivacyPolicyAcknowledgmentId(Guid.Parse(value));
        
        public override string ToString() => Value.ToString();
    }
}
