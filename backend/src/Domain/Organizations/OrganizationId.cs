using System;

namespace DDDNetCore.Domain.Organizations
{
    public record OrganizationId(Guid Value)
    {
        public static OrganizationId NewId() => new OrganizationId(Guid.NewGuid());
        public override string ToString() => Value.ToString();
    }
}
