using System;


namespace DDDNetCore.Domain.Organizations
{
    public record RepresentativeId(Guid Value)
    {
        public static RepresentativeId NewId() => new RepresentativeId(Guid.NewGuid());
        public override string ToString() => Value.ToString();
    }
}