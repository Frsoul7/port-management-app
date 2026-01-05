using System;

namespace DDDNetCore.Domain.Shared
{
    public record EntityId(Guid Value)
    {
        public override string ToString() => Value.ToString();
        public static EntityId NewId() => new EntityId(Guid.NewGuid());
    }
}
