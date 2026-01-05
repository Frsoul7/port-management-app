using System;

namespace DDDNetCore.Domain.Users
{
    public record UserId(Guid Value)
    {
        public static UserId NewId() => new UserId(Guid.NewGuid());
        public override string ToString() => Value.ToString();
    }
}
