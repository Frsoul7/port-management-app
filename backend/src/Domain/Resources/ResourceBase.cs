using System;

namespace DDDNetCore.Domain.Resources
{
    public abstract class Resource
    {
        public string ResourceId { get; protected set; } = Guid.NewGuid().ToString();
    }
}
