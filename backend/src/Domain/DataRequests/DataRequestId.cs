using System;

namespace DDDNetCore.Domain.DataRequests
{
    /// <summary>
    /// Strongly-typed ID for DataRequest aggregate.
    /// US 4.5.4: Non-User Data Rights
    /// </summary>
    public record DataRequestId(Guid Value)
    {
        public static DataRequestId NewId() => new DataRequestId(Guid.NewGuid());
        
        public static DataRequestId FromString(string value) => new DataRequestId(Guid.Parse(value));
        
        public override string ToString() => Value.ToString();
    }
}
