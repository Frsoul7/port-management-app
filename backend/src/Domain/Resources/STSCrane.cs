using System;
using DDDNetCore.Domain.Docks;

namespace DDDNetCore.Domain.Resources
{
    public class STSCrane : PhysicalResource
    {
        public int AvgContainersPerHour { get; private set; }
        public string? InstalledAtDockCode { get; private set; }
        public Dock? InstalledAt { get; private set; }

        private STSCrane() { }

        public STSCrane(
            string code,
            string? description,
            int setupTimeSeconds,
            int avgContainersPerHour,
            string? installedAtDockCode = null)
            : base(code, description, setupTimeSeconds)
        {
            if (avgContainersPerHour <= 0)
                throw new ArgumentException("Average containers per hour must be positive", nameof(avgContainersPerHour));

            AvgContainersPerHour = avgContainersPerHour;
            InstalledAtDockCode = installedAtDockCode?.ToUpperInvariant();
        }

        public void Update(
            string? description,
            int setupTimeSeconds,
            int avgContainersPerHour,
            string? installedAtDockCode)
        {
            base.Update(description, setupTimeSeconds);

            if (avgContainersPerHour <= 0)
                throw new ArgumentException("Average containers per hour must be positive", nameof(avgContainersPerHour));

            AvgContainersPerHour = avgContainersPerHour;
            InstalledAtDockCode = installedAtDockCode?.ToUpperInvariant();
        }
    }
}
