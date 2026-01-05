using System;
using DDDNetCore.Domain.Docks;

namespace DDDNetCore.Domain.Resources
{
    public class MobileEquipment : PhysicalResource
    {
        public MobileEquipmentType MobileEquipmentType { get; private set; }
        public int? MaxSpeedKph { get; private set; }
        public int? ContainersPerTrip { get; private set; }
        public int? AvgContainersPerHour { get; private set; }
        public string? CurrentDockCode { get; private set; }
        public Dock? CurrentAllocation { get; private set; }

        private MobileEquipment() { }

        public MobileEquipment(
            string code,
            string? description,
            int setupTimeSeconds,
            MobileEquipmentType type,
            int? maxSpeedKph = null,
            int? containersPerTrip = null,
            int? avgContainersPerHour = null)
            : base(code, description, setupTimeSeconds)
        {
            if (type == MobileEquipmentType.TRUCK && (!maxSpeedKph.HasValue || !containersPerTrip.HasValue))
                throw new ArgumentException("Trucks require MaxSpeedKph and ContainersPerTrip");
            if (type == MobileEquipmentType.YARD_GANTRY_CRANE && !avgContainersPerHour.HasValue)
                throw new ArgumentException("Yard gantry cranes require AvgContainersPerHour");

            MobileEquipmentType = type;
            MaxSpeedKph = maxSpeedKph;
            ContainersPerTrip = containersPerTrip;
            AvgContainersPerHour = avgContainersPerHour;
        }

        public void Update(
            string? description,
            int setupTimeSeconds,
            int? maxSpeedKph,
            int? containersPerTrip,
            int? avgContainersPerHour)
        {
            base.Update(description, setupTimeSeconds);

            if (MobileEquipmentType == MobileEquipmentType.TRUCK && (!maxSpeedKph.HasValue || !containersPerTrip.HasValue))
                throw new ArgumentException("Trucks require MaxSpeedKph and ContainersPerTrip");
            if (MobileEquipmentType == MobileEquipmentType.YARD_GANTRY_CRANE && !avgContainersPerHour.HasValue)
                throw new ArgumentException("Yard gantry cranes require AvgContainersPerHour");

            MaxSpeedKph = maxSpeedKph;
            ContainersPerTrip = containersPerTrip;
            AvgContainersPerHour = avgContainersPerHour;
        }

        public void AllocateToDock(string? dockCode)
        {
            CurrentDockCode = dockCode?.ToUpperInvariant();
        }
    }
}