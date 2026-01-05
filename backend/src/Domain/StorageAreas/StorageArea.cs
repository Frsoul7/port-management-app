using System;
using System.Collections.Generic;
using DDDNetCore.Domain.Docks;

namespace DDDNetCore.Domain.StorageAreas
{
    public enum StorageAreaType { ORDINARY, YARD, WAREHOUSE }

    public class StorageArea
    {
        public string StorageAreaId { get; private set; } = Guid.NewGuid().ToString();
        public string Name { get; private set; } = null!;
        public string Location { get; private set; } = null!;
        public int MaxCapacityTEU { get; private set; }
        public int CurrentOccupancyTEU { get; private set; }
        public bool ServesAllDocks { get; private set; }
        public StorageAreaType Type { get; private set; }

        public YardSpec? YardSpec { get; private set; }
        public WarehouseSpec? WarehouseSpec { get; private set; }
        public ICollection<Dock> Docks { get; private set; } = new List<Dock>();

        private StorageArea() { }

        /// <summary>
        /// Creates a new storage area.
        /// Type determines if it's ORDINARY (generic), YARD, or WAREHOUSE.
        /// </summary>
        public StorageArea(
            string name,
            string location,
            int maxCapacityTEU,
            StorageAreaType type,
            bool servesAllDocks = true)
        {
            if (string.IsNullOrWhiteSpace(name))
                throw new ArgumentException("Storage area name is required", nameof(name));
            if (string.IsNullOrWhiteSpace(location))
                throw new ArgumentException("Location is required", nameof(location));
            if (maxCapacityTEU <= 0)
                throw new ArgumentException("Max capacity must be positive", nameof(maxCapacityTEU));

            StorageAreaId = Guid.NewGuid().ToString();
            Name = name.Trim();
            Location = location.Trim();
            MaxCapacityTEU = maxCapacityTEU;
            CurrentOccupancyTEU = 0;
            Type = type;
            ServesAllDocks = servesAllDocks;
        }

        /// <summary>
        /// Updates basic properties.
        /// Validates that current occupancy doesn't exceed new max capacity.
        /// </summary>
        public void Update(
            string name,
            string location,
            int maxCapacityTEU,
            bool servesAllDocks)
        {
            if (string.IsNullOrWhiteSpace(name))
                throw new ArgumentException("Storage area name is required", nameof(name));
            if (string.IsNullOrWhiteSpace(location))
                throw new ArgumentException("Location is required", nameof(location));
            if (maxCapacityTEU <= 0)
                throw new ArgumentException("Max capacity must be positive", nameof(maxCapacityTEU));
            if (maxCapacityTEU < CurrentOccupancyTEU)
                throw new InvalidOperationException(
                    $"Cannot reduce max capacity below current occupancy ({CurrentOccupancyTEU} TEU)");

            Name = name.Trim();
            Location = location.Trim();
            MaxCapacityTEU = maxCapacityTEU;
            ServesAllDocks = servesAllDocks;
        }

        /// <summary>
        /// Sets Yard-specific specification (only for YARD type).
        /// </summary>
        public void SetYardSpec(string? notes)
        {
            if (Type != StorageAreaType.YARD)
                throw new InvalidOperationException("YardSpec can only be set for YARD type storage areas");

            YardSpec = new YardSpec { Notes = notes };
            WarehouseSpec = null; // XOR constraint
        }

        /// <summary>
        /// Sets Warehouse-specific specification (only for WAREHOUSE type).
        /// </summary>
        public void SetWarehouseSpec(string? notes)
        {
            if (Type != StorageAreaType.WAREHOUSE)
                throw new InvalidOperationException("WarehouseSpec can only be set for WAREHOUSE type storage areas");

            WarehouseSpec = new WarehouseSpec { Notes = notes };
            YardSpec = null; // XOR constraint
        }

        /// <summary>
        /// Sets which docks this storage area serves (when ServesAllDocks = false).
        /// </summary>
        public void SetServedDocks(IEnumerable<Dock> docks)
        {
            if (ServesAllDocks)
                throw new InvalidOperationException("Cannot set specific docks when ServesAllDocks is true");

            Docks.Clear();
            foreach (var dock in docks)
            {
                Docks.Add(dock);
            }
        }

        /// <summary>
        /// Updates current occupancy. Enforces capacity constraint.
        /// </summary>
        public void UpdateOccupancy(int newOccupancy)
        {
            if (newOccupancy < 0)
                throw new ArgumentException("Occupancy cannot be negative", nameof(newOccupancy));
            if (newOccupancy > MaxCapacityTEU)
                throw new InvalidOperationException(
                    $"Occupancy ({newOccupancy} TEU) cannot exceed max capacity ({MaxCapacityTEU} TEU)");

            CurrentOccupancyTEU = newOccupancy;
        }
    }

    /// <summary>
    /// Yard-specific specification (owned by StorageArea when Type = YARD).
    /// </summary>
    public class YardSpec
    {
        public int YardSpecId { get; private set; }
        public string? Notes { get; set; }
    }

    /// <summary>
    /// Warehouse-specific specification (owned by StorageArea when Type = WAREHOUSE).
    /// </summary>
    public class WarehouseSpec
    {
        public int WarehouseSpecId { get; private set; }
        public string? Notes { get; set; }
    }
}
