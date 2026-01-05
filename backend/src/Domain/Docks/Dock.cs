using System;
using System.Collections.Generic;
using System.Linq;
using DDDNetCore.Domain.Vessels;
using DDDNetCore.Domain.Resources;
using DDDNetCore.Domain.StorageAreas;

namespace DDDNetCore.Domain.Docks
{
    public class Dock
    {
        public string Code { get; private set; } = null!;
        public string Name { get; private set; } = null!;
        public string Location { get; private set; } = null!;
        public double LengthM { get; private set; }
        public double DepthM { get; private set; }
        public double MaxDraftM { get; private set; }

        public ICollection<VesselType> AllowedVesselTypes { get; private set; } = new List<VesselType>();
        public ICollection<StorageArea> ServedStorageAreas { get; private set; } = new List<StorageArea>();
        public ICollection<MobileEquipment> MobileEquipments { get; private set; } = new List<MobileEquipment>();
        public STSCrane? StsCrane { get; private set; }

        private Dock() { }

        public Dock(string code, string name, string location, double lengthM, double depthM, double maxDraftM)
        {
            if (string.IsNullOrWhiteSpace(code)) throw new ArgumentException("Dock code is required", nameof(code));
            if (string.IsNullOrWhiteSpace(name)) throw new ArgumentException("Dock name is required", nameof(name));
            if (string.IsNullOrWhiteSpace(location)) throw new ArgumentException("Dock location is required", nameof(location));
            if (lengthM <= 0) throw new ArgumentException("Length must be positive", nameof(lengthM));
            if (depthM <= 0) throw new ArgumentException("Depth must be positive", nameof(depthM));
            if (maxDraftM <= 0) throw new ArgumentException("Max draft must be positive", nameof(maxDraftM));

            Code = code.ToUpperInvariant().Trim();
            Name = name.Trim();
            Location = location.Trim();
            LengthM = lengthM;
            DepthM = depthM;
            MaxDraftM = maxDraftM;
        }

        /// <summary>
        /// Updates all mutable properties of the Dock.
        /// The Code (ID) cannot be changed after creation.
        /// </summary>
        public void Update(string name, string location, double lengthM, double depthM, double maxDraftM)
        {
            if (string.IsNullOrWhiteSpace(name)) throw new ArgumentException("Dock name is required", nameof(name));
            if (string.IsNullOrWhiteSpace(location)) throw new ArgumentException("Dock location is required", nameof(location));
            if (lengthM <= 0) throw new ArgumentException("Length must be positive", nameof(lengthM));
            if (depthM <= 0) throw new ArgumentException("Depth must be positive", nameof(depthM));
            if (maxDraftM <= 0) throw new ArgumentException("Max draft must be positive", nameof(maxDraftM));

            Name = name.Trim();
            Location = location.Trim();
            LengthM = lengthM;
            DepthM = depthM;
            MaxDraftM = maxDraftM;
        }

        public void SetAllowedVesselTypes(IEnumerable<VesselType> vesselTypes)
        {
            AllowedVesselTypes.Clear();
            foreach (var vt in vesselTypes)
            {
                AllowedVesselTypes.Add(vt);
            }
        }

        public bool CanAccommodateVesselType(string vesselTypeId)
        {
            return AllowedVesselTypes.Any(vt => vt.VesselTypeId == vesselTypeId);
        }
    }

    /// <summary>
    /// Represents the distance between a dock and a storage area.
    /// Used for future planning/optimization.
    /// </summary>
    public class DockStorageDistance
    {
        public int DockStorageDistanceId { get; private set; }
        public int DistanceMeters { get; private set; }

        public string DockCode { get; private set; } = null!;
        public Dock? Dock { get; private set; }

        public string StorageAreaId { get; private set; } = null!;
        public StorageArea? StorageArea { get; private set; }
    }
}
