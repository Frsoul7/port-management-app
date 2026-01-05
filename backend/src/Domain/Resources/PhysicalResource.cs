using System;
using System.Collections.Generic;
using DDDNetCore.Domain.HumanResources;

namespace DDDNetCore.Domain.Resources
{
    public enum PhysicalResourceAvailability 
    { 
        AVAILABLE, 
        MAINTENANCE, 
        TEMP_OUT_OF_SERVICE 
    }

    public enum ResourceType
    {
        STS_CRANE,
        MOBILE_EQUIPMENT
    }

    public enum MobileEquipmentType 
    { 
        TRUCK, 
        YARD_GANTRY_CRANE 
    }

    public abstract class PhysicalResource
    {
        public string ResourceId { get; protected set; } = Guid.NewGuid().ToString();
        public string Code { get; protected set; } = null!;
        public string? Description { get; protected set; }
        public PhysicalResourceAvailability Availability { get; protected set; }
        public int SetupTimeSeconds { get; protected set; }
        public DateTime CreatedAt { get; protected set; }
        public DateTime? DeactivatedAt { get; protected set; }
        public string? DeactivationReason { get; protected set; }

        protected readonly List<StaffMemberQualification> _requiredQualifications = new();
        public IReadOnlyCollection<StaffMemberQualification> RequiredQualifications => _requiredQualifications.AsReadOnly();

        protected PhysicalResource() { }

        protected PhysicalResource(
            string code,
            string? description,
            int setupTimeSeconds)
        {
            if (string.IsNullOrWhiteSpace(code))
                throw new ArgumentException("Resource code is required", nameof(code));
            if (setupTimeSeconds < 0)
                throw new ArgumentException("Setup time cannot be negative", nameof(setupTimeSeconds));

            Code = code.Trim().ToUpperInvariant();
            Description = description?.Trim();
            SetupTimeSeconds = setupTimeSeconds;
            Availability = PhysicalResourceAvailability.AVAILABLE;
            CreatedAt = DateTime.UtcNow;
        }

        public virtual void Update(string? description, int setupTimeSeconds)
        {
            if (setupTimeSeconds < 0)
                throw new ArgumentException("Setup time cannot be negative", nameof(setupTimeSeconds));

            Description = description?.Trim();
            SetupTimeSeconds = setupTimeSeconds;
        }

        public void SetAvailability(PhysicalResourceAvailability availability)
        {
            Availability = availability;
        }

        public void Deactivate(string? reason)
        {
            if (Availability == PhysicalResourceAvailability.TEMP_OUT_OF_SERVICE && DeactivatedAt != null)
                return; // already deactivated

            Availability = PhysicalResourceAvailability.TEMP_OUT_OF_SERVICE;
            DeactivatedAt = DateTime.UtcNow;
            DeactivationReason = reason?.Trim();
        }

        public void Activate()
        {
            if (Availability != PhysicalResourceAvailability.TEMP_OUT_OF_SERVICE)
                return; // already active or in maintenance

            Availability = PhysicalResourceAvailability.AVAILABLE;
            DeactivatedAt = null;
            DeactivationReason = null;
        }

        public void SetRequiredQualifications(IEnumerable<StaffMemberQualification> qualifications)
        {
            _requiredQualifications.Clear();
            if (qualifications != null)
            {
                _requiredQualifications.AddRange(qualifications);
            }
        }
    }
}
