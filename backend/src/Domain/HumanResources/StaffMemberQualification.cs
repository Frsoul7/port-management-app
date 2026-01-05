using System;

namespace DDDNetCore.Domain.HumanResources
{
    public class StaffMemberQualification
    {
        public string QualificationId { get; private set; } = default!; 
        public string Name { get; private set; } = default!;
        public string? Description { get; private set; }

        private StaffMemberQualification() { }

        public StaffMemberQualification(string qualificationId, string name, string? description = null)
        {
            if (string.IsNullOrWhiteSpace(qualificationId)) 
                throw new ArgumentException("Qualification ID is required", nameof(qualificationId));
            if (string.IsNullOrWhiteSpace(name)) 
                throw new ArgumentException("Qualification name is required", nameof(name));

            QualificationId = qualificationId.Trim().ToUpperInvariant();
            Name = name.Trim();
            Description = description?.Trim();
        }

        public void Update(string name, string? description = null)
        {
            if (string.IsNullOrWhiteSpace(name)) 
                throw new ArgumentException("Qualification name is required", nameof(name));

            Name = name.Trim();
            Description = description?.Trim();
        }
    }
}