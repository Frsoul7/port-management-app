using System;
using System.Collections.Generic;
using DDDNetCore.Domain.HumanResources;
using DDDNetCore.Domain.Resources;

namespace DDDNetCore.Tests.Resources.Base
{
    public abstract class BaseResourceTest
    {
        protected const string VALID_CODE = "CRANE001";
        protected const string VALID_DESCRIPTION = "Main STS Crane";
        protected const int VALID_SETUP_TIME = 300;
        protected const int VALID_CONTAINERS_PER_HOUR = 25;
        protected const string VALID_DOCK_CODE = "DOCK-A1";

        protected static StaffMemberQualification CreateQualification(
            string id = "QUAL001",
            string name = "Crane Operator",
            string? description = null)
        {
            return new StaffMemberQualification(id, name, description);
        }

        protected static List<StaffMemberQualification> CreateQualificationList(int count = 2)
        {
            var qualifications = new List<StaffMemberQualification>();
            for (int i = 1; i <= count; i++)
            {
                qualifications.Add(CreateQualification(
                    $"QUAL{i:D3}",
                    $"Qualification {i}",
                    $"Description {i}"
                ));
            }
            return qualifications;
        }

        protected static void AssertResourceBasicProperties(
            PhysicalResource resource,
            string expectedCode,
            string? expectedDescription,
            int expectedSetupTime)
        {
            if (resource == null)
                throw new ArgumentNullException(nameof(resource));

            if (resource.ResourceId == null || resource.ResourceId == Guid.Empty.ToString())
                throw new Exception("ResourceId should be generated");
            
            if (resource.Code != expectedCode.ToUpperInvariant())
                throw new Exception($"Expected Code: {expectedCode.ToUpperInvariant()}, Actual: {resource.Code}");
            
            if (resource.Description != expectedDescription)
                throw new Exception($"Expected Description: {expectedDescription}, Actual: {resource.Description}");
            
            if (resource.SetupTimeSeconds != expectedSetupTime)
                throw new Exception($"Expected SetupTime: {expectedSetupTime}, Actual: {resource.SetupTimeSeconds}");
            
            if (resource.Availability != PhysicalResourceAvailability.AVAILABLE)
                throw new Exception($"Expected Availability: AVAILABLE, Actual: {resource.Availability}");
            
            if (resource.DeactivatedAt != null)
                throw new Exception("DeactivatedAt should be null for new resource");
            
            if (resource.DeactivationReason != null)
                throw new Exception("DeactivationReason should be null for new resource");
        }
    }
}
