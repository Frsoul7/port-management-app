using System;
using System.Collections.Generic;
using Xunit;
using DDDNetCore.Domain.HumanResources;

namespace DDDNetCore.Tests.HumanResources.Base
{
    /// <summary>
    /// Base class for Staff Member tests providing common setup and utilities
    /// </summary>
    public abstract class StaffMemberTestBase : IDisposable
    {
        
        protected List<StaffMemberQualification> CreateDefaultQualifications()
        {
            return new List<StaffMemberQualification>
            {
                new StaffMemberQualification("FORKLIFT", "Forklift Operation", "Basic forklift operation"),
                new StaffMemberQualification("CRANE", "Crane Operation", "Basic crane operation")
            };
        }

        protected StaffMemberQualification CreateQualification(string id, string name, string? description = null)
        {
            return new StaffMemberQualification(id, name, description);
        }

        public void Dispose()
        {
            // Cleanup if needed - currently no resources to dispose
        }
    }
}
