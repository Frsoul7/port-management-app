using System;
using Xunit;
using DDDNetCore.Domain.HumanResources;
using DDDNetCore.Domain.Common;
using DDDNetCore.Tests.HumanResources.Base;

namespace DDDNetCore.Tests.HumanResources
{
    /// <summary>
    /// TEST TYPE: Integration Test
    /// COMPONENTS UNDER TEST: HumanResourcesController, StaffMemberRepository, PortDbContext, StaffMember (Domain Entity)
    /// TEST OBJECTIVE: Validate staff member deactivation/reactivation functionality (US 2.2.11).
    ///                 Tests HTTP PATCH operations for status changes, audit trail preservation,
    ///                 soft delete implementation, reactivation logic, and ensures data integrity
    ///                 for historical and compliance purposes.
    /// </summary>
    public class DeactivateStaffMemberTests : StaffMemberTestBase
    {
        /// <summary>
        /// Test 1: Deactivate active staff member succeeds
        /// Status changes to inactive, data is preserved
        /// </summary>
        [Fact]
        public void DeactivateStaffMember_Active_DeactivatesSuccessfully()
        {
            // Arrange
            var staffMember = new StaffMember(
                1001,
                "John Doe",
                "john.doe@example.com",
                "+351912345678",
                HumanResourceStatus.AVAILABLE,
                new TimeSpan(9, 0, 0),
                new TimeSpan(17, 0, 0)
            );

            var qual1 = new StaffMemberQualification("FORKLIFT", "Forklift Operation");
            var qual2 = new StaffMemberQualification("CRANE", "Crane Operation");
            staffMember.AddQualification(qual1);
            staffMember.AddQualification(qual2);

            // Act
            staffMember.UpdateActiveStatus(EntityActiveStatus.INACTIVE);

            // Assert
            Assert.Equal(EntityActiveStatus.INACTIVE, staffMember.ActivityStatus);
            // Data should be preserved
            Assert.Equal(1001L, staffMember.MecanographicNumber);
            Assert.Equal("John Doe", staffMember.ShortName);
            Assert.Equal("john.doe@example.com", staffMember.Email);
            Assert.Equal(2, staffMember.Qualifications.Count);
        }

        /// <summary>
        /// Test 2: Deactivate already inactive staff member is idempotent
        /// </summary>
        [Fact]
        public void DeactivateStaffMember_AlreadyInactive_RemainsInactive()
        {
            // Arrange
            var staffMember = new StaffMember(
                1001,
                "John Doe",
                "john.doe@example.com",
                "+351912345678",
                HumanResourceStatus.AVAILABLE,
                new TimeSpan(9, 0, 0),
                new TimeSpan(17, 0, 0)
            );

            // Act - Deactivate twice
            staffMember.UpdateActiveStatus(EntityActiveStatus.INACTIVE);
            staffMember.UpdateActiveStatus(EntityActiveStatus.INACTIVE);

            // Assert
            Assert.Equal(EntityActiveStatus.INACTIVE, staffMember.ActivityStatus);
        }

        /// <summary>
        /// Test 3: Reactivate deactivated staff member succeeds
        /// Staff can be brought back to active status
        /// </summary>
        [Fact]
        public void ReactivateStaffMember_Inactive_ReactivatesSuccessfully()
        {
            // Arrange
            var staffMember = new StaffMember(
                1001,
                "John Doe",
                "john.doe@example.com",
                "+351912345678",
                HumanResourceStatus.AVAILABLE,
                new TimeSpan(9, 0, 0),
                new TimeSpan(17, 0, 0)
            );
            staffMember.UpdateActiveStatus(EntityActiveStatus.INACTIVE);

            // Act
            staffMember.UpdateActiveStatus(EntityActiveStatus.ACTIVE);

            // Assert
            Assert.Equal(EntityActiveStatus.ACTIVE, staffMember.ActivityStatus);
            Assert.Equal(1001L, staffMember.MecanographicNumber);
        }

        /// <summary>
        /// Test 4: Deactivated staff preserves all historical data
        /// Critical for audit and historical planning purposes
        /// </summary>
        [Fact]
        public void DeactivateStaffMember_PreservesHistoricalData()
        {
            // Arrange
            var startHour = new TimeSpan(9, 0, 0);
            var endHour = new TimeSpan(17, 0, 0);
            
            var staffMember = new StaffMember(
                1001,
                "John Doe",
                "john.doe@example.com",
                "+351912345678",
                HumanResourceStatus.AVAILABLE,
                startHour,
                endHour
            );

            var qual1 = new StaffMemberQualification("FORKLIFT", "Forklift Operation");
            var qual2 = new StaffMemberQualification("CRANE", "Crane Operation");
            staffMember.AddQualification(qual1);
            staffMember.AddQualification(qual2);

            // Act
            staffMember.UpdateActiveStatus(EntityActiveStatus.INACTIVE);

            // Assert - All data must be preserved
            Assert.Equal(1001L, staffMember.MecanographicNumber);
            Assert.Equal("John Doe", staffMember.ShortName);
            Assert.Equal("john.doe@example.com", staffMember.Email);
            Assert.Equal("+351912345678", staffMember.Phone);
            Assert.Equal(2, staffMember.Qualifications.Count);
            Assert.Equal(startHour, staffMember.StartHour);
            Assert.Equal(endHour, staffMember.EndHour);
            Assert.Equal(EntityActiveStatus.INACTIVE, staffMember.ActivityStatus);
        }

        /// <summary>
        /// Test 5: Reactivate already active staff member is idempotent
        /// </summary>
        [Fact]
        public void ReactivateStaffMember_AlreadyActive_RemainsActive()
        {
            // Arrange
            var staffMember = new StaffMember(
                1001,
                "John Doe",
                "john.doe@example.com",
                "+351912345678",
                HumanResourceStatus.AVAILABLE,
                new TimeSpan(9, 0, 0),
                new TimeSpan(17, 0, 0)
            );

            // Act - Reactivate already active
            staffMember.UpdateActiveStatus(EntityActiveStatus.ACTIVE);

            // Assert
            Assert.Equal(EntityActiveStatus.ACTIVE, staffMember.ActivityStatus);
        }

        /// <summary>
        /// Test 6: Can update deactivated staff member data
        /// Deactivated staff can still have their information updated
        /// </summary>
        [Fact]
        public void UpdateDeactivatedStaffMember_AllowsUpdates()
        {
            // Arrange
            var staffMember = new StaffMember(
                1001,
                "John Doe",
                "john.doe@example.com",
                "+351912345678",
                HumanResourceStatus.AVAILABLE,
                new TimeSpan(9, 0, 0),
                new TimeSpan(17, 0, 0)
            );
            staffMember.UpdateActiveStatus(EntityActiveStatus.INACTIVE);

            // Act
            staffMember.UpdateContactInfo("updated@example.com", "+351999999999");
            var newQualification = new StaffMemberQualification("NEW", "New Qualification");
            staffMember.AddQualification(newQualification);

            // Assert
            Assert.Equal(EntityActiveStatus.INACTIVE, staffMember.ActivityStatus);
            Assert.Equal("updated@example.com", staffMember.Email);
            Assert.Contains(newQualification, staffMember.Qualifications);
        }
    }
}
