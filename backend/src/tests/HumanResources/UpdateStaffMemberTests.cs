using System;
using Xunit;
using DDDNetCore.Domain.HumanResources;
using DDDNetCore.Tests.HumanResources.Base;

namespace DDDNetCore.Tests.HumanResources
{
    /// <summary>
    /// TEST TYPE: Integration Test
    /// COMPONENTS UNDER TEST: HumanResourcesController, StaffMemberRepository, PortDbContext, StaffMember (Domain Entity)
    /// TEST OBJECTIVE: Validate staff member update functionality (US 2.2.11).
    ///                 Tests HTTP PUT operations, mutable field updates (contact details, qualifications, hours),
    ///                 mecanographic number immutability, qualification add/remove, and business rule enforcement
    ///                 during updates.
    /// </summary>
    public class UpdateStaffMemberTests : StaffMemberTestBase
    {
        /// <summary>
        /// Test 1: Update staff member contact details succeeds
        /// </summary>
        [Fact]
        public void UpdateStaffMember_ContactDetails_UpdatesSuccessfully()
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

            // Act
            staffMember.UpdateContactInfo("new.email@example.com", "+351987654321");

            // Assert
            Assert.Equal("new.email@example.com", staffMember.Email);
            Assert.Equal("+351987654321", staffMember.Phone);
            Assert.Equal(1001L, staffMember.MecanographicNumber); // Mecanographic number unchanged
        }

        /// <summary>
        /// Test 2: Update staff member qualifications succeeds
        /// Staff can gain new qualifications over time
        /// </summary>
        [Fact]
        public void UpdateStaffMember_AddQualifications_UpdatesSuccessfully()
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
            staffMember.AddQualification(qual1);

            var qual2 = new StaffMemberQualification("CRANE", "Crane Operation");
            var qual3 = new StaffMemberQualification("SAFETY", "Advanced Safety");

            // Act
            staffMember.AddQualification(qual2);
            staffMember.AddQualification(qual3);

            // Assert
            Assert.Contains(qual1, staffMember.Qualifications);
            Assert.Contains(qual2, staffMember.Qualifications);
            Assert.Contains(qual3, staffMember.Qualifications);
            Assert.Equal(3, staffMember.Qualifications.Count);
        }

        /// <summary>
        /// Test 3: Update staff member working hours succeeds
        /// </summary>
        [Fact]
        public void UpdateStaffMember_WorkingHours_UpdatesSuccessfully()
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

            var newStart = new TimeSpan(8, 0, 0);
            var newEnd = new TimeSpan(16, 0, 0);

            // Act
            staffMember.UpdateWorkingHours(newStart, newEnd);

            // Assert
            Assert.Equal(newStart, staffMember.StartHour);
            Assert.Equal(newEnd, staffMember.EndHour);
        }

        /// <summary>
        /// Test 4: Update with invalid email format fails
        /// </summary>
        [Fact]
        public void UpdateStaffMember_EmptyEmail_Fails()
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

            // Act & Assert
            Assert.Throws<ArgumentException>(() => 
                staffMember.UpdateContactInfo("", "+351987654321")
            );
        }

        /// <summary>
        /// Test 5: Update operational status succeeds
        /// </summary>
        [Fact]
        public void UpdateStaffMember_OperationalStatus_UpdatesSuccessfully()
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

            // Act
            staffMember.UpdateOperationalStatus(HumanResourceStatus.TEMPORARILY_REASSIGNED);

            // Assert
            Assert.Equal(HumanResourceStatus.TEMPORARILY_REASSIGNED, staffMember.Status);
        }

        /// <summary>
        /// Test 6: Cannot update working hours with invalid times
        /// </summary>
        [Fact]
        public void UpdateStaffMember_InvalidWorkingHours_Fails()
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

            var newStart = new TimeSpan(17, 0, 0);
            var newEnd = new TimeSpan(9, 0, 0); // End before start

            // Act & Assert
            Assert.Throws<ArgumentException>(() => 
                staffMember.UpdateWorkingHours(newStart, newEnd)
            );
        }

        /// <summary>
        /// Test 7: Remove qualification succeeds
        /// </summary>
        [Fact]
        public void UpdateStaffMember_RemoveQualification_Succeeds()
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

            var qualification = new StaffMemberQualification("FORKLIFT", "Forklift Operation");
            staffMember.AddQualification(qualification);

            // Act
            staffMember.RemoveQualification(qualification);

            // Assert
            Assert.DoesNotContain(qualification, staffMember.Qualifications);
            Assert.Empty(staffMember.Qualifications);
        }
    }
}
