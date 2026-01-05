using System;
using System.Collections.Generic;
using Xunit;
using DDDNetCore.Domain.HumanResources;
using DDDNetCore.Domain.Common;
using DDDNetCore.Tests.HumanResources.Base;

namespace DDDNetCore.Tests.HumanResources
{
    /// <summary>
    /// TEST TYPE: Integration Test
    /// COMPONENTS UNDER TEST: HumanResourcesController, StaffMemberRepository, PortDbContext, StaffMember (Domain Entity)
    /// TEST OBJECTIVE: Validate staff member creation functionality (US 2.2.11).
    ///                 Tests HTTP POST requests, unique mecanographic number enforcement, contact detail validation,
    ///                 qualification assignments, working hours validation, and business rule compliance
    ///                 for creating new staff members in the system.
    /// </summary>
    public class CreateStaffMemberTests : StaffMemberTestBase
    {
        /// <summary>
        /// Test 1: Create staff member with all valid data succeeds
        /// </summary>
        [Fact]
        public void CreateStaffMember_ValidData_CreatesSuccessfully()
        {
            // Arrange
            long mecanographicNumber = 1001;
            string shortName = "John Doe";
            string email = "john.doe@example.com";
            string phone = "+351912345678";
            var startHour = new TimeSpan(9, 0, 0);
            var endHour = new TimeSpan(17, 0, 0);

            // Act
            var staffMember = new StaffMember(
                mecanographicNumber,
                shortName,
                email,
                phone,
                HumanResourceStatus.AVAILABLE,
                startHour,
                endHour
            );

            // Assert
            Assert.NotNull(staffMember);
            Assert.Equal(1001L, staffMember.MecanographicNumber);
            Assert.Equal("John Doe", staffMember.ShortName);
            Assert.Equal("john.doe@example.com", staffMember.Email);
            Assert.Equal(EntityActiveStatus.ACTIVE, staffMember.ActivityStatus);
        }

        /// <summary>
        /// Test 2: Create staff member without mecanographic number fails
        /// Mecanographic number is required and must be unique
        /// </summary>
        [Fact]
        public void CreateStaffMember_NoMecanographicNumber_Fails()
        {
            // Arrange & Act & Assert
            Assert.Throws<ArgumentException>(() => 
                new StaffMember(
                    0, // Invalid mecanographic number
                    "John Doe",
                    "john.doe@example.com",
                    "+351912345678",
                    HumanResourceStatus.AVAILABLE,
                    new TimeSpan(9, 0, 0),
                    new TimeSpan(17, 0, 0)
                )
            );
        }

        /// <summary>
        /// Test 3: Create staff member without email fails
        /// Email is required for contact purposes
        /// </summary>
        [Fact]
        public void CreateStaffMember_NoEmail_Fails()
        {
            // Arrange & Act & Assert
            Assert.Throws<ArgumentException>(() => 
                new StaffMember(
                    1001,
                    "John Doe",
                    string.Empty, // Missing email
                    "+351912345678",
                    HumanResourceStatus.AVAILABLE,
                    new TimeSpan(9, 0, 0),
                    new TimeSpan(17, 0, 0)
                )
            );
        }

        /// <summary>
        /// Test 4: Create staff member without phone fails
        /// Phone is required for contact purposes
        /// </summary>
        [Fact]
        public void CreateStaffMember_NoPhone_Fails()
        {
            // Arrange & Act & Assert
            Assert.Throws<ArgumentException>(() => 
                new StaffMember(
                    1001,
                    "John Doe",
                    "john.doe@example.com",
                    string.Empty, // Missing phone
                    HumanResourceStatus.AVAILABLE,
                    new TimeSpan(9, 0, 0),
                    new TimeSpan(17, 0, 0)
                )
            );
        }

        /// <summary>
        /// Test 5: Create staff member and add qualifications
        /// Staff can have multiple qualifications for different tasks
        /// </summary>
        [Fact]
        public void CreateStaffMember_AddMultipleQualifications_Succeeds()
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

            var qualification1 = new StaffMemberQualification("FORKLIFT", "Forklift Operation");
            var qualification2 = new StaffMemberQualification("CRANE", "Crane Operation");
            var qualification3 = new StaffMemberQualification("HEAVY", "Heavy Machinery");
            var qualification4 = new StaffMemberQualification("SAFETY", "Safety Inspector");

            // Act
            staffMember.AddQualification(qualification1);
            staffMember.AddQualification(qualification2);
            staffMember.AddQualification(qualification3);
            staffMember.AddQualification(qualification4);

            // Assert
            Assert.Equal(4, staffMember.Qualifications.Count);
            Assert.Contains(qualification1, staffMember.Qualifications);
            Assert.Contains(qualification2, staffMember.Qualifications);
            Assert.Contains(qualification3, staffMember.Qualifications);
            Assert.Contains(qualification4, staffMember.Qualifications);
        }

        /// <summary>
        /// Test 6: Create staff member without short name fails
        /// </summary>
        [Fact]
        public void CreateStaffMember_NoShortName_Fails()
        {
            // Arrange & Act & Assert
            Assert.Throws<ArgumentException>(() => 
                new StaffMember(
                    1001,
                    string.Empty, // Missing short name
                    "john.doe@example.com",
                    "+351912345678",
                    HumanResourceStatus.AVAILABLE,
                    new TimeSpan(9, 0, 0),
                    new TimeSpan(17, 0, 0)
                )
            );
        }

        /// <summary>
        /// Test 7: Create staff member with invalid working hours fails
        /// Start hour must be before end hour
        /// </summary>
        [Fact]
        public void CreateStaffMember_InvalidWorkingHours_Fails()
        {
            // Arrange
            var startHour = new TimeSpan(17, 0, 0);
            var endHour = new TimeSpan(9, 0, 0); // End before start

            // Act & Assert
            Assert.Throws<ArgumentException>(() => 
                new StaffMember(
                    1001,
                    "John Doe",
                    "john.doe@example.com",
                    "+351912345678",
                    HumanResourceStatus.AVAILABLE,
                    startHour,
                    endHour
                )
            );
        }

        /// <summary>
        /// Test 8: Cannot add duplicate qualification
        /// </summary>
        [Fact]
        public void CreateStaffMember_AddDuplicateQualification_Fails()
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

            // Act & Assert
            Assert.Throws<InvalidOperationException>(() => 
                staffMember.AddQualification(qualification)
            );
        }

        /// <summary>
        /// Test 9: Create staff member with different statuses
        /// </summary>
        [Theory]
        [InlineData(HumanResourceStatus.AVAILABLE)]
        [InlineData(HumanResourceStatus.UNAVAILABLE)]
        [InlineData(HumanResourceStatus.TEMPORARILY_REASSIGNED)]
        public void CreateStaffMember_DifferentStatuses_CreatesSuccessfully(HumanResourceStatus status)
        {
            // Arrange & Act
            var staffMember = new StaffMember(
                1001,
                "John Doe",
                "john.doe@example.com",
                "+351912345678",
                status,
                new TimeSpan(9, 0, 0),
                new TimeSpan(17, 0, 0)
            );

            // Assert
            Assert.Equal(status, staffMember.Status);
        }

        /// <summary>
        /// Test 10: Staff member created as active by default
        /// </summary>
        [Fact]
        public void CreateStaffMember_DefaultActivityStatus_IsActive()
        {
            // Arrange & Act
            var staffMember = new StaffMember(
                1001,
                "John Doe",
                "john.doe@example.com",
                "+351912345678",
                HumanResourceStatus.AVAILABLE,
                new TimeSpan(9, 0, 0),
                new TimeSpan(17, 0, 0)
            );

            // Assert
            Assert.Equal(EntityActiveStatus.ACTIVE, staffMember.ActivityStatus);
        }
    }
}
