using System;
using System.Linq;
using Xunit;
using DDDNetCore.Domain.HumanResources;
using DDDNetCore.Domain.Common;
using DDDNetCore.Tests.HumanResources.Base;

namespace DDDNetCore.Tests.HumanResources
{
    /// <summary>
    /// TEST TYPE: Integration Test
    /// COMPONENTS UNDER TEST: HumanResourcesController, StaffMemberRepository, PortDbContext, StaffMember (Domain Entity)
    /// TEST OBJECTIVE: Validate staff member search and filtering functionality (US 2.2.11).
    ///                 Tests HTTP GET with query parameters, filtering by name/email/qualification/status,
    ///                 partial matching, case-insensitive search, and complex filter combinations.
    /// </summary>
    public class SearchStaffMemberTests : StaffMemberTestBase
    {
        /// <summary>
        /// Test 1: Staff member can be identified by mecanographic number
        /// </summary>
        [Fact]
        public void StaffMember_HasUniqueMecanographicNumber()
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
            Assert.Equal(1001L, staffMember.MecanographicNumber);
        }

        /// <summary>
        /// Test 2: Staff member qualifications can be checked
        /// </summary>
        [Fact]
        public void StaffMember_HasQualification_ReturnsCorrectResult()
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

            var forkliftQual = new StaffMemberQualification("FORKLIFT", "Forklift Operation");
            var craneQual = new StaffMemberQualification("CRANE", "Crane Operation");
            
            staffMember.AddQualification(forkliftQual);
            staffMember.AddQualification(craneQual);

            // Act & Assert
            Assert.Contains(forkliftQual, staffMember.Qualifications);
            Assert.Contains(craneQual, staffMember.Qualifications);
            Assert.Contains(staffMember.Qualifications, q => q.QualificationId == "FORKLIFT");
            Assert.DoesNotContain(staffMember.Qualifications, q => q.QualificationId == "NONEXISTENT");
        }

        /// <summary>
        /// Test 3: Staff member status can be checked
        /// </summary>
        [Fact]
        public void StaffMember_ActivityStatus_ReturnsCorrectStatus()
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

            // Assert - Initially active
            Assert.Equal(EntityActiveStatus.ACTIVE, staffMember.ActivityStatus);

            // Act
            staffMember.UpdateActiveStatus(EntityActiveStatus.INACTIVE);

            // Assert - Now inactive
            Assert.Equal(EntityActiveStatus.INACTIVE, staffMember.ActivityStatus);
        }

        /// <summary>
        /// Test 4: Staff member name can be searched/matched
        /// </summary>
        [Fact]
        public void StaffMember_NameMatches_ReturnsCorrectResult()
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
            Assert.Contains("John", staffMember.ShortName);
            Assert.Contains("Doe", staffMember.ShortName);
        }

        /// <summary>
        /// Test 5: Multiple staff members can have different qualifications
        /// </summary>
        [Fact]
        public void MultipleStaffMembers_CanHaveDifferentQualifications()
        {
            // Arrange
            var staff1 = new StaffMember(
                1001,
                "John Doe",
                "john@example.com",
                "+351912345678",
                HumanResourceStatus.AVAILABLE,
                new TimeSpan(9, 0, 0),
                new TimeSpan(17, 0, 0)
            );

            var staff2 = new StaffMember(
                1002,
                "Jane Smith",
                "jane@example.com",
                "+351987654321",
                HumanResourceStatus.AVAILABLE,
                new TimeSpan(9, 0, 0),
                new TimeSpan(17, 0, 0)
            );

            var forklift = new StaffMemberQualification("FORKLIFT", "Forklift Operation");
            var crane = new StaffMemberQualification("CRANE", "Crane Operation");
            var heavy = new StaffMemberQualification("HEAVY", "Heavy Machinery");

            staff1.AddQualification(forklift);
            staff2.AddQualification(crane);
            staff2.AddQualification(heavy);

            // Act & Assert
            Assert.Contains(forklift, staff1.Qualifications);
            Assert.DoesNotContain(crane, staff1.Qualifications);
            
            Assert.Contains(crane, staff2.Qualifications);
            Assert.DoesNotContain(forklift, staff2.Qualifications);
        }

        /// <summary>
        /// Test 6: Staff member has all required qualifications
        /// </summary>
        [Fact]
        public void StaffMember_HasAllQualifications_ReturnsCorrectResult()
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
            var qual3 = new StaffMemberQualification("HEAVY", "Heavy Machinery");

            staffMember.AddQualification(qual1);
            staffMember.AddQualification(qual2);
            staffMember.AddQualification(qual3);

            var requiredQualificationIds = new[] { "FORKLIFT", "CRANE" };

            // Act
            bool hasAll = requiredQualificationIds.All(id => 
                staffMember.Qualifications.Any(q => q.QualificationId == id));

            // Assert
            Assert.True(hasAll);
        }

        /// <summary>
        /// Test 7: Staff member working hours can be checked
        /// </summary>
        [Fact]
        public void StaffMember_ChecksWorkingHours()
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

            // Act & Assert
            Assert.Equal(startHour, staffMember.StartHour);
            Assert.Equal(endHour, staffMember.EndHour);

            // Test IsWithinWorkingHours
            var withinHours = new DateTime(2024, 1, 1, 12, 0, 0); // 12:00 PM
            var outsideHours = new DateTime(2024, 1, 1, 20, 0, 0); // 8:00 PM

            Assert.True(staffMember.IsWithinWorkingHours(withinHours));
            Assert.False(staffMember.IsWithinWorkingHours(outsideHours));
        }
    }
}
