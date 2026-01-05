using System;
using System.Linq;
using DDDNetCore.Domain.Resources;
using DDDNetCore.Tests.Resources.Base;
using Xunit;

namespace DDDNetCore.Tests.Resources
{
    /// <summary>
    /// TEST TYPE: Unit Test
    /// ENTITY UNDER TEST: STSCrane (Domain Entity - Physical Resource)
    /// TEST OBJECTIVE: Validate STSCrane entity creation, validation rules, and business logic in isolation.
    ///                 Tests constructor validation, property assignments, dock code normalization,
    ///                 and business constraints (containers per hour, setup time).
    /// </summary>
    public class STSCraneTests : BaseResourceTest
    {
        [Fact]
        public void Constructor_WithValidData_ShouldCreateSTSCrane()
        {
            // Arrange & Act
            var crane = new STSCrane(
                VALID_CODE,
                VALID_DESCRIPTION,
                VALID_SETUP_TIME,
                VALID_CONTAINERS_PER_HOUR,
                VALID_DOCK_CODE
            );

            // Assert
            AssertResourceBasicProperties(crane, VALID_CODE, VALID_DESCRIPTION, VALID_SETUP_TIME);
            Assert.Equal(VALID_CONTAINERS_PER_HOUR, crane.AvgContainersPerHour);
            Assert.Equal(VALID_DOCK_CODE.ToUpperInvariant(), crane.InstalledAtDockCode);
        }

        [Fact]
        public void Constructor_WithoutDockCode_ShouldCreateSTSCrane()
        {
            // Arrange & Act
            var crane = new STSCrane(
                VALID_CODE,
                VALID_DESCRIPTION,
                VALID_SETUP_TIME,
                VALID_CONTAINERS_PER_HOUR
            );

            // Assert
            AssertResourceBasicProperties(crane, VALID_CODE, VALID_DESCRIPTION, VALID_SETUP_TIME);
            Assert.Null(crane.InstalledAtDockCode);
        }

        [Theory]
        [InlineData("")]
        [InlineData("   ")]
        public void Constructor_WithInvalidCode_ShouldThrowException(string invalidCode)
        {
            // Act & Assert
            Assert.Throws<ArgumentException>(() =>
                new STSCrane(invalidCode, VALID_DESCRIPTION, VALID_SETUP_TIME, VALID_CONTAINERS_PER_HOUR)
            );
        }

        [Theory]
        [InlineData(0)]
        [InlineData(-1)]
        [InlineData(-100)]
        public void Constructor_WithInvalidContainersPerHour_ShouldThrowException(int invalidContainers)
        {
            // Act & Assert
            Assert.Throws<ArgumentException>(() =>
                new STSCrane(VALID_CODE, VALID_DESCRIPTION, VALID_SETUP_TIME, invalidContainers)
            );
        }

        [Fact]
        public void Constructor_WithNegativeSetupTime_ShouldThrowException()
        {
            // Act & Assert
            Assert.Throws<ArgumentException>(() =>
                new STSCrane(VALID_CODE, VALID_DESCRIPTION, -1, VALID_CONTAINERS_PER_HOUR)
            );
        }

        [Fact]
        public void Update_WithValidData_ShouldUpdateSTSCrane()
        {
            // Arrange
            var crane = new STSCrane(VALID_CODE, VALID_DESCRIPTION, VALID_SETUP_TIME, VALID_CONTAINERS_PER_HOUR);
            const string newDescription = "Updated Crane";
            const int newSetupTime = 450;
            const int newContainers = 30;
            const string newDockCode = "DOCK-B2";

            // Act
            crane.Update(newDescription, newSetupTime, newContainers, newDockCode);

            // Assert
            Assert.Equal(newDescription, crane.Description);
            Assert.Equal(newSetupTime, crane.SetupTimeSeconds);
            Assert.Equal(newContainers, crane.AvgContainersPerHour);
            Assert.Equal(newDockCode.ToUpperInvariant(), crane.InstalledAtDockCode);
        }

        [Fact]
        public void Update_WithInvalidContainersPerHour_ShouldThrowException()
        {
            // Arrange
            var crane = new STSCrane(VALID_CODE, VALID_DESCRIPTION, VALID_SETUP_TIME, VALID_CONTAINERS_PER_HOUR);

            // Act & Assert
            Assert.Throws<ArgumentException>(() =>
                crane.Update(VALID_DESCRIPTION, VALID_SETUP_TIME, 0, VALID_DOCK_CODE)
            );
        }

        [Fact]
        public void SetRequiredQualifications_ShouldSetQualifications()
        {
            // Arrange
            var crane = new STSCrane(VALID_CODE, VALID_DESCRIPTION, VALID_SETUP_TIME, VALID_CONTAINERS_PER_HOUR);
            var qualifications = CreateQualificationList(3);

            // Act
            crane.SetRequiredQualifications(qualifications);

            // Assert
            Assert.Equal(3, crane.RequiredQualifications.Count);
            Assert.Equal(qualifications[0].QualificationId, crane.RequiredQualifications.First().QualificationId);
        }

        [Fact]
        public void Deactivate_ShouldSetDeactivationDetails()
        {
            // Arrange
            var crane = new STSCrane(VALID_CODE, VALID_DESCRIPTION, VALID_SETUP_TIME, VALID_CONTAINERS_PER_HOUR);
            const string reason = "Scheduled maintenance";

            // Act
            crane.Deactivate(reason);

            // Assert
            Assert.Equal(PhysicalResourceAvailability.TEMP_OUT_OF_SERVICE, crane.Availability);
            Assert.NotNull(crane.DeactivatedAt);
            Assert.Equal(reason, crane.DeactivationReason);
        }

        [Fact]
        public void Activate_ShouldClearDeactivationDetails()
        {
            // Arrange
            var crane = new STSCrane(VALID_CODE, VALID_DESCRIPTION, VALID_SETUP_TIME, VALID_CONTAINERS_PER_HOUR);
            crane.Deactivate("Test reason");

            // Act
            crane.Activate();

            // Assert
            Assert.Equal(PhysicalResourceAvailability.AVAILABLE, crane.Availability);
            Assert.Null(crane.DeactivatedAt);
            Assert.Null(crane.DeactivationReason);
        }

        [Fact]
        public void SetAvailability_ShouldUpdateAvailabilityStatus()
        {
            // Arrange
            var crane = new STSCrane(VALID_CODE, VALID_DESCRIPTION, VALID_SETUP_TIME, VALID_CONTAINERS_PER_HOUR);

            // Act
            crane.SetAvailability(PhysicalResourceAvailability.MAINTENANCE);

            // Assert
            Assert.Equal(PhysicalResourceAvailability.MAINTENANCE, crane.Availability);
        }
    }
}
