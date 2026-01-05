using System;
using System.Linq;
using DDDNetCore.Domain.Resources;
using DDDNetCore.Tests.Resources.Base;
using Xunit;

namespace DDDNetCore.Tests.Resources
{
    /// <summary>
    /// TEST TYPE: Unit Test
    /// ENTITY UNDER TEST: MobileEquipment (Domain Entity - Physical Resource)
    /// TEST OBJECTIVE: Validate MobileEquipment entity creation, validation rules, and polymorphic behavior.
    ///                 Tests different equipment types (TRUCK, YARD_GANTRY_CRANE, FORKLIFT, REACH_STACKER),
    ///                 validates type-specific attributes (max speed, containers per trip/hour),
    ///                 and verifies dock assignment logic.
    /// </summary>
    public class MobileEquipmentTests : BaseResourceTest
    {
        private const int VALID_MAX_SPEED = 60;
        private const int VALID_CONTAINERS_PER_TRIP = 2;

        [Fact]
        public void Constructor_WithValidTruckData_ShouldCreateMobileEquipment()
        {
            // Arrange & Act
            var truck = new MobileEquipment(
                VALID_CODE,
                VALID_DESCRIPTION,
                VALID_SETUP_TIME,
                MobileEquipmentType.TRUCK,
                VALID_MAX_SPEED,
                VALID_CONTAINERS_PER_TRIP
            );

            // Assert
            AssertResourceBasicProperties(truck, VALID_CODE, VALID_DESCRIPTION, VALID_SETUP_TIME);
            Assert.Equal(MobileEquipmentType.TRUCK, truck.MobileEquipmentType);
            Assert.Equal(VALID_MAX_SPEED, truck.MaxSpeedKph);
            Assert.Equal(VALID_CONTAINERS_PER_TRIP, truck.ContainersPerTrip);
            Assert.Null(truck.AvgContainersPerHour);
        }

        [Fact]
        public void Constructor_WithValidYardCraneData_ShouldCreateMobileEquipment()
        {
            // Arrange & Act
            var crane = new MobileEquipment(
                VALID_CODE,
                VALID_DESCRIPTION,
                VALID_SETUP_TIME,
                MobileEquipmentType.YARD_GANTRY_CRANE,
                avgContainersPerHour: VALID_CONTAINERS_PER_HOUR
            );

            // Assert
            AssertResourceBasicProperties(crane, VALID_CODE, VALID_DESCRIPTION, VALID_SETUP_TIME);
            Assert.Equal(MobileEquipmentType.YARD_GANTRY_CRANE, crane.MobileEquipmentType);
            Assert.Equal(VALID_CONTAINERS_PER_HOUR, crane.AvgContainersPerHour);
            Assert.Null(crane.MaxSpeedKph);
            Assert.Null(crane.ContainersPerTrip);
        }

        [Fact]
        public void Constructor_TruckWithoutMaxSpeed_ShouldThrowException()
        {
            // Act & Assert
            Assert.Throws<ArgumentException>(() =>
                new MobileEquipment(
                    VALID_CODE,
                    VALID_DESCRIPTION,
                    VALID_SETUP_TIME,
                    MobileEquipmentType.TRUCK,
                    containersPerTrip: VALID_CONTAINERS_PER_TRIP
                )
            );
        }

        [Fact]
        public void Constructor_TruckWithoutContainersPerTrip_ShouldThrowException()
        {
            // Act & Assert
            Assert.Throws<ArgumentException>(() =>
                new MobileEquipment(
                    VALID_CODE,
                    VALID_DESCRIPTION,
                    VALID_SETUP_TIME,
                    MobileEquipmentType.TRUCK,
                    maxSpeedKph: VALID_MAX_SPEED
                )
            );
        }

        [Fact]
        public void Constructor_YardCraneWithoutAvgContainers_ShouldThrowException()
        {
            // Act & Assert
            Assert.Throws<ArgumentException>(() =>
                new MobileEquipment(
                    VALID_CODE,
                    VALID_DESCRIPTION,
                    VALID_SETUP_TIME,
                    MobileEquipmentType.YARD_GANTRY_CRANE
                )
            );
        }

        [Theory]
        [InlineData("")]
        [InlineData("   ")]
        public void Constructor_WithInvalidCode_ShouldThrowException(string invalidCode)
        {
            // Act & Assert
            Assert.Throws<ArgumentException>(() =>
                new MobileEquipment(
                    invalidCode,
                    VALID_DESCRIPTION,
                    VALID_SETUP_TIME,
                    MobileEquipmentType.TRUCK,
                    VALID_MAX_SPEED,
                    VALID_CONTAINERS_PER_TRIP
                )
            );
        }

        [Fact]
        public void Update_TruckWithValidData_ShouldUpdateMobileEquipment()
        {
            // Arrange
            var truck = new MobileEquipment(
                VALID_CODE,
                VALID_DESCRIPTION,
                VALID_SETUP_TIME,
                MobileEquipmentType.TRUCK,
                VALID_MAX_SPEED,
                VALID_CONTAINERS_PER_TRIP
            );
            const string newDescription = "Updated Truck";
            const int newSetupTime = 200;
            const int newMaxSpeed = 70;
            const int newContainers = 3;

            // Act
            truck.Update(newDescription, newSetupTime, newMaxSpeed, newContainers, null);

            // Assert
            Assert.Equal(newDescription, truck.Description);
            Assert.Equal(newSetupTime, truck.SetupTimeSeconds);
            Assert.Equal(newMaxSpeed, truck.MaxSpeedKph);
            Assert.Equal(newContainers, truck.ContainersPerTrip);
        }

        [Fact]
        public void Update_YardCraneWithValidData_ShouldUpdateMobileEquipment()
        {
            // Arrange
            var crane = new MobileEquipment(
                VALID_CODE,
                VALID_DESCRIPTION,
                VALID_SETUP_TIME,
                MobileEquipmentType.YARD_GANTRY_CRANE,
                avgContainersPerHour: VALID_CONTAINERS_PER_HOUR
            );
            const int newAvgContainers = 30;

            // Act
            crane.Update(VALID_DESCRIPTION, VALID_SETUP_TIME, null, null, newAvgContainers);

            // Assert
            Assert.Equal(newAvgContainers, crane.AvgContainersPerHour);
        }

        [Fact]
        public void Update_TruckWithoutRequiredFields_ShouldThrowException()
        {
            // Arrange
            var truck = new MobileEquipment(
                VALID_CODE,
                VALID_DESCRIPTION,
                VALID_SETUP_TIME,
                MobileEquipmentType.TRUCK,
                VALID_MAX_SPEED,
                VALID_CONTAINERS_PER_TRIP
            );

            // Act & Assert
            Assert.Throws<ArgumentException>(() =>
                truck.Update(VALID_DESCRIPTION, VALID_SETUP_TIME, null, null, null)
            );
        }

        [Fact]
        public void AllocateToDock_ShouldSetDockCode()
        {
            // Arrange
            var truck = new MobileEquipment(
                VALID_CODE,
                VALID_DESCRIPTION,
                VALID_SETUP_TIME,
                MobileEquipmentType.TRUCK,
                VALID_MAX_SPEED,
                VALID_CONTAINERS_PER_TRIP
            );
            const string dockCode = "DOCK-C3";

            // Act
            truck.AllocateToDock(dockCode);

            // Assert
            Assert.Equal(dockCode.ToUpperInvariant(), truck.CurrentDockCode);
        }

        [Fact]
        public void SetRequiredQualifications_ShouldSetQualifications()
        {
            // Arrange
            var truck = new MobileEquipment(
                VALID_CODE,
                VALID_DESCRIPTION,
                VALID_SETUP_TIME,
                MobileEquipmentType.TRUCK,
                VALID_MAX_SPEED,
                VALID_CONTAINERS_PER_TRIP
            );
            var qualifications = CreateQualificationList(2);

            // Act
            truck.SetRequiredQualifications(qualifications);

            // Assert
            Assert.Equal(2, truck.RequiredQualifications.Count);
            Assert.Equal(qualifications[0].QualificationId, truck.RequiredQualifications.First().QualificationId);
        }

        [Fact]
        public void Deactivate_ShouldSetDeactivationDetails()
        {
            // Arrange
            var truck = new MobileEquipment(
                VALID_CODE,
                VALID_DESCRIPTION,
                VALID_SETUP_TIME,
                MobileEquipmentType.TRUCK,
                VALID_MAX_SPEED,
                VALID_CONTAINERS_PER_TRIP
            );
            const string reason = "Scheduled maintenance";

            // Act
            truck.Deactivate(reason);

            // Assert
            Assert.Equal(PhysicalResourceAvailability.TEMP_OUT_OF_SERVICE, truck.Availability);
            Assert.NotNull(truck.DeactivatedAt);
            Assert.Equal(reason, truck.DeactivationReason);
        }

        [Fact]
        public void Activate_ShouldClearDeactivationDetails()
        {
            // Arrange
            var truck = new MobileEquipment(
                VALID_CODE,
                VALID_DESCRIPTION,
                VALID_SETUP_TIME,
                MobileEquipmentType.TRUCK,
                VALID_MAX_SPEED,
                VALID_CONTAINERS_PER_TRIP
            );
            truck.Deactivate("Test reason");

            // Act
            truck.Activate();

            // Assert
            Assert.Equal(PhysicalResourceAvailability.AVAILABLE, truck.Availability);
            Assert.Null(truck.DeactivatedAt);
            Assert.Null(truck.DeactivationReason);
        }

        [Fact]
        public void SetAvailability_ShouldUpdateAvailabilityStatus()
        {
            // Arrange
            var truck = new MobileEquipment(
                VALID_CODE,
                VALID_DESCRIPTION,
                VALID_SETUP_TIME,
                MobileEquipmentType.TRUCK,
                VALID_MAX_SPEED,
                VALID_CONTAINERS_PER_TRIP
            );

            // Act
            truck.SetAvailability(PhysicalResourceAvailability.MAINTENANCE);

            // Assert
            Assert.Equal(PhysicalResourceAvailability.MAINTENANCE, truck.Availability);
        }
    }
}
