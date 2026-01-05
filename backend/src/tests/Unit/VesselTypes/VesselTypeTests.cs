using System;
using Xunit;
using DDDNetCore.Domain.Vessels;

namespace DDDNetCore.Tests.Unit.VesselTypes
{
    /// <summary>
    /// TEST TYPE: Unit Test
    /// ENTITY UNDER TEST: VesselType (Domain Entity)
    /// TEST OBJECTIVE: Validate VesselType entity creation, identifier/name validation,
    ///                 Update() method with capacity/dimension constraints (non-negative values),
    ///                 optional field handling (Description, OperationalConstraints),
    ///                 and whitespace normalization in isolation.
    /// </summary>
    public class VesselTypeTests
    {
        #region Constructor Tests - Valid Cases

        [Fact]
        public void Constructor_WithValidIdAndName_CreatesVesselType()
        {
            // Arrange
            var id = "container-ship";
            var name = "Container Ship";

            // Act
            var vesselType = new VesselType(id, name);

            // Assert
            Assert.NotNull(vesselType);
            Assert.Equal(id, vesselType.VesselTypeId);
            Assert.Equal(name, vesselType.Name);
            Assert.Null(vesselType.Description);
            Assert.Null(vesselType.CapacityTEU);
            Assert.Null(vesselType.MaxRows);
            Assert.Null(vesselType.MaxBays);
            Assert.Null(vesselType.MaxTiers);
            Assert.Null(vesselType.OperationalConstraints);
        }

        [Theory]
        [InlineData("  container-ship  ", "container-ship")]
        [InlineData("bulk-carrier", "bulk-carrier")]
        [InlineData("  tanker  ", "tanker")]
        public void Constructor_WithWhitespaceInId_TrimsId(string inputId, string expectedId)
        {
            // Arrange
            var name = "Test Vessel Type";

            // Act
            var vesselType = new VesselType(inputId, name);

            // Assert
            Assert.Equal(expectedId, vesselType.VesselTypeId);
        }

        [Theory]
        [InlineData("  Container Ship  ", "Container Ship")]
        [InlineData("Bulk Carrier", "Bulk Carrier")]
        [InlineData("  Oil Tanker  ", "Oil Tanker")]
        public void Constructor_WithWhitespaceInName_TrimsName(string inputName, string expectedName)
        {
            // Arrange
            var id = "test-type";

            // Act
            var vesselType = new VesselType(id, inputName);

            // Assert
            Assert.Equal(expectedName, vesselType.Name);
        }

        #endregion

        #region Constructor Tests - Invalid Cases

        [Theory]
        [InlineData("")]
        [InlineData("   ")]
        [InlineData(null)]
        public void Constructor_WithEmptyOrNullId_ThrowsArgumentException(string? invalidId)
        {
            // Arrange
            var name = "Valid Name";

            // Act & Assert
            var exception = Assert.Throws<ArgumentException>(() => new VesselType(invalidId, name));
            Assert.Contains("Id is required", exception.Message);
        }

        [Theory]
        [InlineData("")]
        [InlineData("   ")]
        [InlineData(null)]
        public void Constructor_WithEmptyOrNullName_ThrowsArgumentException(string? invalidName)
        {
            // Arrange
            var id = "valid-id";

            // Act & Assert
            var exception = Assert.Throws<ArgumentException>(() => new VesselType(id, invalidName));
            Assert.Contains("Name is required", exception.Message);
        }

        #endregion

        #region Update Method Tests - Valid Cases

        [Fact]
        public void Update_WithAllValidParameters_UpdatesAllFields()
        {
            // Arrange
            var vesselType = new VesselType("container-ship", "Container Ship");
            var newName = "Large Container Ship";
            var newDescription = "Post-Panamax container vessel";
            var newCapacity = 18000;
            var newMaxRows = 24;
            var newMaxBays = 20;
            var newMaxTiers = 10;
            var newConstraints = "Requires deep water berth";

            // Act
            vesselType.Update(newName, newDescription, newCapacity, newMaxRows, newMaxBays, newMaxTiers, newConstraints);

            // Assert
            Assert.Equal(newName, vesselType.Name);
            Assert.Equal(newDescription, vesselType.Description);
            Assert.Equal(newCapacity, vesselType.CapacityTEU);
            Assert.Equal(newMaxRows, vesselType.MaxRows);
            Assert.Equal(newMaxBays, vesselType.MaxBays);
            Assert.Equal(newMaxTiers, vesselType.MaxTiers);
            Assert.Equal(newConstraints, vesselType.OperationalConstraints);
        }

        [Fact]
        public void Update_WithOptionalFieldsNull_SetsThemToNull()
        {
            // Arrange
            var vesselType = new VesselType("tanker", "Oil Tanker");
            vesselType.Update("Tanker", "Old description", 5000, 10, 8, 5, "Old constraints");

            // Act
            vesselType.Update("Updated Tanker", null, null, null, null, null, null);

            // Assert
            Assert.Equal("Updated Tanker", vesselType.Name);
            Assert.Null(vesselType.Description);
            Assert.Null(vesselType.CapacityTEU);
            Assert.Null(vesselType.MaxRows);
            Assert.Null(vesselType.MaxBays);
            Assert.Null(vesselType.MaxTiers);
            Assert.Null(vesselType.OperationalConstraints);
        }

        [Theory]
        [InlineData("  Updated Name  ", "Updated Name")]
        [InlineData("New Vessel Type", "New Vessel Type")]
        public void Update_WithWhitespaceInName_TrimsName(string inputName, string expectedName)
        {
            // Arrange
            var vesselType = new VesselType("test-type", "Original Name");

            // Act
            vesselType.Update(inputName, null, null, null, null, null, null);

            // Assert
            Assert.Equal(expectedName, vesselType.Name);
        }

        [Theory]
        [InlineData("  Description with spaces  ", "Description with spaces")]
        [InlineData("   ", null)] // Whitespace only becomes null
        [InlineData("", null)] // Empty becomes null
        public void Update_WithWhitespaceInDescription_TrimsOrSetsNull(string inputDescription, string? expectedDescription)
        {
            // Arrange
            var vesselType = new VesselType("test-type", "Test Name");

            // Act
            vesselType.Update("Test Name", inputDescription, null, null, null, null, null);

            // Assert
            Assert.Equal(expectedDescription, vesselType.Description);
        }

        [Theory]
        [InlineData("  Operational constraints  ", "Operational constraints")]
        [InlineData("   ", null)] // Whitespace only becomes null
        [InlineData("", null)] // Empty becomes null
        public void Update_WithWhitespaceInConstraints_TrimsOrSetsNull(string inputConstraints, string? expectedConstraints)
        {
            // Arrange
            var vesselType = new VesselType("test-type", "Test Name");

            // Act
            vesselType.Update("Test Name", null, null, null, null, null, inputConstraints);

            // Assert
            Assert.Equal(expectedConstraints, vesselType.OperationalConstraints);
        }

        [Theory]
        [InlineData(0)] // Zero is valid
        [InlineData(1)] // Positive values
        [InlineData(25000)]
        public void Update_WithValidCapacityTEU_UpdatesCapacity(int validCapacity)
        {
            // Arrange
            var vesselType = new VesselType("test-type", "Test Name");

            // Act
            vesselType.Update("Test Name", null, validCapacity, null, null, null, null);

            // Assert
            Assert.Equal(validCapacity, vesselType.CapacityTEU);
        }

        [Theory]
        [InlineData(0)] // Zero is valid
        [InlineData(10)]
        [InlineData(50)]
        public void Update_WithValidDimensions_UpdatesDimensions(int validDimension)
        {
            // Arrange
            var vesselType = new VesselType("test-type", "Test Name");

            // Act
            vesselType.Update("Test Name", null, null, validDimension, validDimension, validDimension, null);

            // Assert
            Assert.Equal(validDimension, vesselType.MaxRows);
            Assert.Equal(validDimension, vesselType.MaxBays);
            Assert.Equal(validDimension, vesselType.MaxTiers);
        }

        #endregion

        #region Update Method Tests - Invalid Cases

        [Theory]
        [InlineData("")]
        [InlineData("   ")]
        [InlineData(null)]
        public void Update_WithEmptyOrNullName_ThrowsArgumentException(string? invalidName)
        {
            // Arrange
            var vesselType = new VesselType("test-type", "Original Name");

            // Act & Assert
            var exception = Assert.Throws<ArgumentException>(() =>
                vesselType.Update(invalidName, null, null, null, null, null, null));
            Assert.Contains("Name is required", exception.Message);
        }

        [Theory]
        [InlineData(-1)]
        [InlineData(-100)]
        [InlineData(-5000)]
        public void Update_WithNegativeCapacityTEU_ThrowsArgumentException(int negativeCapacity)
        {
            // Arrange
            var vesselType = new VesselType("test-type", "Test Name");

            // Act & Assert
            var exception = Assert.Throws<ArgumentException>(() =>
                vesselType.Update("Test Name", null, negativeCapacity, null, null, null, null));
            Assert.Contains("CapacityTEU cannot be negative", exception.Message);
        }

        [Theory]
        [InlineData(-1)]
        [InlineData(-50)]
        public void Update_WithNegativeMaxRows_ThrowsArgumentException(int negativeRows)
        {
            // Arrange
            var vesselType = new VesselType("test-type", "Test Name");

            // Act & Assert
            var exception = Assert.Throws<ArgumentException>(() =>
                vesselType.Update("Test Name", null, null, negativeRows, null, null, null));
            Assert.Contains("MaxRows cannot be negative", exception.Message);
        }

        [Theory]
        [InlineData(-1)]
        [InlineData(-30)]
        public void Update_WithNegativeMaxBays_ThrowsArgumentException(int negativeBays)
        {
            // Arrange
            var vesselType = new VesselType("test-type", "Test Name");

            // Act & Assert
            var exception = Assert.Throws<ArgumentException>(() =>
                vesselType.Update("Test Name", null, null, null, negativeBays, null, null));
            Assert.Contains("MaxBays cannot be negative", exception.Message);
        }

        [Theory]
        [InlineData(-1)]
        [InlineData(-20)]
        public void Update_WithNegativeMaxTiers_ThrowsArgumentException(int negativeTiers)
        {
            // Arrange
            var vesselType = new VesselType("test-type", "Test Name");

            // Act & Assert
            var exception = Assert.Throws<ArgumentException>(() =>
                vesselType.Update("Test Name", null, null, null, null, negativeTiers, null));
            Assert.Contains("MaxTiers cannot be negative", exception.Message);
        }

        #endregion

        #region Integration Tests

        [Fact]
        public void CreateAndUpdate_FullLifecycle_WorksCorrectly()
        {
            // Arrange & Act - Create
            var vesselType = new VesselType("container-xl", "Extra Large Container Ship");

            // Assert - Initial state
            Assert.Equal("container-xl", vesselType.VesselTypeId);
            Assert.Equal("Extra Large Container Ship", vesselType.Name);
            Assert.Null(vesselType.CapacityTEU);

            // Act - Update
            vesselType.Update(
                "Mega Container Ship",
                "Ultra Large Container Vessel (ULCV)",
                24000,
                26,
                22,
                11,
                "Requires specialized port infrastructure");

            // Assert - Updated state
            Assert.Equal("Mega Container Ship", vesselType.Name);
            Assert.Equal("Ultra Large Container Vessel (ULCV)", vesselType.Description);
            Assert.Equal(24000, vesselType.CapacityTEU);
            Assert.Equal(26, vesselType.MaxRows);
            Assert.Equal(22, vesselType.MaxBays);
            Assert.Equal(11, vesselType.MaxTiers);
            Assert.Equal("Requires specialized port infrastructure", vesselType.OperationalConstraints);
        }

        [Fact]
        public void CreateBulkCarrier_WithMinimalData_Succeeds()
        {
            // Arrange & Act
            var vesselType = new VesselType("bulk-carrier", "Bulk Carrier");

            // Assert
            Assert.NotNull(vesselType);
            Assert.Equal("bulk-carrier", vesselType.VesselTypeId);
            Assert.Equal("Bulk Carrier", vesselType.Name);
            Assert.Null(vesselType.CapacityTEU); // Bulk carriers may not use TEU
        }

        #endregion

        #region VesselTypeId Immutability Tests

        [Fact]
        public void Update_DoesNotChangeVesselTypeId()
        {
            // Arrange
            var originalId = "original-id";
            var vesselType = new VesselType(originalId, "Original Name");

            // Act
            vesselType.Update("New Name", "New description", 10000, 20, 15, 8, "New constraints");

            // Assert
            Assert.Equal(originalId, vesselType.VesselTypeId); // ID should remain unchanged
        }

        #endregion
    }
}
