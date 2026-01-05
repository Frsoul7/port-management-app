using System;
using System.Linq;
using Xunit;
using DDDNetCore.Domain.StorageAreas;
using DDDNetCore.Domain.Docks;

namespace DDDNetCore.Tests.Unit.StorageAreas
{
    /// <summary>
    /// TEST TYPE: Unit Test
    /// ENTITY UNDER TEST: StorageArea (Domain Entity)
    /// TEST OBJECTIVE: Validate StorageArea entity creation, name/location validation,
    ///                 capacity constraints (positive max capacity, occupancy <= capacity),
    ///                 storage area types (ORDINARY, YARD, WAREHOUSE), XOR constraint for specs,
    ///                 dock serving logic, and occupancy management in isolation.
    /// </summary>
    public class StorageAreaTests
    {
        #region Constructor Tests - Valid Cases

        [Fact]
        public void Constructor_WithValidData_CreatesStorageArea()
        {
            // Arrange
            var name = "Container Storage A";
            var location = "Zone 1";
            var maxCapacity = 5000;
            var type = StorageAreaType.ORDINARY;

            // Act
            var storageArea = new StorageArea(name, location, maxCapacity, type);

            // Assert
            Assert.NotNull(storageArea);
            Assert.NotNull(storageArea.StorageAreaId);
            Assert.Equal(name, storageArea.Name);
            Assert.Equal(location, storageArea.Location);
            Assert.Equal(maxCapacity, storageArea.MaxCapacityTEU);
            Assert.Equal(0, storageArea.CurrentOccupancyTEU); // Starts at 0
            Assert.Equal(type, storageArea.Type);
            Assert.True(storageArea.ServesAllDocks); // Default value
        }

        [Theory]
        [InlineData(StorageAreaType.ORDINARY)]
        [InlineData(StorageAreaType.YARD)]
        [InlineData(StorageAreaType.WAREHOUSE)]
        public void Constructor_WithDifferentTypes_SetsTypeCorrectly(StorageAreaType type)
        {
            // Arrange
            var name = "Test Storage";
            var location = "Test Location";
            var maxCapacity = 3000;

            // Act
            var storageArea = new StorageArea(name, location, maxCapacity, type);

            // Assert
            Assert.Equal(type, storageArea.Type);
        }

        [Theory]
        [InlineData(true)]
        [InlineData(false)]
        public void Constructor_WithServesAllDocksParameter_SetsCorrectly(bool servesAllDocks)
        {
            // Arrange
            var name = "Test Storage";
            var location = "Zone 2";
            var maxCapacity = 2000;

            // Act
            var storageArea = new StorageArea(name, location, maxCapacity, StorageAreaType.ORDINARY, servesAllDocks);

            // Assert
            Assert.Equal(servesAllDocks, storageArea.ServesAllDocks);
        }

        [Theory]
        [InlineData("  Container Storage  ", "Container Storage")]
        [InlineData("Yard A", "Yard A")]
        public void Constructor_WithWhitespaceInName_TrimsName(string inputName, string expectedName)
        {
            // Arrange
            var location = "Zone 1";
            var maxCapacity = 1000;

            // Act
            var storageArea = new StorageArea(inputName, location, maxCapacity, StorageAreaType.ORDINARY);

            // Assert
            Assert.Equal(expectedName, storageArea.Name);
        }

        [Theory]
        [InlineData("  Zone 1  ", "Zone 1")]
        [InlineData("North Port", "North Port")]
        public void Constructor_WithWhitespaceInLocation_TrimsLocation(string inputLocation, string expectedLocation)
        {
            // Arrange
            var name = "Storage A";
            var maxCapacity = 1000;

            // Act
            var storageArea = new StorageArea(name, inputLocation, maxCapacity, StorageAreaType.ORDINARY);

            // Assert
            Assert.Equal(expectedLocation, storageArea.Location);
        }

        #endregion

        #region Constructor Tests - Invalid Cases

        [Theory]
        [InlineData("")]
        [InlineData("   ")]
        [InlineData(null)]
        public void Constructor_WithEmptyOrNullName_ThrowsArgumentException(string? invalidName)
        {
            // Arrange
            var location = "Zone 1";
            var maxCapacity = 1000;

            // Act & Assert
            var exception = Assert.Throws<ArgumentException>(() =>
                new StorageArea(invalidName, location, maxCapacity, StorageAreaType.ORDINARY));
            Assert.Contains("name is required", exception.Message);
        }

        [Theory]
        [InlineData("")]
        [InlineData("   ")]
        [InlineData(null)]
        public void Constructor_WithEmptyOrNullLocation_ThrowsArgumentException(string? invalidLocation)
        {
            // Arrange
            var name = "Storage A";
            var maxCapacity = 1000;

            // Act & Assert
            var exception = Assert.Throws<ArgumentException>(() =>
                new StorageArea(name, invalidLocation, maxCapacity, StorageAreaType.ORDINARY));
            Assert.Contains("Location is required", exception.Message);
        }

        [Theory]
        [InlineData(0)]
        [InlineData(-1)]
        [InlineData(-1000)]
        public void Constructor_WithZeroOrNegativeCapacity_ThrowsArgumentException(int invalidCapacity)
        {
            // Arrange
            var name = "Storage A";
            var location = "Zone 1";

            // Act & Assert
            var exception = Assert.Throws<ArgumentException>(() =>
                new StorageArea(name, location, invalidCapacity, StorageAreaType.ORDINARY));
            Assert.Contains("Max capacity must be positive", exception.Message);
        }

        #endregion

        #region Update Method Tests - Valid Cases

        [Fact]
        public void Update_WithValidData_UpdatesAllFields()
        {
            // Arrange
            var storageArea = new StorageArea("Original Name", "Original Location", 5000, StorageAreaType.ORDINARY);
            var newName = "Updated Storage";
            var newLocation = "New Zone";
            var newCapacity = 8000;
            var newServesAllDocks = false;

            // Act
            storageArea.Update(newName, newLocation, newCapacity, newServesAllDocks);

            // Assert
            Assert.Equal(newName, storageArea.Name);
            Assert.Equal(newLocation, storageArea.Location);
            Assert.Equal(newCapacity, storageArea.MaxCapacityTEU);
            Assert.Equal(newServesAllDocks, storageArea.ServesAllDocks);
        }

        [Theory]
        [InlineData("  Updated Name  ", "Updated Name")]
        [InlineData("New Storage", "New Storage")]
        public void Update_WithWhitespaceInName_TrimsName(string inputName, string expectedName)
        {
            // Arrange
            var storageArea = new StorageArea("Original", "Zone 1", 1000, StorageAreaType.ORDINARY);

            // Act
            storageArea.Update(inputName, "Zone 1", 1000, true);

            // Assert
            Assert.Equal(expectedName, storageArea.Name);
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
            var storageArea = new StorageArea("Original", "Zone 1", 1000, StorageAreaType.ORDINARY);

            // Act & Assert
            var exception = Assert.Throws<ArgumentException>(() =>
                storageArea.Update(invalidName, "Zone 1", 1000, true));
            Assert.Contains("name is required", exception.Message);
        }

        [Theory]
        [InlineData(0)]
        [InlineData(-1)]
        public void Update_WithZeroOrNegativeCapacity_ThrowsArgumentException(int invalidCapacity)
        {
            // Arrange
            var storageArea = new StorageArea("Storage A", "Zone 1", 1000, StorageAreaType.ORDINARY);

            // Act & Assert
            var exception = Assert.Throws<ArgumentException>(() =>
                storageArea.Update("Storage A", "Zone 1", invalidCapacity, true));
            Assert.Contains("Max capacity must be positive", exception.Message);
        }

        [Fact]
        public void Update_WithCapacityBelowCurrentOccupancy_ThrowsInvalidOperationException()
        {
            // Arrange
            var storageArea = new StorageArea("Storage A", "Zone 1", 5000, StorageAreaType.ORDINARY);
            storageArea.UpdateOccupancy(3000); // Set current occupancy

            // Act & Assert
            var exception = Assert.Throws<InvalidOperationException>(() =>
                storageArea.Update("Storage A", "Zone 1", 2000, true)); // Try to set capacity below occupancy
            Assert.Contains("Cannot reduce max capacity below current occupancy", exception.Message);
        }

        #endregion

        #region Occupancy Management Tests

        [Theory]
        [InlineData(0)]
        [InlineData(1000)]
        [InlineData(5000)] // Max capacity
        public void UpdateOccupancy_WithValidValue_UpdatesOccupancy(int validOccupancy)
        {
            // Arrange
            var storageArea = new StorageArea("Storage A", "Zone 1", 5000, StorageAreaType.ORDINARY);

            // Act
            storageArea.UpdateOccupancy(validOccupancy);

            // Assert
            Assert.Equal(validOccupancy, storageArea.CurrentOccupancyTEU);
        }

        [Fact]
        public void UpdateOccupancy_WithNegativeValue_ThrowsArgumentException()
        {
            // Arrange
            var storageArea = new StorageArea("Storage A", "Zone 1", 5000, StorageAreaType.ORDINARY);

            // Act & Assert
            var exception = Assert.Throws<ArgumentException>(() =>
                storageArea.UpdateOccupancy(-100));
            Assert.Contains("Occupancy cannot be negative", exception.Message);
        }

        [Fact]
        public void UpdateOccupancy_ExceedingMaxCapacity_ThrowsInvalidOperationException()
        {
            // Arrange
            var storageArea = new StorageArea("Storage A", "Zone 1", 5000, StorageAreaType.ORDINARY);

            // Act & Assert
            var exception = Assert.Throws<InvalidOperationException>(() =>
                storageArea.UpdateOccupancy(6000)); // Exceeds max capacity
            Assert.Contains("Occupancy", exception.Message);
            Assert.Contains("cannot exceed max capacity", exception.Message);
        }

        #endregion

        #region Type-Specific Spec Tests

        [Fact]
        public void SetYardSpec_ForYardType_SetsYardSpecAndClearsWarehouseSpec()
        {
            // Arrange
            var storageArea = new StorageArea("Yard A", "Zone 1", 3000, StorageAreaType.YARD);
            var notes = "Outdoor container yard";

            // Act
            storageArea.SetYardSpec(notes);

            // Assert
            Assert.NotNull(storageArea.YardSpec);
            Assert.Equal(notes, storageArea.YardSpec.Notes);
            Assert.Null(storageArea.WarehouseSpec); // XOR constraint
        }

        [Fact]
        public void SetYardSpec_ForNonYardType_ThrowsInvalidOperationException()
        {
            // Arrange
            var storageArea = new StorageArea("Warehouse A", "Zone 1", 2000, StorageAreaType.WAREHOUSE);

            // Act & Assert
            var exception = Assert.Throws<InvalidOperationException>(() =>
                storageArea.SetYardSpec("Some notes"));
            Assert.Contains("YardSpec can only be set for YARD type", exception.Message);
        }

        [Fact]
        public void SetWarehouseSpec_ForWarehouseType_SetsWarehouseSpecAndClearsYardSpec()
        {
            // Arrange
            var storageArea = new StorageArea("Warehouse A", "Zone 1", 2000, StorageAreaType.WAREHOUSE);
            var notes = "Climate-controlled warehouse";

            // Act
            storageArea.SetWarehouseSpec(notes);

            // Assert
            Assert.NotNull(storageArea.WarehouseSpec);
            Assert.Equal(notes, storageArea.WarehouseSpec.Notes);
            Assert.Null(storageArea.YardSpec); // XOR constraint
        }

        [Fact]
        public void SetWarehouseSpec_ForNonWarehouseType_ThrowsInvalidOperationException()
        {
            // Arrange
            var storageArea = new StorageArea("Yard A", "Zone 1", 3000, StorageAreaType.YARD);

            // Act & Assert
            var exception = Assert.Throws<InvalidOperationException>(() =>
                storageArea.SetWarehouseSpec("Some notes"));
            Assert.Contains("WarehouseSpec can only be set for WAREHOUSE type", exception.Message);
        }

        #endregion

        #region Dock Serving Tests

        [Fact]
        public void SetServedDocks_WhenServesAllDocksIsFalse_SetsDocks()
        {
            // Arrange
            var storageArea = new StorageArea("Storage A", "Zone 1", 5000, StorageAreaType.ORDINARY, servesAllDocks: false);
            var dock1 = new Dock("DOCK01", "Dock 1", "Location 1", 300, 15, 12);
            var dock2 = new Dock("DOCK02", "Dock 2", "Location 2", 350, 18, 14);

            // Act
            storageArea.SetServedDocks(new[] { dock1, dock2 });

            // Assert
            Assert.Equal(2, storageArea.Docks.Count);
            Assert.Contains(dock1, storageArea.Docks);
            Assert.Contains(dock2, storageArea.Docks);
        }

        [Fact]
        public void SetServedDocks_WhenServesAllDocksIsTrue_ThrowsInvalidOperationException()
        {
            // Arrange
            var storageArea = new StorageArea("Storage A", "Zone 1", 5000, StorageAreaType.ORDINARY, servesAllDocks: true);
            var dock = new Dock("DOCK01", "Dock 1", "Location 1", 300, 15, 12);

            // Act & Assert
            var exception = Assert.Throws<InvalidOperationException>(() =>
                storageArea.SetServedDocks(new[] { dock }));
            Assert.Contains("Cannot set specific docks when ServesAllDocks is true", exception.Message);
        }

        #endregion

        #region Integration Tests

        [Fact]
        public void CreateYardStorageArea_WithFullConfiguration_Succeeds()
        {
            // Arrange & Act
            var yard = new StorageArea("Container Yard 1", "North Zone", 10000, StorageAreaType.YARD, servesAllDocks: false);
            yard.SetYardSpec("Open-air container yard with crane access");
            yard.UpdateOccupancy(2500);

            // Assert
            Assert.Equal("Container Yard 1", yard.Name);
            Assert.Equal(StorageAreaType.YARD, yard.Type);
            Assert.NotNull(yard.YardSpec);
            Assert.Equal(2500, yard.CurrentOccupancyTEU);
            Assert.Equal(10000, yard.MaxCapacityTEU);
            Assert.False(yard.ServesAllDocks);
        }

        [Fact]
        public void CreateWarehouseStorageArea_WithFullConfiguration_Succeeds()
        {
            // Arrange & Act
            var warehouse = new StorageArea("Warehouse A", "South Zone", 5000, StorageAreaType.WAREHOUSE);
            warehouse.SetWarehouseSpec("Temperature-controlled storage for sensitive cargo");
            warehouse.UpdateOccupancy(1200);

            // Assert
            Assert.Equal("Warehouse A", warehouse.Name);
            Assert.Equal(StorageAreaType.WAREHOUSE, warehouse.Type);
            Assert.NotNull(warehouse.WarehouseSpec);
            Assert.Equal(1200, warehouse.CurrentOccupancyTEU);
            Assert.True(warehouse.ServesAllDocks);
        }

        #endregion
    }
}
