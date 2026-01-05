using System;
using System.Linq;
using Xunit;
using DDDNetCore.Domain.Docks;
using DDDNetCore.Domain.Vessels;

namespace DDDNetCore.Tests.Unit.Docks
{
    /// <summary>
    /// TEST TYPE: Unit Test
    /// ENTITY UNDER TEST: Dock (Domain Entity)
    /// TEST OBJECTIVE: Validate Dock entity creation, code/name/location validation,
    ///                 dimension constraints (positive length/depth/draft), code immutability,
    ///                 code uppercase normalization, Update() method, vessel type management,
    ///                 and CanAccommodateVesselType() logic in isolation.
    /// </summary>
    public class DockTests
    {
        #region Constructor Tests - Valid Cases

        [Fact]
        public void Constructor_WithValidData_CreatesDock()
        {
            // Arrange
            var code = "DOCK01";
            var name = "Container Dock 1";
            var location = "North Berth";
            var lengthM = 350.5;
            var depthM = 15.0;
            var maxDraftM = 12.5;

            // Act
            var dock = new Dock(code, name, location, lengthM, depthM, maxDraftM);

            // Assert
            Assert.NotNull(dock);
            Assert.Equal(code, dock.Code);
            Assert.Equal(name, dock.Name);
            Assert.Equal(location, dock.Location);
            Assert.Equal(lengthM, dock.LengthM);
            Assert.Equal(depthM, dock.DepthM);
            Assert.Equal(maxDraftM, dock.MaxDraftM);
            Assert.Empty(dock.AllowedVesselTypes);
        }

        [Theory]
        [InlineData("dock01", "DOCK01")] // Lowercase to uppercase
        [InlineData("Dock02", "DOCK02")] // Mixed case to uppercase
        [InlineData("DOCK03", "DOCK03")] // Already uppercase
        [InlineData("  dock04  ", "DOCK04")] // With whitespace
        public void Constructor_WithVariousCodes_NormalizesToUppercase(string inputCode, string expectedCode)
        {
            // Arrange
            var name = "Test Dock";
            var location = "Test Location";

            // Act
            var dock = new Dock(inputCode, name, location, 300, 15, 12);

            // Assert
            Assert.Equal(expectedCode, dock.Code);
        }

        [Theory]
        [InlineData("  Dock Name  ", "Dock Name")]
        [InlineData("Container Terminal", "Container Terminal")]
        public void Constructor_WithWhitespaceInName_TrimsName(string inputName, string expectedName)
        {
            // Arrange
            var code = "DOCK01";
            var location = "Berth 1";

            // Act
            var dock = new Dock(code, inputName, location, 300, 15, 12);

            // Assert
            Assert.Equal(expectedName, dock.Name);
        }

        [Theory]
        [InlineData("  North Berth  ", "North Berth")]
        [InlineData("South Terminal", "South Terminal")]
        public void Constructor_WithWhitespaceInLocation_TrimsLocation(string inputLocation, string expectedLocation)
        {
            // Arrange
            var code = "DOCK01";
            var name = "Dock 1";

            // Act
            var dock = new Dock(code, name, inputLocation, 300, 15, 12);

            // Assert
            Assert.Equal(expectedLocation, dock.Location);
        }

        [Theory]
        [InlineData(100.5, 10.0, 8.5)] // Small dock
        [InlineData(500.0, 20.0, 15.0)] // Large dock
        [InlineData(0.1, 0.1, 0.1)] // Minimum positive values
        public void Constructor_WithValidDimensions_SetsDimensions(double lengthM, double depthM, double maxDraftM)
        {
            // Arrange
            var code = "DOCK01";
            var name = "Test Dock";
            var location = "Test Location";

            // Act
            var dock = new Dock(code, name, location, lengthM, depthM, maxDraftM);

            // Assert
            Assert.Equal(lengthM, dock.LengthM);
            Assert.Equal(depthM, dock.DepthM);
            Assert.Equal(maxDraftM, dock.MaxDraftM);
        }

        #endregion

        #region Constructor Tests - Invalid Cases

        [Theory]
        [InlineData("")]
        [InlineData("   ")]
        [InlineData(null)]
        public void Constructor_WithEmptyOrNullCode_ThrowsArgumentException(string? invalidCode)
        {
            // Arrange
            var name = "Dock 1";
            var location = "Berth 1";

            // Act & Assert
            var exception = Assert.Throws<ArgumentException>(() =>
                new Dock(invalidCode, name, location, 300, 15, 12));
            Assert.Contains("Dock code is required", exception.Message);
        }

        [Theory]
        [InlineData("")]
        [InlineData("   ")]
        [InlineData(null)]
        public void Constructor_WithEmptyOrNullName_ThrowsArgumentException(string? invalidName)
        {
            // Arrange
            var code = "DOCK01";
            var location = "Berth 1";

            // Act & Assert
            var exception = Assert.Throws<ArgumentException>(() =>
                new Dock(code, invalidName, location, 300, 15, 12));
            Assert.Contains("Dock name is required", exception.Message);
        }

        [Theory]
        [InlineData("")]
        [InlineData("   ")]
        [InlineData(null)]
        public void Constructor_WithEmptyOrNullLocation_ThrowsArgumentException(string? invalidLocation)
        {
            // Arrange
            var code = "DOCK01";
            var name = "Dock 1";

            // Act & Assert
            var exception = Assert.Throws<ArgumentException>(() =>
                new Dock(code, name, invalidLocation, 300, 15, 12));
            Assert.Contains("Dock location is required", exception.Message);
        }

        [Theory]
        [InlineData(0)]
        [InlineData(-1)]
        [InlineData(-100.5)]
        public void Constructor_WithZeroOrNegativeLength_ThrowsArgumentException(double invalidLength)
        {
            // Arrange
            var code = "DOCK01";
            var name = "Dock 1";
            var location = "Berth 1";

            // Act & Assert
            var exception = Assert.Throws<ArgumentException>(() =>
                new Dock(code, name, location, invalidLength, 15, 12));
            Assert.Contains("Length must be positive", exception.Message);
        }

        [Theory]
        [InlineData(0)]
        [InlineData(-1)]
        [InlineData(-50.5)]
        public void Constructor_WithZeroOrNegativeDepth_ThrowsArgumentException(double invalidDepth)
        {
            // Arrange
            var code = "DOCK01";
            var name = "Dock 1";
            var location = "Berth 1";

            // Act & Assert
            var exception = Assert.Throws<ArgumentException>(() =>
                new Dock(code, name, location, 300, invalidDepth, 12));
            Assert.Contains("Depth must be positive", exception.Message);
        }

        [Theory]
        [InlineData(0)]
        [InlineData(-1)]
        [InlineData(-25.5)]
        public void Constructor_WithZeroOrNegativeMaxDraft_ThrowsArgumentException(double invalidMaxDraft)
        {
            // Arrange
            var code = "DOCK01";
            var name = "Dock 1";
            var location = "Berth 1";

            // Act & Assert
            var exception = Assert.Throws<ArgumentException>(() =>
                new Dock(code, name, location, 300, 15, invalidMaxDraft));
            Assert.Contains("Max draft must be positive", exception.Message);
        }

        #endregion

        #region Update Method Tests - Valid Cases

        [Fact]
        public void Update_WithValidData_UpdatesAllFields()
        {
            // Arrange
            var dock = new Dock("DOCK01", "Original Name", "Original Location", 300, 15, 12);
            var newName = "Updated Dock";
            var newLocation = "New Berth";
            var newLength = 400.0;
            var newDepth = 18.0;
            var newMaxDraft = 14.0;

            // Act
            dock.Update(newName, newLocation, newLength, newDepth, newMaxDraft);

            // Assert
            Assert.Equal(newName, dock.Name);
            Assert.Equal(newLocation, dock.Location);
            Assert.Equal(newLength, dock.LengthM);
            Assert.Equal(newDepth, dock.DepthM);
            Assert.Equal(newMaxDraft, dock.MaxDraftM);
        }

        [Theory]
        [InlineData("  Updated Name  ", "Updated Name")]
        [InlineData("New Dock", "New Dock")]
        public void Update_WithWhitespaceInName_TrimsName(string inputName, string expectedName)
        {
            // Arrange
            var dock = new Dock("DOCK01", "Original", "Location", 300, 15, 12);

            // Act
            dock.Update(inputName, "Location", 300, 15, 12);

            // Assert
            Assert.Equal(expectedName, dock.Name);
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
            var dock = new Dock("DOCK01", "Original", "Location", 300, 15, 12);

            // Act & Assert
            var exception = Assert.Throws<ArgumentException>(() =>
                dock.Update(invalidName, "Location", 300, 15, 12));
            Assert.Contains("Dock name is required", exception.Message);
        }

        [Theory]
        [InlineData(0)]
        [InlineData(-1)]
        public void Update_WithZeroOrNegativeLength_ThrowsArgumentException(double invalidLength)
        {
            // Arrange
            var dock = new Dock("DOCK01", "Dock 1", "Location", 300, 15, 12);

            // Act & Assert
            var exception = Assert.Throws<ArgumentException>(() =>
                dock.Update("Dock 1", "Location", invalidLength, 15, 12));
            Assert.Contains("Length must be positive", exception.Message);
        }

        #endregion

        #region Code Immutability Tests

        [Fact]
        public void Update_DoesNotChangeCode()
        {
            // Arrange
            var originalCode = "DOCK01";
            var dock = new Dock(originalCode, "Original Name", "Location", 300, 15, 12);

            // Act
            dock.Update("New Name", "New Location", 400, 18, 14);

            // Assert
            Assert.Equal(originalCode, dock.Code); // Code should remain unchanged
        }

        #endregion

        #region Vessel Type Management Tests

        [Fact]
        public void SetAllowedVesselTypes_WithValidTypes_SetsTypes()
        {
            // Arrange
            var dock = new Dock("DOCK01", "Container Dock", "North Berth", 400, 18, 14);
            var vesselType1 = new VesselType("container-ship", "Container Ship");
            var vesselType2 = new VesselType("bulk-carrier", "Bulk Carrier");
            var vesselTypes = new[] { vesselType1, vesselType2 };

            // Act
            dock.SetAllowedVesselTypes(vesselTypes);

            // Assert
            Assert.Equal(2, dock.AllowedVesselTypes.Count);
            Assert.Contains(vesselType1, dock.AllowedVesselTypes);
            Assert.Contains(vesselType2, dock.AllowedVesselTypes);
        }

        [Fact]
        public void SetAllowedVesselTypes_ReplacesExistingTypes()
        {
            // Arrange
            var dock = new Dock("DOCK01", "Container Dock", "North Berth", 400, 18, 14);
            var oldType = new VesselType("tanker", "Oil Tanker");
            dock.SetAllowedVesselTypes(new[] { oldType });

            var newType1 = new VesselType("container-ship", "Container Ship");
            var newType2 = new VesselType("bulk-carrier", "Bulk Carrier");

            // Act
            dock.SetAllowedVesselTypes(new[] { newType1, newType2 });

            // Assert
            Assert.Equal(2, dock.AllowedVesselTypes.Count);
            Assert.DoesNotContain(oldType, dock.AllowedVesselTypes);
            Assert.Contains(newType1, dock.AllowedVesselTypes);
            Assert.Contains(newType2, dock.AllowedVesselTypes);
        }

        [Fact]
        public void CanAccommodateVesselType_WithAllowedType_ReturnsTrue()
        {
            // Arrange
            var dock = new Dock("DOCK01", "Container Dock", "North Berth", 400, 18, 14);
            var vesselType = new VesselType("container-ship", "Container Ship");
            dock.SetAllowedVesselTypes(new[] { vesselType });

            // Act
            var result = dock.CanAccommodateVesselType("container-ship");

            // Assert
            Assert.True(result);
        }

        [Fact]
        public void CanAccommodateVesselType_WithNonAllowedType_ReturnsFalse()
        {
            // Arrange
            var dock = new Dock("DOCK01", "Container Dock", "North Berth", 400, 18, 14);
            var vesselType = new VesselType("container-ship", "Container Ship");
            dock.SetAllowedVesselTypes(new[] { vesselType });

            // Act
            var result = dock.CanAccommodateVesselType("tanker");

            // Assert
            Assert.False(result);
        }

        [Fact]
        public void CanAccommodateVesselType_WithNoAllowedTypes_ReturnsFalse()
        {
            // Arrange
            var dock = new Dock("DOCK01", "Container Dock", "North Berth", 400, 18, 14);

            // Act
            var result = dock.CanAccommodateVesselType("container-ship");

            // Assert
            Assert.False(result);
        }

        #endregion

        #region Integration Tests

        [Fact]
        public void CreateContainerDock_WithFullConfiguration_Succeeds()
        {
            // Arrange & Act
            var dock = new Dock("CNTDOCK1", "Main Container Terminal", "North Port", 450.0, 20.0, 16.0);
            var containerType = new VesselType("ultra-large-container", "Ultra Large Container Vessel");
            dock.SetAllowedVesselTypes(new[] { containerType });

            // Assert
            Assert.Equal("CNTDOCK1", dock.Code);
            Assert.Equal("Main Container Terminal", dock.Name);
            Assert.Equal(450.0, dock.LengthM);
            Assert.Equal(20.0, dock.DepthM);
            Assert.Equal(16.0, dock.MaxDraftM);
            Assert.Single(dock.AllowedVesselTypes);
            Assert.True(dock.CanAccommodateVesselType("ultra-large-container"));
        }

        [Fact]
        public void UpdateDockDimensions_AfterConstruction_Succeeds()
        {
            // Arrange
            var dock = new Dock("DOCK01", "Small Dock", "West Berth", 200, 10, 8);

            // Act - Upgrade to larger dimensions
            dock.Update("Large Dock", "West Berth Expanded", 400, 18, 14);

            // Assert
            Assert.Equal("Large Dock", dock.Name);
            Assert.Equal(400, dock.LengthM);
            Assert.Equal(18, dock.DepthM);
            Assert.Equal(14, dock.MaxDraftM);
            Assert.Equal("DOCK01", dock.Code); // Code remains unchanged
        }

        #endregion
    }
}
