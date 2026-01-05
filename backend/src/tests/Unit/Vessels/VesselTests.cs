using System;
using Xunit;
using DDDNetCore.Domain.Vessels;
using DDDNetCore.Domain.Organizations;
using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Tests.Unit.Vessels
{
    /// <summary>
    /// TEST TYPE: Unit Test
    /// ENTITY UNDER TEST: Vessel (Domain Entity)
    /// TEST OBJECTIVE: Validate Vessel entity creation, IMO validation/normalization, name validation,
    ///                 capacity validation, update operations, and business rule enforcement in isolation.
    ///                 Tests the IMO check digit algorithm, immutability constraints, and entity state management.
    /// </summary>
    public class VesselTests
    {
        #region Test Data Helpers

        private static OrganizationId CreateValidOrganizationId() => OrganizationId.NewId();

        // Valid IMO numbers (7 digits with correct check digit)
        // Formula: (d1*7 + d2*6 + d3*5 + d4*4 + d5*3 + d6*2) % 10 = check digit
        private const string ValidImo1 = "9176187"; // MSC Oscar: 9*7 + 1*6 + 7*5 + 6*4 + 1*3 + 8*2 = 137, 137%10 = 7 ✓
        private const string ValidImo2 = "9321483"; // Emma Maersk: 9*7 + 3*6 + 2*5 + 1*4 + 4*3 + 8*2 = 123, 123%10 = 3 ✓
        private const string ValidImo3 = "9362255"; // CMA CGM: 9*7 + 3*6 + 6*5 + 2*4 + 2*3 + 5*2 = 135, 135%10 = 5 ✓

        #endregion

        #region Constructor Tests - Valid Cases

        [Fact]
        public void Constructor_WithValidData_CreatesVessel()
        {
            // Arrange
            var imoNumber = ValidImo1;
            var name = "MSC Oscar";
            var vesselTypeId = "container-ship";
            var orgId = CreateValidOrganizationId();
            var capacity = 19224;

            // Act
            var vessel = new Vessel(imoNumber, name, vesselTypeId, orgId, capacity);

            // Assert
            Assert.NotNull(vessel);
            Assert.Equal(imoNumber, vessel.ImoNumber);
            Assert.Equal(name.Trim(), vessel.Name);
            Assert.Equal(vesselTypeId.Trim(), vessel.VesselTypeId);
            Assert.Equal(orgId, vessel.OwnerOrganizationId);
            Assert.Equal(capacity, vessel.CapacityTEU);
        }

        [Fact]
        public void Constructor_WithOptionalCapacity_CreatesVesselWithNullCapacity()
        {
            // Arrange
            var imoNumber = ValidImo2;
            var name = "Emma Maersk";
            var vesselTypeId = "container-ship";
            var orgId = CreateValidOrganizationId();

            // Act
            var vessel = new Vessel(imoNumber, name, vesselTypeId, orgId);

            // Assert
            Assert.NotNull(vessel);
            Assert.Null(vessel.CapacityTEU);
        }

        [Theory]
        [InlineData("  MSC Oscar  ", "MSC Oscar")] // Leading/trailing spaces
        [InlineData("Emma Maersk", "Emma Maersk")] // No spaces
        [InlineData("  CMA CGM  ", "CMA CGM")] // Multiple spaces
        public void Constructor_WithWhitespaceInName_TrimsName(string inputName, string expectedName)
        {
            // Arrange
            var imoNumber = ValidImo1;
            var vesselTypeId = "container-ship";
            var orgId = CreateValidOrganizationId();

            // Act
            var vessel = new Vessel(imoNumber, inputName, vesselTypeId, orgId);

            // Assert
            Assert.Equal(expectedName, vessel.Name);
        }

        [Theory]
        [InlineData("  container-ship  ", "container-ship")]
        [InlineData("bulk-carrier", "bulk-carrier")]
        [InlineData("  tanker  ", "tanker")]
        public void Constructor_WithWhitespaceInVesselTypeId_TrimsVesselTypeId(string inputTypeId, string expectedTypeId)
        {
            // Arrange
            var imoNumber = ValidImo1;
            var name = "Test Vessel";
            var orgId = CreateValidOrganizationId();

            // Act
            var vessel = new Vessel(imoNumber, name, inputTypeId, orgId);

            // Assert
            Assert.Equal(expectedTypeId, vessel.VesselTypeId);
        }

        #endregion

        #region IMO Number Validation Tests

        [Theory]
        [InlineData(ValidImo1)] // 9176187
        [InlineData(ValidImo2)] // 9321483
        [InlineData(ValidImo3)] // 9362255
        public void Constructor_WithValidImoNumber_Succeeds(string validImo)
        {
            // Arrange
            var name = "Test Vessel";
            var vesselTypeId = "container-ship";
            var orgId = CreateValidOrganizationId();

            // Act
            var vessel = new Vessel(validImo, name, vesselTypeId, orgId);

            // Assert
            Assert.Equal(validImo, vessel.ImoNumber);
        }

        [Theory]
        [InlineData("9176180")] // Invalid check digit (should be 7, not 0)
        [InlineData("9321480")] // Invalid check digit (should be 3, not 0)
        [InlineData("1234560")] // Invalid check digit: 1*7+2*6+3*5+4*4+5*3+6*2=77, 77%10=7, but digit is 0
        public void Constructor_WithInvalidImoCheckDigit_ThrowsArgumentException(string invalidImo)
        {
            // Arrange
            var name = "Test Vessel";
            var vesselTypeId = "container-ship";
            var orgId = CreateValidOrganizationId();

            // Act & Assert
            var exception = Assert.Throws<ArgumentException>(() =>
                new Vessel(invalidImo, name, vesselTypeId, orgId));
            Assert.Contains("Invalid IMO number", exception.Message);
        }

        [Theory]
        [InlineData("")] // Empty
        [InlineData("   ")] // Whitespace only
        [InlineData(null)] // Null
        public void Constructor_WithEmptyOrNullImo_ThrowsArgumentException(string? invalidImo)
        {
            // Arrange
            var name = "Test Vessel";
            var vesselTypeId = "container-ship";
            var orgId = CreateValidOrganizationId();

            // Act & Assert
            var exception = Assert.Throws<ArgumentException>(() =>
                new Vessel(invalidImo, name, vesselTypeId, orgId));
            Assert.Contains("IMO number is required", exception.Message);
        }

        [Theory]
        [InlineData("123")] // Too short
        [InlineData("12345678")] // Too long
        [InlineData("ABCDEFG")] // Non-numeric
        [InlineData("123456A")] // Contains letter
        public void Constructor_WithInvalidImoFormat_ThrowsArgumentException(string invalidImo)
        {
            // Arrange
            var name = "Test Vessel";
            var vesselTypeId = "container-ship";
            var orgId = CreateValidOrganizationId();

            // Act & Assert
            var exception = Assert.Throws<ArgumentException>(() =>
                new Vessel(invalidImo, name, vesselTypeId, orgId));
            Assert.Contains("Invalid IMO number", exception.Message);
        }

        #endregion

        #region Name Validation Tests

        [Theory]
        [InlineData("")] // Empty
        [InlineData("   ")] // Whitespace only
        [InlineData(null)] // Null
        public void Constructor_WithEmptyOrNullName_ThrowsArgumentException(string? invalidName)
        {
            // Arrange
            var imoNumber = ValidImo1;
            var vesselTypeId = "container-ship";
            var orgId = CreateValidOrganizationId();

            // Act & Assert
            var exception = Assert.Throws<ArgumentException>(() =>
                new Vessel(imoNumber, invalidName, vesselTypeId, orgId));
            Assert.Contains("Vessel name is required", exception.Message);
        }

        #endregion

        #region VesselTypeId Validation Tests

        [Theory]
        [InlineData("")] // Empty
        [InlineData("   ")] // Whitespace only
        [InlineData(null)] // Null
        public void Constructor_WithEmptyOrNullVesselTypeId_ThrowsArgumentException(string? invalidTypeId)
        {
            // Arrange
            var imoNumber = ValidImo1;
            var name = "Test Vessel";
            var orgId = CreateValidOrganizationId();

            // Act & Assert
            var exception = Assert.Throws<ArgumentException>(() =>
                new Vessel(imoNumber, name, invalidTypeId, orgId));
            Assert.Contains("Vessel type is required", exception.Message);
        }

        #endregion

        #region OrganizationId Validation Tests

        [Fact]
        public void Constructor_WithNullOrganizationId_ThrowsArgumentNullException()
        {
            // Arrange
            var imoNumber = ValidImo1;
            var name = "Test Vessel";
            var vesselTypeId = "container-ship";

            // Act & Assert
            Assert.Throws<ArgumentNullException>(() =>
                new Vessel(imoNumber, name, vesselTypeId, null!));
        }

        #endregion

        #region Factory Method Tests

        [Fact]
        public void Create_WithValidData_CreatesVessel()
        {
            // Arrange
            var imoNumber = ValidImo1;
            var name = "Test Vessel";
            var vesselTypeId = "container-ship";
            var orgId = CreateValidOrganizationId();
            var capacity = 15000;

            // Act
            var vessel = Vessel.Create(imoNumber, name, vesselTypeId, orgId, capacity);

            // Assert
            Assert.NotNull(vessel);
            Assert.Equal(imoNumber, vessel.ImoNumber);
            Assert.Equal(name, vessel.Name);
            Assert.Equal(capacity, vessel.CapacityTEU);
        }

        [Fact]
        public void Create_WithInvalidImo_ThrowsArgumentException()
        {
            // Arrange
            var invalidImo = "1234560"; // Invalid check digit: should be 7, not 0
            var name = "Test Vessel";
            var vesselTypeId = "container-ship";
            var orgId = CreateValidOrganizationId();

            // Act & Assert
            var exception = Assert.Throws<ArgumentException>(() =>
                Vessel.Create(invalidImo, name, vesselTypeId, orgId));
            Assert.Contains("Invalid IMO number", exception.Message);
        }

        #endregion

        #region Update Tests

        [Fact]
        public void UpdateBasics_WithValidData_UpdatesAllFields()
        {
            // Arrange
            var vessel = new Vessel(ValidImo1, "Original Name", "container-ship", CreateValidOrganizationId());
            var newName = "Updated Name";
            var newVesselTypeId = "bulk-carrier";
            var newOrgId = CreateValidOrganizationId();
            var newCapacity = 20000;

            // Act
            vessel.UpdateBasics(newName, newVesselTypeId, newOrgId, newCapacity);

            // Assert
            Assert.Equal(newName, vessel.Name);
            Assert.Equal(newVesselTypeId, vessel.VesselTypeId);
            Assert.Equal(newOrgId, vessel.OwnerOrganizationId);
            Assert.Equal(newCapacity, vessel.CapacityTEU);
        }

        [Fact]
        public void UpdateBasics_DoesNotChangeImoNumber()
        {
            // Arrange
            var originalImo = ValidImo1;
            var vessel = new Vessel(originalImo, "Original Name", "container-ship", CreateValidOrganizationId());
            var newName = "Updated Name";
            var newVesselTypeId = "bulk-carrier";
            var newOrgId = CreateValidOrganizationId();

            // Act
            vessel.UpdateBasics(newName, newVesselTypeId, newOrgId, null);

            // Assert - IMO number remains unchanged (immutable)
            Assert.Equal(originalImo, vessel.ImoNumber);
        }

        [Theory]
        [InlineData("")] // Empty
        [InlineData("   ")] // Whitespace only
        [InlineData(null)] // Null
        public void UpdateBasics_WithEmptyOrNullName_ThrowsArgumentException(string? invalidName)
        {
            // Arrange
            var vessel = new Vessel(ValidImo1, "Original Name", "container-ship", CreateValidOrganizationId());
            var vesselTypeId = "bulk-carrier";
            var orgId = CreateValidOrganizationId();

            // Act & Assert
            var exception = Assert.Throws<ArgumentException>(() =>
                vessel.UpdateBasics(invalidName, vesselTypeId, orgId, null));
            Assert.Contains("Vessel name is required", exception.Message);
        }

        [Theory]
        [InlineData("")] // Empty
        [InlineData("   ")] // Whitespace only
        [InlineData(null)] // Null
        public void UpdateBasics_WithEmptyOrNullVesselTypeId_ThrowsArgumentException(string? invalidTypeId)
        {
            // Arrange
            var vessel = new Vessel(ValidImo1, "Original Name", "container-ship", CreateValidOrganizationId());
            var name = "Updated Name";
            var orgId = CreateValidOrganizationId();

            // Act & Assert
            var exception = Assert.Throws<ArgumentException>(() =>
                vessel.UpdateBasics(name, invalidTypeId, orgId, null));
            Assert.Contains("Vessel type is required", exception.Message);
        }

        [Fact]
        public void UpdateBasics_WithNullOrganizationId_ThrowsArgumentNullException()
        {
            // Arrange
            var vessel = new Vessel(ValidImo1, "Original Name", "container-ship", CreateValidOrganizationId());
            var name = "Updated Name";
            var vesselTypeId = "bulk-carrier";

            // Act & Assert
            Assert.Throws<ArgumentNullException>(() =>
                vessel.UpdateBasics(name, vesselTypeId, null!, null));
        }

        [Theory]
        [InlineData("  Updated Name  ", "Updated Name")]
        [InlineData("  New Vessel  ", "New Vessel")]
        public void UpdateBasics_WithWhitespaceInName_TrimsName(string inputName, string expectedName)
        {
            // Arrange
            var vessel = new Vessel(ValidImo1, "Original Name", "container-ship", CreateValidOrganizationId());
            var vesselTypeId = "bulk-carrier";
            var orgId = CreateValidOrganizationId();

            // Act
            vessel.UpdateBasics(inputName, vesselTypeId, orgId, null);

            // Assert
            Assert.Equal(expectedName, vessel.Name);
        }

        [Theory]
        [InlineData("  bulk-carrier  ", "bulk-carrier")]
        [InlineData("  tanker  ", "tanker")]
        public void UpdateBasics_WithWhitespaceInVesselTypeId_TrimsVesselTypeId(string inputTypeId, string expectedTypeId)
        {
            // Arrange
            var vessel = new Vessel(ValidImo1, "Original Name", "container-ship", CreateValidOrganizationId());
            var name = "Updated Name";
            var orgId = CreateValidOrganizationId();

            // Act
            vessel.UpdateBasics(name, inputTypeId, orgId, null);

            // Assert
            Assert.Equal(expectedTypeId, vessel.VesselTypeId);
        }

        #endregion

        #region Capacity Tests

        [Theory]
        [InlineData(null)] // No capacity
        [InlineData(0)] // Zero capacity
        [InlineData(10000)] // Normal capacity
        [InlineData(25000)] // Large capacity
        public void Constructor_WithVariousCapacities_SetsCapacityCorrectly(int? capacity)
        {
            // Arrange
            var imoNumber = ValidImo1;
            var name = "Test Vessel";
            var vesselTypeId = "container-ship";
            var orgId = CreateValidOrganizationId();

            // Act
            var vessel = new Vessel(imoNumber, name, vesselTypeId, orgId, capacity);

            // Assert
            Assert.Equal(capacity, vessel.CapacityTEU);
        }

        [Theory]
        [InlineData(null)] // Clear capacity
        [InlineData(0)] // Zero capacity
        [InlineData(5000)] // New capacity
        [InlineData(30000)] // Large capacity update
        public void UpdateBasics_WithVariousCapacities_UpdatesCapacityCorrectly(int? newCapacity)
        {
            // Arrange
            var vessel = new Vessel(ValidImo1, "Test Vessel", "container-ship", CreateValidOrganizationId(), 10000);
            var name = "Test Vessel";
            var vesselTypeId = "container-ship";
            var orgId = CreateValidOrganizationId();

            // Act
            vessel.UpdateBasics(name, vesselTypeId, orgId, newCapacity);

            // Assert
            Assert.Equal(newCapacity, vessel.CapacityTEU);
        }

        #endregion

        #region IMO Normalization Tests (ImoValidator Integration)

        [Fact]
        public void ImoValidator_Normalize_TrimsWhitespace()
        {
            // Arrange
            var imoWithSpaces = "  9176187  ";

            // Act
            var normalized = ImoValidator.Normalize(imoWithSpaces);

            // Assert
            Assert.Equal("9176187", normalized);
        }

        [Fact]
        public void ImoValidator_IsValid_WithValidImo_ReturnsTrue()
        {
            // Arrange
            var validImo = ValidImo1; // 9176187

            // Act
            var isValid = ImoValidator.IsValid(validImo);

            // Assert
            Assert.True(isValid);
        }

        [Theory]
        [InlineData("9176180")] // Wrong check digit
        [InlineData("1234560")] // Wrong check digit: 1*7+2*6+3*5+4*4+5*3+6*2=77, 77%10=7, but digit is 0
        [InlineData("ABCDEFG")] // Non-numeric
        [InlineData("12345")] // Too short
        [InlineData("123456789")] // Too long
        [InlineData("")] // Empty
        [InlineData(null)] // Null
        public void ImoValidator_IsValid_WithInvalidImo_ReturnsFalse(string? invalidImo)
        {
            // Act
            var isValid = ImoValidator.IsValid(invalidImo);

            // Assert
            Assert.False(isValid);
        }

        #endregion
    }
}
