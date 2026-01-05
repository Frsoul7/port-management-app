using System;
using System.Collections.Generic;
using System.Linq;
using Xunit;
using DDDNetCore.Domain.Organizations;

namespace DDDNetCore.Tests.Unit.Organizations
{
    /// <summary>
    /// TEST TYPE: Unit Test
    /// ENTITY UNDER TEST: Organization (Domain Aggregate Root)
    /// TEST OBJECTIVE: Validate Organization entity creation, identifier validation/normalization, legal name validation,
    ///                 tax number validation, representative management (add, unique email constraint),
    ///                 organization type enforcement, and business rule validation in isolation.
    /// </summary>
    public class OrganizationTests
    {
        #region Test Data Helpers

        private static Guid CreateValidGuid() => Guid.NewGuid();

        private static Representative CreateValidRepresentative(string email = "rep1@example.com")
        {
            return new Representative(
                name: "John Doe",
                citizenId: "123456789",
                nationality: "US",
                email: email,
                phone: "+1234567890"
            );
        }

        #endregion

        #region Constructor Tests - Valid Cases

        [Fact]
        public void Constructor_WithValidDataAndNoRepresentatives_CreatesOrganization()
        {
            // Arrange
            var id = CreateValidGuid();
            var identifier = "ORG123";
            var legalName = "Test Organization Ltd.";
            var alternativeName = "TestOrg";
            var addressLine = "123 Main Street, City";
            var taxNumber = "PT123456789";
            var type = OrganizationType.SHIPPING_AGENT;

            // Act
            var org = new Organization(id, identifier, legalName, alternativeName, addressLine, taxNumber, type);

            // Assert
            Assert.NotNull(org);
            Assert.Equal(id, org.Id);
            Assert.Equal(id, org.OrganizationId.Value);
            Assert.Equal(identifier, org.Identifier);
            Assert.Equal(legalName, org.LegalName);
            Assert.Equal(alternativeName, org.AlternativeName);
            Assert.Equal(addressLine, org.AddressLine);
            Assert.Equal(taxNumber, org.TaxNumber);
            Assert.Equal(type, org.Type);
            Assert.Empty(org.Representatives);
        }

        [Fact]
        public void Constructor_WithValidDataAndRepresentatives_CreatesOrganization()
        {
            // Arrange
            var id = CreateValidGuid();
            var identifier = "ORG456";
            var legalName = "Shipping Company Inc.";
            var alternativeName = "ShipCo";
            var addressLine = "456 Port Avenue";
            var taxNumber = "PT987654321";
            var type = OrganizationType.SHIPPING_AGENT;
            var representatives = new[]
            {
                CreateValidRepresentative("rep1@company.com"),
                CreateValidRepresentative("rep2@company.com")
            };

            // Act
            var org = new Organization(id, identifier, legalName, alternativeName, addressLine, taxNumber, type, representatives);

            // Assert
            Assert.NotNull(org);
            Assert.Equal(2, org.Representatives.Count);
            Assert.Contains(org.Representatives, r => r.Email == "rep1@company.com");
            Assert.Contains(org.Representatives, r => r.Email == "rep2@company.com");
        }

        [Theory]
        [InlineData(OrganizationType.SHIPPING_AGENT)]
        [InlineData(OrganizationType.PORT_AUTHORITY)]
        [InlineData(OrganizationType.LOGISTICS_OPERATOR)]
        public void Constructor_WithDifferentOrganizationTypes_SetsTypeCorrectly(OrganizationType type)
        {
            // Arrange
            var id = CreateValidGuid();
            var identifier = "ORG789";
            var legalName = "Test Org";
            var alternativeName = "";
            var addressLine = "123 Street";
            var taxNumber = "TAX123";

            // Act
            var org = new Organization(id, identifier, legalName, alternativeName, addressLine, taxNumber, type);

            // Assert
            Assert.Equal(type, org.Type);
        }

        [Fact]
        public void Constructor_WithNullRepresentatives_CreatesOrganizationWithEmptyList()
        {
            // Arrange
            var id = CreateValidGuid();
            var identifier = "ORG999";
            var legalName = "No Reps Org";
            var alternativeName = "";
            var addressLine = "Address";
            var taxNumber = "TAX999";
            var type = OrganizationType.PORT_AUTHORITY;

            // Act
            var org = new Organization(id, identifier, legalName, alternativeName, addressLine, taxNumber, type, null);

            // Assert
            Assert.Empty(org.Representatives);
        }

        #endregion

        #region Identifier Validation Tests

        [Theory]
        [InlineData("ORG123")] // Alphanumeric
        [InlineData("ABC")] // All letters
        [InlineData("123")] // All numbers
        [InlineData("1234567890")] // Max length (10)
        [InlineData("  ORG123  ")] // Whitespace (will be trimmed)
        public void SetIdentifier_WithValidIdentifier_SetsIdentifier(string validIdentifier)
        {
            // Arrange
            var org = new Organization(
                CreateValidGuid(), "TEMP", "Legal Name", "", "Address", "TAX123",
                OrganizationType.SHIPPING_AGENT);

            // Act
            org.SetIdentifier(validIdentifier);

            // Assert
            Assert.Equal(validIdentifier.Trim(), org.Identifier);
        }

        [Theory]
        [InlineData("")] // Empty
        [InlineData("   ")] // Whitespace only
        [InlineData(null)] // Null
        public void SetIdentifier_WithEmptyOrNullIdentifier_ThrowsArgumentException(string? invalidIdentifier)
        {
            // Arrange
            var org = new Organization(
                CreateValidGuid(), "TEMP", "Legal Name", "", "Address", "TAX123",
                OrganizationType.SHIPPING_AGENT);

            // Act & Assert
            var exception = Assert.Throws<ArgumentException>(() => org.SetIdentifier(invalidIdentifier));
            Assert.Contains("Identifier is required", exception.Message);
        }

        [Fact]
        public void SetIdentifier_WithTooLongIdentifier_ThrowsArgumentException()
        {
            // Arrange
            var org = new Organization(
                CreateValidGuid(), "TEMP", "Legal Name", "", "Address", "TAX123",
                OrganizationType.SHIPPING_AGENT);
            var tooLong = "12345678901"; // 11 characters (max is 10)

            // Act & Assert
            var exception = Assert.Throws<ArgumentException>(() => org.SetIdentifier(tooLong));
            Assert.Contains("max length is 10", exception.Message);
        }

        [Theory]
        [InlineData("ORG-123")] // Contains hyphen
        [InlineData("ORG 123")] // Contains space
        [InlineData("ORG@123")] // Contains special character
        public void SetIdentifier_WithNonAlphanumericCharacters_ThrowsArgumentException(string invalidIdentifier)
        {
            // Arrange
            var org = new Organization(
                CreateValidGuid(), "TEMP", "Legal Name", "", "Address", "TAX123",
                OrganizationType.SHIPPING_AGENT);

            // Act & Assert
            var exception = Assert.Throws<ArgumentException>(() => org.SetIdentifier(invalidIdentifier));
            Assert.Contains("must be alphanumeric", exception.Message);
        }

        #endregion

        #region Legal Name Validation Tests

        [Theory]
        [InlineData("Test Organization Ltd.")]
        [InlineData("A")] // Single character
        [InlineData("  Test Organization  ")] // Whitespace (will be trimmed)
        public void SetLegalName_WithValidName_SetsLegalName(string validName)
        {
            // Arrange
            var org = new Organization(
                CreateValidGuid(), "ORG1", "Temp", "", "Address", "TAX123",
                OrganizationType.SHIPPING_AGENT);

            // Act
            org.SetLegalName(validName);

            // Assert
            Assert.Equal(validName.Trim(), org.LegalName);
        }

        [Theory]
        [InlineData("")] // Empty
        [InlineData("   ")] // Whitespace only
        [InlineData(null)] // Null
        public void SetLegalName_WithEmptyOrNullName_ThrowsArgumentException(string? invalidName)
        {
            // Arrange
            var org = new Organization(
                CreateValidGuid(), "ORG1", "Temp", "", "Address", "TAX123",
                OrganizationType.SHIPPING_AGENT);

            // Act & Assert
            var exception = Assert.Throws<ArgumentException>(() => org.SetLegalName(invalidName));
            Assert.Contains("Legal name is required", exception.Message);
        }

        #endregion

        #region Alternative Name Tests

        [Theory]
        [InlineData("ShortName")]
        [InlineData("")]
        [InlineData("  AltName  ")] // Whitespace (will be trimmed)
        public void SetAlternativeName_WithVariousValues_SetsAlternativeName(string alternativeName)
        {
            // Arrange
            var org = new Organization(
                CreateValidGuid(), "ORG1", "Legal Name", "", "Address", "TAX123",
                OrganizationType.SHIPPING_AGENT);

            // Act
            org.SetAlternativeName(alternativeName);

            // Assert
            var expected = alternativeName?.Trim() ?? string.Empty;
            Assert.Equal(expected, org.AlternativeName);
        }

        #endregion

        #region Address Line Validation Tests

        [Theory]
        [InlineData("123 Main Street, City, Country")]
        [InlineData("A")] // Single character
        [InlineData("  123 Main St  ")] // Whitespace (will be trimmed)
        public void SetAddressLine_WithValidAddress_SetsAddressLine(string validAddress)
        {
            // Arrange
            var org = new Organization(
                CreateValidGuid(), "ORG1", "Legal Name", "", "Temp", "TAX123",
                OrganizationType.SHIPPING_AGENT);

            // Act
            org.SetAddressLine(validAddress);

            // Assert
            Assert.Equal(validAddress.Trim(), org.AddressLine);
        }

        [Theory]
        [InlineData("")] // Empty
        [InlineData("   ")] // Whitespace only
        [InlineData(null)] // Null
        public void SetAddressLine_WithEmptyOrNullAddress_ThrowsArgumentException(string? invalidAddress)
        {
            // Arrange
            var org = new Organization(
                CreateValidGuid(), "ORG1", "Legal Name", "", "Temp", "TAX123",
                OrganizationType.SHIPPING_AGENT);

            // Act & Assert
            var exception = Assert.Throws<ArgumentException>(() => org.SetAddressLine(invalidAddress));
            Assert.Contains("Address is required", exception.Message);
        }

        #endregion

        #region Tax Number Validation Tests

        [Theory]
        [InlineData("PT123456789")] // Portuguese format
        [InlineData("ES12345678Z")] // Spanish format
        [InlineData("123456789")] // Generic number
        [InlineData("  PT123456789  ")] // Whitespace (will be trimmed)
        public void SetTaxNumber_WithValidTaxNumber_SetsTaxNumber(string validTaxNumber)
        {
            // Arrange
            var org = new Organization(
                CreateValidGuid(), "ORG1", "Legal Name", "", "Address", "TEMP",
                OrganizationType.SHIPPING_AGENT);

            // Act
            org.SetTaxNumber(validTaxNumber);

            // Assert
            Assert.Equal(validTaxNumber.Trim(), org.TaxNumber);
        }

        [Theory]
        [InlineData("")] // Empty
        [InlineData("   ")] // Whitespace only
        [InlineData(null)] // Null
        public void SetTaxNumber_WithEmptyOrNullTaxNumber_ThrowsArgumentException(string? invalidTaxNumber)
        {
            // Arrange
            var org = new Organization(
                CreateValidGuid(), "ORG1", "Legal Name", "", "Address", "TEMP",
                OrganizationType.SHIPPING_AGENT);

            // Act & Assert
            var exception = Assert.Throws<ArgumentException>(() => org.SetTaxNumber(invalidTaxNumber));
            Assert.Contains("Tax number is required", exception.Message);
        }

        #endregion

        #region Representative Management Tests

        [Fact]
        public void Constructor_WithUniqueRepresentativeEmails_CreatesOrganization()
        {
            // Arrange
            var id = CreateValidGuid();
            var representatives = new[]
            {
                CreateValidRepresentative("rep1@company.com"),
                CreateValidRepresentative("rep2@company.com")
            };

            // Act
            var org = new Organization(id, "ORG1", "Legal Name", "", "Address", "TAX123",
                OrganizationType.SHIPPING_AGENT, representatives);

            // Assert
            Assert.Equal(2, org.Representatives.Count);
        }

        [Fact]
        public void Constructor_WithDuplicateRepresentativeEmails_ThrowsArgumentException()
        {
            // Arrange
            var id = CreateValidGuid();
            var representatives = new[]
            {
                CreateValidRepresentative("duplicate@company.com"),
                CreateValidRepresentative("duplicate@company.com") // Duplicate!
            };

            // Act & Assert
            var exception = Assert.Throws<ArgumentException>(() =>
                new Organization(id, "ORG1", "Legal Name", "", "Address", "TAX123",
                    OrganizationType.SHIPPING_AGENT, representatives));
            Assert.Contains("emails must be unique", exception.Message);
        }

        [Fact]
        public void Constructor_WithDuplicateEmailsDifferentCase_ThrowsArgumentException()
        {
            // Arrange
            var id = CreateValidGuid();
            var representatives = new[]
            {
                CreateValidRepresentative("Email@Company.com"),
                CreateValidRepresentative("email@company.com") // Same email, different case
            };

            // Act & Assert
            var exception = Assert.Throws<ArgumentException>(() =>
                new Organization(id, "ORG1", "Legal Name", "", "Address", "TAX123",
                    OrganizationType.SHIPPING_AGENT, representatives));
            Assert.Contains("emails must be unique", exception.Message);
        }

        [Fact]
        public void AddRepresentatives_WithValidRepresentatives_AddsToCollection()
        {
            // Arrange
            var org = new Organization(
                CreateValidGuid(), "ORG1", "Legal Name", "", "Address", "TAX123",
                OrganizationType.SHIPPING_AGENT);
            var newReps = new[]
            {
                CreateValidRepresentative("new1@company.com"),
                CreateValidRepresentative("new2@company.com")
            };

            // Act
            org.AddRepresentatives(newReps);

            // Assert
            Assert.Equal(2, org.Representatives.Count);
        }

        #endregion

        #region Integration Tests - Full Entity Creation

        [Fact]
        public void CreateShippingAgentOrganization_WithAllRequiredFields_Succeeds()
        {
            // Arrange
            var id = CreateValidGuid();
            var identifier = "MAERSK";
            var legalName = "A.P. Moller - Maersk Group";
            var alternativeName = "Maersk";
            var addressLine = "Esplanaden 50, 1098 Copenhagen, Denmark";
            var taxNumber = "DK12345678";
            var type = OrganizationType.SHIPPING_AGENT;
            var reps = new[]
            {
                CreateValidRepresentative("john.doe@maersk.com"),
                CreateValidRepresentative("jane.smith@maersk.com")
            };

            // Act
            var org = new Organization(id, identifier, legalName, alternativeName, addressLine, taxNumber, type, reps);

            // Assert
            Assert.NotNull(org);
            Assert.Equal(identifier, org.Identifier);
            Assert.Equal(legalName, org.LegalName);
            Assert.Equal(alternativeName, org.AlternativeName);
            Assert.Equal(addressLine, org.AddressLine);
            Assert.Equal(taxNumber, org.TaxNumber);
            Assert.Equal(type, org.Type);
            Assert.Equal(2, org.Representatives.Count);
        }

        [Fact]
        public void CreatePortAuthorityOrganization_WithoutRepresentatives_Succeeds()
        {
            // Arrange
            var id = CreateValidGuid();
            var identifier = "PORTLX";
            var legalName = "Port of Leixões Authority";
            var alternativeName = "APL";
            var addressLine = "Porto, Portugal";
            var taxNumber = "PT500000000";
            var type = OrganizationType.PORT_AUTHORITY;

            // Act
            var org = new Organization(id, identifier, legalName, alternativeName, addressLine, taxNumber, type, null);

            // Assert
            Assert.NotNull(org);
            Assert.Equal(type, org.Type);
            Assert.Empty(org.Representatives);
        }

        #endregion
    }
}
