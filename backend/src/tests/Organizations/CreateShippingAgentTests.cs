using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using Xunit;
using Microsoft.AspNetCore.Mvc;
using DDDNetCore.Domain.Organizations;
using DDDNetCore.Application.DTOs.Organizations;
using DDDNetCore.Tests.Organizations.Base;

namespace DDDNetCore.Tests.Organizations
{
    /// <summary>
    /// TEST TYPE: Integration Test
    /// COMPONENTS UNDER TEST: OrganizationsController, OrganizationRepository, PortDbContext, Organization (Domain Entity)
    /// TEST OBJECTIVE: Validate end-to-end shipping agent organization creation (US 2.2.5).
    ///                 Tests HTTP POST requests, DTO validation, required fields (identifier, legal name, alternative name, address, tax number),
    ///                 representative assignment (at least one required at registration), organization type enforcement (ShippingAgent),
    ///                 duplicate identifier prevention, and database persistence.
    /// </summary>
    public class CreateShippingAgentTests : OrganizationTestBase
    {
        /// <summary>
        /// Test 1: Create shipping agent with all required fields
        /// AC1: Must have identifier, legal name, alternative name, address, and tax number
        /// AC2: Must include at least one representative at registration
        /// </summary>
        [Fact]
        public async Task CreateShippingAgent_ValidData_ReturnsCreatedOrganization()
        {
            // Arrange
            var createDto = new CreateOrganizationDto
            {
                Identifier = "SHIP001",
                LegalName = "Global Shipping Ltd",
                AlternativeName = "Global Ship",
                Address = "123 Harbor Road, Lisbon",
                TaxNumber = "PT123456789",
                Type = OrganizationType.SHIPPING_AGENT,
                Representatives = new List<RepresentativeInputDto>
                {
                    new RepresentativeInputDto
                    {
                        Name = "John Smith",
                        CitizenId = "12345678",
                        Nationality = "PT",
                        Email = "john.smith@gmail.com",
                        Phone = "+351910000001"
                    }
                }
            };

            // Act
            var result = await Controller.Post(createDto);

            // Assert
            var createdResult = Assert.IsType<CreatedAtActionResult>(result);
            var response = Assert.IsType<OrganizationResponseDto>(createdResult.Value);
            
            Assert.Equal("SHIP001", response.Identifier);
            Assert.Equal("Global Shipping Ltd", response.LegalName);
            Assert.Equal("Global Ship", response.AlternativeName);
            Assert.Equal("123 Harbor Road, Lisbon", response.Address);
            Assert.Equal("PT123456789", response.TaxNumber);
            Assert.Equal("SHIPPING_AGENT", response.Type);
            Assert.Single(response.Representatives);
            Assert.Equal("John Smith", response.Representatives[0].Name);
        }

        /// <summary>
        /// Test 2: Create shipping agent with alphanumeric identifier (max 10 chars)
        /// Client clarification: Identifier is alphanumeric code (max length 10)
        /// </summary>
        [Fact]
        public async Task CreateShippingAgent_AlphanumericIdentifier_AcceptsValidFormat()
        {
            // Arrange
            var createDto = new CreateOrganizationDto
            {
                Identifier = "ABC123XYZ",  // Alphanumeric, 9 chars
                LegalName = "Test Shipping Co",
                AlternativeName = "Test Ship",
                Address = "Test Address",
                TaxNumber = "PT999888777",
                Type = OrganizationType.SHIPPING_AGENT,
                Representatives = new List<RepresentativeInputDto>
                {
                    new RepresentativeInputDto
                    {
                        Name = "Jane Doe",
                        CitizenId = "87654321",
                        Nationality = "PT",
                        Email = "jane@gmail.com",
                        Phone = "+351920000001"
                    }
                }
            };

            // Act
            var result = await Controller.Post(createDto);

            // Assert
            var createdResult = Assert.IsType<CreatedAtActionResult>(result);
            var response = Assert.IsType<OrganizationResponseDto>(createdResult.Value);
            
            Assert.Equal("ABC123XYZ", response.Identifier);
        }

        /// <summary>
        /// Test 3: Create shipping agent with one legal and one alternative name
        /// Client clarification: One legal name and one alternative is enough
        /// AC1: Legal and alternative names required
        /// </summary>
        [Fact]
        public async Task CreateShippingAgent_LegalAndAlternativeNames_BothStored()
        {
            // Arrange
            var createDto = new CreateOrganizationDto
            {
                Identifier = "NAMES01",
                LegalName = "Maritime Transport Solutions Limited",
                AlternativeName = "MTS Ltd",
                Address = "456 Port Avenue, Porto",
                TaxNumber = "PT111222333",
                Type = OrganizationType.SHIPPING_AGENT,
                Representatives = new List<RepresentativeInputDto>
                {
                    new RepresentativeInputDto
                    {
                        Name = "Alice Waters",
                        CitizenId = "11112222",
                        Nationality = "GB",
                        Email = "alice@gmail.com",
                        Phone = "+351930000001"
                    }
                }
            };

            // Act
            var result = await Controller.Post(createDto);

            // Assert
            var createdResult = Assert.IsType<CreatedAtActionResult>(result);
            var response = Assert.IsType<OrganizationResponseDto>(createdResult.Value);
            
            Assert.Equal("Maritime Transport Solutions Limited", response.LegalName);
            Assert.Equal("MTS Ltd", response.AlternativeName);
        }

        /// <summary>
        /// Test 4: Create shipping agent with European tax numbers
        /// Client clarification: System must support Tax Numbers of every European Country
        /// </summary>
        [Theory]
        [InlineData("PT123456789", "Portuguese")]
        [InlineData("DE123456789", "German")]
        [InlineData("FR12345678901", "French")]
        [InlineData("ES12345678X", "Spanish")]
        [InlineData("IT12345678901", "Italian")]
        public async Task CreateShippingAgent_EuropeanTaxNumbers_AcceptsVariousFormats(string taxNumber, string country)
        {
            // Arrange
            var createDto = new CreateOrganizationDto
            {
                Identifier = $"TAX{country.Substring(0, 3).ToUpper()}",
                LegalName = $"{country} Shipping Company",
                AlternativeName = $"{country} Ship",
                Address = $"Test Address {country}",
                TaxNumber = taxNumber,
                Type = OrganizationType.SHIPPING_AGENT,
                Representatives = new List<RepresentativeInputDto>
                {
                    new RepresentativeInputDto
                    {
                        Name = $"Rep {country}",
                        CitizenId = "12345678",
                        Nationality = "PT",
                        Email = $"rep.{country.ToLower()}@gmail.com",
                        Phone = "+351940000001"
                    }
                }
            };

            // Act
            var result = await Controller.Post(createDto);

            // Assert
            var createdResult = Assert.IsType<CreatedAtActionResult>(result);
            var response = Assert.IsType<OrganizationResponseDto>(createdResult.Value);
            
            Assert.Equal(taxNumber, response.TaxNumber);
        }

        /// <summary>
        /// Test 5: Create shipping agent with multiple representatives
        /// AC2: Must include at least one representative (can have more)
        /// </summary>
        [Fact]
        public async Task CreateShippingAgent_MultipleRepresentatives_AllStored()
        {
            // Arrange
            var createDto = new CreateOrganizationDto
            {
                Identifier = "MULTI01",
                LegalName = "Multi-Rep Shipping Ltd",
                AlternativeName = "Multi Ship",
                Address = "789 Dock Street, Faro",
                TaxNumber = "PT444555666",
                Type = OrganizationType.SHIPPING_AGENT,
                Representatives = new List<RepresentativeInputDto>
                {
                    new RepresentativeInputDto
                    {
                        Name = "Rep One",
                        CitizenId = "11111111",
                        Nationality = "PT",
                        Email = "rep1@gmail.com",
                        Phone = "+351950000001"
                    },
                    new RepresentativeInputDto
                    {
                        Name = "Rep Two",
                        CitizenId = "22222222",
                        Nationality = "ES",
                        Email = "rep2@gmail.com",
                        Phone = "+351960000001"
                    }
                }
            };

            // Act
            var result = await Controller.Post(createDto);

            // Assert
            var createdResult = Assert.IsType<CreatedAtActionResult>(result);
            var response = Assert.IsType<OrganizationResponseDto>(createdResult.Value);
            
            Assert.Equal(2, response.Representatives.Count);
            Assert.Contains(response.Representatives, r => r.Name == "Rep One");
            Assert.Contains(response.Representatives, r => r.Name == "Rep Two");
        }

        /// <summary>
        /// Test 6: Representative includes all required fields
        /// AC3: Representatives must include name, citizen ID, nationality, email, and phone
        /// </summary>
        [Fact]
        public async Task CreateShippingAgent_RepresentativeData_AllFieldsStored()
        {
            // Arrange
            var createDto = new CreateOrganizationDto
            {
                Identifier = "REPDATA",
                LegalName = "Complete Rep Data Ltd",
                AlternativeName = "CRD Ltd",
                Address = "Complete Address",
                TaxNumber = "PT777888999",
                Type = OrganizationType.SHIPPING_AGENT,
                Representatives = new List<RepresentativeInputDto>
                {
                    new RepresentativeInputDto
                    {
                        Name = "Complete Representative",
                        CitizenId = "98765432",
                        Nationality = "FR",
                        Email = "complete@gmail.com",
                        Phone = "+351970000001"
                    }
                }
            };

            // Act
            var result = await Controller.Post(createDto);

            // Assert
            var createdResult = Assert.IsType<CreatedAtActionResult>(result);
            var response = Assert.IsType<OrganizationResponseDto>(createdResult.Value);
            
            var rep = response.Representatives[0];
            Assert.Equal("Complete Representative", rep.Name);
            Assert.Equal("98765432", rep.CitizenId);
            Assert.Equal("FR", rep.Nationality);
            Assert.Equal("complete@gmail.com", rep.Email);
            Assert.Equal("+351970000001", rep.Phone);
        }

        /// <summary>
        /// Test 7: Email and phone stored for notifications and authentication
        /// AC4: Email and phone must be used for system notifications and authentication
        /// </summary>
        [Fact]
        public async Task CreateShippingAgent_ContactInformation_StoredForNotifications()
        {
            // Arrange
            var createDto = new CreateOrganizationDto
            {
                Identifier = "CONTACT1",
                LegalName = "Contact Test Shipping",
                AlternativeName = "CTS",
                Address = "Contact Address",
                TaxNumber = "PT123123123",
                Type = OrganizationType.SHIPPING_AGENT,
                Representatives = new List<RepresentativeInputDto>
                {
                    new RepresentativeInputDto
                    {
                        Name = "Contact Rep",
                        CitizenId = "55555555",
                        Nationality = "PT",
                        Email = "contact@gmail.com",
                        Phone = "+351980000001"
                    }
                }
            };

            // Act
            var result = await Controller.Post(createDto);

            // Assert
            var createdResult = Assert.IsType<CreatedAtActionResult>(result);
            var response = Assert.IsType<OrganizationResponseDto>(createdResult.Value);
            
            // Verify email and phone are stored (required for notifications/auth)
            Assert.NotNull(response.Representatives[0].Email);
            Assert.NotNull(response.Representatives[0].Phone);
            Assert.Contains("@", response.Representatives[0].Email);
        }

        /// <summary>
        /// Test 8: Identifier must be unique (case-insensitive)
        /// </summary>
        [Fact]
        public async Task CreateShippingAgent_DuplicateIdentifier_ReturnsConflict()
        {
            // Arrange
            var createDto1 = new CreateOrganizationDto
            {
                Identifier = "UNIQUE01",
                LegalName = "First Organization",
                AlternativeName = "First Org",
                Address = "First Address",
                TaxNumber = "PT111111111",
                Type = OrganizationType.SHIPPING_AGENT,
                Representatives = new List<RepresentativeInputDto>
                {
                    new RepresentativeInputDto
                    {
                        Name = "First Rep",
                        CitizenId = "11111111",
                        Nationality = "PT",
                        Email = "first@gmail.com",
                        Phone = "+351910000001"
                    }
                }
            };

            var createDto2 = new CreateOrganizationDto
            {
                Identifier = "unique01",  // Same identifier, different case
                LegalName = "Second Organization",
                AlternativeName = "Second Org",
                Address = "Second Address",
                TaxNumber = "PT222222222",
                Type = OrganizationType.SHIPPING_AGENT,
                Representatives = new List<RepresentativeInputDto>
                {
                    new RepresentativeInputDto
                    {
                        Name = "Second Rep",
                        CitizenId = "22222222",
                        Nationality = "PT",
                        Email = "second@gmail.com",
                        Phone = "+351920000001"
                    }
                }
            };

            // Act
            await Controller.Post(createDto1);
            var result = await Controller.Post(createDto2);

            // Assert
            Assert.IsType<ConflictObjectResult>(result);
        }

        /// <summary>
        /// Test 9: Create organization and retrieve it by ID
        /// </summary>
        [Fact]
        public async Task CreateShippingAgent_ThenRetrieve_ReturnsCorrectData()
        {
            // Arrange
            var createDto = new CreateOrganizationDto
            {
                Identifier = "RETRIEVE",
                LegalName = "Retrievable Shipping",
                AlternativeName = "RS Ltd",
                Address = "Retrieve Address",
                TaxNumber = "PT333333333",
                Type = OrganizationType.SHIPPING_AGENT,
                Representatives = new List<RepresentativeInputDto>
                {
                    new RepresentativeInputDto
                    {
                        Name = "Retrieve Rep",
                        CitizenId = "33333333",
                        Nationality = "PT",
                        Email = "retrieve@gmail.com",
                        Phone = "+351930000001"
                    }
                }
            };

            // Act
            var createResult = await Controller.Post(createDto);
            var created = ((CreatedAtActionResult)createResult).Value as OrganizationResponseDto;
            
            var getResult = await Controller.Get(created!.OrganizationId);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(getResult);
            var response = Assert.IsType<OrganizationResponseDto>(okResult.Value);
            
            Assert.Equal("RETRIEVE", response.Identifier);
            Assert.Equal("Retrievable Shipping", response.LegalName);
            Assert.Single(response.Representatives);
        }
    }
}
