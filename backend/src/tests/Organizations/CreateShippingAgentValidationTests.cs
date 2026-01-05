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
    /// TEST OBJECTIVE: Validate input validation and business rules for shipping agent creation (US 2.2.5).
    ///                 Tests HTTP POST validation, required field enforcement, representative count validation (minimum 1 required),
    ///                 tax number format validation, empty/null field rejection, and business constraint enforcement.
    /// </summary>
    public class CreateShippingAgentValidationTests : OrganizationTestBase
    {
        /// <summary>
        /// Test 1: Shipping agent without representatives fails
        /// AC2: Must include at least one representative at registration
        /// </summary>
        [Fact]
        public async Task CreateShippingAgent_NoRepresentatives_ReturnsBadRequest()
        {
            // Arrange
            var createDto = new CreateOrganizationDto
            {
                Identifier = "NOREP01",
                LegalName = "No Representative Shipping",
                AlternativeName = "NRS",
                Address = "Test Address",
                TaxNumber = "PT123456789",
                Type = OrganizationType.SHIPPING_AGENT,
                Representatives = null  // No representatives
            };

            // Act
            var result = await Controller.Post(createDto);

            // Assert
            var badRequestResult = Assert.IsType<BadRequestObjectResult>(result);
            Assert.NotNull(badRequestResult.Value);
        }

        /// <summary>
        /// Test 2: Shipping agent with empty representatives list fails
        /// AC2: Must include at least one representative
        /// </summary>
        [Fact]
        public async Task CreateShippingAgent_EmptyRepresentativesList_ReturnsBadRequest()
        {
            // Arrange
            var createDto = new CreateOrganizationDto
            {
                Identifier = "EMPTY01",
                LegalName = "Empty Reps Shipping",
                AlternativeName = "ERS",
                Address = "Test Address",
                TaxNumber = "PT123456789",
                Type = OrganizationType.SHIPPING_AGENT,
                Representatives = new List<RepresentativeInputDto>()  // Empty list
            };

            // Act
            var result = await Controller.Post(createDto);

            // Assert
            var badRequestResult = Assert.IsType<BadRequestObjectResult>(result);
            Assert.NotNull(badRequestResult.Value);
        }

        /// <summary>
        /// Test 3: Missing alternative name fails
        /// Client clarification: One legal name and one alternative required
        /// </summary>
        [Fact]
        public async Task CreateShippingAgent_MissingAlternativeName_ReturnsBadRequest()
        {
            // Arrange
            var createDto = new CreateOrganizationDto
            {
                Identifier = "NOALT01",
                LegalName = "No Alternative Name Shipping",
                AlternativeName = null,  // Missing alternative name
                Address = "Test Address",
                TaxNumber = "PT123456789",
                Type = OrganizationType.SHIPPING_AGENT,
                Representatives = new List<RepresentativeInputDto>
                {
                    new RepresentativeInputDto
                    {
                        Name = "Test Rep",
                        CitizenId = "12345678",
                        Nationality = "PT",
                        Email = "test@gmail.com",
                        Phone = "+351910000001"
                    }
                }
            };

            // Act
            var result = await Controller.Post(createDto);

            // Assert
            var badRequestResult = Assert.IsType<BadRequestObjectResult>(result);
            Assert.NotNull(badRequestResult.Value);
        }

        /// <summary>
        /// Test 4: Identifier exceeding 10 characters fails
        /// Client clarification: Identifier max length 10
        /// </summary>
        [Fact]
        public async Task CreateShippingAgent_IdentifierTooLong_ReturnsBadRequest()
        {
            // Arrange
            var createDto = new CreateOrganizationDto
            {
                Identifier = "12345678901",  // 11 characters (exceeds max 10)
                LegalName = "Long ID Shipping",
                AlternativeName = "LIS",
                Address = "Test Address",
                TaxNumber = "PT123456789",
                Type = OrganizationType.SHIPPING_AGENT,
                Representatives = new List<RepresentativeInputDto>
                {
                    new RepresentativeInputDto
                    {
                        Name = "Test Rep",
                        CitizenId = "12345678",
                        Nationality = "PT",
                        Email = "test@gmail.com",
                        Phone = "+351910000001"
                    }
                }
            };

            // Act
            var result = await Controller.Post(createDto);

            // Assert
            Assert.IsType<BadRequestObjectResult>(result);
        }

        /// <summary>
        /// Test 5: Non-alphanumeric identifier fails
        /// Client clarification: Identifier must be alphanumeric
        /// </summary>
        [Fact]
        public async Task CreateShippingAgent_NonAlphanumericIdentifier_ReturnsBadRequest()
        {
            // Arrange
            var createDto = new CreateOrganizationDto
            {
                Identifier = "SHIP@#$",  // Contains special characters
                LegalName = "Special Char Shipping",
                AlternativeName = "SCS",
                Address = "Test Address",
                TaxNumber = "PT123456789",
                Type = OrganizationType.SHIPPING_AGENT,
                Representatives = new List<RepresentativeInputDto>
                {
                    new RepresentativeInputDto
                    {
                        Name = "Test Rep",
                        CitizenId = "12345678",
                        Nationality = "PT",
                        Email = "test@gmail.com",
                        Phone = "+351910000001"
                    }
                }
            };

            // Act
            var result = await Controller.Post(createDto);

            // Assert
            Assert.IsType<BadRequestObjectResult>(result);
        }

        /// <summary>
        /// Test 6: Representative with invalid nationality (not 2 letters) fails
        /// AC3: Nationality must be 2-letter ISO code
        /// </summary>
        [Fact]
        public async Task CreateShippingAgent_InvalidNationality_ReturnsBadRequest()
        {
            // Arrange
            var createDto = new CreateOrganizationDto
            {
                Identifier = "BADNAT01",
                LegalName = "Bad Nationality Shipping",
                AlternativeName = "BNS",
                Address = "Test Address",
                TaxNumber = "PT123456789",
                Type = OrganizationType.SHIPPING_AGENT,
                Representatives = new List<RepresentativeInputDto>
                {
                    new RepresentativeInputDto
                    {
                        Name = "Bad Nat Rep",
                        CitizenId = "12345678",
                        Nationality = "PORTUGAL",  // Should be "PT" (2 letters)
                        Email = "test@gmail.com",
                        Phone = "+351910000001"
                    }
                }
            };

            // Act
            var result = await Controller.Post(createDto);

            // Assert
            var badRequestResult = Assert.IsType<BadRequestObjectResult>(result);
            Assert.NotNull(badRequestResult.Value);
        }

        /// <summary>
        /// Test 7: Duplicate representative email in payload fails
        /// </summary>
        [Fact]
        public async Task CreateShippingAgent_DuplicateEmailInPayload_ReturnsBadRequest()
        {
            // Arrange
            var createDto = new CreateOrganizationDto
            {
                Identifier = "DUPEMAIL",
                LegalName = "Duplicate Email Shipping",
                AlternativeName = "DES",
                Address = "Test Address",
                TaxNumber = "PT123456789",
                Type = OrganizationType.SHIPPING_AGENT,
                Representatives = new List<RepresentativeInputDto>
                {
                    new RepresentativeInputDto
                    {
                        Name = "Rep One",
                        CitizenId = "11111111",
                        Nationality = "PT",
                        Email = "same@gmail.com",
                        Phone = "+351910000001"
                    },
                    new RepresentativeInputDto
                    {
                        Name = "Rep Two",
                        CitizenId = "22222222",
                        Nationality = "PT",
                        Email = "same@gmail.com",  // Duplicate email
                        Phone = "+351920000001"
                    }
                }
            };

            // Act
            var result = await Controller.Post(createDto);

            // Assert
            var badRequestResult = Assert.IsType<BadRequestObjectResult>(result);
            Assert.NotNull(badRequestResult.Value);
        }

        /// <summary>
        /// Test 8: Duplicate representative phone in payload fails
        /// </summary>
        [Fact]
        public async Task CreateShippingAgent_DuplicatePhoneInPayload_ReturnsBadRequest()
        {
            // Arrange
            var createDto = new CreateOrganizationDto
            {
                Identifier = "DUPPHONE",
                LegalName = "Duplicate Phone Shipping",
                AlternativeName = "DPS",
                Address = "Test Address",
                TaxNumber = "PT123456789",
                Type = OrganizationType.SHIPPING_AGENT,
                Representatives = new List<RepresentativeInputDto>
                {
                    new RepresentativeInputDto
                    {
                        Name = "Rep One",
                        CitizenId = "11111111",
                        Nationality = "PT",
                        Email = "rep1.test@gmail.com",
                        Phone = "+351910000001"
                    },
                    new RepresentativeInputDto
                    {
                        Name = "Rep Two",
                        CitizenId = "22222222",
                        Nationality = "PT",
                        Email = "rep2.test@gmail.com",
                        Phone = "+351910000001"  // Duplicate phone
                    }
                }
            };

            // Act
            var result = await Controller.Post(createDto);

            // Assert
            var badRequestResult = Assert.IsType<BadRequestObjectResult>(result);
            Assert.NotNull(badRequestResult.Value);
        }

        /// <summary>
        /// Test 9: Representative email already in use system-wide fails
        /// </summary>
        [Fact]
        public async Task CreateShippingAgent_EmailAlreadyInUse_ReturnsConflict()
        {
            // Arrange - Create first organization
            var createDto1 = new CreateOrganizationDto
            {
                Identifier = "FIRST01",
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
                        Email = "existing@gmail.com",
                        Phone = "+351910000001"
                    }
                }
            };
            await Controller.Post(createDto1);

            // Arrange - Try to create second organization with same email
            var createDto2 = new CreateOrganizationDto
            {
                Identifier = "SECOND01",
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
                        Email = "existing@gmail.com",  // Email already in use
                        Phone = "+351920000001"
                    }
                }
            };

            // Act
            var result = await Controller.Post(createDto2);

            // Assert
            Assert.IsType<ConflictObjectResult>(result);
        }

        /// <summary>
        /// Test 10: Representative phone already in use system-wide fails
        /// </summary>
        [Fact]
        public async Task CreateShippingAgent_PhoneAlreadyInUse_ReturnsConflict()
        {
            // Arrange - Create first organization
            var createDto1 = new CreateOrganizationDto
            {
                Identifier = "PHONE01",
                LegalName = "Phone Test Organization",
                AlternativeName = "PTO",
                Address = "Phone Address",
                TaxNumber = "PT333333333",
                Type = OrganizationType.SHIPPING_AGENT,
                Representatives = new List<RepresentativeInputDto>
                {
                    new RepresentativeInputDto
                    {
                        Name = "Phone Rep",
                        CitizenId = "33333333",
                        Nationality = "PT",
                        Email = "phone1.test@gmail.com",
                        Phone = "+351999999999"
                    }
                }
            };
            await Controller.Post(createDto1);

            // Arrange - Try to create second organization with same phone
            var createDto2 = new CreateOrganizationDto
            {
                Identifier = "PHONE02",
                LegalName = "Phone Test 2 Organization",
                AlternativeName = "PT2O",
                Address = "Phone Address 2",
                TaxNumber = "PT444444444",
                Type = OrganizationType.SHIPPING_AGENT,
                Representatives = new List<RepresentativeInputDto>
                {
                    new RepresentativeInputDto
                    {
                        Name = "Phone Rep 2",
                        CitizenId = "44444444",
                        Nationality = "PT",
                        Email = "phone2.test@gmail.com",
                        Phone = "+351999999999"  // Phone already in use
                    }
                }
            };

            // Act
            var result = await Controller.Post(createDto2);

            // Assert
            Assert.IsType<ConflictObjectResult>(result);
        }

        /// <summary>
        /// Test 11: Missing required representative fields fails
        /// AC3: All representative fields are required
        /// </summary>
        [Fact]
        public async Task CreateShippingAgent_MissingRepresentativeName_ReturnsBadRequest()
        {
            // Arrange
            var createDto = new CreateOrganizationDto
            {
                Identifier = "MISSING",
                LegalName = "Missing Fields Shipping",
                AlternativeName = "MFS",
                Address = "Test Address",
                TaxNumber = "PT123456789",
                Type = OrganizationType.SHIPPING_AGENT,
                Representatives = new List<RepresentativeInputDto>
                {
                    new RepresentativeInputDto
                    {
                        Name = null!, // Missing name
                        CitizenId = "12345678",
                        Nationality = "PT",
                        Email = "test@gmail.com",
                        Phone = "+351910000001"
                    }
                }
            };

            // Act
            var result = await Controller.Post(createDto);

            // Assert
            Assert.IsType<BadRequestObjectResult>(result);
        }

        [Fact]
        public async Task CreateShippingAgent_MissingRepresentativeCitizenId_ReturnsBadRequest()
        {
            // Arrange
            var createDto = new CreateOrganizationDto
            {
                Identifier = "MISSING2",
                LegalName = "Missing Fields Shipping",
                AlternativeName = "MFS",
                Address = "Test Address",
                TaxNumber = "PT123456789",
                Type = OrganizationType.SHIPPING_AGENT,
                Representatives = new List<RepresentativeInputDto>
                {
                    new RepresentativeInputDto
                    {
                        Name = "Test Rep",
                        CitizenId = null!, // Missing citizen ID
                        Nationality = "PT",
                        Email = "test@gmail.com",
                        Phone = "+351910000001"
                    }
                }
            };

            // Act
            var result = await Controller.Post(createDto);

            // Assert
            Assert.IsType<BadRequestObjectResult>(result);
        }

        [Fact]
        public async Task CreateShippingAgent_MissingRepresentativeNationality_ReturnsBadRequest()
        {
            // Arrange
            var createDto = new CreateOrganizationDto
            {
                Identifier = "MISSING3",
                LegalName = "Missing Fields Shipping",
                AlternativeName = "MFS",
                Address = "Test Address",
                TaxNumber = "PT123456789",
                Type = OrganizationType.SHIPPING_AGENT,
                Representatives = new List<RepresentativeInputDto>
                {
                    new RepresentativeInputDto
                    {
                        Name = "Test Rep",
                        CitizenId = "12345678",
                        Nationality = null!, // Missing nationality
                        Email = "test@gmail.com",
                        Phone = "+351910000001"
                    }
                }
            };

            // Act
            var result = await Controller.Post(createDto);

            // Assert
            Assert.IsType<BadRequestObjectResult>(result);
        }

        [Fact]
        public async Task CreateShippingAgent_MissingRepresentativeEmail_ReturnsBadRequest()
        {
            // Arrange
            var createDto = new CreateOrganizationDto
            {
                Identifier = "MISSING4",
                LegalName = "Missing Fields Shipping",
                AlternativeName = "MFS",
                Address = "Test Address",
                TaxNumber = "PT123456789",
                Type = OrganizationType.SHIPPING_AGENT,
                Representatives = new List<RepresentativeInputDto>
                {
                    new RepresentativeInputDto
                    {
                        Name = "Test Rep",
                        CitizenId = "12345678",
                        Nationality = "PT",
                        Email = null!, // Missing email
                        Phone = "+351910000001"
                    }
                }
            };

            // Act
            var result = await Controller.Post(createDto);

            // Assert
            Assert.IsType<BadRequestObjectResult>(result);
        }
    }
}
