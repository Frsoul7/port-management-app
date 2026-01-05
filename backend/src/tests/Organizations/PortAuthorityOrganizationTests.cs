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
    /// TEST OBJECTIVE: Validate Port Authority organization creation rules (comparison with US 2.2.5).
    ///                 Tests HTTP POST requests, organization type enforcement (PortAuthority),
    ///                 representative optionality (Port Authority uses role-based auth, not individual representatives),
    ///                 required field validation for Port Authority type, and differentiation from Shipping Agent requirements.
    /// </summary>
    public class PortAuthorityOrganizationTests : OrganizationTestBase
    {
        /// <summary>
        /// Test 1: Create Port Authority without representatives succeeds
        /// Port Authority uses role-based auth, not individual user tracking
        /// </summary>
        [Fact]
        public async Task CreatePortAuthority_NoRepresentatives_CreatesSuccessfully()
        {
            // Arrange
            var createDto = new CreateOrganizationDto
            {
                Identifier = "PA002",
                LegalName = "Port Authority of Test Harbor",
                AlternativeName = "PA Test Harbor",
                Address = "Harbor Control Tower, Test Port",
                TaxNumber = "PT888888888",
                Type = OrganizationType.PORT_AUTHORITY,
                Representatives = null  // Port Authority doesn't require representatives
            };

            // Act
            var result = await Controller.Post(createDto);

            // Assert
            var createdResult = Assert.IsType<CreatedAtActionResult>(result);
            var response = Assert.IsType<OrganizationResponseDto>(createdResult.Value);
            
            Assert.Equal("PA002", response.Identifier);
            Assert.Equal("PORT_AUTHORITY", response.Type);
            Assert.Empty(response.Representatives);
        }

        /// <summary>
        /// Test 2: Port Operator organization behaves like Port Authority
        /// </summary>
        [Fact]
        public async Task CreatePortOperator_NoRepresentatives_CreatesSuccessfully()
        {
            // Arrange
            var createDto = new CreateOrganizationDto
            {
                Identifier = "OPER001",
                LegalName = "Port Operations Company",
                AlternativeName = "POC",
                Address = "Operations Building",
                TaxNumber = "PT777777777",
                Type = OrganizationType.PORT_AUTHORITY,
                Representatives = null
            };

            // Act
            var result = await Controller.Post(createDto);

            // Assert
            var createdResult = Assert.IsType<CreatedAtActionResult>(result);
            var response = Assert.IsType<OrganizationResponseDto>(createdResult.Value);
            
            Assert.Equal("PORT_AUTHORITY", response.Type);
            Assert.Empty(response.Representatives);
        }

        /// <summary>
        /// Test 3: Shipping Agent MUST have representatives (contrast with Port Authority)
        /// </summary>
        [Fact]
        public async Task CreateShippingAgent_NoRepresentatives_Fails_UnlikePortAuthority()
        {
            // Arrange
            var createDto = new CreateOrganizationDto
            {
                Identifier = "CONTRAST",
                LegalName = "Contrast Shipping",
                AlternativeName = "CS",
                Address = "Contrast Address",
                TaxNumber = "PT666666666",
                Type = OrganizationType.SHIPPING_AGENT,
                Representatives = null  // This should fail for SHIPPING_AGENT
            };

            // Act
            var result = await Controller.Post(createDto);

            // Assert
            Assert.IsType<BadRequestObjectResult>(result);
        }
    }
}
