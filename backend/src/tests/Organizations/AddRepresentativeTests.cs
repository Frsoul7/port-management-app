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
    /// COMPONENTS UNDER TEST: OrganizationsController, OrganizationRepository, PortDbContext, Organization + Representative (Domain Entities)
    /// TEST OBJECTIVE: Validate adding representatives to existing organizations (US 2.2.6 - AC1).
    ///                 Tests HTTP POST requests, representative creation with required fields (name, email, phone, nif),
    ///                 duplicate email prevention within the same organization, email format validation,
    ///                 and successful association of representatives to organizations by GUID or identifier.
    /// </summary>
    public class AddRepresentativeTests : OrganizationTestBase
    {
        /// <summary>
        /// Test 1: Add representative to organization by GUID
        /// AC1: Add representatives after organization creation
        /// </summary>
        [Fact]
        public async Task AddRepresentative_ByOrganizationId_ReturnsSuccess()
        {
            // Arrange - Create organization first
            var createDto = new CreateOrganizationDto
            {
                Identifier = "SHIP100",
                LegalName = "Maritime Services Ltd",
                AlternativeName = "Maritime Srv",
                Address = "Port Avenue, Porto",
                TaxNumber = "PT111222333",
                Type = OrganizationType.SHIPPING_AGENT,
                Representatives = new List<RepresentativeInputDto>
                {
                    new RepresentativeInputDto
                    {
                        Name = "Initial Representative",
                        CitizenId = "11111111",
                        Nationality = "PT",
                        Email = "initial.rep@gmail.com",
                        Phone = "+351910100100"
                    }
                }
            };

            var createResult = await Controller.Post(createDto);
            var createdResponse = (OrganizationResponseDto)((CreatedAtActionResult)createResult).Value!;
            var orgId = Guid.Parse(createdResponse.Id);

            var newRepDto = new AddRepresentativesDto
            {
                Representatives = new List<RepresentativeInputDto>
                {
                    new RepresentativeInputDto
                    {
                        Name = "New Representative",
                        CitizenId = "22222222",
                        Nationality = "ES",
                        Email = "newrep@gmail.com",
                        Phone = "+351910100200"
                    }
                }
            };

            // Act
            var result = await Controller.AddRepresentativesById(orgId, newRepDto);

            // Assert
            var createdResult = Assert.IsType<CreatedResult>(result);
            var response = Assert.IsType<List<RepresentativeResponseDto>>(createdResult.Value);
            
            Assert.Single(response);
            Assert.Equal("New Representative", response[0].Name);
            Assert.Equal("newrep@gmail.com", response[0].Email);
        }

        /// <summary>
        /// Test 2: Add representative to organization by identifier (alphanumeric code)
        /// AC1: Add representatives after organization creation using identifier
        /// </summary>
        [Fact]
        public async Task AddRepresentative_ByIdentifier_ReturnsSuccess()
        {
            // Arrange - Create organization first
            var createDto = new CreateOrganizationDto
            {
                Identifier = "SHIP200",
                LegalName = "Ocean Logistics Inc",
                AlternativeName = "Ocean Log",
                Address = "Coastal Road, Faro",
                TaxNumber = "PT444555666",
                Type = OrganizationType.SHIPPING_AGENT,
                Representatives = new List<RepresentativeInputDto>
                {
                    new RepresentativeInputDto
                    {
                        Name = "First Contact",
                        CitizenId = "33333333",
                        Nationality = "PT",
                        Email = "first.contact@gmail.com",
                        Phone = "+351910200100"
                    }
                }
            };

            await Controller.Post(createDto);

            var newRepDto = new AddRepresentativesDto
            {
                Representatives = new List<RepresentativeInputDto>
                {
                    new RepresentativeInputDto
                    {
                        Name = "Second Contact",
                        CitizenId = "44444444",
                        Nationality = "FR",
                        Email = "second.contact@gmail.com",
                        Phone = "+351910200200"
                    }
                }
            };

            // Act
            var result = await Controller.AddRepresentativesByIdentifier("SHIP200", newRepDto);

            // Assert
            var createdResult = Assert.IsType<CreatedResult>(result);
            var response = Assert.IsType<List<RepresentativeResponseDto>>(createdResult.Value);
            
            Assert.Single(response);
            Assert.Equal("Second Contact", response[0].Name);
        }

        /// <summary>
        /// Test 3: Add multiple representatives at once
        /// AC1: System should support adding multiple representatives
        /// </summary>
        [Fact]
        public async Task AddRepresentative_MultipleAtOnce_ReturnsSuccess()
        {
            // Arrange - Create organization first
            var createDto = new CreateOrganizationDto
            {
                Identifier = "SHIP300",
                LegalName = "Cargo Handlers Ltd",
                AlternativeName = "Cargo Handle",
                Address = "Industrial Zone, Setúbal",
                TaxNumber = "PT777888999",
                Type = OrganizationType.SHIPPING_AGENT,
                Representatives = new List<RepresentativeInputDto>
                {
                    new RepresentativeInputDto
                    {
                        Name = "Manager",
                        CitizenId = "55555555",
                        Nationality = "PT",
                        Email = "manager.cargo@gmail.com",
                        Phone = "+351910300100"
                    }
                }
            };

            var createResult = await Controller.Post(createDto);
            var createdResponse = (OrganizationResponseDto)((CreatedAtActionResult)createResult).Value!;
            var orgId = Guid.Parse(createdResponse.Id);

            var newReps = new AddRepresentativesDto
            {
                Representatives = new List<RepresentativeInputDto>
                {
                    new RepresentativeInputDto
                    {
                        Name = "Assistant One",
                        CitizenId = "66666666",
                        Nationality = "ES",
                        Email = "assistant1@gmail.com",
                        Phone = "+351910300200"
                    },
                    new RepresentativeInputDto
                    {
                        Name = "Assistant Two",
                        CitizenId = "77777777",
                        Nationality = "IT",
                        Email = "assistant2@gmail.com",
                        Phone = "+351910300300"
                    }
                }
            };

            // Act
            var result = await Controller.AddRepresentativesById(orgId, newReps);

            // Assert
            var createdResult = Assert.IsType<CreatedResult>(result);
            var response = Assert.IsType<List<RepresentativeResponseDto>>(createdResult.Value);
            
            Assert.Equal(2, response.Count);
            Assert.Contains(response, r => r.Name == "Assistant One");
            Assert.Contains(response, r => r.Name == "Assistant Two");
        }

        /// <summary>
        /// Test 4: Reject duplicate email when adding representative
        /// AC2: Email and phone must be unique system-wide
        /// </summary>
        [Fact]
        public async Task AddRepresentative_DuplicateEmail_ReturnsBadRequest()
        {
            // Arrange - Create organization with existing representative
            var createDto = new CreateOrganizationDto
            {
                Identifier = "SHIP400",
                LegalName = "Unique Contact Ltd",
                AlternativeName = "Unique",
                Address = "Test Address",
                TaxNumber = "PT123123123",
                Type = OrganizationType.SHIPPING_AGENT,
                Representatives = new List<RepresentativeInputDto>
                {
                    new RepresentativeInputDto
                    {
                        Name = "Existing Rep",
                        CitizenId = "88888888",
                        Nationality = "PT",
                        Email = "existing.unique@gmail.com",
                        Phone = "+351910400100"
                    }
                }
            };

            var createResult = await Controller.Post(createDto);
            var createdResponse = (OrganizationResponseDto)((CreatedAtActionResult)createResult).Value!;
            var orgId = Guid.Parse(createdResponse.Id);

            var duplicateEmailDto = new AddRepresentativesDto
            {
                Representatives = new List<RepresentativeInputDto>
                {
                    new RepresentativeInputDto
                    {
                        Name = "Duplicate Email Person",
                        CitizenId = "99999999",
                        Nationality = "ES",
                        Email = "existing.unique@gmail.com",  // Duplicate email
                        Phone = "+351910400200"
                    }
                }
            };

            // Act
            var result = await Controller.AddRepresentativesById(orgId, duplicateEmailDto);

            // Assert
            var conflictResult = Assert.IsType<ConflictObjectResult>(result);
        }
    }
}
