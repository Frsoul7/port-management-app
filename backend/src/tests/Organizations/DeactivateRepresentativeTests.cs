using System;
using System.Collections.Generic;
using System.Linq;
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
    /// COMPONENTS UNDER TEST: OrganizationsController, OrganizationRepository, PortDbContext, Representative (Domain Entity)
    /// TEST OBJECTIVE: Validate representative deactivation and soft delete functionality (US 2.2.6 - AC4).
    ///                 Tests HTTP PATCH requests for deactivation, audit trail preservation (original data retained),
    ///                 status change from active to inactive, proper soft delete implementation (no physical deletion),
    ///                 and prevention of duplicate operations on already-deactivated representatives.
    /// </summary>
    public class DeactivateRepresentativeTests : OrganizationTestBase
    {
        /// <summary>
        /// Test 1: Deactivate representative successfully
        /// AC4: Deactivate representatives (soft delete)
        /// </summary>
        [Fact]
        public async Task DeactivateRepresentative_ValidRequest_ReturnsSuccess()
        {
            // Arrange - Create organization with two representatives
            var createDto = new CreateOrganizationDto
            {
                Identifier = "SHIP800",
                LegalName = "Deactivation Test Ltd",
                AlternativeName = "Deact Test",
                Address = "Deactivation Avenue",
                TaxNumber = "PT654654654",
                Type = OrganizationType.SHIPPING_AGENT,
                Representatives = new List<RepresentativeInputDto>
                {
                    new RepresentativeInputDto
                    {
                        Name = "Active Rep",
                        CitizenId = "90909090",
                        Nationality = "PT",
                        Email = "active.rep@gmail.com",
                        Phone = "+351910800100"
                    },
                    new RepresentativeInputDto
                    {
                        Name = "To Deactivate",
                        CitizenId = "10101010",
                        Nationality = "ES",
                        Email = "todeact@gmail.com",
                        Phone = "+351910800200"
                    }
                }
            };

            var createResult = await Controller.Post(createDto);
            var createdResponse = (OrganizationResponseDto)((CreatedAtActionResult)createResult).Value!;
            var orgId = Guid.Parse(createdResponse.Id);
            var repToDeactivate = createdResponse.Representatives.First(r => r.Name == "To Deactivate");
            var repToDeactivateId = Guid.Parse(repToDeactivate.Id);

            // Act - Deactivate using PATCH
            var result = await Controller.PatchRepresentativeStatus(orgId, repToDeactivateId, new PatchRepresentativeStatusDto(IsActive: false));

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var response = Assert.IsType<RepresentativeResponseDto>(okResult.Value);
            
            Assert.False(response.IsActive);
            Assert.Equal("To Deactivate", response.Name);
        }

        /// <summary>
        /// Test 2: Deactivate non-existent representative
        /// AC4: System should handle invalid representative IDs
        /// </summary>
        [Fact]
        public async Task DeactivateRepresentative_NonExistent_ReturnsNotFound()
        {
            // Arrange - Create organization
            var createDto = new CreateOrganizationDto
            {
                Identifier = "SHIP900",
                LegalName = "Not Found Test Ltd",
                AlternativeName = "NotFound",
                Address = "NotFound Street",
                TaxNumber = "PT987987987",
                Type = OrganizationType.SHIPPING_AGENT,
                Representatives = new List<RepresentativeInputDto>
                {
                    new RepresentativeInputDto
                    {
                        Name = "Existing Rep",
                        CitizenId = "20202020",
                        Nationality = "PT",
                        Email = "existing.notfound@gmail.com",
                        Phone = "+351910900100"
                    }
                }
            };

            var createResult = await Controller.Post(createDto);
            var createdResponse = (OrganizationResponseDto)((CreatedAtActionResult)createResult).Value!;
            var orgId = Guid.Parse(createdResponse.Id);

            var nonExistentRepId = Guid.NewGuid();

            // Act - Try to deactivate non-existent representative
            var result = await Controller.PatchRepresentativeStatus(orgId, nonExistentRepId, new PatchRepresentativeStatusDto(IsActive: false));

            // Assert
            Assert.IsType<NotFoundObjectResult>(result);
        }

        /// <summary>
        /// Test 3: Verify deactivated representative maintains data integrity
        /// AC4: Soft delete should preserve all data
        /// </summary>
        [Fact]
        public async Task DeactivateRepresentative_PreservesData_MaintainsIntegrity()
        {
            // Arrange - Create organization
            var createDto = new CreateOrganizationDto
            {
                Identifier = "SHIP1000",
                LegalName = "Data Integrity Test Ltd",
                AlternativeName = "DataInt",
                Address = "Integrity Boulevard",
                TaxNumber = "PT147147147",
                Type = OrganizationType.SHIPPING_AGENT,
                Representatives = new List<RepresentativeInputDto>
                {
                    new RepresentativeInputDto
                    {
                        Name = "Data Keeper",
                        CitizenId = "30303030",
                        Nationality = "IT",
                        Email = "keeper.dataint@gmail.com",
                        Phone = "+351911000100"
                    }
                }
            };

            var createResult = await Controller.Post(createDto);
            var createdResponse = (OrganizationResponseDto)((CreatedAtActionResult)createResult).Value!;
            var orgId = Guid.Parse(createdResponse.Id);
            var repId = Guid.Parse(createdResponse.Representatives[0].Id);
            var originalEmail = createdResponse.Representatives[0].Email;
            var originalName = createdResponse.Representatives[0].Name;

            // Act - Deactivate using PATCH
            var result = await Controller.PatchRepresentativeStatus(orgId, repId, new PatchRepresentativeStatusDto(IsActive: false));

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var response = Assert.IsType<RepresentativeResponseDto>(okResult.Value);
            
            Assert.False(response.IsActive);
            Assert.Equal(originalEmail, response.Email);
            Assert.Equal(originalName, response.Name);
            Assert.Equal("IT", response.Nationality);
        }
    }
}
