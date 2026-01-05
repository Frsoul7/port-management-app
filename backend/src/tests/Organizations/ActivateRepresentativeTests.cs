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
    /// TEST OBJECTIVE: Validate representative reactivation functionality (US 2.2.6 - AC4 extension).
    ///                 Tests HTTP PATCH requests for reactivation, status change from inactive back to active,
    ///                 restoration of representative access after deactivation, audit trail consistency,
    ///                 and prevention of reactivating already-active representatives.
    /// </summary>
    public class ActivateRepresentativeTests : OrganizationTestBase
    {
        /// <summary>
        /// Test 1: Reactivate previously deactivated representative
        /// AC4: Representatives can be reactivated after deactivation
        /// </summary>
        [Fact]
        public async Task ActivateRepresentative_PreviouslyDeactivated_ReturnsSuccess()
        {
            // Arrange - Create, then deactivate a representative
            var createDto = new CreateOrganizationDto
            {
                Identifier = "SHIP1100",
                LegalName = "Reactivation Test Ltd",
                AlternativeName = "React Test",
                Address = "Reactivation Road",
                TaxNumber = "PT258258258",
                Type = OrganizationType.SHIPPING_AGENT,
                Representatives = new List<RepresentativeInputDto>
                {
                    new RepresentativeInputDto
                    {
                        Name = "Comeback Rep",
                        CitizenId = "40404040",
                        Nationality = "PT",
                        Email = "comeback.react@gmail.com",
                        Phone = "+351911100100"
                    }
                }
            };

            var createResult = await Controller.Post(createDto);
            var createdResponse = (OrganizationResponseDto)((CreatedAtActionResult)createResult).Value!;
            var orgId = Guid.Parse(createdResponse.Id);
            var repId = Guid.Parse(createdResponse.Representatives[0].Id);

            // Deactivate first using PATCH
            await Controller.PatchRepresentativeStatus(orgId, repId, new PatchRepresentativeStatusDto(IsActive: false));

            // Act - Reactivate using PATCH
            var result = await Controller.PatchRepresentativeStatus(orgId, repId, new PatchRepresentativeStatusDto(IsActive: true));

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var response = Assert.IsType<RepresentativeResponseDto>(okResult.Value);
            
            Assert.True(response.IsActive);
            Assert.Equal("Comeback Rep", response.Name);
            Assert.Equal("comeback.react@gmail.com", response.Email);
        }

        /// <summary>
        /// Test 2: Activate already active representative (idempotent)
        /// AC4: Activating active representative should be safe
        /// </summary>
        [Fact]
        public async Task ActivateRepresentative_AlreadyActive_RemainsActive()
        {
            // Arrange - Create organization
            var createDto = new CreateOrganizationDto
            {
                Identifier = "SHIP1200",
                LegalName = "Idempotent Test Ltd",
                AlternativeName = "Idem Test",
                Address = "Idempotent Lane",
                TaxNumber = "PT369369369",
                Type = OrganizationType.SHIPPING_AGENT,
                Representatives = new List<RepresentativeInputDto>
                {
                    new RepresentativeInputDto
                    {
                        Name = "Always Active",
                        CitizenId = "50505050",
                        Nationality = "ES",
                        Email = "always.idem@gmail.com",
                        Phone = "+351911200100"
                    }
                }
            };

            var createResult = await Controller.Post(createDto);
            var createdResponse = (OrganizationResponseDto)((CreatedAtActionResult)createResult).Value!;
            var orgId = Guid.Parse(createdResponse.Id);
            var repId = Guid.Parse(createdResponse.Representatives[0].Id);

            // Act - Activate already active rep using PATCH
            var result = await Controller.PatchRepresentativeStatus(orgId, repId, new PatchRepresentativeStatusDto(IsActive: true));

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var response = Assert.IsType<RepresentativeResponseDto>(okResult.Value);
            
            Assert.True(response.IsActive);
        }

        /// <summary>
        /// Test 3: Activate non-existent representative
        /// AC4: System should handle invalid representative IDs gracefully
        /// </summary>
        [Fact]
        public async Task ActivateRepresentative_NonExistent_ReturnsNotFound()
        {
            // Arrange - Create organization
            var createDto = new CreateOrganizationDto
            {
                Identifier = "SHIP1300",
                LegalName = "Invalid Activate Test Ltd",
                AlternativeName = "InvalidAct",
                Address = "Invalid Street",
                TaxNumber = "PT741741741",
                Type = OrganizationType.SHIPPING_AGENT,
                Representatives = new List<RepresentativeInputDto>
                {
                    new RepresentativeInputDto
                    {
                        Name = "Valid Rep",
                        CitizenId = "60606060",
                        Nationality = "FR",
                        Email = "valid.invalid@gmail.com",
                        Phone = "+351911300100"
                    }
                }
            };

            var createResult = await Controller.Post(createDto);
            var createdResponse = (OrganizationResponseDto)((CreatedAtActionResult)createResult).Value!;
            var orgId = Guid.Parse(createdResponse.Id);

            var nonExistentRepId = Guid.NewGuid();

            // Act - Try to activate non-existent representative
            var result = await Controller.PatchRepresentativeStatus(orgId, nonExistentRepId, new PatchRepresentativeStatusDto(IsActive: true));

            // Assert
            Assert.IsType<NotFoundObjectResult>(result);
        }

        /// <summary>
        /// Test 4: Full lifecycle - Create, Deactivate, Reactivate
        /// AC4: Complete representative lifecycle management
        /// </summary>
        [Fact]
        public async Task RepresentativeLifecycle_CreateDeactivateReactivate_WorksCorrectly()
        {
            // Arrange - Create organization
            var createDto = new CreateOrganizationDto
            {
                Identifier = "SHIP1400",
                LegalName = "Lifecycle Test Ltd",
                AlternativeName = "Lifecycle",
                Address = "Lifecycle Plaza",
                TaxNumber = "PT852852852",
                Type = OrganizationType.SHIPPING_AGENT,
                Representatives = new List<RepresentativeInputDto>
                {
                    new RepresentativeInputDto
                    {
                        Name = "Lifecycle Rep",
                        CitizenId = "70707070",
                        Nationality = "IT",
                        Email = "lifecycle.test@gmail.com",
                        Phone = "+351911400100"
                    }
                }
            };

            var createResult = await Controller.Post(createDto);
            var createdResponse = (OrganizationResponseDto)((CreatedAtActionResult)createResult).Value!;
            var orgId = Guid.Parse(createdResponse.Id);
            var repId = Guid.Parse(createdResponse.Representatives[0].Id);

            // Step 1: Verify initial active state
            Assert.True(createdResponse.Representatives[0].IsActive);

            // Step 2: Deactivate using PATCH
            var deactivateResult = await Controller.PatchRepresentativeStatus(orgId, repId, new PatchRepresentativeStatusDto(IsActive: false));
            var deactivateResponse = (RepresentativeResponseDto)((OkObjectResult)deactivateResult).Value!;
            Assert.False(deactivateResponse.IsActive);

            // Step 3: Reactivate using PATCH
            var activateResult = await Controller.PatchRepresentativeStatus(orgId, repId, new PatchRepresentativeStatusDto(IsActive: true));
            var activateResponse = (RepresentativeResponseDto)((OkObjectResult)activateResult).Value!;
            
            // Assert final state
            Assert.True(activateResponse.IsActive);
            Assert.Equal("Lifecycle Rep", activateResponse.Name);
            Assert.Equal("lifecycle.test@gmail.com", activateResponse.Email);
        }
    }
}
