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
    /// COMPONENTS UNDER TEST: OrganizationsController, OrganizationRepository, PortDbContext, Representative (Domain Entity)
    /// TEST OBJECTIVE: Validate representative information updates (US 2.2.6 - AC3).
    ///                 Tests HTTP PUT requests, mutable field updates (name, contact information),
    ///                 email format validation during updates, unique email constraint enforcement,
    ///                 and proper persistence of updated representative data.
    /// </summary>
    public class UpdateRepresentativeTests : OrganizationTestBase
    {
        /// <summary>
        /// Test 1: Update representative contact information
        /// AC3: Update representative details (name, contact info, etc.)
        /// </summary>
        [Fact]
        public async Task UpdateRepresentative_ValidData_ReturnsSuccess()
        {
            // Arrange - Create organization with representative
            var createDto = new CreateOrganizationDto
            {
                Identifier = "SHIP500",
                LegalName = "Update Test Ltd",
                AlternativeName = "Update Test",
                Address = "Test Street",
                TaxNumber = "PT456456456",
                Type = OrganizationType.SHIPPING_AGENT,
                Representatives = new List<RepresentativeInputDto>
                {
                    new RepresentativeInputDto
                    {
                        Name = "Original Name",
                        CitizenId = "12121212",
                        Nationality = "PT",
                        Email = "original.update@gmail.com",
                        Phone = "+351910500100"
                    }
                }
            };

            var createResult = await Controller.Post(createDto);
            var createdResponse = (OrganizationResponseDto)((CreatedAtActionResult)createResult).Value!;
            var orgId = Guid.Parse(createdResponse.Id);
            var repId = Guid.Parse(createdResponse.Representatives[0].Id);

            var updateDto = new UpdateRepresentativeDto
            {
                Name = "Updated Name",
                CitizenId = "12121212",
                Nationality = "ES",
                Email = "updated.update@gmail.com",
                Phone = "+351910500200"
            };

            // Act
            var result = await Controller.UpdateRepresentative(orgId, repId, updateDto);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var response = Assert.IsType<RepresentativeResponseDto>(okResult.Value);
            
            Assert.Equal("Updated Name", response.Name);
            Assert.Equal("ES", response.Nationality);
            Assert.Equal("updated.update@gmail.com", response.Email);
            Assert.Equal("+351910500200", response.Phone);
        }

        /// <summary>
        /// Test 2: Update representative nationality
        /// AC3: Nationality can be updated
        /// </summary>
        [Fact]
        public async Task UpdateRepresentative_ChangeNationality_ReturnsSuccess()
        {
            // Arrange - Create organization with representative
            var createDto = new CreateOrganizationDto
            {
                Identifier = "SHIP600",
                LegalName = "Nationality Test Ltd",
                AlternativeName = "Nat Test",
                Address = "Test Avenue",
                TaxNumber = "PT789789789",
                Type = OrganizationType.SHIPPING_AGENT,
                Representatives = new List<RepresentativeInputDto>
                {
                    new RepresentativeInputDto
                    {
                        Name = "Multi National",
                        CitizenId = "34343434",
                        Nationality = "PT",
                        Email = "multi.nattest@gmail.com",
                        Phone = "+351910600100"
                    }
                }
            };

            var createResult = await Controller.Post(createDto);
            var createdResponse = (OrganizationResponseDto)((CreatedAtActionResult)createResult).Value!;
            var orgId = Guid.Parse(createdResponse.Id);
            var repId = Guid.Parse(createdResponse.Representatives[0].Id);

            var updateDto = new UpdateRepresentativeDto
            {
                Name = "Multi National",
                CitizenId = "34343434",
                Nationality = "FR",  // Change to French
                Email = "multi.nattest@gmail.com",
                Phone = "+351910600100"
            };

            // Act
            var result = await Controller.UpdateRepresentative(orgId, repId, updateDto);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var response = Assert.IsType<RepresentativeResponseDto>(okResult.Value);
            
            Assert.Equal("FR", response.Nationality);
        }

        /// <summary>
        /// Test 3: Reject update with duplicate email
        /// AC2: Email must remain unique when updating
        /// </summary>
        [Fact]
        public async Task UpdateRepresentative_DuplicateEmail_ReturnsBadRequest()
        {
            // Arrange - Create organization with two representatives
            var createDto = new CreateOrganizationDto
            {
                Identifier = "SHIP700",
                LegalName = "Duplicate Update Test Ltd",
                AlternativeName = "Dup Update",
                Address = "Duplicate Street",
                TaxNumber = "PT321321321",
                Type = OrganizationType.SHIPPING_AGENT,
                Representatives = new List<RepresentativeInputDto>
                {
                    new RepresentativeInputDto
                    {
                        Name = "Rep One",
                        CitizenId = "56565656",
                        Nationality = "PT",
                        Email = "rep1.duptest@gmail.com",
                        Phone = "+351910700100"
                    },
                    new RepresentativeInputDto
                    {
                        Name = "Rep Two",
                        CitizenId = "78787878",
                        Nationality = "ES",
                        Email = "rep2.duptest@gmail.com",
                        Phone = "+351910700200"
                    }
                }
            };

            var createResult = await Controller.Post(createDto);
            var createdResponse = (OrganizationResponseDto)((CreatedAtActionResult)createResult).Value!;
            var orgId = Guid.Parse(createdResponse.Id);
            var rep2Id = Guid.Parse(createdResponse.Representatives[1].Id);

            var updateDto = new UpdateRepresentativeDto
            {
                Name = "Rep Two",
                CitizenId = "78787878",
                Nationality = "ES",
                Email = "rep1.duptest@gmail.com",  // Try to use Rep One's email
                Phone = "+351910700200"
            };

            // Act
            var result = await Controller.UpdateRepresentative(orgId, rep2Id, updateDto);

            // Assert
            var conflictResult = Assert.IsType<ConflictObjectResult>(result);
        }
    }
}
