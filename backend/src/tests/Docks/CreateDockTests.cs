using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Xunit;
using Microsoft.EntityFrameworkCore;
using Microsoft.AspNetCore.Mvc;
using DDDNetCore.Application.DTOs.Docks;
using DDDNetCore.Tests.Docks.Base;

namespace DDDNetCore.Tests.Docks
{
    /// <summary>
    /// TEST TYPE: Integration Test
    /// COMPONENTS UNDER TEST: DocksController, DockRepository, PortDbContext, Dock (Domain Entity)
    /// TEST OBJECTIVE: Validate end-to-end dock creation functionality (US 2.2.3).
    ///                 Tests HTTP POST requests through controller, DTO validation, business rule enforcement,
    ///                 repository persistence, unique code constraint, vessel type associations,
    ///                 and storage area/mobile equipment assignments.
    /// </summary>
    public class CreateDockTests : DockTestBase
    {
        /// <summary>
        /// Test 1: Create dock with all required fields
        /// AC1: A dock record must include unique identifier, name, location, and physical characteristics
        /// </summary>
        [Fact]
        public async Task CreateDock_ValidData_ReturnsCreatedDock()
        {
            // Arrange
            var vesselTypeId = await Context.VesselTypes.Select(vt => vt.VesselTypeId).FirstAsync();
            var createDto = new CreateDockDto(
                Code: "DOCK01",
                Name: "Dock 1 - North Terminal",
                Location: "North side of the port, near warehouse A",
                LengthM: 350.5,
                DepthM: 15.0,
                MaxDraftM: 14.5,
                AllowedVesselTypeIds: new List<string> { vesselTypeId }
            );

            // Act
            var result = await Controller.Create(createDto);

            // Assert
            var createdResult = Assert.IsType<CreatedAtActionResult>(result);
            var response = Assert.IsType<DockResponseDto>(createdResult.Value);
            
            Assert.Equal("DOCK01", response.Code);
            Assert.Equal("Dock 1 - North Terminal", response.Name);
            Assert.Equal("North side of the port, near warehouse A", response.Location);
            Assert.Equal(350.5, response.LengthM);
            Assert.Equal(15.0, response.DepthM);
            Assert.Equal(14.5, response.MaxDraftM);
            Assert.Single(response.AllowedVesselTypeIds);
        }

        /// <summary>
        /// Test 2: Create dock with physical characteristics
        /// AC1: Must include length, depth, and max draft
        /// </summary>
        [Fact]
        public async Task CreateDock_PhysicalCharacteristics_StoredCorrectly()
        {
            // Arrange
            var createDto = new CreateDockDto(
                Code: "DOCK02",
                Name: "Test Dock",
                Location: "Test Location",
                LengthM: 400.0,
                DepthM: 18.0,
                MaxDraftM: 17.5,
                AllowedVesselTypeIds: null
            );

            // Act
            await Controller.Create(createDto);
            var getResult = await Controller.GetByCode("DOCK02");

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(getResult);
            var dock = Assert.IsType<DockResponseDto>(okResult.Value);
            
            Assert.Equal(400.0, dock.LengthM);
            Assert.Equal(18.0, dock.DepthM);
            Assert.Equal(17.5, dock.MaxDraftM);
        }

        /// <summary>
        /// Test 3: Create dock with multiple allowed vessel types
        /// AC2: Officer must specify vessel types allowed to berth
        /// </summary>
        [Fact]
        public async Task CreateDock_MultipleVesselTypes_AllStored()
        {
            // Arrange
            var vesselTypeIds = await Context.VesselTypes
                .Select(vt => vt.VesselTypeId)
                .ToListAsync();

            var createDto = new CreateDockDto(
                Code: "DOCK03",
                Name: "Multi-Purpose Dock",
                Location: "Central area",
                LengthM: 450.0,
                DepthM: 20.0,
                MaxDraftM: 19.0,
                AllowedVesselTypeIds: vesselTypeIds
            );

            // Act
            var result = await Controller.Create(createDto);

            // Assert
            var createdResult = Assert.IsType<CreatedAtActionResult>(result);
            var response = Assert.IsType<DockResponseDto>(createdResult.Value);
            
            Assert.Equal(vesselTypeIds.Count, response.AllowedVesselTypeIds.Count);
            Assert.Equal(vesselTypeIds.Count, response.AllowedVesselTypes.Count);
        }

        /// <summary>
        /// Test 4: Create dock with duplicate code fails
        /// AC1: Unique identifier required
        /// </summary>
        [Fact]
        public async Task CreateDock_DuplicateCode_ReturnsConflict()
        {
            // Arrange
            var createDto1 = new CreateDockDto(
                Code: "DOCK04",
                Name: "First Dock",
                Location: "Location 1",
                LengthM: 300.0,
                DepthM: 15.0,
                MaxDraftM: 14.0,
                AllowedVesselTypeIds: null
            );

            var createDto2 = new CreateDockDto(
                Code: "DOCK04", // Same code
                Name: "Second Dock",
                Location: "Location 2",
                LengthM: 350.0,
                DepthM: 16.0,
                MaxDraftM: 15.0,
                AllowedVesselTypeIds: null
            );

            // Act
            await Controller.Create(createDto1);
            var result = await Controller.Create(createDto2);

            // Assert
            Assert.IsType<ConflictObjectResult>(result);
        }

        /// <summary>
        /// Test 5: Create dock with invalid vessel type fails
        /// AC2: Vessel types must exist in system
        /// </summary>
        [Fact]
        public async Task CreateDock_InvalidVesselType_ReturnsBadRequest()
        {
            // Arrange
            var createDto = new CreateDockDto(
                Code: "DOCK05",
                Name: "Test Dock",
                Location: "Test Location",
                LengthM: 300.0,
                DepthM: 15.0,
                MaxDraftM: 14.0,
                AllowedVesselTypeIds: new List<string> { "NONEXISTENT_TYPE" }
            );

            // Act
            var result = await Controller.Create(createDto);

            // Assert
            var badRequestResult = Assert.IsType<BadRequestObjectResult>(result);
            Assert.NotNull(badRequestResult.Value);
        }

        /// <summary>
        /// Test 6: Create dock with location as free text
        /// Client answer: Location should be stored as free text
        /// </summary>
        [Fact]
        public async Task CreateDock_LocationFreeText_StoredCorrectly()
        {
            // Arrange
            var createDto = new CreateDockDto(
                Code: "DOCK06",
                Name: "Test Dock",
                Location: "East Terminal, Zone 3, Adjacent to Warehouse B, GPS: 41.123,-8.456",
                LengthM: 300.0,
                DepthM: 15.0,
                MaxDraftM: 14.0,
                AllowedVesselTypeIds: null
            );

            // Act
            await Controller.Create(createDto);
            var getResult = await Controller.GetByCode("DOCK06");

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(getResult);
            var dock = Assert.IsType<DockResponseDto>(okResult.Value);
            
            Assert.Equal("East Terminal, Zone 3, Adjacent to Warehouse B, GPS: 41.123,-8.456", dock.Location);
        }

        /// <summary>
        /// Test 7: Create dock with minimal data (no vessel types)
        /// </summary>
        [Fact]
        public async Task CreateDock_NoVesselTypes_CreatesSuccessfully()
        {
            // Arrange
            var createDto = new CreateDockDto(
                Code: "MINIMAL",
                Name: "Minimal Dock",
                Location: "Test",
                LengthM: 100.0,
                DepthM: 10.0,
                MaxDraftM: 9.0,
                AllowedVesselTypeIds: null
            );

            // Act
            var result = await Controller.Create(createDto);

            // Assert
            var createdResult = Assert.IsType<CreatedAtActionResult>(result);
            var response = Assert.IsType<DockResponseDto>(createdResult.Value);
            
            Assert.Empty(response.AllowedVesselTypeIds);
        }

        /// <summary>
        /// Test 8: Code normalization (uppercase)
        /// </summary>
        [Fact]
        public async Task CreateDock_LowercaseCode_NormalizedToUppercase()
        {
            // Arrange
            var createDto = new CreateDockDto(
                Code: "dock08",
                Name: "Test Dock",
                Location: "Test",
                LengthM: 300.0,
                DepthM: 15.0,
                MaxDraftM: 14.0,
                AllowedVesselTypeIds: null
            );

            // Act
            await Controller.Create(createDto);
            var result = await Controller.GetByCode("DOCK08");

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var dock = Assert.IsType<DockResponseDto>(okResult.Value);
            
            Assert.Equal("DOCK08", dock.Code);
        }
    }
}
