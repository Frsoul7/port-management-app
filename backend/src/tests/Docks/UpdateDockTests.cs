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
    /// TEST OBJECTIVE: Validate dock update functionality (US 2.2.3).
    ///                 Tests HTTP PUT operations, partial updates of mutable fields (all except code/ID),
    ///                 vessel type association updates, storage area/mobile equipment reassignments,
    ///                 and validation of business rules during updates.
    /// </summary>
    public class UpdateDockTests : DockTestBase
    {
        /// <summary>
        /// Test 1: Update dock name
        /// Client answer: All fields except ID can be updated
        /// </summary>
        [Fact]
        public async Task UpdateDock_ChangeName_UpdatesSuccessfully()
        {
            // Arrange
            var createDto = new CreateDockDto(
                Code: "DOCK01",
                Name: "Original Name",
                Location: "Original Location",
                LengthM: 300.0,
                DepthM: 15.0,
                MaxDraftM: 14.0,
                AllowedVesselTypeIds: null
            );
            await Controller.Create(createDto);

            var updateDto = new UpdateDockDto(
                Name: "Updated Name",
                Location: "Original Location",
                LengthM: 300.0,
                DepthM: 15.0,
                MaxDraftM: 14.0,
                AllowedVesselTypeIds: null
            );

            // Act
            var result = await Controller.Update("DOCK01", updateDto);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var response = Assert.IsType<DockResponseDto>(okResult.Value);
            
            Assert.Equal("DOCK01", response.Code); // Code unchanged
            Assert.Equal("Updated Name", response.Name);
        }

        /// <summary>
        /// Test 2: Update dock physical characteristics
        /// Client answer: All fields except ID can be updated
        /// </summary>
        [Fact]
        public async Task UpdateDock_ChangePhysicalCharacteristics_UpdatesSuccessfully()
        {
            // Arrange
            var createDto = new CreateDockDto(
                Code: "DOCK02",
                Name: "Test Dock",
                Location: "Test Location",
                LengthM: 300.0,
                DepthM: 15.0,
                MaxDraftM: 14.0,
                AllowedVesselTypeIds: null
            );
            await Controller.Create(createDto);

            var updateDto = new UpdateDockDto(
                Name: "Test Dock",
                Location: "Test Location",
                LengthM: 400.0,  // Changed
                DepthM: 18.0,    // Changed
                MaxDraftM: 17.0, // Changed
                AllowedVesselTypeIds: null
            );

            // Act
            var result = await Controller.Update("DOCK02", updateDto);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var response = Assert.IsType<DockResponseDto>(okResult.Value);
            
            Assert.Equal(400.0, response.LengthM);
            Assert.Equal(18.0, response.DepthM);
            Assert.Equal(17.0, response.MaxDraftM);
        }

        /// <summary>
        /// Test 3: Update dock allowed vessel types
        /// AC2: Officer can modify allowed vessel types
        /// </summary>
        [Fact]
        public async Task UpdateDock_ChangeAllowedVesselTypes_UpdatesSuccessfully()
        {
            // Arrange
            var vesselTypeIds = await Context.VesselTypes
                .Select(vt => vt.VesselTypeId)
                .ToListAsync();

            var createDto = new CreateDockDto(
                Code: "DOCK03",
                Name: "Test Dock",
                Location: "Test Location",
                LengthM: 300.0,
                DepthM: 15.0,
                MaxDraftM: 14.0,
                AllowedVesselTypeIds: new List<string> { vesselTypeIds[0] }
            );
            await Controller.Create(createDto);

            var updateDto = new UpdateDockDto(
                Name: "Test Dock",
                Location: "Test Location",
                LengthM: 300.0,
                DepthM: 15.0,
                MaxDraftM: 14.0,
                AllowedVesselTypeIds: vesselTypeIds // All types now
            );

            // Act
            var result = await Controller.Update("DOCK03", updateDto);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var response = Assert.IsType<DockResponseDto>(okResult.Value);
            
            Assert.Equal(vesselTypeIds.Count, response.AllowedVesselTypeIds.Count);
        }

        /// <summary>
        /// Test 4: Update dock location
        /// Client answer: Location is free text
        /// </summary>
        [Fact]
        public async Task UpdateDock_ChangeLocation_UpdatesSuccessfully()
        {
            // Arrange
            var createDto = new CreateDockDto(
                Code: "DOCK04",
                Name: "Test Dock",
                Location: "Original Location",
                LengthM: 300.0,
                DepthM: 15.0,
                MaxDraftM: 14.0,
                AllowedVesselTypeIds: null
            );
            await Controller.Create(createDto);

            var updateDto = new UpdateDockDto(
                Name: "Test Dock",
                Location: "New Location - West Terminal, Zone 5",
                LengthM: 300.0,
                DepthM: 15.0,
                MaxDraftM: 14.0,
                AllowedVesselTypeIds: null
            );

            // Act
            var result = await Controller.Update("DOCK04", updateDto);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var response = Assert.IsType<DockResponseDto>(okResult.Value);
            
            Assert.Equal("New Location - West Terminal, Zone 5", response.Location);
        }

        /// <summary>
        /// Test 5: Update non-existent dock fails
        /// </summary>
        [Fact]
        public async Task UpdateDock_NonExistent_ReturnsNotFound()
        {
            // Arrange
            var updateDto = new UpdateDockDto(
                Name: "Test Dock",
                Location: "Test Location",
                LengthM: 300.0,
                DepthM: 15.0,
                MaxDraftM: 14.0,
                AllowedVesselTypeIds: null
            );

            // Act
            var result = await Controller.Update("NONEXISTENT", updateDto);

            // Assert
            Assert.IsType<NotFoundObjectResult>(result);
        }

        /// <summary>
        /// Test 6: Update dock with invalid vessel type fails
        /// </summary>
        [Fact]
        public async Task UpdateDock_InvalidVesselType_ReturnsBadRequest()
        {
            // Arrange
            var createDto = new CreateDockDto(
                Code: "DOCK06",
                Name: "Test Dock",
                Location: "Test Location",
                LengthM: 300.0,
                DepthM: 15.0,
                MaxDraftM: 14.0,
                AllowedVesselTypeIds: null
            );
            await Controller.Create(createDto);

            var updateDto = new UpdateDockDto(
                Name: "Test Dock",
                Location: "Test Location",
                LengthM: 300.0,
                DepthM: 15.0,
                MaxDraftM: 14.0,
                AllowedVesselTypeIds: new List<string> { "INVALID_TYPE" }
            );

            // Act
            var result = await Controller.Update("DOCK06", updateDto);

            // Assert
            Assert.IsType<BadRequestObjectResult>(result);
        }
    }
}
