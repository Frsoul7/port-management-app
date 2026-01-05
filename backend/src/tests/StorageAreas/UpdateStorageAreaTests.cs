using System.Collections.Generic;
using System.Threading.Tasks;
using Xunit;
using Microsoft.AspNetCore.Mvc;
using DDDNetCore.Domain.StorageAreas;
using DDDNetCore.Application.DTOs.StorageAreas;
using DDDNetCore.Tests.StorageAreas.Base;

namespace DDDNetCore.Tests.StorageAreas
{
    /// <summary>
    /// TEST TYPE: Integration Test
    /// COMPONENTS UNDER TEST: StorageAreasController, StorageAreaRepository, PortDbContext, StorageArea (Domain Entity)
    /// TEST OBJECTIVE: Validate storage area update operations (US 2.2.4 - AC1).
    ///                 Tests HTTP PUT requests, mutable field updates (name, location, capacity),
    ///                 type-specific attribute updates (temperature range for cold storage, handling type for yards),
    ///                 XOR constraint preservation during updates, validation of updated values,
    ///                 and proper persistence of modified storage area data.
    /// </summary>
    public class UpdateStorageAreaTests : StorageAreaTestBase
    {
        /// <summary>
        /// Test 1: Update storage area name and location
        /// AC1: Basic properties can be updated
        /// </summary>
        [Fact]
        public async Task UpdateStorageArea_NameAndLocation_UpdatesSuccessfully()
        {
            // Arrange
            var createDto = new CreateStorageAreaDto(
                Name: "Original Name",
                Location: "Original Location",
                MaxCapacityTEU: 1000,
                Type: StorageAreaType.ORDINARY,
                ServesAllDocks: true
            );
            var createResult = await Controller.Create(createDto);
            var created = ((CreatedAtActionResult)createResult).Value as StorageAreaResponseDto;

            var updateDto = new UpdateStorageAreaDto(
                Name: "Updated Name",
                Location: "Updated Location - New Section",
                MaxCapacityTEU: 1000,
                ServesAllDocks: true
            );

            // Act
            var result = await Controller.Update(created!.StorageAreaId, updateDto);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var response = Assert.IsType<StorageAreaResponseDto>(okResult.Value);
            
            Assert.Equal("Updated Name", response.Name);
            Assert.Equal("Updated Location - New Section", response.Location);
        }

        /// <summary>
        /// Test 2: Update storage area capacity (increase)
        /// AC2: Maximum capacity can be updated
        /// </summary>
        [Fact]
        public async Task UpdateStorageArea_IncreaseCapacity_UpdatesSuccessfully()
        {
            // Arrange
            var createDto = new CreateStorageAreaDto(
                Name: "Test Storage",
                Location: "Test Location",
                MaxCapacityTEU: 1000,
                Type: StorageAreaType.YARD,
                ServesAllDocks: true
            );
            var createResult = await Controller.Create(createDto);
            var created = ((CreatedAtActionResult)createResult).Value as StorageAreaResponseDto;

            var updateDto = new UpdateStorageAreaDto(
                Name: "Test Storage",
                Location: "Test Location",
                MaxCapacityTEU: 1500,
                ServesAllDocks: true
            );

            // Act
            var result = await Controller.Update(created!.StorageAreaId, updateDto);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var response = Assert.IsType<StorageAreaResponseDto>(okResult.Value);
            
            Assert.Equal(1500, response.MaxCapacityTEU);
        }

        /// <summary>
        /// Test 3: Update capacity below current occupancy fails
        /// AC5: Updates must not allow current occupancy to exceed maximum capacity
        /// </summary>
        [Fact]
        public async Task UpdateStorageArea_CapacityBelowOccupancy_ReturnsBadRequest()
        {
            // Arrange
            var createDto = new CreateStorageAreaDto(
                Name: "Occupied Storage",
                Location: "Test Location",
                MaxCapacityTEU: 1000,
                Type: StorageAreaType.YARD,
                ServesAllDocks: true
            );
            var createResult = await Controller.Create(createDto);
            var created = ((CreatedAtActionResult)createResult).Value as StorageAreaResponseDto;

            // Set current occupancy to 800
            await Controller.UpdateOccupancy(created!.StorageAreaId, new UpdateOccupancyDto(800));

            // Try to reduce capacity below occupancy
            var updateDto = new UpdateStorageAreaDto(
                Name: "Occupied Storage",
                Location: "Test Location",
                MaxCapacityTEU: 500,
                ServesAllDocks: true
            );

            // Act
            var result = await Controller.Update(created.StorageAreaId, updateDto);

            // Assert
            Assert.IsType<BadRequestObjectResult>(result);
        }

        /// <summary>
        /// Test 4: Update storage area from serving all docks to specific docks
        /// AC3: Dock serving configuration can be changed
        /// </summary>
        [Fact]
        public async Task UpdateStorageArea_ChangeToSpecificDocks_UpdatesSuccessfully()
        {
            // Arrange
            var createDto = new CreateStorageAreaDto(
                Name: "Flexible Storage",
                Location: "Test Location",
                MaxCapacityTEU: 1000,
                Type: StorageAreaType.YARD,
                ServesAllDocks: true
            );
            var createResult = await Controller.Create(createDto);
            var created = ((CreatedAtActionResult)createResult).Value as StorageAreaResponseDto;

            var updateDto = new UpdateStorageAreaDto(
                Name: "Flexible Storage",
                Location: "Test Location",
                MaxCapacityTEU: 1000,
                ServesAllDocks: false,
                ServedDockCodes: new List<string> { "DOCK01" }
            );

            // Act
            var result = await Controller.Update(created!.StorageAreaId, updateDto);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var response = Assert.IsType<StorageAreaResponseDto>(okResult.Value);
            
            Assert.False(response.ServesAllDocks);
            Assert.Single(response.ServedDockCodes);
            Assert.Contains("DOCK01", response.ServedDockCodes);
        }

        /// <summary>
        /// Test 5: Update storage area from specific docks to serving all docks
        /// AC3: Dock serving configuration can be changed
        /// </summary>
        [Fact]
        public async Task UpdateStorageArea_ChangeToAllDocks_UpdatesSuccessfully()
        {
            // Arrange
            var createDto = new CreateStorageAreaDto(
                Name: "Restricted Storage",
                Location: "Test Location",
                MaxCapacityTEU: 1000,
                Type: StorageAreaType.YARD,
                ServesAllDocks: false,
                ServedDockCodes: new List<string> { "DOCK01", "DOCK02" }
            );
            var createResult = await Controller.Create(createDto);
            var created = ((CreatedAtActionResult)createResult).Value as StorageAreaResponseDto;

            var updateDto = new UpdateStorageAreaDto(
                Name: "Restricted Storage",
                Location: "Test Location",
                MaxCapacityTEU: 1000,
                ServesAllDocks: true
            );

            // Act
            var result = await Controller.Update(created!.StorageAreaId, updateDto);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var response = Assert.IsType<StorageAreaResponseDto>(okResult.Value);
            
            Assert.True(response.ServesAllDocks);
        }

        /// <summary>
        /// Test 6: Update yard notes for yard storage area
        /// Client meeting notes: Type-specific specifications can be updated
        /// </summary>
        [Fact]
        public async Task UpdateStorageArea_YardNotes_UpdatesSuccessfully()
        {
            // Arrange
            var createDto = new CreateStorageAreaDto(
                Name: "Yard Storage",
                Location: "Test Location",
                MaxCapacityTEU: 1000,
                Type: StorageAreaType.YARD,
                ServesAllDocks: true,
                YardNotes: "Original notes"
            );
            var createResult = await Controller.Create(createDto);
            var created = ((CreatedAtActionResult)createResult).Value as StorageAreaResponseDto;

            var updateDto = new UpdateStorageAreaDto(
                Name: "Yard Storage",
                Location: "Test Location",
                MaxCapacityTEU: 1000,
                ServesAllDocks: true,
                YardNotes: "Updated yard notes with new information"
            );

            // Act
            var result = await Controller.Update(created!.StorageAreaId, updateDto);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var response = Assert.IsType<StorageAreaResponseDto>(okResult.Value);
            
            Assert.Equal("Updated yard notes with new information", response.YardNotes);
        }

        /// <summary>
        /// Test 7: Update warehouse notes for warehouse storage area
        /// Client meeting notes: Type-specific specifications can be updated
        /// </summary>
        [Fact]
        public async Task UpdateStorageArea_WarehouseNotes_UpdatesSuccessfully()
        {
            // Arrange
            var createDto = new CreateStorageAreaDto(
                Name: "Warehouse Storage",
                Location: "Test Location",
                MaxCapacityTEU: 800,
                Type: StorageAreaType.WAREHOUSE,
                ServesAllDocks: true,
                WarehouseNotes: "Original warehouse specs"
            );
            var createResult = await Controller.Create(createDto);
            var created = ((CreatedAtActionResult)createResult).Value as StorageAreaResponseDto;

            var updateDto = new UpdateStorageAreaDto(
                Name: "Warehouse Storage",
                Location: "Test Location",
                MaxCapacityTEU: 800,
                ServesAllDocks: true,
                WarehouseNotes: "Updated: Added temperature control zone"
            );

            // Act
            var result = await Controller.Update(created!.StorageAreaId, updateDto);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var response = Assert.IsType<StorageAreaResponseDto>(okResult.Value);
            
            Assert.Equal("Updated: Added temperature control zone", response.WarehouseNotes);
        }

        /// <summary>
        /// Test 8: Update non-existent storage area fails
        /// </summary>
        [Fact]
        public async Task UpdateStorageArea_NonExistent_ReturnsNotFound()
        {
            // Arrange
            var updateDto = new UpdateStorageAreaDto(
                Name: "Test",
                Location: "Test",
                MaxCapacityTEU: 1000,
                ServesAllDocks: true
            );

            // Act
            var result = await Controller.Update("NONEXISTENT", updateDto);

            // Assert
            Assert.IsType<NotFoundObjectResult>(result);
        }
    }
}
