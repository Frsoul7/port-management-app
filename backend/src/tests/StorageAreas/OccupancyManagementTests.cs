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
    /// TEST OBJECTIVE: Validate storage area occupancy tracking and capacity enforcement (US 2.2.4 - AC2, AC5).
    ///                 Tests HTTP PATCH/PUT for occupancy updates, real-time occupancy tracking,
    ///                 capacity constraint validation (occupancy cannot exceed capacity),
    ///                 occupancy reduction, occupancy queries, and business rule enforcement
    ///                 preventing over-allocation of storage space.
    /// </summary>
    public class OccupancyManagementTests : StorageAreaTestBase
    {
        /// <summary>
        /// Test 1: Update occupancy within capacity
        /// </summary>
        [Fact]
        public async Task UpdateOccupancy_WithinCapacity_UpdatesSuccessfully()
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

            // Act
            var result = await Controller.UpdateOccupancy(created!.StorageAreaId, new UpdateOccupancyDto(750));

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var response = Assert.IsType<StorageAreaResponseDto>(okResult.Value);
            
            Assert.Equal(750, response.CurrentOccupancyTEU);
            Assert.Equal(1000, response.MaxCapacityTEU);
        }

        /// <summary>
        /// Test 2: Update occupancy exceeding capacity fails
        /// </summary>
        [Fact]
        public async Task UpdateOccupancy_ExceedsCapacity_ReturnsBadRequest()
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

            // Act
            var result = await Controller.UpdateOccupancy(created!.StorageAreaId, new UpdateOccupancyDto(1500));

            // Assert
            Assert.IsType<BadRequestObjectResult>(result);
        }

        /// <summary>
        /// Test 3: Update occupancy to negative value fails
        /// </summary>
        [Fact]
        public async Task UpdateOccupancy_NegativeValue_ReturnsBadRequest()
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

            // Act
            var result = await Controller.UpdateOccupancy(created!.StorageAreaId, new UpdateOccupancyDto(-100));

            // Assert
            Assert.IsType<BadRequestObjectResult>(result);
        }

        /// <summary>
        /// Test 4: Update occupancy at maximum capacity
        /// </summary>
        [Fact]
        public async Task UpdateOccupancy_AtMaxCapacity_UpdatesSuccessfully()
        {
            // Arrange
            var createDto = new CreateStorageAreaDto("Full Storage", "Test Loc", 1000, StorageAreaType.YARD, true);
            var createResult = await Controller.Create(createDto);
            var created = ((CreatedAtActionResult)createResult).Value as StorageAreaResponseDto;

            // Act
            var result = await Controller.UpdateOccupancy(created!.StorageAreaId, new UpdateOccupancyDto(1000));

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var response = Assert.IsType<StorageAreaResponseDto>(okResult.Value);
            
            Assert.Equal(1000, response.CurrentOccupancyTEU);
            Assert.Equal(1000, response.MaxCapacityTEU);
        }
    }
}
