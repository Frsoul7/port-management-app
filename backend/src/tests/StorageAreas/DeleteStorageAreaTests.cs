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
    /// TEST OBJECTIVE: Validate storage area retrieval and deletion (US 2.2.4).
    ///                 Tests HTTP GET by identifier, HTTP DELETE operations, occupancy constraint validation
    ///                 (deletion only allowed when occupancy is zero), 404 handling for non-existent areas,
    ///                 and proper cleanup of storage area records from the database.
    /// </summary>
    public class DeleteStorageAreaTests : StorageAreaTestBase
    {
        /// <summary>
        /// Test 1: Delete storage area with zero occupancy
        /// </summary>
        [Fact]
        public async Task DeleteStorageArea_ZeroOccupancy_DeletesSuccessfully()
        {
            // Arrange
            var createDto = new CreateStorageAreaDto("To Delete", "Test Loc", 1000, StorageAreaType.YARD, true);
            var createResult = await Controller.Create(createDto);
            var created = ((CreatedAtActionResult)createResult).Value as StorageAreaResponseDto;

            // Act
            var deleteResult = await Controller.Delete(created!.StorageAreaId);
            var getResult = await Controller.GetById(created.StorageAreaId);

            // Assert
            Assert.IsType<NoContentResult>(deleteResult);
            Assert.IsType<NotFoundObjectResult>(getResult);
        }

        /// <summary>
        /// Test 2: Delete storage area with occupancy fails
        /// </summary>
        [Fact]
        public async Task DeleteStorageArea_WithOccupancy_ReturnsBadRequest()
        {
            // Arrange
            var createDto = new CreateStorageAreaDto("Occupied Area", "Test Loc", 1000, StorageAreaType.YARD, true);
            var createResult = await Controller.Create(createDto);
            var created = ((CreatedAtActionResult)createResult).Value as StorageAreaResponseDto;

            // Set occupancy
            await Controller.UpdateOccupancy(created!.StorageAreaId, new UpdateOccupancyDto(500));

            // Act
            var result = await Controller.Delete(created.StorageAreaId);

            // Assert
            Assert.IsType<BadRequestObjectResult>(result);
        }

        /// <summary>
        /// Test 3: Delete non-existent storage area
        /// </summary>
        [Fact]
        public async Task DeleteStorageArea_NonExistent_ReturnsNotFound()
        {
            // Act
            var result = await Controller.Delete("NONEXISTENT");

            // Assert
            Assert.IsType<NotFoundObjectResult>(result);
        }

        /// <summary>
        /// Test 4: Get storage area by ID
        /// </summary>
        [Fact]
        public async Task GetStorageAreaById_ValidId_ReturnsArea()
        {
            // Arrange
            var createDto = new CreateStorageAreaDto("Test Area", "Test Loc", 1000, StorageAreaType.YARD, true);
            var createResult = await Controller.Create(createDto);
            var created = ((CreatedAtActionResult)createResult).Value as StorageAreaResponseDto;

            // Act
            var result = await Controller.GetById(created!.StorageAreaId);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var response = Assert.IsType<StorageAreaResponseDto>(okResult.Value);
            
            Assert.Equal(created.StorageAreaId, response.StorageAreaId);
            Assert.Equal("Test Area", response.Name);
        }

        /// <summary>
        /// Test 5: Get non-existent storage area
        /// </summary>
        [Fact]
        public async Task GetStorageAreaById_InvalidId_ReturnsNotFound()
        {
            // Act
            var result = await Controller.GetById("NONEXISTENT");

            // Assert
            Assert.IsType<NotFoundObjectResult>(result);
        }
    }
}
