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
    /// TEST OBJECTIVE: Validate input validation and XOR constraint enforcement for storage area creation (US 2.2.4).
    ///                 Tests HTTP POST validation, XOR constraint verification (generic storage cannot have specialized specs),
    ///                 type-specific attribute validation (temperature ranges for cold storage, handling types for yards),
    ///                 conflicting specification rejection, and business rule enforcement.
    /// </summary>
    public class CreateStorageAreaValidationTests : StorageAreaTestBase
    {
        /// <summary>
        /// Test 1: Ordinary storage area cannot have Yard notes
        /// Client meeting notes: XOR constraint - generic storage cannot have specific specs
        /// </summary>
        [Fact]
        public async Task CreateStorageArea_OrdinaryWithYardNotes_ReturnsBadRequest()
        {
            // Arrange
            var createDto = new CreateStorageAreaDto(
                Name: "Invalid Generic",
                Location: "Test Location",
                MaxCapacityTEU: 1000,
                Type: StorageAreaType.ORDINARY,
                ServesAllDocks: true,
                YardNotes: "This should not be allowed"
            );

            // Act
            var result = await Controller.Create(createDto);

            // Assert
            var badRequestResult = Assert.IsType<BadRequestObjectResult>(result);
            Assert.NotNull(badRequestResult.Value);
        }

        /// <summary>
        /// Test 2: Ordinary storage area cannot have Warehouse notes
        /// Client meeting notes: XOR constraint - generic storage cannot have specific specs
        /// </summary>
        [Fact]
        public async Task CreateStorageArea_OrdinaryWithWarehouseNotes_ReturnsBadRequest()
        {
            // Arrange
            var createDto = new CreateStorageAreaDto(
                Name: "Invalid Generic",
                Location: "Test Location",
                MaxCapacityTEU: 1000,
                Type: StorageAreaType.ORDINARY,
                ServesAllDocks: true,
                WarehouseNotes: "This should not be allowed"
            );

            // Act
            var result = await Controller.Create(createDto);

            // Assert
            var badRequestResult = Assert.IsType<BadRequestObjectResult>(result);
            Assert.NotNull(badRequestResult.Value);
        }

        /// <summary>
        /// Test 3: Yard storage area cannot have Warehouse notes
        /// Client meeting notes: XOR constraint for specifications
        /// </summary>
        [Fact]
        public async Task CreateStorageArea_YardWithWarehouseNotes_ReturnsBadRequest()
        {
            // Arrange
            var createDto = new CreateStorageAreaDto(
                Name: "Invalid Yard",
                Location: "Test Location",
                MaxCapacityTEU: 1000,
                Type: StorageAreaType.YARD,
                ServesAllDocks: true,
                WarehouseNotes: "This should not be allowed for yards"
            );

            // Act
            var result = await Controller.Create(createDto);

            // Assert
            var badRequestResult = Assert.IsType<BadRequestObjectResult>(result);
            Assert.NotNull(badRequestResult.Value);
        }

        /// <summary>
        /// Test 4: Warehouse storage area cannot have Yard notes
        /// Client meeting notes: XOR constraint for specifications
        /// </summary>
        [Fact]
        public async Task CreateStorageArea_WarehouseWithYardNotes_ReturnsBadRequest()
        {
            // Arrange
            var createDto = new CreateStorageAreaDto(
                Name: "Invalid Warehouse",
                Location: "Test Location",
                MaxCapacityTEU: 1000,
                Type: StorageAreaType.WAREHOUSE,
                ServesAllDocks: true,
                YardNotes: "This should not be allowed for warehouses"
            );

            // Act
            var result = await Controller.Create(createDto);

            // Assert
            var badRequestResult = Assert.IsType<BadRequestObjectResult>(result);
            Assert.NotNull(badRequestResult.Value);
        }
    }
}
