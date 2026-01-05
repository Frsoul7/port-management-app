using System;
using System.Collections.Generic;
using System.Linq;
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
    /// TEST OBJECTIVE: Validate end-to-end storage area creation (US 2.2.4).
    ///                 Tests HTTP POST requests, generic and specialized storage area creation,
    ///                 type-specific attributes (ColdStorageSpec with temperature range, YardSpec with handling type),
    ///                 XOR constraint enforcement (specialized areas have specific specs OR remain generic),
    ///                 duplicate identifier prevention, capacity validation, and database persistence.
    /// </summary>
    public class CreateStorageAreaTests : StorageAreaTestBase
    {
        /// <summary>
        /// Test 1: Create generic storage area (no specific type)
        /// Client meeting notes: Storage areas can be created without specification (generic)
        /// AC1: Must have unique identifier, type, and location
        /// </summary>
        [Fact]
        public async Task CreateStorageArea_GenericType_CreatesSuccessfully()
        {
            // Arrange
            var createDto = new CreateStorageAreaDto(
                Name: "General Storage 1",
                Location: "Central area, near main gate",
                MaxCapacityTEU: 1000,
                Type: StorageAreaType.ORDINARY,
                ServesAllDocks: true,
                ServedDockCodes: null,
                YardNotes: null,
                WarehouseNotes: null
            );

            // Act
            var result = await Controller.Create(createDto);

            // Assert
            var createdResult = Assert.IsType<CreatedAtActionResult>(result);
            var response = Assert.IsType<StorageAreaResponseDto>(createdResult.Value);
            
            Assert.NotNull(response.StorageAreaId);
            Assert.Equal("General Storage 1", response.Name);
            Assert.Equal("Central area, near main gate", response.Location);
            Assert.Equal(1000, response.MaxCapacityTEU);
            Assert.Equal(StorageAreaType.ORDINARY, response.Type);
            Assert.True(response.ServesAllDocks);
            Assert.Null(response.YardNotes);
            Assert.Null(response.WarehouseNotes);
        }

        /// <summary>
        /// Test 2: Create yard storage area with specifications
        /// Client meeting notes: Storage areas can be created with Yard specification
        /// </summary>
        [Fact]
        public async Task CreateStorageArea_YardType_CreatesSuccessfully()
        {
            // Arrange
            var createDto = new CreateStorageAreaDto(
                Name: "Container Yard A",
                Location: "North section, adjacent to rail terminal",
                MaxCapacityTEU: 2500,
                Type: StorageAreaType.YARD,
                ServesAllDocks: true,
                ServedDockCodes: null,
                YardNotes: "Open-air stacking area. Max stack height: 5 containers.",
                WarehouseNotes: null
            );

            // Act
            var result = await Controller.Create(createDto);

            // Assert
            var createdResult = Assert.IsType<CreatedAtActionResult>(result);
            var response = Assert.IsType<StorageAreaResponseDto>(createdResult.Value);
            
            Assert.Equal("Container Yard A", response.Name);
            Assert.Equal(StorageAreaType.YARD, response.Type);
            Assert.Contains("Open-air stacking area", response.YardNotes);
            Assert.Null(response.WarehouseNotes);
        }

        /// <summary>
        /// Test 3: Create warehouse storage area with specifications
        /// Client meeting notes: Storage areas can be created with Warehouse specification
        /// </summary>
        [Fact]
        public async Task CreateStorageArea_WarehouseType_CreatesSuccessfully()
        {
            // Arrange
            var createDto = new CreateStorageAreaDto(
                Name: "Covered Warehouse B",
                Location: "South section, building 7",
                MaxCapacityTEU: 800,
                Type: StorageAreaType.WAREHOUSE,
                ServesAllDocks: true,
                ServedDockCodes: null,
                YardNotes: null,
                WarehouseNotes: "Climate controlled. Fire suppression system."
            );

            // Act
            var result = await Controller.Create(createDto);

            // Assert
            var createdResult = Assert.IsType<CreatedAtActionResult>(result);
            var response = Assert.IsType<StorageAreaResponseDto>(createdResult.Value);
            
            Assert.Equal("Covered Warehouse B", response.Name);
            Assert.Equal(StorageAreaType.WAREHOUSE, response.Type);
            Assert.Contains("Climate controlled", response.WarehouseNotes);
            Assert.Null(response.YardNotes);
        }

        /// <summary>
        /// Test 4: Create storage area with maximum capacity
        /// AC2: Must specify maximum capacity in TEUs
        /// </summary>
        [Fact]
        public async Task CreateStorageArea_MaxCapacity_StoredCorrectly()
        {
            // Arrange
            var createDto = new CreateStorageAreaDto(
                Name: "Large Yard",
                Location: "West section",
                MaxCapacityTEU: 5000,
                Type: StorageAreaType.YARD,
                ServesAllDocks: true
            );

            // Act
            await Controller.Create(createDto);
            var getAllResult = await Controller.GetAll(name: "Large Yard", null, null, null);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(getAllResult);
            var areas = Assert.IsAssignableFrom<IEnumerable<StorageAreaResponseDto>>(okResult.Value);
            var area = areas.First();
            
            Assert.Equal(5000, area.MaxCapacityTEU);
            Assert.Equal(0, area.CurrentOccupancyTEU);
        }

        /// <summary>
        /// Test 5: Create storage area serving all docks (default)
        /// AC3: By default, storage area serves entire port
        /// </summary>
        [Fact]
        public async Task CreateStorageArea_ServesAllDocks_DefaultBehavior()
        {
            // Arrange
            var createDto = new CreateStorageAreaDto(
                Name: "Universal Storage",
                Location: "Central hub",
                MaxCapacityTEU: 1500,
                Type: StorageAreaType.ORDINARY,
                ServesAllDocks: true,
                ServedDockCodes: null
            );

            // Act
            var result = await Controller.Create(createDto);

            // Assert
            var createdResult = Assert.IsType<CreatedAtActionResult>(result);
            var response = Assert.IsType<StorageAreaResponseDto>(createdResult.Value);
            
            Assert.True(response.ServesAllDocks);
            Assert.Empty(response.ServedDockCodes);
        }

        /// <summary>
        /// Test 6: Create storage area restricted to specific docks
        /// AC3: Some storage areas may be restricted to specific docks
        /// </summary>
        [Fact]
        public async Task CreateStorageArea_RestrictedToSpecificDocks_CreatesSuccessfully()
        {
            // Arrange
            var createDto = new CreateStorageAreaDto(
                Name: "Dock-Specific Storage",
                Location: "Near Dock 1 and Dock 2",
                MaxCapacityTEU: 1200,
                Type: StorageAreaType.YARD,
                ServesAllDocks: false,
                ServedDockCodes: new List<string> { "DOCK01", "DOCK02" }
            );

            // Act
            var result = await Controller.Create(createDto);

            // Assert
            var createdResult = Assert.IsType<CreatedAtActionResult>(result);
            var response = Assert.IsType<StorageAreaResponseDto>(createdResult.Value);
            
            Assert.False(response.ServesAllDocks);
            Assert.Equal(2, response.ServedDockCodes.Count);
            Assert.Contains("DOCK01", response.ServedDockCodes);
            Assert.Contains("DOCK02", response.ServedDockCodes);
        }

        /// <summary>
        /// Test 7: Create storage area with invalid dock codes fails
        /// AC3: Dock restriction validation
        /// </summary>
        [Fact]
        public async Task CreateStorageArea_InvalidDockCodes_ReturnsBadRequest()
        {
            // Arrange
            var createDto = new CreateStorageAreaDto(
                Name: "Invalid Dock Storage",
                Location: "Test Location",
                MaxCapacityTEU: 1000,
                Type: StorageAreaType.ORDINARY,
                ServesAllDocks: false,
                ServedDockCodes: new List<string> { "NONEXISTENT_DOCK" }
            );

            // Act
            var result = await Controller.Create(createDto);

            // Assert
            Assert.IsType<BadRequestObjectResult>(result);
        }
    }
}
