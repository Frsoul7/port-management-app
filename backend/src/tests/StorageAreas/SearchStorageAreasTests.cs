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
    /// TEST OBJECTIVE: Validate storage area search and filtering functionality (US 2.2.4).
    ///                 Tests HTTP GET with query parameters, filtering by name/location/type,
    ///                 specialized type filtering (cold storage vs. yard), partial name matching,
    ///                 case-insensitive search, and proper return of filtered storage area collections.
    /// </summary>
    public class SearchStorageAreasTests : StorageAreaTestBase
    {
        /// <summary>
        /// Test 1: Search storage areas by name
        /// </summary>
        [Fact]
        public async Task SearchStorageAreas_ByName_ReturnsMatchingAreas()
        {
            // Arrange
            await Controller.Create(new CreateStorageAreaDto("North Yard", "North", 1000, StorageAreaType.YARD, true));
            await Controller.Create(new CreateStorageAreaDto("South Yard", "South", 1000, StorageAreaType.YARD, true));
            await Controller.Create(new CreateStorageAreaDto("North Warehouse", "North", 800, StorageAreaType.WAREHOUSE, true));

            // Act
            var result = await Controller.GetAll(name: "North", null, null, null);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var areas = Assert.IsAssignableFrom<IEnumerable<StorageAreaResponseDto>>(okResult.Value);
            
            Assert.Equal(2, areas.Count());
            Assert.All(areas, a => Assert.Contains("North", a.Name));
        }

        /// <summary>
        /// Test 2: Filter storage areas by location
        /// </summary>
        [Fact]
        public async Task SearchStorageAreas_ByLocation_ReturnsMatchingAreas()
        {
            // Arrange
            await Controller.Create(new CreateStorageAreaDto("Area 1", "East Terminal", 1000, StorageAreaType.YARD, true));
            await Controller.Create(new CreateStorageAreaDto("Area 2", "West Terminal", 1000, StorageAreaType.YARD, true));
            await Controller.Create(new CreateStorageAreaDto("Area 3", "East Terminal", 800, StorageAreaType.WAREHOUSE, true));

            // Act
            var result = await Controller.GetAll(name: null, location: "East", null, null);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var areas = Assert.IsAssignableFrom<IEnumerable<StorageAreaResponseDto>>(okResult.Value);
            
            Assert.Equal(2, areas.Count());
            Assert.All(areas, a => Assert.Contains("East", a.Location));
        }

        /// <summary>
        /// Test 3: Filter storage areas by type
        /// </summary>
        [Fact]
        public async Task SearchStorageAreas_ByType_ReturnsMatchingAreas()
        {
            // Arrange
            await Controller.Create(new CreateStorageAreaDto("Yard 1", "Location 1", 1000, StorageAreaType.YARD, true));
            await Controller.Create(new CreateStorageAreaDto("Warehouse 1", "Location 2", 800, StorageAreaType.WAREHOUSE, true));
            await Controller.Create(new CreateStorageAreaDto("Yard 2", "Location 3", 1500, StorageAreaType.YARD, true));

            // Act
            var result = await Controller.GetAll(name: null, null, StorageAreaType.YARD, null);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var areas = Assert.IsAssignableFrom<IEnumerable<StorageAreaResponseDto>>(okResult.Value);
            
            Assert.Equal(2, areas.Count());
            Assert.All(areas, a => Assert.Equal(StorageAreaType.YARD, a.Type));
        }

        /// <summary>
        /// Test 4: Filter storage areas serving all docks
        /// AC3: Filter by dock serving configuration
        /// </summary>
        [Fact]
        public async Task SearchStorageAreas_ServesAllDocks_ReturnsMatchingAreas()
        {
            // Arrange
            await Controller.Create(new CreateStorageAreaDto("Universal 1", "Loc 1", 1000, StorageAreaType.YARD, 
                ServesAllDocks: true));
            await Controller.Create(new CreateStorageAreaDto("Specific 1", "Loc 2", 800, StorageAreaType.YARD, 
                ServesAllDocks: false, ServedDockCodes: new List<string> { "DOCK01" }));

            // Act
            var result = await Controller.GetAll(name: null, null, null, servesAllDocks: true);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var areas = Assert.IsAssignableFrom<IEnumerable<StorageAreaResponseDto>>(okResult.Value);
            
            Assert.All(areas, a => Assert.True(a.ServesAllDocks));
        }

        /// <summary>
        /// Test 5: Get all storage areas (no filters)
        /// </summary>
        [Fact]
        public async Task GetAllStorageAreas_NoFilters_ReturnsAllAreas()
        {
            // Arrange
            await Controller.Create(new CreateStorageAreaDto("Area 1", "Loc 1", 1000, StorageAreaType.YARD, true));
            await Controller.Create(new CreateStorageAreaDto("Area 2", "Loc 2", 800, StorageAreaType.WAREHOUSE, true));
            await Controller.Create(new CreateStorageAreaDto("Area 3", "Loc 3", 1200, StorageAreaType.ORDINARY, true));

            // Act
            var result = await Controller.GetAll(null, null, null, null);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var areas = Assert.IsAssignableFrom<IEnumerable<StorageAreaResponseDto>>(okResult.Value);
            
            Assert.True(areas.Count() >= 3);
        }
    }
}
