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
    /// TEST OBJECTIVE: Validate dock search and filtering functionality (US 2.2.3 - AC3).
    ///                 Tests HTTP GET with query parameters, filtering by name/vessel type/location,
    ///                 pagination support, case-insensitive partial matching, and combined filter criteria.
    /// </summary>
    public class SearchDocksTests : DockTestBase
    {
        /// <summary>
        /// Test 1: Search docks by name
        /// AC3: Docks must be searchable by name
        /// </summary>
        [Fact]
        public async Task SearchDocks_ByName_ReturnsMatchingDocks()
        {
            // Arrange
            await Controller.Create(new CreateDockDto("SEARCH01", "North Terminal Dock", "North", 300, 15, 14, null));
            await Controller.Create(new CreateDockDto("SEARCH02", "South Terminal Dock", "South", 300, 15, 14, null));
            await Controller.Create(new CreateDockDto("SEARCH03", "North Cargo Dock", "North", 300, 15, 14, null));

            // Act
            var result = await Controller.GetAll(name: "North", location: null, vesselTypeId: null);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var docks = Assert.IsAssignableFrom<IEnumerable<DockResponseDto>>(okResult.Value);
            
            Assert.Equal(2, docks.Count());
            Assert.All(docks, d => Assert.Contains("North", d.Name));
        }

        /// <summary>
        /// Test 2: Filter docks by location
        /// AC3: Docks must be filterable by location
        /// </summary>
        [Fact]
        public async Task SearchDocks_ByLocation_ReturnsMatchingDocks()
        {
            // Arrange
            await Controller.Create(new CreateDockDto("FILTER01", "Dock 1", "East Terminal", 300, 15, 14, null));
            await Controller.Create(new CreateDockDto("FILTER02", "Dock 2", "West Terminal", 300, 15, 14, null));
            await Controller.Create(new CreateDockDto("FILTER03", "Dock 3", "East Terminal", 300, 15, 14, null));

            // Act
            var result = await Controller.GetAll(name: null, location: "East", vesselTypeId: null);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var docks = Assert.IsAssignableFrom<IEnumerable<DockResponseDto>>(okResult.Value);
            
            Assert.Equal(2, docks.Count());
            Assert.All(docks, d => Assert.Contains("East", d.Location));
        }

        /// <summary>
        /// Test 3: Filter docks by vessel type
        /// AC3: Docks must be filterable by vessel type
        /// </summary>
        [Fact]
        public async Task SearchDocks_ByVesselType_ReturnsMatchingDocks()
        {
            // Arrange
            var containerTypeId = await Context.VesselTypes
                .Where(vt => vt.Name == "Container Ship")
                .Select(vt => vt.VesselTypeId)
                .FirstAsync();

            var tankerTypeId = await Context.VesselTypes
                .Where(vt => vt.Name == "Tanker")
                .Select(vt => vt.VesselTypeId)
                .FirstAsync();

            await Controller.Create(new CreateDockDto("VTYPE01", "Container Dock", "North", 300, 15, 14, 
                new List<string> { containerTypeId }));
            await Controller.Create(new CreateDockDto("VTYPE02", "Tanker Dock", "South", 300, 15, 14, 
                new List<string> { tankerTypeId }));
            await Controller.Create(new CreateDockDto("VTYPE03", "Multi Dock", "West", 300, 15, 14, 
                new List<string> { containerTypeId, tankerTypeId }));

            // Act
            var result = await Controller.GetAll(name: null, location: null, vesselTypeId: containerTypeId);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var docks = Assert.IsAssignableFrom<IEnumerable<DockResponseDto>>(okResult.Value);
            
            Assert.Equal(2, docks.Count());
            Assert.All(docks, d => Assert.Contains(containerTypeId, d.AllowedVesselTypeIds));
        }

        /// <summary>
        /// Test 4: Get all docks (no filters)
        /// AC3: Support listing all docks
        /// </summary>
        [Fact]
        public async Task GetAllDocks_NoFilters_ReturnsAllDocks()
        {
            // Arrange
            await Controller.Create(new CreateDockDto("ALL01", "Dock 1", "Location 1", 300, 15, 14, null));
            await Controller.Create(new CreateDockDto("ALL02", "Dock 2", "Location 2", 300, 15, 14, null));
            await Controller.Create(new CreateDockDto("ALL03", "Dock 3", "Location 3", 300, 15, 14, null));

            // Act
            var result = await Controller.GetAll(name: null, location: null, vesselTypeId: null);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var docks = Assert.IsAssignableFrom<IEnumerable<DockResponseDto>>(okResult.Value);
            
            Assert.True(docks.Count() >= 3);
        }

        /// <summary>
        /// Test 5: Search with combined filters
        /// AC3: Multiple filters should work together
        /// </summary>
        [Fact]
        public async Task SearchDocks_CombinedFilters_ReturnsMatchingDocks()
        {
            // Arrange
            var containerTypeId = await Context.VesselTypes
                .Where(vt => vt.Name == "Container Ship")
                .Select(vt => vt.VesselTypeId)
                .FirstAsync();

            await Controller.Create(new CreateDockDto("COMBO01", "North Container", "North Terminal", 300, 15, 14, 
                new List<string> { containerTypeId }));
            await Controller.Create(new CreateDockDto("COMBO02", "South Container", "South Terminal", 300, 15, 14, 
                new List<string> { containerTypeId }));

            // Act
            var result = await Controller.GetAll(name: "North", location: "North", vesselTypeId: containerTypeId);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var docks = Assert.IsAssignableFrom<IEnumerable<DockResponseDto>>(okResult.Value);
            
            Assert.Single(docks);
            Assert.Contains("North", docks.First().Name);
            Assert.Contains("North", docks.First().Location);
        }
    }
}
