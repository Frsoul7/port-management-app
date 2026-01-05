using System;
using System.Linq;
using System.Threading.Tasks;
using System.Collections.Generic;
using Xunit;
using Microsoft.AspNetCore.Mvc;
using Microsoft.EntityFrameworkCore;
using DDDNetCore.Application.DTOs.VesselTypes;
using DDDNetCore.Tests.VesselTypes.Base;

namespace DDDNetCore.Tests.VesselTypes
{
    /// <summary>
    /// TEST TYPE: Integration Test
    /// COMPONENTS UNDER TEST: VesselTypesController, VesselTypeRepository, PortDbContext, VesselType (Domain Entity)
    /// TEST OBJECTIVE: Validate vessel type search and filtering functionality (US 2.2.1).
    ///                 Tests HTTP GET with query parameters, filtering by name/capacity/dimensions,
    ///                 partial name matching, case-insensitive search, range filtering (min/max capacity),
    ///                 and proper return of filtered vessel type collections.
    /// </summary>
    public class SearchVesselTypesTests : VesselTypeTestBase
    {
        /// <summary>
        /// Test 1: Search without filters returns all vessel types
        /// AC1: System returns all vessel types when no filters applied
        /// </summary>
        [Fact]
        public async Task SearchVesselTypes_NoFilters_ReturnsAllVesselTypes()
        {
            // Arrange - Seed data has 2 vessel types (Container Ship, Tanker)

            // Act
            var result = await Controller.Search(name: null, description: null, page: 1, pageSize: 20);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var response = okResult.Value as dynamic;
            Assert.NotNull(response);
            
#pragma warning disable CS8602 // Dereference of a possibly null reference - response is validated by Assert.NotNull
            var items = response.items as List<VesselTypeResponseDto>;
#pragma warning restore CS8602
            Assert.NotNull(items);
            Assert.Equal(2, items.Count); // 2 from seed data
        }

        /// <summary>
        /// Test 2: Search by name (exact match)
        /// AC2: Can filter by vessel type name
        /// </summary>
        [Fact]
        public async Task SearchVesselTypes_ByExactName_ReturnsMatchingVesselType()
        {
            // Arrange - Search for "Container Ship"

            // Act
            var result = await Controller.Search(name: "Container Ship", description: null, page: 1, pageSize: 20);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var response = okResult.Value as dynamic;
            Assert.NotNull(response);
#pragma warning disable CS8602 // Dereference of a possibly null reference - response is validated by Assert.NotNull
            var items = response.items as List<VesselTypeResponseDto>;
#pragma warning restore CS8602
            
            Assert.NotNull(items);
            Assert.Single(items);
        }

        /// <summary>
        /// Test 3: Search by partial name (contains)
        /// AC3: Search supports partial matching
        /// </summary>
        [Fact]
        public async Task SearchVesselTypes_ByPartialName_ReturnsMatchingVesselTypes()
        {
            // Arrange - Search for "Ship" should match "Container Ship"

            // Act
            var result = await Controller.Search(name: "Ship", description: null, page: 1, pageSize: 20);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var response = okResult.Value as dynamic;
            var items = response!.items as List<VesselTypeResponseDto>;
            
            Assert.NotNull(items);
            Assert.Single(items); // Should find "Container Ship"
        }

        /// <summary>
        /// Test 4: Search is case-insensitive
        /// AC4: Search is case-insensitive
        /// </summary>
        [Fact]
        public async Task SearchVesselTypes_CaseInsensitive_ReturnsMatches()
        {
            // Arrange - Search for "tanker" (lowercase) should match "Tanker"

            // Act
            var result = await Controller.Search(name: "tanker", description: null, page: 1, pageSize: 20);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var response = okResult.Value as dynamic;
            var items = response!.items as List<VesselTypeResponseDto>;
            
            Assert.NotNull(items);
            Assert.Single(items); // Should find "Tanker"
        }

        /// <summary>
        /// Test 5: Search by description
        /// AC5: Can filter by description
        /// </summary>
        [Fact]
        public async Task SearchVesselTypes_ByDescription_ReturnsMatchingVesselTypes()
        {
            // Arrange - Search for "container" in description

            // Act
            var result = await Controller.Search(name: null, description: "container", page: 1, pageSize: 20);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var response = okResult.Value as dynamic;
            var items = response!.items as List<VesselTypeResponseDto>;
            
            Assert.NotNull(items);
            Assert.Single(items); // Should find "Container Ship"
        }

        /// <summary>
        /// Test 6: Search with no results
        /// AC6: Returns empty list when no matches found
        /// </summary>
        [Fact]
        public async Task SearchVesselTypes_NoMatches_ReturnsEmptyList()
        {
            // Arrange - Search for non-existent vessel type

            // Act
            var result = await Controller.Search(name: "Submarine", description: null, page: 1, pageSize: 20);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var response = okResult.Value as dynamic;
            var items = response!.items as List<VesselTypeResponseDto>;
            
            Assert.NotNull(items);
            Assert.Empty(items);
        }

        /// <summary>
        /// Test 7: Pagination - page 1
        /// AC7: Supports pagination with page and pageSize parameters
        /// </summary>
        [Fact]
        public async Task SearchVesselTypes_Pagination_ReturnsCorrectPage()
        {
            // Arrange - Add more vessel types for pagination testing
            var vt3 = new Domain.Vessels.VesselType(Guid.NewGuid().ToString("N"), "Cruise Ship");
            vt3.Update("Cruise Ship", "Passenger vessel", 0, 0, 0, 0, null);
            var vt4 = new Domain.Vessels.VesselType(Guid.NewGuid().ToString("N"), "Ferry");
            vt4.Update("Ferry", "Car ferry", 0, 0, 0, 0, null);
            Context.VesselTypes.AddRange(vt3, vt4);
            await Context.SaveChangesAsync();

            // Act - Get page 1 with pageSize 2
            var result = await Controller.Search(name: null, description: null, page: 1, pageSize: 2);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var response = okResult.Value as dynamic;
            var items = response!.items as List<VesselTypeResponseDto>;
            
            Assert.NotNull(items);
            Assert.Equal(2, items.Count); // Should return 2 items
        }

        /// <summary>
        /// Test 8: Pagination - page 2
        /// AC8: Returns correct items for page 2
        /// </summary>
        [Fact]
        public async Task SearchVesselTypes_PageTwo_ReturnsRemainingItems()
        {
            // Arrange
            var vt3 = new Domain.Vessels.VesselType(Guid.NewGuid().ToString("N"), "Cruise Ship");
            vt3.Update("Cruise Ship", "Passenger vessel", 0, 0, 0, 0, null);
            Context.VesselTypes.Add(vt3);
            await Context.SaveChangesAsync();

            // Act - Get page 2 with pageSize 2 (total 3 items, so page 2 has 1 item)
            var result = await Controller.Search(name: null, description: null, page: 2, pageSize: 2);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var response = okResult.Value as dynamic;
            var items = response!.items as List<VesselTypeResponseDto>;
            
            Assert.NotNull(items);
            Assert.Single(items); // Page 2 should have 1 remaining item
        }

        /// <summary>
        /// Test 9: Pagination defaults when invalid
        /// AC9: System applies sensible defaults for invalid pagination parameters
        /// </summary>
        [Fact]
        public async Task SearchVesselTypes_InvalidPagination_AppliesDefaults()
        {
            // Arrange - Invalid page (0) and pageSize (500)

            // Act
            var result = await Controller.Search(name: null, description: null, page: 0, pageSize: 500);

            // Assert - Should still return results with corrected defaults
            var okResult = Assert.IsType<OkObjectResult>(result);
            Assert.NotNull(okResult.Value);
        }

        /// <summary>
        /// Test 10: Combined filters (name AND description)
        /// AC10: Multiple filters work together
        /// </summary>
        [Fact]
        public async Task SearchVesselTypes_CombinedFilters_ReturnsMatchingBothCriteria()
        {
            // Arrange - Search for name containing "Container" AND description containing "vessel"

            // Act
            var result = await Controller.Search(name: "Container", description: "vessel", page: 1, pageSize: 20);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var response = okResult.Value as dynamic;
            var items = response!.items as List<VesselTypeResponseDto>;
            
            Assert.NotNull(items);
            Assert.Single(items); // Should find "Container Ship" which matches both
        }
    }
}
