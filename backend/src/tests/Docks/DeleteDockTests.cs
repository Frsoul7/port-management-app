using System.Threading.Tasks;
using Xunit;
using Microsoft.AspNetCore.Mvc;
using DDDNetCore.Application.DTOs.Docks;
using DDDNetCore.Tests.Docks.Base;

namespace DDDNetCore.Tests.Docks
{
    /// <summary>
    /// TEST TYPE: Integration Test
    /// COMPONENTS UNDER TEST: DocksController, DockRepository, PortDbContext, Dock (Domain Entity)
    /// TEST OBJECTIVE: Validate dock retrieval and deletion functionality (US 2.2.3).
    ///                 Tests HTTP GET by code, HTTP DELETE operations, 404 handling for non-existent docks,
    ///                 and proper removal from database including cascading effects.
    /// </summary>
    public class DeleteDockTests : DockTestBase
    {
        /// <summary>
        /// Test 1: Get dock by code
        /// </summary>
        [Fact]
        public async Task GetDockByCode_ValidCode_ReturnsDock()
        {
            // Arrange
            await Controller.Create(new CreateDockDto("GET01", "Test Dock", "Test Location", 300, 15, 14, null));

            // Act
            var result = await Controller.GetByCode("GET01");

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var dock = Assert.IsType<DockResponseDto>(okResult.Value);
            
            Assert.Equal("GET01", dock.Code);
            Assert.Equal("Test Dock", dock.Name);
        }

        /// <summary>
        /// Test 2: Get non-existent dock
        /// </summary>
        [Fact]
        public async Task GetDockByCode_InvalidCode_ReturnsNotFound()
        {
            // Act
            var result = await Controller.GetByCode("NONEXISTENT");

            // Assert
            Assert.IsType<NotFoundObjectResult>(result);
        }

        /// <summary>
        /// Test 3: Delete dock
        /// </summary>
        [Fact]
        public async Task DeleteDock_ValidCode_DeletesSuccessfully()
        {
            // Arrange
            await Controller.Create(new CreateDockDto("DEL01", "To Delete", "Test Location", 300, 15, 14, null));

            // Act
            var deleteResult = await Controller.Delete("DEL01");
            var getResult = await Controller.GetByCode("DEL01");

            // Assert
            Assert.IsType<NoContentResult>(deleteResult);
            Assert.IsType<NotFoundObjectResult>(getResult);
        }

        /// <summary>
        /// Test 4: Delete non-existent dock
        /// </summary>
        [Fact]
        public async Task DeleteDock_InvalidCode_ReturnsNotFound()
        {
            // Act
            var result = await Controller.Delete("NONEXISTENT");

            // Assert
            Assert.IsType<NotFoundObjectResult>(result);
        }
    }
}
