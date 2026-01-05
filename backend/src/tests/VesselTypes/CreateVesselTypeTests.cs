using System;
using System.Threading.Tasks;
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
    /// TEST OBJECTIVE: Validate end-to-end vessel type creation (US 2.2.1).
    ///                 Tests HTTP POST requests, DTO validation, required fields (name, capacity, dimensions),
    ///                 optional field handling (description, draft), duplicate name prevention,
    ///                 numeric constraint validation (positive values), and successful persistence to database.
    /// </summary>
    public class CreateVesselTypeTests : VesselTypeTestBase
    {
        #region Happy Path Tests

        /// <summary>
        /// Test 1: Create vessel type with all required and optional fields
        /// AC1: System allows Port Authority Officer to create a new vessel type with all attributes
        /// </summary>
        [Fact]
        public async Task CreateVesselType_AllFields_ReturnsCreatedWithAllData()
        {
            // Arrange
            var createDto = new CreateVesselTypeDto(
                Name: "Bulk Carrier",
                Description: "Large cargo vessel for dry bulk commodities",
                CapacityTEU: 4000,
                MaxRows: 20,
                MaxBays: 22,
                MaxTiers: 9,
                OperationalConstraints: "MARPOL Annex I compliant; requires cargo heating system",
                VesselTypeId: null // Auto-generate
            );

            // Act
            var result = await Controller.Create(createDto);

            // Assert
            var createdResult = Assert.IsType<CreatedResult>(result);
            var response = Assert.IsType<VesselTypeResponseDto>(createdResult.Value);
            
            Assert.NotNull(response.VesselTypeId);
            Assert.Equal("Bulk Carrier", response.Name);
            Assert.Equal("Large cargo vessel for dry bulk commodities", response.Description);
            Assert.Equal(4000, response.CapacityTEU);
            Assert.Equal(20, response.MaxRows);
            Assert.Equal(22, response.MaxBays);
            Assert.Equal(9, response.MaxTiers);
            Assert.Equal("MARPOL Annex I compliant; requires cargo heating system", response.OperationalConstraints);
        }

        /// <summary>
        /// Test 2: Create vessel type with only required fields (name)
        /// AC1: Name is the minimum required field
        /// </summary>
        [Fact]
        public async Task CreateVesselType_OnlyRequiredFields_ReturnsCreatedWithDefaults()
        {
            // Arrange
            var createDto = new CreateVesselTypeDto(
                Name: "RoRo Vessel",
                Description: null,
                CapacityTEU: null,
                MaxRows: null,
                MaxBays: null,
                MaxTiers: null,
                OperationalConstraints: null,
                VesselTypeId: null
            );

            // Act
            var result = await Controller.Create(createDto);

            // Assert
            var createdResult = Assert.IsType<CreatedResult>(result);
            var response = Assert.IsType<VesselTypeResponseDto>(createdResult.Value);
            
            Assert.Equal("RoRo Vessel", response.Name);
            Assert.Null(response.Description);
            Assert.Null(response.CapacityTEU);
            Assert.Null(response.MaxRows);
            Assert.Null(response.MaxBays);
            Assert.Null(response.MaxTiers);
            Assert.Null(response.OperationalConstraints);
        }

        /// <summary>
        /// Test 3: Create vessel type with custom ID
        /// AC2: System generates unique identifier or accepts provided ID
        /// </summary>
        [Fact]
        public async Task CreateVesselType_CustomId_UsesProvidedId()
        {
            // Arrange
            var customId = "VTCUSTOM001";
            var createDto = new CreateVesselTypeDto(
                Name: "Custom ID Vessel",
                Description: null,
                CapacityTEU: null,
                MaxRows: null,
                MaxBays: null,
                MaxTiers: null,
                OperationalConstraints: null,
                VesselTypeId: customId
            );

            // Act
            var result = await Controller.Create(createDto);

            // Assert
            var createdResult = Assert.IsType<CreatedResult>(result);
            var response = Assert.IsType<VesselTypeResponseDto>(createdResult.Value);
            
            Assert.Equal(customId, response.VesselTypeId);
        }

        /// <summary>
        /// Test 4: Create vessel type with auto-generated ID
        /// AC2: System generates unique identifier when not provided
        /// </summary>
        [Fact]
        public async Task CreateVesselType_NoId_GeneratesUniqueId()
        {
            // Arrange
            var createDto = new CreateVesselTypeDto(
                Name: "Auto ID Vessel",
                Description: null,
                CapacityTEU: null,
                MaxRows: null,
                MaxBays: null,
                MaxTiers: null,
                OperationalConstraints: null,
                VesselTypeId: null
            );

            // Act
            var result = await Controller.Create(createDto);

            // Assert
            var createdResult = Assert.IsType<CreatedResult>(result);
            var response = Assert.IsType<VesselTypeResponseDto>(createdResult.Value);
            
            Assert.NotNull(response.VesselTypeId);
            Assert.NotEmpty(response.VesselTypeId);
        }

        /// <summary>
        /// Test 5: Verify vessel type is persisted in database
        /// AC3: Vessel type is stored and can be retrieved
        /// </summary>
        [Fact]
        public async Task CreateVesselType_ValidData_PersistedInDatabase()
        {
            // Arrange
            var createDto = new CreateVesselTypeDto(
                Name: "Cruise Ship",
                Description: "Passenger cruise vessel",
                CapacityTEU: 0,
                MaxRows: 0,
                MaxBays: 0,
                MaxTiers: 0,
                OperationalConstraints: "SOLAS passenger ship regulations",
                VesselTypeId: null
            );

            // Act
            var result = await Controller.Create(createDto);
            var createdResult = Assert.IsType<CreatedResult>(result);
            var response = Assert.IsType<VesselTypeResponseDto>(createdResult.Value);

            // Assert - Verify in database
            var savedVesselType = await Context.VesselTypes
                .AsNoTracking()
                .FirstOrDefaultAsync(vt => vt.VesselTypeId == response.VesselTypeId);
            
            Assert.NotNull(savedVesselType);
            Assert.Equal("Cruise Ship", savedVesselType.Name);
            Assert.Equal("Passenger cruise vessel", savedVesselType.Description);
        }

        /// <summary>
        /// Test 6: Create vessel type with maximum valid values
        /// AC4: System accepts large but valid numeric values
        /// </summary>
        [Fact]
        public async Task CreateVesselType_MaximumValues_AcceptsLargeNumbers()
        {
            // Arrange
            var createDto = new CreateVesselTypeDto(
                Name: "Ultra Large Container Vessel",
                Description: "Largest container ship category",
                CapacityTEU: 24000,
                MaxRows: 50,
                MaxBays: 60,
                MaxTiers: 20,
                OperationalConstraints: null,
                VesselTypeId: null
            );

            // Act
            var result = await Controller.Create(createDto);

            // Assert
            var createdResult = Assert.IsType<CreatedResult>(result);
            var response = Assert.IsType<VesselTypeResponseDto>(createdResult.Value);
            
            Assert.Equal(24000, response.CapacityTEU);
            Assert.Equal(50, response.MaxRows);
            Assert.Equal(60, response.MaxBays);
            Assert.Equal(20, response.MaxTiers);
        }

        /// <summary>
        /// Test 7: Create vessel type with zero values for numeric fields
        /// AC5: Zero is a valid value (e.g., for non-container vessels)
        /// </summary>
        [Fact]
        public async Task CreateVesselType_ZeroValues_AcceptsZeros()
        {
            // Arrange
            var createDto = new CreateVesselTypeDto(
                Name: "Research Vessel",
                Description: "Scientific research ship with no cargo capacity",
                CapacityTEU: 0,
                MaxRows: 0,
                MaxBays: 0,
                MaxTiers: 0,
                OperationalConstraints: null,
                VesselTypeId: null
            );

            // Act
            var result = await Controller.Create(createDto);

            // Assert
            var createdResult = Assert.IsType<CreatedResult>(result);
            var response = Assert.IsType<VesselTypeResponseDto>(createdResult.Value);
            
            Assert.Equal(0, response.CapacityTEU);
            Assert.Equal(0, response.MaxRows);
            Assert.Equal(0, response.MaxBays);
            Assert.Equal(0, response.MaxTiers);
        }

        #endregion

        #region Validation Tests

        /// <summary>
        /// Test 8: Create vessel type without name (required field)
        /// AC6: Name is mandatory - returns BadRequest
        /// </summary>
        [Fact]
        public async Task CreateVesselType_MissingName_ReturnsBadRequest()
        {
            // Arrange
            var createDto = new CreateVesselTypeDto(
                Name: null!, // Required field missing
                Description: "Test description",
                CapacityTEU: null,
                MaxRows: null,
                MaxBays: null,
                MaxTiers: null,
                OperationalConstraints: null,
                VesselTypeId: null
            );

            // Act
            var result = await Controller.Create(createDto);

            // Assert
            var badRequestResult = Assert.IsType<BadRequestObjectResult>(result);
            Assert.NotNull(badRequestResult.Value);
        }

        /// <summary>
        /// Test 9: Create vessel type with empty name
        /// AC6: Empty name is invalid - returns BadRequest
        /// </summary>
        [Fact]
        public async Task CreateVesselType_EmptyName_ReturnsBadRequest()
        {
            // Arrange
            var createDto = new CreateVesselTypeDto(
                Name: "   ", // Whitespace only
                Description: "Test description",
                CapacityTEU: null,
                MaxRows: null,
                MaxBays: null,
                MaxTiers: null,
                OperationalConstraints: null,
                VesselTypeId: null
            );

            // Act
            var result = await Controller.Create(createDto);

            // Assert
            var badRequestResult = Assert.IsType<BadRequestObjectResult>(result);
            Assert.NotNull(badRequestResult.Value);
        }

        /// <summary>
        /// Test 10: Create vessel type with duplicate name (case-insensitive)
        /// AC7: Name must be unique - returns Conflict (409)
        /// </summary>
        [Fact]
        public async Task CreateVesselType_DuplicateName_ReturnsConflict()
        {
            // Arrange - "Container Ship" already exists in seed data
            var createDto = new CreateVesselTypeDto(
                Name: "container ship", // Different case but same name
                Description: "Duplicate test",
                CapacityTEU: null,
                MaxRows: null,
                MaxBays: null,
                MaxTiers: null,
                OperationalConstraints: null,
                VesselTypeId: null
            );

            // Act
            var result = await Controller.Create(createDto);

            // Assert
            var conflictResult = Assert.IsType<ConflictObjectResult>(result);
            Assert.NotNull(conflictResult.Value);
        }

        /// <summary>
        /// Test 11: Create vessel type with duplicate name (exact case)
        /// AC7: Name uniqueness is enforced
        /// </summary>
        [Fact]
        public async Task CreateVesselType_ExactDuplicateName_ReturnsConflict()
        {
            // Arrange
            var createDto = new CreateVesselTypeDto(
                Name: "Container Ship", // Exact match with seed data
                Description: "Duplicate test",
                CapacityTEU: null,
                MaxRows: null,
                MaxBays: null,
                MaxTiers: null,
                OperationalConstraints: null,
                VesselTypeId: null
            );

            // Act
            var result = await Controller.Create(createDto);

            // Assert
            Assert.IsType<ConflictObjectResult>(result);
        }

        /// <summary>
        /// Test 12: Create vessel type with negative CapacityTEU
        /// AC8: Negative values are invalid - domain should throw exception
        /// </summary>
        [Fact]
        public async Task CreateVesselType_NegativeCapacityTEU_ThrowsException()
        {
            // Arrange
            var createDto = new CreateVesselTypeDto(
                Name: "Invalid Capacity Vessel",
                Description: null,
                CapacityTEU: -100, // Invalid negative value
                MaxRows: null,
                MaxBays: null,
                MaxTiers: null,
                OperationalConstraints: null,
                VesselTypeId: null
            );

            // Act
            var result = await Controller.Create(createDto);

            // Assert
            var badRequest = Assert.IsType<BadRequestObjectResult>(result);
            Assert.NotNull(badRequest.Value);
        }

        /// <summary>
        /// Test 13: Create vessel type with negative MaxRows
        /// AC8: Negative values are invalid
        /// </summary>
        [Fact]
        public async Task CreateVesselType_NegativeMaxRows_ThrowsException()
        {
            // Arrange
            var createDto = new CreateVesselTypeDto(
                Name: "Invalid Rows Vessel",
                Description: null,
                CapacityTEU: null,
                MaxRows: -10, // Invalid negative value
                MaxBays: null,
                MaxTiers: null,
                OperationalConstraints: null,
                VesselTypeId: null
            );

            // Act
            var result = await Controller.Create(createDto);

            // Assert
            var badRequest = Assert.IsType<BadRequestObjectResult>(result);
            Assert.NotNull(badRequest.Value);
        }

        /// <summary>
        /// Test 14: Create vessel type with negative MaxBays
        /// AC8: Negative values are invalid
        /// </summary>
        [Fact]
        public async Task CreateVesselType_NegativeMaxBays_ThrowsException()
        {
            // Arrange
            var createDto = new CreateVesselTypeDto(
                Name: "Invalid Bays Vessel",
                Description: null,
                CapacityTEU: null,
                MaxRows: null,
                MaxBays: -5, // Invalid negative value
                MaxTiers: null,
                OperationalConstraints: null,
                VesselTypeId: null
            );

            // Act
            var result = await Controller.Create(createDto);

            // Assert
            var badRequest = Assert.IsType<BadRequestObjectResult>(result);
            Assert.NotNull(badRequest.Value);
        }

        /// <summary>
        /// Test 15: Create vessel type with negative MaxTiers
        /// AC8: Negative values are invalid
        /// </summary>
        [Fact]
        public async Task CreateVesselType_NegativeMaxTiers_ThrowsException()
        {
            // Arrange
            var createDto = new CreateVesselTypeDto(
                Name: "Invalid Tiers Vessel",
                Description: null,
                CapacityTEU: null,
                MaxRows: null,
                MaxBays: null,
                MaxTiers: -3, // Invalid negative value
                OperationalConstraints: null,
                VesselTypeId: null
            );

            // Act
            var result = await Controller.Create(createDto);

            // Assert
            var badRequest = Assert.IsType<BadRequestObjectResult>(result);
            Assert.NotNull(badRequest.Value);
        }

        #endregion

        #region Edge Cases

        /// <summary>
        /// Test 16: Create vessel type with very long name
        /// AC9: System handles long names appropriately
        /// </summary>
        [Fact]
        public async Task CreateVesselType_VeryLongName_AcceptsLongString()
        {
            // Arrange
            var longName = new string('A', 200); // 200 characters
            var createDto = new CreateVesselTypeDto(
                Name: longName,
                Description: null,
                CapacityTEU: null,
                MaxRows: null,
                MaxBays: null,
                MaxTiers: null,
                OperationalConstraints: null,
                VesselTypeId: null
            );

            // Act
            var result = await Controller.Create(createDto);

            // Assert
            var createdResult = Assert.IsType<CreatedResult>(result);
            var response = Assert.IsType<VesselTypeResponseDto>(createdResult.Value);
            Assert.Equal(longName, response.Name);
        }

        /// <summary>
        /// Test 17: Create vessel type with name containing leading/trailing whitespace
        /// AC10: System trims whitespace from names
        /// </summary>
        [Fact]
        public async Task CreateVesselType_NameWithWhitespace_TrimsWhitespace()
        {
            // Arrange
            var createDto = new CreateVesselTypeDto(
                Name: "  Ferry Vessel  ", // Leading and trailing spaces
                Description: null,
                CapacityTEU: null,
                MaxRows: null,
                MaxBays: null,
                MaxTiers: null,
                OperationalConstraints: null,
                VesselTypeId: null
            );

            // Act
            var result = await Controller.Create(createDto);

            // Assert
            var createdResult = Assert.IsType<CreatedResult>(result);
            var response = Assert.IsType<VesselTypeResponseDto>(createdResult.Value);
            Assert.Equal("Ferry Vessel", response.Name); // Whitespace trimmed
        }

        /// <summary>
        /// Test 18: Create multiple vessel types in sequence
        /// AC11: System allows creating multiple vessel types
        /// </summary>
        [Fact]
        public async Task CreateVesselType_MultipleCreations_AllSucceed()
        {
            // Arrange
            var dto1 = new CreateVesselTypeDto("Type A", null, null, null, null, null, null, null);
            var dto2 = new CreateVesselTypeDto("Type B", null, null, null, null, null, null, null);
            var dto3 = new CreateVesselTypeDto("Type C", null, null, null, null, null, null, null);

            // Act
            var result1 = await Controller.Create(dto1);
            var result2 = await Controller.Create(dto2);
            var result3 = await Controller.Create(dto3);

            // Assert
            Assert.IsType<CreatedResult>(result1);
            Assert.IsType<CreatedResult>(result2);
            Assert.IsType<CreatedResult>(result3);

            var totalCount = await Context.VesselTypes.CountAsync();
            Assert.Equal(5, totalCount); // 2 from seed + 3 created
        }

        #endregion
    }
}
