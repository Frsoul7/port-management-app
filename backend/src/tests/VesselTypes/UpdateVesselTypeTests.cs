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
    /// TEST OBJECTIVE: Validate vessel type update operations (US 2.2.1 - AC1: Port Authority Officer can update).
    ///                 Tests HTTP PUT requests, mutable field updates (name, capacity, dimensions, description),
    ///                 validation of updated values, duplicate name prevention during updates,
    ///                 numeric constraint enforcement, and proper persistence of modified vessel type data.
    /// </summary>
    public class UpdateVesselTypeTests : VesselTypeTestBase
    {
        /// <summary>
        /// Test 1: Update vessel type with all fields
        /// AC1: Port Authority Officer can update existing vessel type
        /// </summary>
        [Fact]
        public async Task UpdateVesselType_AllFields_ReturnsOkWithUpdatedData()
        {
            // Arrange - Get existing vessel type from seed data
            var existingVt = await Context.VesselTypes.FirstAsync(vt => vt.Name == "Container Ship");
            var updateDto = new UpdateVesselTypeDto(
                Name: "Updated Container Ship",
                Description: "Updated description for container vessels",
                CapacityTEU: 6000,
                MaxRows: 25,
                MaxBays: 28,
                MaxTiers: 12,
                OperationalConstraints: "Updated operational constraints"
            );

            // Act
            var result = await Controller.Update(existingVt.VesselTypeId, updateDto);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var response = Assert.IsType<VesselTypeResponseDto>(okResult.Value);
            
            Assert.Equal("Updated Container Ship", response.Name);
            Assert.Equal("Updated description for container vessels", response.Description);
            Assert.Equal(6000, response.CapacityTEU);
            Assert.Equal(25, response.MaxRows);
            Assert.Equal(28, response.MaxBays);
            Assert.Equal(12, response.MaxTiers);
        }

        /// <summary>
        /// Test 2: Update only specific fields (partial update)
        /// AC2: System allows partial updates (nullable fields in DTO)
        /// </summary>
        [Fact]
        public async Task UpdateVesselType_PartialUpdate_OnlyUpdatesSpecifiedFields()
        {
            // Arrange
            var existingVt = await Context.VesselTypes.FirstAsync(vt => vt.Name == "Tanker");
            var originalDescription = existingVt.Description;
            var updateDto = new UpdateVesselTypeDto(
                Name: null, // Don't update name
                Description: null, // Don't update description
                CapacityTEU: 4000, // Update this
                MaxRows: null, // Don't update
                MaxBays: null,
                MaxTiers: null,
                OperationalConstraints: null
            );

            // Act
            var result = await Controller.Update(existingVt.VesselTypeId, updateDto);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var response = Assert.IsType<VesselTypeResponseDto>(okResult.Value);
            
            Assert.Equal("Tanker", response.Name); // Unchanged
            Assert.Equal(originalDescription, response.Description); // Unchanged
            Assert.Equal(4000, response.CapacityTEU); // Updated
        }

        /// <summary>
        /// Test 3: Update non-existent vessel type
        /// AC3: Returns 404 NotFound for invalid ID
        /// </summary>
        [Fact]
        public async Task UpdateVesselType_NonExistentId_ReturnsNotFound()
        {
            // Arrange
            var nonExistentId = "NONEXISTENT123";
            var updateDto = new UpdateVesselTypeDto(
                Name: "Test",
                Description: null,
                CapacityTEU: null,
                MaxRows: null,
                MaxBays: null,
                MaxTiers: null,
                OperationalConstraints: null
            );

            // Act
            var result = await Controller.Update(nonExistentId, updateDto);

            // Assert
            Assert.IsType<NotFoundResult>(result);
        }

        /// <summary>
        /// Test 4: Update vessel type name to duplicate name
        /// AC4: Name uniqueness is enforced - returns 409 Conflict
        /// </summary>
        [Fact]
        public async Task UpdateVesselType_DuplicateName_ReturnsConflict()
        {
            // Arrange - Try to rename "Tanker" to "Container Ship" (which exists)
            var tankerVt = await Context.VesselTypes.FirstAsync(vt => vt.Name == "Tanker");
            var updateDto = new UpdateVesselTypeDto(
                Name: "Container Ship", // This name already exists
                Description: null,
                CapacityTEU: null,
                MaxRows: null,
                MaxBays: null,
                MaxTiers: null,
                OperationalConstraints: null
            );

            // Act
            var result = await Controller.Update(tankerVt.VesselTypeId, updateDto);

            // Assert
            var conflictResult = Assert.IsType<ConflictObjectResult>(result);
            Assert.NotNull(conflictResult.Value);
        }

        /// <summary>
        /// Test 5: Update with negative values
        /// AC5: Validation rules enforced - negative values rejected
        /// </summary>
        [Fact]
        public async Task UpdateVesselType_NegativeValues_ThrowsException()
        {
            // Arrange
            var existingVt = await Context.VesselTypes.FirstAsync();
            var updateDto = new UpdateVesselTypeDto(
                Name: null,
                Description: null,
                CapacityTEU: -500, // Invalid negative value
                MaxRows: null,
                MaxBays: null,
                MaxTiers: null,
                OperationalConstraints: null
            );

            // Act
            var result = await Controller.Update(existingVt.VesselTypeId, updateDto);

            // Assert
            var badRequest = Assert.IsType<BadRequestObjectResult>(result);
            Assert.NotNull(badRequest.Value);
        }

        /// <summary>
        /// Test 6: Update changes are persisted in database
        /// AC6: Updates are saved to database
        /// </summary>
        [Fact]
        public async Task UpdateVesselType_ValidUpdate_PersistedInDatabase()
        {
            // Arrange
            var existingVt = await Context.VesselTypes.FirstAsync(vt => vt.Name == "Container Ship");
            var updateDto = new UpdateVesselTypeDto(
                Name: "Container Carrier",
                Description: "Modified description",
                CapacityTEU: 7000,
                MaxRows: null,
                MaxBays: null,
                MaxTiers: null,
                OperationalConstraints: null
            );

            // Act
            await Controller.Update(existingVt.VesselTypeId, updateDto);

            // Assert - Verify in database
            Context.ChangeTracker.Clear(); // Clear tracking to force fresh read
            var updated = await Context.VesselTypes
                .AsNoTracking()
                .FirstAsync(vt => vt.VesselTypeId == existingVt.VesselTypeId);
            
            Assert.Equal("Container Carrier", updated.Name);
            Assert.Equal("Modified description", updated.Description);
            Assert.Equal(7000, updated.CapacityTEU);
        }

        /// <summary>
        /// Test 7: Update name with case-insensitive duplicate check
        /// AC7: Name uniqueness check is case-insensitive
        /// </summary>
        [Fact]
        public async Task UpdateVesselType_DuplicateNameDifferentCase_ReturnsConflict()
        {
            // Arrange
            var tankerVt = await Context.VesselTypes.FirstAsync(vt => vt.Name == "Tanker");
            var updateDto = new UpdateVesselTypeDto(
                Name: "container ship", // Different case but same as existing "Container Ship"
                Description: null,
                CapacityTEU: null,
                MaxRows: null,
                MaxBays: null,
                MaxTiers: null,
                OperationalConstraints: null
            );

            // Act
            var result = await Controller.Update(tankerVt.VesselTypeId, updateDto);

            // Assert
            Assert.IsType<ConflictObjectResult>(result);
        }

        /// <summary>
        /// Test 8: Update vessel type to same name (no conflict with itself)
        /// AC8: Can update vessel type keeping same name
        /// </summary>
        [Fact]
        public async Task UpdateVesselType_SameName_NoConflict()
        {
            // Arrange - Update Container Ship keeping the same name
            var existingVt = await Context.VesselTypes.FirstAsync(vt => vt.Name == "Container Ship");
            var updateDto = new UpdateVesselTypeDto(
                Name: "Container Ship", // Same name
                Description: "Updated description only",
                CapacityTEU: null,
                MaxRows: null,
                MaxBays: null,
                MaxTiers: null,
                OperationalConstraints: null
            );

            // Act
            var result = await Controller.Update(existingVt.VesselTypeId, updateDto);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var response = Assert.IsType<VesselTypeResponseDto>(okResult.Value);
            Assert.Equal("Container Ship", response.Name);
            Assert.Equal("Updated description only", response.Description);
        }

        /// <summary>
        /// Test 9: Update with trimmed whitespace
        /// AC9: System trims whitespace from input
        /// </summary>
        [Fact]
        public async Task UpdateVesselType_WhitespaceInName_TrimsWhitespace()
        {
            // Arrange
            var existingVt = await Context.VesselTypes.FirstAsync(vt => vt.Name == "Tanker");
            var updateDto = new UpdateVesselTypeDto(
                Name: "  Bulk Tanker  ", // Leading and trailing spaces
                Description: null,
                CapacityTEU: null,
                MaxRows: null,
                MaxBays: null,
                MaxTiers: null,
                OperationalConstraints: null
            );

            // Act
            var result = await Controller.Update(existingVt.VesselTypeId, updateDto);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var response = Assert.IsType<VesselTypeResponseDto>(okResult.Value);
            Assert.Equal("Bulk Tanker", response.Name); // Whitespace trimmed
        }

        /// <summary>
        /// Test 10: Update with zero values
        /// AC10: Zero is valid for numeric fields
        /// </summary>
        [Fact]
        public async Task UpdateVesselType_ZeroValues_AcceptsZeros()
        {
            // Arrange
            var existingVt = await Context.VesselTypes.FirstAsync();
            var updateDto = new UpdateVesselTypeDto(
                Name: null,
                Description: null,
                CapacityTEU: 0, // Zero is valid (non-container vessel)
                MaxRows: 0,
                MaxBays: 0,
                MaxTiers: 0,
                OperationalConstraints: null
            );

            // Act
            var result = await Controller.Update(existingVt.VesselTypeId, updateDto);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            var response = Assert.IsType<VesselTypeResponseDto>(okResult.Value);
            Assert.Equal(0, response.CapacityTEU);
            Assert.Equal(0, response.MaxRows);
            Assert.Equal(0, response.MaxBays);
            Assert.Equal(0, response.MaxTiers);
        }
    }
}
