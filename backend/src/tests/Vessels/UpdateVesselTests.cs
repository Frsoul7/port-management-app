using DDDNetCore.Presentation.Controllers;
using DDDNetCore.Application.DTOs.Vessels;
using Microsoft.AspNetCore.Mvc;
using Tests.Vessels.Base;
using Xunit;

namespace Tests.Vessels;

/// <summary>
/// TEST TYPE: Integration Test
/// COMPONENTS UNDER TEST: VesselsController, VesselRepository, PortDbContext, Vessel (Domain Entity)
/// TEST OBJECTIVE: Validate vessel information updates (US 2.2.1).
///                 Tests HTTP PUT requests, mutable field updates (name, vessel type, capacity),
///                 shipping agent reassignment, IMO number immutability (cannot be changed after registration),
///                 validation of updated values, and proper persistence of modified vessel data.
/// </summary>
public class UpdateVesselTests : VesselTestBase
{
    [Fact]
    public async Task UpdateVessel_WithAllFields_ShouldReturnOkAndPersist()
    {
        // Arrange
        var controller = CreateController();
        var dto = new UpdateVesselDto(
            Name: "MSC OSCAR UPDATED",
            VesselTypeId: BulkCarrierTypeId.ToString(), // Change from Container to Bulk
            OrganizationId: ShippingAgentOrgId.ToString(), // Keep same shipping agent owner
            CapacityTEU: 20000 // Update capacity
        );

        // Act
        var result = await controller.Update(ExistingVesselImo, dto);

        // Assert
        var okResult = Assert.IsType<OkObjectResult>(result);
        var response = Assert.IsType<VesselResponseDto>(okResult.Value);
        Assert.Equal("MSC OSCAR UPDATED", response.Name);
        Assert.Equal(BulkCarrierTypeId.ToString(), response.VesselTypeId);
        Assert.Equal(ShippingAgentOrgId.ToString(), response.OrganizationId);
        Assert.Equal(20000, response.CapacityTEU);

        // Verify persistence
        var vessel = await DbContext.Vessels.FindAsync(ExistingVesselImo);
        Assert.NotNull(vessel);
        Assert.Equal("MSC OSCAR UPDATED", vessel.Name);
        Assert.Equal(BulkCarrierTypeId.ToString(), vessel.VesselTypeId);
    }

    [Fact]
    public async Task UpdateVessel_WithPartialFields_ShouldUpdateOnlyProvidedFields()
    {
        // Arrange - Only update name and capacity
        var controller = CreateController();
        var dto = new UpdateVesselDto(
            Name: "MSC OSCAR RENAMED",
            VesselTypeId: ContainerVesselTypeId.ToString(), // Keep same type
            OrganizationId: ShippingAgentOrgId.ToString(), // Keep same owner
            CapacityTEU: 19000
        );

        // Act
        var result = await controller.Update(ExistingVesselImo, dto);

        // Assert
        var okResult = Assert.IsType<OkObjectResult>(result);
        var response = Assert.IsType<VesselResponseDto>(okResult.Value);
        Assert.Equal("MSC OSCAR RENAMED", response.Name);
        Assert.Equal(19000, response.CapacityTEU);
        Assert.Equal(ExistingVesselImo, response.ImoNumber); // IMO unchanged
    }

    [Fact]
    public async Task UpdateVessel_NonExistent_ShouldReturnNotFound()
    {
        // Arrange
        var controller = CreateController();
        var dto = new UpdateVesselDto(
            Name: "GHOST SHIP",
            VesselTypeId: ContainerVesselTypeId.ToString(),
            OrganizationId: ShippingAgentOrgId.ToString(),
            CapacityTEU: 1000
        );

        // Act
        var result = await controller.Update("9999999", dto); // Non-existent IMO

        // Assert
        var notFound = Assert.IsType<NotFoundResult>(result);
        Assert.Equal(404, notFound.StatusCode);
    }

    [Fact]
    public async Task UpdateVessel_WithDuplicateName_ShouldReturnConflict()
    {
        // Arrange - Create another vessel first
        var controller = CreateController();
        // IMO 9362255: 9*7 + 3*6 + 6*5 + 2*4 + 2*3 + 5*2 = 135, check digit = 5
        var newVessel = new DDDNetCore.Domain.Vessels.Vessel(
            "9362255", // Valid IMO
            "MAERSK TRIPLE E",
            ContainerVesselTypeId.ToString(),
            new DDDNetCore.Domain.Organizations.OrganizationId(ShippingAgentOrgId),
            18340
        );
        DbContext.Vessels.Add(newVessel);
        await DbContext.SaveChangesAsync();

        // Try to update existing vessel with the new vessel's name
        var dto = new UpdateVesselDto(
            Name: "MAERSK TRIPLE E", // Duplicate name
            VesselTypeId: ContainerVesselTypeId.ToString(),
            OrganizationId: ShippingAgentOrgId.ToString(),
            CapacityTEU: 18000
        );

        // Act
        var result = await controller.Update(ExistingVesselImo, dto);

        // Assert
        var conflict = Assert.IsType<ConflictObjectResult>(result);
        Assert.Equal(409, conflict.StatusCode);
        Assert.Contains("name", conflict.Value?.ToString(), StringComparison.OrdinalIgnoreCase);
    }

    [Fact]
    public async Task UpdateVessel_WithSameName_ShouldSucceed()
    {
        // Arrange - Update with same name (no self-conflict)
        var controller = CreateController();
        var dto = new UpdateVesselDto(
            Name: ExistingVesselName, // Same name
            VesselTypeId: BulkCarrierTypeId.ToString(), // Change type
            OrganizationId: ShippingAgentOrgId.ToString(),
            CapacityTEU: 17000
        );

        // Act
        var result = await controller.Update(ExistingVesselImo, dto);

        // Assert
        var okResult = Assert.IsType<OkObjectResult>(result);
        var response = Assert.IsType<VesselResponseDto>(okResult.Value);
        Assert.Equal(ExistingVesselName, response.Name); // Name unchanged
        Assert.Equal(BulkCarrierTypeId.ToString(), response.VesselTypeId); // Type changed
    }

    [Fact]
    public async Task UpdateVessel_WithNonExistentVesselType_ShouldReturnBadRequest()
    {
        // Arrange
        var controller = CreateController();
        var dto = new UpdateVesselDto(
            Name: "MSC OSCAR",
            VesselTypeId: Guid.NewGuid().ToString(), // Non-existent vessel type
            OrganizationId: ShippingAgentOrgId.ToString(),
            CapacityTEU: 18000
        );

        // Act
        var result = await controller.Update(ExistingVesselImo, dto);

        // Assert
        var badRequest = Assert.IsType<BadRequestObjectResult>(result);
        Assert.Equal(400, badRequest.StatusCode);
        Assert.Contains("VesselType", badRequest.Value?.ToString());
    }

    [Fact]
    public async Task UpdateVessel_WithNonExistentOrganization_ShouldReturnBadRequest()
    {
        // Arrange
        var controller = CreateController();
        var dto = new UpdateVesselDto(
            Name: "MSC OSCAR",
            VesselTypeId: ContainerVesselTypeId.ToString(),
            OrganizationId: Guid.NewGuid().ToString(), // Non-existent organization
            CapacityTEU: 18000
        );

        // Act
        var result = await controller.Update(ExistingVesselImo, dto);

        // Assert
        var badRequest = Assert.IsType<BadRequestObjectResult>(result);
        Assert.Equal(400, badRequest.StatusCode);
        Assert.Contains("Organization", badRequest.Value?.ToString());
    }

    [Fact]
    public async Task UpdateVessel_WithNullCapacityTEU_ShouldSucceed()
    {
        // Arrange - Set capacity to null
        var controller = CreateController();
        var dto = new UpdateVesselDto(
            Name: "MSC OSCAR",
            VesselTypeId: ContainerVesselTypeId.ToString(),
            OrganizationId: ShippingAgentOrgId.ToString(),
            CapacityTEU: null
        );

        // Act
        var result = await controller.Update(ExistingVesselImo, dto);

        // Assert
        var okResult = Assert.IsType<OkObjectResult>(result);
        var response = Assert.IsType<VesselResponseDto>(okResult.Value);
        Assert.Null(response.CapacityTEU);
    }

    [Fact]
    public async Task UpdateVessel_WithZeroCapacityTEU_ShouldSucceed()
    {
        // Arrange - Zero capacity (e.g., non-container vessels)
        var controller = CreateController();
        var dto = new UpdateVesselDto(
            Name: "MSC OSCAR",
            VesselTypeId: BulkCarrierTypeId.ToString(), // Bulk carriers don't have TEU
            OrganizationId: ShippingAgentOrgId.ToString(),
            CapacityTEU: 0
        );

        // Act
        var result = await controller.Update(ExistingVesselImo, dto);

        // Assert
        var okResult = Assert.IsType<OkObjectResult>(result);
        var response = Assert.IsType<VesselResponseDto>(okResult.Value);
        Assert.Equal(0, response.CapacityTEU);
    }

    [Fact]
    public async Task UpdateVessel_ImoIsImmutable_CannotChangeViaUpdate()
    {
        // Arrange
        var controller = CreateController();
        var originalImo = ExistingVesselImo;
        var dto = new UpdateVesselDto(
            Name: "UPDATED NAME",
            VesselTypeId: ContainerVesselTypeId.ToString(),
            OrganizationId: ShippingAgentOrgId.ToString(),
            CapacityTEU: 15000
        );

        // Act
        var result = await controller.Update(originalImo, dto);

        // Assert
        var okResult = Assert.IsType<OkObjectResult>(result);
        var response = Assert.IsType<VesselResponseDto>(okResult.Value);
        Assert.Equal(originalImo, response.ImoNumber); // IMO unchanged

        // Verify in database
        var vessel = await DbContext.Vessels.FindAsync(originalImo);
        Assert.NotNull(vessel);
        Assert.Equal(originalImo, vessel.ImoNumber); // Still the same IMO
    }
}
