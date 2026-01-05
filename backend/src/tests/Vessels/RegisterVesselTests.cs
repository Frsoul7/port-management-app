using DDDNetCore.Presentation.Controllers;
using DDDNetCore.Application.DTOs.Vessels;
using Microsoft.AspNetCore.Mvc;
using Tests.Vessels.Base;
using Xunit;

namespace Tests.Vessels;

/// <summary>
/// TEST TYPE: Integration Test
/// COMPONENTS UNDER TEST: VesselsController, VesselRepository, PortDbContext, Vessel (Domain Entity)
/// TEST OBJECTIVE: Validate end-to-end vessel registration (US 2.2.1).
///                 Tests HTTP POST requests, IMO number validation and normalization, vessel name validation,
///                 duplicate IMO prevention, vessel type assignment, shipping agent association,
///                 capacity (TEU) validation, and successful persistence to the database.
/// </summary>
public class RegisterVesselTests : VesselTestBase
{
    [Fact]
    public async Task RegisterVessel_WithValidData_ShouldReturnCreated()
    {
        // Arrange
        var controller = CreateController();
        var dto = new CreateVesselDto(
            ImoNumber: "9176187", // Valid IMO: check digit = 7
            Name: "EMMA MAERSK",
            VesselTypeId: ContainerVesselTypeId.ToString(),
            OrganizationId: ShippingAgentOrgId.ToString(),
            CapacityTEU: 15000
        );

        // Act
        var result = await controller.Create(dto);

        // Assert - Updated: Returns CreatedResult (not CreatedAtActionResult) since GET by IMO was removed
        var createdResult = Assert.IsType<CreatedResult>(result);
        Assert.Equal(201, createdResult.StatusCode);
        var response = Assert.IsType<VesselResponseDto>(createdResult.Value);
        Assert.Equal("9176187", response.ImoNumber);
        Assert.Equal("EMMA MAERSK", response.Name);
        Assert.Equal(15000, response.CapacityTEU);
    }

    [Fact]
    public async Task RegisterVessel_WithAllFieldsIncludingOptional_ShouldPersistCorrectly()
    {
        // Arrange
        var controller = CreateController();
        var dto = new CreateVesselDto(
            ImoNumber: "9321483",
            Name: "CMA CGM MARCO POLO",
            VesselTypeId: ContainerVesselTypeId.ToString(),
            OrganizationId: ShippingAgentOrgId.ToString(),
            CapacityTEU: 16000
        );

        // Act
        await controller.Create(dto);

        // Assert - verify persistence
        var vessel = await DbContext.Vessels.FindAsync("9321483");
        Assert.NotNull(vessel);
        Assert.Equal("CMA CGM MARCO POLO", vessel.Name);
        Assert.Equal(16000, vessel.CapacityTEU);
        Assert.Equal(ContainerVesselTypeId.ToString(), vessel.VesselTypeId);
        Assert.Equal(ShippingAgentOrgId, vessel.OwnerOrganizationId.Value);
    }

    [Fact]
    public async Task RegisterVessel_WithNullCapacityTEU_ShouldSucceed()
    {
        // Arrange - CapacityTEU is optional
        var controller = CreateController();
        var dto = new CreateVesselDto(
            ImoNumber: "9465227", // Fixed: valid IMO with check digit 7
            Name: "NORDIC HALIFAX",
            VesselTypeId: BulkCarrierTypeId.ToString(),
            OrganizationId: ShippingAgentOrgId.ToString(),
            CapacityTEU: null
        );

        // Act
        var result = await controller.Create(dto);

        // Assert - Updated: Returns CreatedResult (not CreatedAtActionResult) since GET by IMO was removed
        var createdResult = Assert.IsType<CreatedResult>(result);
        var response = Assert.IsType<VesselResponseDto>(createdResult.Value);
        Assert.Null(response.CapacityTEU);
    }

    [Fact]
    public async Task RegisterVessel_WithInvalidImoCheckDigit_ShouldReturnBadRequest()
    {
        // Arrange - Check digit should be 7, not 8
        var controller = CreateController();
        var dto = new CreateVesselDto(
            ImoNumber: "9176188",
            Name: "INVALID VESSEL",
            VesselTypeId: ContainerVesselTypeId.ToString(),
            OrganizationId: ShippingAgentOrgId.ToString(),
            CapacityTEU: 10000
        );

        // Act
        var result = await controller.Create(dto);

        // Assert
        var badRequest = Assert.IsType<BadRequestObjectResult>(result);
        Assert.Equal(400, badRequest.StatusCode);
    }

    [Fact]
    public async Task RegisterVessel_WithImoTooShort_ShouldReturnBadRequest()
    {
        // Arrange - IMO must be 7 digits
        var controller = CreateController();
        var dto = new CreateVesselDto(
            ImoNumber: "123456", // Only 6 digits
            Name: "SHORT IMO VESSEL",
            VesselTypeId: ContainerVesselTypeId.ToString(),
            OrganizationId: ShippingAgentOrgId.ToString(),
            CapacityTEU: 5000
        );

        // Act
        var result = await controller.Create(dto);

        // Assert
        var badRequest = Assert.IsType<BadRequestObjectResult>(result);
        Assert.Equal(400, badRequest.StatusCode);
    }

    [Fact]
    public async Task RegisterVessel_WithImoTooLong_ShouldReturnBadRequest()
    {
        // Arrange - IMO must be exactly 7 digits
        var controller = CreateController();
        var dto = new CreateVesselDto(
            ImoNumber: "91761877", // 8 digits
            Name: "LONG IMO VESSEL",
            VesselTypeId: ContainerVesselTypeId.ToString(),
            OrganizationId: ShippingAgentOrgId.ToString(),
            CapacityTEU: 5000
        );

        // Act
        var result = await controller.Create(dto);

        // Assert
        var badRequest = Assert.IsType<BadRequestObjectResult>(result);
        Assert.Equal(400, badRequest.StatusCode);
    }

    [Fact]
    public async Task RegisterVessel_WithNonNumericImo_ShouldReturnBadRequest()
    {
        // Arrange
        var controller = CreateController();
        var dto = new CreateVesselDto(
            ImoNumber: "ABC1234",
            Name: "ALPHA VESSEL",
            VesselTypeId: ContainerVesselTypeId.ToString(),
            OrganizationId: ShippingAgentOrgId.ToString(),
            CapacityTEU: 8000
        );

        // Act
        var result = await controller.Create(dto);

        // Assert
        var badRequest = Assert.IsType<BadRequestObjectResult>(result);
        Assert.Equal(400, badRequest.StatusCode);
    }

    [Fact]
    public async Task RegisterVessel_WithDuplicateImo_ShouldReturnConflict()
    {
        // Arrange - Use existing vessel's IMO
        var controller = CreateController();
        var dto = new CreateVesselDto(
            ImoNumber: ExistingVesselImo, // "9074729" already exists
            Name: "DIFFERENT NAME",
            VesselTypeId: ContainerVesselTypeId.ToString(),
            OrganizationId: ShippingAgentOrgId.ToString(),
            CapacityTEU: 10000
        );

        // Act
        var result = await controller.Create(dto);

        // Assert
        var conflict = Assert.IsType<ConflictObjectResult>(result);
        Assert.Equal(409, conflict.StatusCode);
        Assert.Contains("IMO", conflict.Value?.ToString());
    }

    [Fact]
    public async Task RegisterVessel_WithDuplicateName_ShouldReturnConflict()
    {
        // Arrange - Use existing vessel's name (case-insensitive)
        var controller = CreateController();
        var dto = new CreateVesselDto(
            ImoNumber: "9245328", // Fixed: valid IMO with check digit 8
            Name: "msc oscar", // Same as ExistingVesselName but lowercase
            VesselTypeId: ContainerVesselTypeId.ToString(),
            OrganizationId: ShippingAgentOrgId.ToString(),
            CapacityTEU: 12000
        );

        // Act
        var result = await controller.Create(dto);

        // Assert
        var conflict = Assert.IsType<ConflictObjectResult>(result);
        Assert.Equal(409, conflict.StatusCode);
        Assert.Contains("name", conflict.Value?.ToString(), StringComparison.OrdinalIgnoreCase);
    }

    [Fact]
    public async Task RegisterVessel_WithNonExistentVesselType_ShouldReturnBadRequest()
    {
        // Arrange
        var controller = CreateController();
        var dto = new CreateVesselDto(
            ImoNumber: "9354662", // Fixed: valid IMO with check digit 2
            Name: "INVALID TYPE VESSEL",
            VesselTypeId: Guid.NewGuid().ToString(), // Non-existent type
            OrganizationId: ShippingAgentOrgId.ToString(),
            CapacityTEU: 8000
        );

        // Act
        var result = await controller.Create(dto);

        // Assert
        var badRequest = Assert.IsType<BadRequestObjectResult>(result);
        Assert.Equal(400, badRequest.StatusCode);
        Assert.Contains("VesselType", badRequest.Value?.ToString());
    }

    [Fact]
    public async Task RegisterVessel_WithNonExistentOrganization_ShouldReturnBadRequest()
    {
        // Arrange
        var controller = CreateController();
        var dto = new CreateVesselDto(
            ImoNumber: "9422287", // Fixed: valid IMO with check digit 7
            Name: "INVALID ORG VESSEL",
            VesselTypeId: ContainerVesselTypeId.ToString(),
            OrganizationId: Guid.NewGuid().ToString(), // Non-existent organization
            CapacityTEU: 9000
        );

        // Act
        var result = await controller.Create(dto);

        // Assert
        var badRequest = Assert.IsType<BadRequestObjectResult>(result);
        Assert.Equal(400, badRequest.StatusCode);
        Assert.Contains("Organization", badRequest.Value?.ToString());
    }

    [Fact]
    public async Task RegisterVessel_VerifyImoIsStoredNormalized_ShouldStripWhitespace()
    {
        // Arrange - IMO with leading/trailing spaces (controller normalizes)
        var controller = CreateController();
        var dto = new CreateVesselDto(
            ImoNumber: "  9267455  ",
            Name: "SPACE VESSEL",
            VesselTypeId: ContainerVesselTypeId.ToString(),
            OrganizationId: ShippingAgentOrgId.ToString(),
            CapacityTEU: 7000
        );

        // Act
        await controller.Create(dto);

        // Assert - should be stored without spaces
        var vessel = await DbContext.Vessels.FindAsync("9267455");
        Assert.NotNull(vessel);
        Assert.Equal("9267455", vessel.ImoNumber); // No spaces
    }
}
