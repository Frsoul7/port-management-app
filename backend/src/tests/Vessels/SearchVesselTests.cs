using DDDNetCore.Presentation.Controllers;
using DDDNetCore.Application.DTOs.Vessels;
using Microsoft.AspNetCore.Mvc;
using Tests.Vessels.Base;
using Xunit;

namespace Tests.Vessels;

/// <summary>
/// TEST TYPE: Integration Test
/// COMPONENTS UNDER TEST: VesselsController, VesselRepository, PortDbContext, Vessel (Domain Entity)
/// TEST OBJECTIVE: Validate vessel search and filtering functionality (US 2.2.1).
///                 Tests HTTP GET with query parameters, filtering by IMO number/name/vessel type/shipping agent,
///                 partial name matching, case-insensitive search, multiple filter combinations,
///                 and proper return of filtered vessel collections.
/// </summary>
public class SearchVesselTests : VesselTestBase
{
    public SearchVesselTests()
    {
        // Add more vessels for search testing
        SeedAdditionalVessels();
    }

    private void SeedAdditionalVessels()
    {
        // IMO 9362255: 9*7 + 3*6 + 6*5 + 2*4 + 2*3 + 5*2 = 135, check digit = 5
        var vessel2 = new DDDNetCore.Domain.Vessels.Vessel(
            "9362255", // Changed to valid IMO
            "MAERSK TRIPLE E",
            ContainerVesselTypeId.ToString(),
            new DDDNetCore.Domain.Organizations.OrganizationId(ShippingAgentOrgId),
            18340
        );

        // IMO 9321483 is already valid: 9*7 + 3*6 + 2*5 + 1*4 + 4*3 + 8*2 = 123, check digit = 3
        var vessel3 = new DDDNetCore.Domain.Vessels.Vessel(
            "9321483",
            "CMA CGM MARCO POLO",
            ContainerVesselTypeId.ToString(),
            new DDDNetCore.Domain.Organizations.OrganizationId(ShippingAgent2OrgId), // Different shipping agent
            16020
        );

        DbContext.Vessels.AddRange(vessel2, vessel3);
        DbContext.SaveChanges();
    }

    [Fact]
    public async Task SearchVessels_NoFilters_ShouldReturnAllVessels()
    {
        // Arrange
        var controller = CreateController();

        // Act
        var result = await controller.Search(null, null, null);

        // Assert
        var okResult = Assert.IsType<OkObjectResult>(result);
        var vessels = Assert.IsAssignableFrom<List<VesselResponseDto>>(okResult.Value);
        Assert.Equal(3, vessels.Count); // 1 from base + 2 from SeedAdditionalVessels
        Assert.Contains(vessels, v => v.ImoNumber == ExistingVesselImo);
        Assert.Contains(vessels, v => v.Name == "MAERSK TRIPLE E");
        Assert.Contains(vessels, v => v.Name == "CMA CGM MARCO POLO");
    }

    [Fact]
    public async Task SearchVessels_ByExactImo_ShouldReturnSingleVessel()
    {
        // Arrange
        var controller = CreateController();

        // Act
        var result = await controller.Search(imo: "9362255", null, null);

        // Assert
        var okResult = Assert.IsType<OkObjectResult>(result);
        var vessels = Assert.IsAssignableFrom<List<VesselResponseDto>>(okResult.Value);
        Assert.Single(vessels);
        Assert.Equal("9362255", vessels[0].ImoNumber);
        Assert.Equal("MAERSK TRIPLE E", vessels[0].Name);
    }

    [Fact]
    public async Task SearchVessels_ByImoWithNormalization_ShouldFindVessel()
    {
        // Arrange
        var controller = CreateController();

        // Act - Search with spaces (controller should normalize)
        var result = await controller.Search(imo: "  9362255  ", null, null);

        // Assert
        var okResult = Assert.IsType<OkObjectResult>(result);
        var vessels = Assert.IsAssignableFrom<List<VesselResponseDto>>(okResult.Value);
        Assert.Single(vessels);
        Assert.Equal("9362255", vessels[0].ImoNumber);
    }

    [Fact]
    public async Task SearchVessels_ByPartialName_ShouldReturnMatchingVessels()
    {
        // Arrange
        var controller = CreateController();

        // Act - Search for "MAERSK" (partial match)
        var result = await controller.Search(null, name: "MAERSK", null);

        // Assert
        var okResult = Assert.IsType<OkObjectResult>(result);
        var vessels = Assert.IsAssignableFrom<List<VesselResponseDto>>(okResult.Value);
        Assert.Single(vessels);
        Assert.Contains("MAERSK", vessels[0].Name);
    }

    [Fact]
    public async Task SearchVessels_ByNameCaseInsensitive_ShouldReturnMatches()
    {
        // Arrange
        var controller = CreateController();

        // Act - Search with lowercase
        var result = await controller.Search(null, name: "marco polo", null);

        // Assert
        var okResult = Assert.IsType<OkObjectResult>(result);
        var vessels = Assert.IsAssignableFrom<List<VesselResponseDto>>(okResult.Value);
        Assert.Single(vessels);
        Assert.Equal("CMA CGM MARCO POLO", vessels[0].Name);
    }

    [Fact]
    public async Task SearchVessels_ByOrganizationName_ShouldReturnVesselsFromThatOrganization()
    {
        // Arrange
        var controller = CreateController();

        // Act - Search by organization name "Mediterranean"
        var result = await controller.Search(null, null, organizationName: "Mediterranean");

        // Assert
        var okResult = Assert.IsType<OkObjectResult>(result);
        var vessels = Assert.IsAssignableFrom<List<VesselResponseDto>>(okResult.Value);
        Assert.Equal(2, vessels.Count); // MSC OSCAR and MAERSK TRIPLE E both owned by ShippingAgentOrgId
        Assert.All(vessels, v => Assert.Contains("Mediterranean", v.OrganizationName));
    }

    // Removed: SearchVessels_ByVesselTypeId - typeId parameter removed per US 2.2.2
    // Removed: SearchVessels_ByOrganizationId - organizationId parameter removed per US 2.2.2
    // US 2.2.2 only requires search by: IMO, name, or organization name (operator)

    [Fact]
    public async Task SearchVessels_NoMatches_ShouldReturnEmptyList()
    {
        // Arrange
        var controller = CreateController();

        // Act - Search for non-existent IMO
        var result = await controller.Search(imo: "9999999", null, null);

        // Assert
        var okResult = Assert.IsType<OkObjectResult>(result);
        var vessels = Assert.IsAssignableFrom<List<VesselResponseDto>>(okResult.Value);
        Assert.Empty(vessels);
    }

    [Fact]
    public async Task SearchVessels_ResultsOrderedByName_ShouldReturnSortedList()
    {
        // Arrange
        var controller = CreateController();

        // Act
        var result = await controller.Search(null, null, null);

        // Assert
        var okResult = Assert.IsType<OkObjectResult>(result);
        var vessels = Assert.IsAssignableFrom<List<VesselResponseDto>>(okResult.Value);
        
        // Verify alphabetical order
        var names = vessels.Select(v => v.Name).ToList();
        var sortedNames = names.OrderBy(n => n).ToList();
        Assert.Equal(sortedNames, names);
    }
}
