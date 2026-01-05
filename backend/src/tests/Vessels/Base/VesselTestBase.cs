using DDDNetCore.Presentation.Controllers;
using DDDNetCore.Domain.Organizations;
using DDDNetCore.Domain.Vessels;
using DDDNetCore.Infrastructure;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.Primitives;

namespace Tests.Vessels.Base;

public class VesselTestBase : IDisposable
{
    protected readonly PortDbContext DbContext;
    protected readonly IHeaderDictionary Headers;

    // Seed data IDs
    protected readonly Guid PortAuthorityOrgId = Guid.NewGuid();
    protected readonly Guid ShippingAgentOrgId = Guid.NewGuid();
    protected readonly Guid ShippingAgent2OrgId = Guid.NewGuid(); // Second shipping agent for test variety
    protected readonly Guid ContainerVesselTypeId = Guid.NewGuid();
    protected readonly Guid BulkCarrierTypeId = Guid.NewGuid();

    // Existing vessels for testing
    protected const string ExistingVesselImo = "9074729"; // Valid IMO with check digit
    protected const string ExistingVesselName = "MSC OSCAR";

    public VesselTestBase()
    {
        var options = new DbContextOptionsBuilder<PortDbContext>()
            .UseInMemoryDatabase(databaseName: Guid.NewGuid().ToString())
            .Options;

        DbContext = new PortDbContext(options);
        
        // Setup mock headers for Port Authority Officer authorization
        var headers = new HeaderDictionary
        {
            { "X-Role", new StringValues("PortAuthorityOfficer") },
            { "X-Org-Id", new StringValues(PortAuthorityOrgId.ToString()) }
        };
        Headers = headers;

        SeedDatabase();
    }

    protected VesselsController CreateController()
    {
        var unitOfWork = new DDDNetCore.Infrastructure.UnitOfWork(DbContext);
        var vesselFactory = new DDDNetCore.Domain.Factory.VesselFactory();
        var vesselService = new DDDNetCore.Application.Services.VesselService(unitOfWork, vesselFactory);
        var controller = new VesselsController(vesselService);
        
        // Mock HTTP context with headers
        controller.ControllerContext = new ControllerContext
        {
            HttpContext = new DefaultHttpContext()
        };
        
        foreach (var header in Headers)
        {
            controller.ControllerContext.HttpContext.Request.Headers[header.Key] = header.Value;
        }
        
        return controller;
    }

    private void SeedDatabase()
    {
        // Seed Organizations
        var portAuthOrg = new Organization(
            id: PortAuthorityOrgId,
            identifier: "PORTLX",
            legalName: "Port Authority of Lisbon",
            alternativeName: "Lisboa Port Authority",
            addressLine: "Rua do Porto, Lisboa",
            taxNumber: "PT123456789",
            type: OrganizationType.PORT_AUTHORITY
        );

        var shippingAgentOrg = new Organization(
            id: ShippingAgentOrgId,
            identifier: "MSC001",
            legalName: "Mediterranean Shipping Company",
            alternativeName: "MSC",
            addressLine: "Geneva, Switzerland",
            taxNumber: "CH987654321",
            type: OrganizationType.SHIPPING_AGENT
        );

        var shippingAgent2Org = new Organization(
            id: ShippingAgent2OrgId,
            identifier: "CMA001",
            legalName: "CMA CGM Group",
            alternativeName: "CMA CGM",
            addressLine: "Marseille, France",
            taxNumber: "FR123456789",
            type: OrganizationType.SHIPPING_AGENT
        );

        DbContext.Organizations.AddRange(portAuthOrg, shippingAgentOrg, shippingAgent2Org);

        // Seed Vessel Types
        var containerType = new VesselType(ContainerVesselTypeId.ToString(), "Container Ship");
        var bulkType = new VesselType(BulkCarrierTypeId.ToString(), "Bulk Carrier");

        DbContext.VesselTypes.AddRange(containerType, bulkType);

        // Seed an existing vessel for testing conflicts and updates
        var existingVessel = new Vessel(
            ExistingVesselImo,
            ExistingVesselName,
            ContainerVesselTypeId.ToString(),
            new OrganizationId(ShippingAgentOrgId),
            18000 // CapacityTEU
        );

        DbContext.Vessels.Add(existingVessel);

        DbContext.SaveChanges();
    }

    public void Dispose()
    {
        DbContext?.Dispose();
    }
}
