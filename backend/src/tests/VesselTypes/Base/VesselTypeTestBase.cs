using System;
using System.Threading.Tasks;
using Xunit;
using Microsoft.EntityFrameworkCore;
using DDDNetCore.Presentation.Controllers;
using DDDNetCore.Domain.Vessels;
using DDDNetCore.Domain.Organizations;
using DDDNetCore.Infrastructure;

namespace DDDNetCore.Tests.VesselTypes.Base
{
    /// <summary>
    /// Base class for VesselType tests providing common setup and teardown
    /// </summary>
    public abstract class VesselTypeTestBase : IDisposable
    {
        protected readonly PortDbContext Context;
        protected readonly VesselTypesController Controller;

        protected VesselTypeTestBase()
        {
            var options = new DbContextOptionsBuilder<PortDbContext>()
                .UseInMemoryDatabase(databaseName: Guid.NewGuid().ToString())
                .Options;

            Context = new PortDbContext(options);
            var unitOfWork = new DDDNetCore.Infrastructure.UnitOfWork(Context);
            var vesselTypeFactory = new DDDNetCore.Domain.Factory.VesselTypeFactory();
            var vesselTypeService = new DDDNetCore.Application.Services.VesselTypeService(unitOfWork, vesselTypeFactory);
            Controller = new VesselTypesController(vesselTypeService);

            SeedTestData().Wait();
        }

        private async Task SeedTestData()
        {
            // Create Port Authority organization for authentication context
            var portAuthOrg = new Organization(
                Guid.Parse("aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa"),
                "PA001",
                "Port Authority Test",
                "PA Test",
                "Test Address",
                "PT999999999",
                OrganizationType.PORT_AUTHORITY,
                null
            );
            Context.Organizations.Add(portAuthOrg);

            // Seed some existing vessel types for testing
            var containerType = new VesselType(Guid.NewGuid().ToString("N"), "Container Ship");
            containerType.Update("Container Ship", "Standard container vessel", 5000, 22, 24, 10, "SOLAS compliant");
            
            var tankerType = new VesselType(Guid.NewGuid().ToString("N"), "Tanker");
            tankerType.Update("Tanker", "Oil tanker vessel", 3000, 18, 20, 8, null);

            Context.VesselTypes.AddRange(containerType, tankerType);

            await Context.SaveChangesAsync();
        }

        public void Dispose()
        {
            Context.Database.EnsureDeleted();
            Context.Dispose();
        }
    }
}
