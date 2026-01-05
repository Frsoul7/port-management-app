using System;
using System.Threading.Tasks;
using Xunit;
using Microsoft.EntityFrameworkCore;
using DDDNetCore.Presentation.Controllers;
using DDDNetCore.Domain.Vessels;
using DDDNetCore.Domain.Organizations;
using DDDNetCore.Infrastructure;
using DDDNetCore.Infrastructure.Repositories;
using DDDNetCore.Application.Interfaces;

namespace DDDNetCore.Tests.Docks.Base
{
    public abstract class DockTestBase : IDisposable
    {
        protected readonly PortDbContext Context;
        protected readonly DockRepository DockRepo;
        protected readonly VesselTypeRepository VesselTypeRepo;
        protected readonly IUnitOfWork UnitOfWork;
        protected readonly DocksController Controller;

        protected DockTestBase()
        {
            var options = new DbContextOptionsBuilder<PortDbContext>()
                .UseInMemoryDatabase(databaseName: Guid.NewGuid().ToString())
                .Options;

            Context = new PortDbContext(options);
            DockRepo = new DockRepository(Context);
            VesselTypeRepo = new VesselTypeRepository(Context);
            UnitOfWork = new UnitOfWork(Context);
            var dockService = new DDDNetCore.Application.Services.DockService(DockRepo, VesselTypeRepo, UnitOfWork);
            Controller = new DocksController(dockService);

            SeedTestData().Wait();
        }

        private async Task SeedTestData()
        {
            // Create test vessel types
            var containerType = new VesselType(Guid.NewGuid().ToString("N"), "Container Ship");
            containerType.Update("Container Ship", "Standard container vessel", 5000, 22, 24, 10, null);
            
            var tankerType = new VesselType(Guid.NewGuid().ToString("N"), "Tanker");
            tankerType.Update("Tanker", "Oil tanker vessel", 3000, 18, 20, 8, null);

            Context.VesselTypes.AddRange(containerType, tankerType);

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

            await Context.SaveChangesAsync();
        }

        public void Dispose()
        {
            Context.Database.EnsureDeleted();
            Context.Dispose();
        }
    }
}
