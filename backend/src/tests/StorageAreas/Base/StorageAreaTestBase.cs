using System;
using System.Threading.Tasks;
using Xunit;
using Microsoft.EntityFrameworkCore;
using DDDNetCore.Presentation.Controllers;
using DDDNetCore.Domain.Docks;
using DDDNetCore.Domain.Organizations;
using DDDNetCore.Infrastructure;
using DDDNetCore.Infrastructure.Repositories;
using DDDNetCore.Application.DTOs.StorageAreas;
using DDDNetCore.Application.Interfaces;

namespace DDDNetCore.Tests.StorageAreas.Base
{
    public abstract class StorageAreaTestBase : IDisposable
    {
        protected readonly PortDbContext Context;
        protected readonly StorageAreaRepository StorageAreaRepo;
        protected readonly DockRepository DockRepo;
        protected readonly IUnitOfWork UnitOfWork;
        protected readonly StorageAreasController Controller;

        protected StorageAreaTestBase()
        {
            var options = new DbContextOptionsBuilder<PortDbContext>()
                .UseInMemoryDatabase(databaseName: Guid.NewGuid().ToString())
                .Options;

            Context = new PortDbContext(options);
            StorageAreaRepo = new StorageAreaRepository(Context);
            DockRepo = new DockRepository(Context);
            UnitOfWork = new UnitOfWork(Context);
            var storageAreaService = new DDDNetCore.Application.Services.StorageAreaService(StorageAreaRepo, DockRepo, UnitOfWork);
            Controller = new StorageAreasController(storageAreaService);

            SeedTestData().Wait();
        }

        private async Task SeedTestData()
        {
            // Create test docks
            var dock1 = new Dock("DOCK01", "Dock 1", "North Terminal", 300, 15, 14);
            var dock2 = new Dock("DOCK02", "Dock 2", "South Terminal", 350, 16, 15);
            var dock3 = new Dock("DOCK03", "Dock 3", "East Terminal", 400, 18, 17);

            Context.Docks.AddRange(dock1, dock2, dock3);

            // Create Port Authority organization
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
