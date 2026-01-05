using System;
using System.Threading.Tasks;
using Xunit;
using Microsoft.EntityFrameworkCore;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.Logging;
using DDDNetCore.Presentation.Controllers;
using DDDNetCore.Domain.Organizations;
using DDDNetCore.Infrastructure;
using DDDNetCore.Infrastructure.Repositories;
using DDDNetCore.Application.DTOs.Organizations;
using DDDNetCore.Application.Interfaces;

namespace DDDNetCore.Tests.Organizations.Base
{
    // Simple mock implementations for testing
    public class MockEmailService : IEmailService
    {
        public Task<bool> SendActivationEmailAsync(string email, string name, string activationLink)
        {
            return Task.FromResult(true);
        }

        public Task<bool> SendEmailAsync(string toEmail, string subject, string htmlBody)
        {
            return Task.FromResult(true);
        }
    }

    public class MockLogger<T> : ILogger<T>
    {
        public IDisposable? BeginScope<TState>(TState state) where TState : notnull => null;
        public bool IsEnabled(LogLevel logLevel) => true;
        public void Log<TState>(LogLevel logLevel, EventId eventId, TState state, Exception? exception, Func<TState, Exception?, string> formatter) { }
    }

    public abstract class OrganizationTestBase : IDisposable
    {
        protected readonly PortDbContext Context;
        protected readonly OrganizationRepository OrgRepo;
        protected readonly OrganizationsController Controller;

        protected OrganizationTestBase()
        {
            var options = new DbContextOptionsBuilder<PortDbContext>()
                .UseInMemoryDatabase(databaseName: Guid.NewGuid().ToString())
                .Options;

            Context = new PortDbContext(options);
            OrgRepo = new OrganizationRepository(Context);
            var unitOfWork = new DDDNetCore.Infrastructure.UnitOfWork(Context);
            var organizationFactory = new DDDNetCore.Domain.Factory.OrganizationFactory();
            
            // Create mock dependencies
            var emailService = new MockEmailService();
            var logger = new MockLogger<DDDNetCore.Application.Services.OrganizationService>();
            var configuration = new ConfigurationBuilder().Build();
            
            var orgService = new DDDNetCore.Application.Services.OrganizationService(
                unitOfWork, 
                organizationFactory, 
                emailService, 
                logger, 
                configuration
            );
            
            Controller = new OrganizationsController(orgService);

            // Mock HTTP context for controller (needed for Response.Headers)
            Controller.ControllerContext = new ControllerContext
            {
                HttpContext = new DefaultHttpContext()
            };

            // Seed any common test data if needed
            SeedTestData().Wait();
        }

        private async Task SeedTestData()
        {
            // Seed a Port Authority organization for reference if needed
            var portAuth = new Organization(
                Guid.Parse("aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa"),
                "PA001",
                "Port Authority Test",
                "PA Test",
                "Test Address",
                "PT999999999",
                OrganizationType.PORT_AUTHORITY,
                null
            );
            Context.Organizations.Add(portAuth);

            await Context.SaveChangesAsync();
        }

        public void Dispose()
        {
            Context.Database.EnsureDeleted();
            Context.Dispose();
        }
    }
}
