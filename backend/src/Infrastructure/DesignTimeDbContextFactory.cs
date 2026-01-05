using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Design;
using Microsoft.Extensions.Configuration;
using System.IO;

namespace DDDNetCore.Infrastructure
{
    /// <summary>
    /// Factory for creating PortDbContext at design time (for EF Core migrations).
    /// This is required because the DbContext is configured via DI in Program.cs.
    /// </summary>
    public class DesignTimeDbContextFactory : IDesignTimeDbContextFactory<PortDbContext>
    {
        public PortDbContext CreateDbContext(string[] args)
        {
            // Build configuration from appsettings.json
            var configuration = new ConfigurationBuilder()
                .SetBasePath(Directory.GetCurrentDirectory())
                .AddJsonFile("appsettings.json", optional: false)
                .AddJsonFile("appsettings.Development.json", optional: true)
                .Build();

            var optionsBuilder = new DbContextOptionsBuilder<PortDbContext>();
            
            var connectionString = configuration.GetConnectionString("DefaultConnection");
            optionsBuilder.UseSqlServer(connectionString);

            return new PortDbContext(optionsBuilder.Options);
        }
    }
}
