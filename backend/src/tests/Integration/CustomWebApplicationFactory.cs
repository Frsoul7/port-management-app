using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Mvc.Testing;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.DependencyInjection;
using DDDNetCore.Infrastructure;
using Microsoft.AspNetCore.Authentication;
using Microsoft.Extensions.Logging;
using Microsoft.Extensions.Options;
using System.Security.Claims;
using System.Text.Encodings.Web;

namespace DDDNetCore.Tests.Integration
{
    /// <summary>
    /// Custom Web Application Factory for integration testing.
    /// Configures the application to use an in-memory database for testing.
    /// </summary>
    public class CustomWebApplicationFactory : WebApplicationFactory<Program>
    {
        private readonly string _dbName = "IntegrationTestDb_" + Guid.NewGuid().ToString();
        
        protected override void ConfigureWebHost(IWebHostBuilder builder)
        {
            // Set content root to avoid directory path issues
            builder.UseContentRoot(Directory.GetCurrentDirectory());
            
            builder.ConfigureServices(services =>
            {
                // Remove the existing DbContext registration
                var descriptor = services.SingleOrDefault(
                    d => d.ServiceType == typeof(DbContextOptions<PortDbContext>));

                if (descriptor != null)
                {
                    services.Remove(descriptor);
                }

                // Add DbContext using in-memory database for testing
                // Use SAME database name for all requests in this factory instance
                services.AddDbContext<PortDbContext>(options =>
                {
                    options.UseInMemoryDatabase(_dbName);
                });

                // Configure test authentication scheme
                services.AddAuthentication(options =>
                {
                    options.DefaultAuthenticateScheme = "TestScheme";
                    options.DefaultChallengeScheme = "TestScheme";
                })
                .AddScheme<AuthenticationSchemeOptions, TestAuthHandler>("TestScheme", options => { });

                // Modify authorization policies to accept TestScheme
                services.PostConfigure<Microsoft.AspNetCore.Authorization.AuthorizationOptions>(authOptions =>
                {
                    var policies = new[] { "PortAuthority", "ShippingAgent", "LogisticsPlanner" };
                    foreach (var policyName in policies)
                    {
                        var policy = authOptions.GetPolicy(policyName);
                        if (policy != null)
                        {
                            // Create a new policy builder based on the existing policy
                            // We must include existing schemes (JwtBearer) and add TestScheme
                            var schemes = policy.AuthenticationSchemes.ToList();
                            schemes.Add("TestScheme");
                            
                            var builder = new Microsoft.AspNetCore.Authorization.AuthorizationPolicyBuilder(schemes.ToArray());
                            
                            // Copy requirements
                            foreach (var req in policy.Requirements)
                            {
                                builder.AddRequirements(req);
                            }
                            
                            // Replace the policy
                            authOptions.AddPolicy(policyName, builder.Build());
                        }
                    }
                });

                // Build the service provider
                var sp = services.BuildServiceProvider();

                // Create a scope to obtain a reference to the database context
                using (var scope = sp.CreateScope())
                {
                    var scopedServices = scope.ServiceProvider;
                    var db = scopedServices.GetRequiredService<PortDbContext>();

                    // Ensure the database is created
                    db.Database.EnsureCreated();
                }
            });
        }
    }

    /// <summary>
    /// Test authentication handler that reads role from X-Role header.
    /// </summary>
    public class TestAuthHandler : AuthenticationHandler<AuthenticationSchemeOptions>
    {
        public TestAuthHandler(
            IOptionsMonitor<AuthenticationSchemeOptions> options,
            ILoggerFactory logger,
            UrlEncoder encoder) : base(options, logger, encoder)
        {
        }

        protected override Task<AuthenticateResult> HandleAuthenticateAsync()
        {
            var claims = new List<Claim>();

            // Read role from X-Role header
            if (Request.Headers.TryGetValue("X-Role", out var roleHeader))
            {
                claims.Add(new Claim(ClaimTypes.Role, roleHeader.ToString()));
            }

            // Read User ID from X-User-Id header (map to 'sub' claim)
            if (Request.Headers.TryGetValue("X-User-Id", out var userIdHeader) && 
                Guid.TryParse(userIdHeader.ToString(), out var userId))
            {
                claims.Add(new Claim("sub", userId.ToString()));
            }

            if (claims.Count > 0)
            {
                var identity = new ClaimsIdentity(claims, "TestScheme");
                var principal = new ClaimsPrincipal(identity);
                var ticket = new AuthenticationTicket(principal, "TestScheme");

                return Task.FromResult(AuthenticateResult.Success(ticket));
            }

            // If no headers, authenticate as anonymous
            var anonymousIdentity = new ClaimsIdentity();
            var anonymousPrincipal = new ClaimsPrincipal(anonymousIdentity);
            var anonymousTicket = new AuthenticationTicket(anonymousPrincipal, "TestScheme");
            
            return Task.FromResult(AuthenticateResult.Success(anonymousTicket));
        }
    }
}
