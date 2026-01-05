using Microsoft.AspNetCore.Builder;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Microsoft.EntityFrameworkCore;
using DDDNetCore.Infrastructure;
using DDDNetCore.Infrastructure.Repositories;
using Scalar.AspNetCore;
using DDDNetCore.Domain.HumanResources;
using System;
using System.Linq; // for Any(), FirstAsync()
using DDDNetCore.Domain.Organizations; // for Organization, OrganizationType
using DDDNetCore.Domain.Vessels;       // for VesselType, Vessel
using DDDNetCore.Application.Services;
using DDDNetCore.Domain.Visits.Policies;
using DDDNetCore.Domain.Shared; // DateTime conversions
using DDDNetCore.Domain.Visits; // VesselVisitNotification, VisitPurpose, VVNState
using DDDNetCore.Domain.PrivacyPolicy; // GDPR - Privacy Policy
using DDDNetCore.Domain.Users; // UserId for privacy policy seed
using Microsoft.AspNetCore.Authentication.JwtBearer;
using Microsoft.IdentityModel.Tokens;
using System.Text;


var builder = WebApplication.CreateBuilder(args);

builder.Services.AddControllers()
    .AddJsonOptions(options =>
    {
        // (Optional) keep your existing enum-as-string converter
        options.JsonSerializerOptions.Converters.Add(new System.Text.Json.Serialization.JsonStringEnumConverter());

        // Date format for all DateTime / DateTime? in responses
        options.JsonSerializerOptions.Converters.Add(new JsonDateTimeConverter("yyyy-MM-dd HH:mm:ss"));
        options.JsonSerializerOptions.Converters.Add(new JsonNullableDateTimeConverter("yyyy-MM-dd HH:mm:ss"));
    });

// Add an option to use an in-memory database for testing purposes
if (builder.Configuration.GetValue<bool>("UseInMemoryDatabase"))
{
    builder.Services.AddDbContext<PortDbContext>(options =>
        options.UseInMemoryDatabase("PortManagementDB_Test"));
}
else
{
    builder.Services.AddDbContext<PortDbContext>(options =>
        options.UseSqlServer(builder.Configuration.GetConnectionString("DefaultConnection"))
    );
}


// Layer 4 (Frameworks & Drivers) - Register repository implementations via interfaces (Layer 1 - Domain)
builder.Services.AddScoped<DDDNetCore.Domain.IRepository.IOrganizationRepository, OrganizationRepository>();
builder.Services.AddScoped<DDDNetCore.Domain.IRepository.IDockRepository, DockRepository>();
builder.Services.AddScoped<DDDNetCore.Domain.IRepository.IVesselTypeRepository, VesselTypeRepository>();
builder.Services.AddScoped<DDDNetCore.Domain.IRepository.IStorageAreaRepository, StorageAreaRepository>();
builder.Services.AddScoped<DDDNetCore.Domain.IRepository.IStaffMemberRepository, StaffMemberRepository>();
builder.Services.AddScoped<DDDNetCore.Domain.IRepository.IQualificationRepository, QualificationRepository>();
builder.Services.AddScoped<DDDNetCore.Domain.IRepository.IPhysicalResourceRepository, PhysicalResourceRepository>();
builder.Services.AddScoped<DDDNetCore.Domain.IRepository.IVesselRepository, VesselRepository>();
builder.Services.AddScoped<DDDNetCore.Domain.IRepository.IUserRepository, UserRepository>();
builder.Services.AddScoped<DDDNetCore.Domain.IRepository.IVesselVisitNotificationRepository, DDDNetCore.Infrastructure.Repositories.VesselVisitNotificationRepository>();
builder.Services.AddScoped<DDDNetCore.Domain.IRepository.IPrivacyPolicyRepository, DDDNetCore.Infrastructure.Repositories.PrivacyPolicyRepository>();
builder.Services.AddScoped<DDDNetCore.Domain.IRepository.IDataRequestRepository, DDDNetCore.Infrastructure.Repositories.DataRequestRepository>();
// Phase 7: Register UnitOfWork for coordinated repository access and transaction management
builder.Services.AddScoped<DDDNetCore.Application.Interfaces.IUnitOfWork, DDDNetCore.Infrastructure.UnitOfWork>();

// Layer 1 (Domain) - Register domain factories
builder.Services.AddScoped<DDDNetCore.Domain.Factory.IVesselFactory, DDDNetCore.Domain.Factory.VesselFactory>();
builder.Services.AddScoped<DDDNetCore.Domain.Factory.IOrganizationFactory, DDDNetCore.Domain.Factory.OrganizationFactory>();
builder.Services.AddScoped<DDDNetCore.Domain.Factory.IVesselTypeFactory, DDDNetCore.Domain.Factory.VesselTypeFactory>();

builder.Services.AddEndpointsApiExplorer();
builder.Services.AddSwaggerGen(options =>
{
    options.SwaggerDoc("v1", new Microsoft.OpenApi.Models.OpenApiInfo
    {
        Title = "Port Management System API",
        Version = "v1",
        Description = @"
# Required Headers (Development/Testing)
- `X-User-Id`: User GUID
- `X-Org-Id`: Organization GUID (required for Shipping Agents)
- `X-Role`: Role name (PortAuthorityOfficer, ShippingAgentRep, LogisticsPlanner, Admin)
"
    });

    // Add JWT Bearer authentication to Swagger
    options.AddSecurityDefinition("Bearer", new Microsoft.OpenApi.Models.OpenApiSecurityScheme
    {
        Description = "JWT Authorization header using the Bearer scheme. Enter 'Bearer' [space] and then your token.",
        Name = "Authorization",
        In = Microsoft.OpenApi.Models.ParameterLocation.Header,
        Type = Microsoft.OpenApi.Models.SecuritySchemeType.ApiKey,
        Scheme = "Bearer"
    });

    options.AddSecurityRequirement(new Microsoft.OpenApi.Models.OpenApiSecurityRequirement
    {
        {
            new Microsoft.OpenApi.Models.OpenApiSecurityScheme
            {
                Reference = new Microsoft.OpenApi.Models.OpenApiReference
                {
                    Type = Microsoft.OpenApi.Models.ReferenceType.SecurityScheme,
                    Id = "Bearer"
                }
            },
            Array.Empty<string>()
        }
    });

    // Include XML comments if available
    var xmlFile = $"{System.Reflection.Assembly.GetExecutingAssembly().GetName().Name}.xml";
    var xmlPath = System.IO.Path.Combine(AppContext.BaseDirectory, xmlFile);
    if (System.IO.File.Exists(xmlPath))
    {
        options.IncludeXmlComments(xmlPath);
    }
});

// Layer 2 (Application Services) - Register application services with interfaces
builder.Services.AddScoped<DDDNetCore.Application.Interfaces.IOrganizationService, OrganizationService>();
builder.Services.AddScoped<DDDNetCore.Application.Interfaces.IVesselService, VesselService>();
builder.Services.AddScoped<DDDNetCore.Application.Interfaces.IVesselTypeService, VesselTypeService>();
builder.Services.AddScoped<DDDNetCore.Application.Interfaces.IVesselVisitService, VesselVisitService>();
builder.Services.AddScoped<DDDNetCore.Application.Interfaces.IAuthenticationService, AuthenticationService>();
builder.Services.AddScoped<DDDNetCore.Application.Interfaces.IUserService, UserService>();
builder.Services.AddScoped<DDDNetCore.Application.Interfaces.IEmailService, SmtpEmailService>();
builder.Services.AddScoped<DDDNetCore.Application.Interfaces.IStaffMemberService, StaffMemberService>();
builder.Services.AddScoped<DDDNetCore.Application.Interfaces.IDockService, DockService>();
builder.Services.AddScoped<DDDNetCore.Application.Interfaces.IStorageAreaService, StorageAreaService>();
builder.Services.AddScoped<DDDNetCore.Application.Interfaces.IQualificationService, QualificationService>();
builder.Services.AddScoped<DDDNetCore.Application.Interfaces.IPhysicalResourceService, PhysicalResourceService>();
builder.Services.AddScoped<DDDNetCore.Application.Interfaces.IPrivacyPolicyService, DDDNetCore.Application.Services.PrivacyPolicyService>();
builder.Services.AddScoped<DDDNetCore.Application.Interfaces.IDataRequestService, DDDNetCore.Application.Services.DataRequestService>();
builder.Services.AddScoped<DDDNetCore.Application.Interfaces.IDataRightsService, DDDNetCore.Application.Services.DataRightsService>();

builder.Services.AddScoped<VvnIdGenerator>();
builder.Services.AddScoped<ICrewCompliancePolicy, HazardousRequiresCrewPolicy>();

// Register HttpClientFactory for authentication service
builder.Services.AddHttpClient();

// Phase 7: JWT Authentication
var jwtSecret = builder.Configuration["Jwt:Secret"] ?? "your-secret-key-min-32-chars-long!";
var key = Encoding.UTF8.GetBytes(jwtSecret);

builder.Services.AddAuthentication(options =>
{
    options.DefaultAuthenticateScheme = JwtBearerDefaults.AuthenticationScheme;
    options.DefaultChallengeScheme = JwtBearerDefaults.AuthenticationScheme;
})
.AddJwtBearer(options =>
{
    options.TokenValidationParameters = new TokenValidationParameters
    {
        ValidateIssuerSigningKey = true,
        IssuerSigningKey = new SymmetricSecurityKey(key),
        ValidateIssuer = false,
        ValidateAudience = false,
        ClockSkew = TimeSpan.Zero
    };
});

// Phase 6: Authorization - Register authorization handlers and policies
builder.Services.AddHttpContextAccessor(); // Required for authorization handlers to access HTTP context
builder.Services.AddAuthorization(options =>
{
    options.AddPolicy("PortAuthority", policy =>
    {
        policy.AuthenticationSchemes.Add(JwtBearerDefaults.AuthenticationScheme);
        policy.Requirements.Add(new DDDNetCore.Application.Authorization.PortAuthorityRequirement());
    });
    
    options.AddPolicy("ShippingAgent", policy =>
    {
        policy.AuthenticationSchemes.Add(JwtBearerDefaults.AuthenticationScheme);
        policy.Requirements.Add(new DDDNetCore.Application.Authorization.ShippingAgentRequirement());
    });
    
    options.AddPolicy("LogisticsPlanner", policy =>
    {
        policy.AuthenticationSchemes.Add(JwtBearerDefaults.AuthenticationScheme);
        policy.Requirements.Add(new DDDNetCore.Application.Authorization.LogisticsPlannerRequirement());
    });
});
// Register handlers as Scoped (not Singleton) because they depend on scoped PortDbContext
builder.Services.AddScoped<Microsoft.AspNetCore.Authorization.IAuthorizationHandler, 
    DDDNetCore.Application.Authorization.PortAuthorityHandler>();
builder.Services.AddScoped<Microsoft.AspNetCore.Authorization.IAuthorizationHandler, 
    DDDNetCore.Application.Authorization.ShippingAgentHandler>();
builder.Services.AddScoped<Microsoft.AspNetCore.Authorization.IAuthorizationHandler, 
    DDDNetCore.Application.Authorization.LogisticsPlannerHandler>();

// CORS configuration for Angular frontend (Sprint B - US 3.1.1)
builder.Services.AddCors(options =>
{
    options.AddPolicy("AllowAngularApp", policy =>
    {
        // Read frontend URL from configuration, fallback to localhost for development
        var frontendUrl = builder.Configuration["Frontend:Url"] ?? "http://localhost:4200";
        
        policy.WithOrigins(
                frontendUrl,                           // Environment-based frontend URL
                "http://localhost:4200",               // Angular default dev server port
                "http://127.0.0.1:4200",              // Localhost alternative
                "http://10.9.10.27:4200",             // VPN internal IP for frontend
                "http://10.9.10.27:5174",             // VPN internal IP for API
                "http://vm.nunoepteixeira.me",        // Cloudflare tunnel frontend
                "https://vm.nunoepteixeira.me",       // Cloudflare tunnel frontend (HTTPS)
                "http://api.nunoepteixeira.me",       // Cloudflare tunnel API
                "https://api.nunoepteixeira.me"       // Cloudflare tunnel API (HTTPS)
              )
              .AllowAnyHeader()
              .AllowAnyMethod()
              .AllowCredentials()
              .SetIsOriginAllowedToAllowWildcardSubdomains();
    });
});

var app = builder.Build();

if (app.Environment.IsDevelopment())
{
    app.UseSwagger(options =>
    {
        options.RouteTemplate = "openapi/{documentName}.json";
    });

    app.MapScalarApiReference(options =>
    {
        options.WithTitle("Port Management API")
               .WithTheme(ScalarTheme.Default)
               .WithDefaultHttpClient(ScalarTarget.CSharp, ScalarClient.HttpClient);
    });
}

// --- Dev seed: create 1 org, 1 vessel type, 1 vessel (only in Development) ---
using (var scope = app.Services.CreateScope())
{
    var env = scope.ServiceProvider.GetRequiredService<IHostEnvironment>();
    if (env.IsDevelopment())
    {
        var db = scope.ServiceProvider.GetRequiredService<PortDbContext>();

        // Only run migrations for SQL Server, not for in-memory databases (used in tests)
        if (db.Database.IsSqlServer())
        {
            await db.Database.MigrateAsync();
        }
        else
        {
            // For in-memory databases, just ensure the schema is created
            await db.Database.EnsureCreatedAsync();
        }

        // Predeclare variable for later use (will be populated after orgs are seeded)
        DDDNetCore.Domain.Organizations.OrganizationId? shippingAgentOrgId = null;

        // 1) Organizations (PA + Shipping Agent + Admin)
        if (!db.Organizations.Any())
        {
            // Create Port Authority organization without representatives (role-based auth only)
            var pa = new Organization(
                id: Guid.Parse("aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa"), // Fixed GUID for convenience
                identifier: "PA001",
                legalName: "Port Authority of Demo",
                alternativeName: "PA Demo",
                addressLine: "Harbor Ave 1, 4450-000 Leixões, PT",
                taxNumber: "PT999999990",
                type: OrganizationType.PORT_AUTHORITY,
                representatives: null // Port Authority doesn't require representatives
            );
            db.Organizations.Add(pa);

            // Create second Port Authority organization
            var pa2 = new Organization(
                id: Guid.Parse("eeeeeeee-eeee-eeee-eeee-eeeeeeeeeeee"), // Fixed GUID for convenience
                identifier: "PA002",
                legalName: "Port Authority General",
                alternativeName: "PA General",
                addressLine: "Harbor main 2, Leixoes, PT",
                taxNumber: "PT111111111",
                type: OrganizationType.PORT_AUTHORITY,
                representatives: null // Port Authority doesn't require representatives
            );
            db.Organizations.Add(pa2);

            // Create Administrator organization
            var adminOrg = new Organization(
                id: Guid.Parse("ffffffff-ffff-ffff-ffff-ffffffffffff"),
                identifier: "ADMIN001",
                legalName: "System Administration",
                alternativeName: "Admin",
                addressLine: "System HQ",
                taxNumber: "PT000000000",
                type: OrganizationType.ADMINISTRATOR,
                representatives: null
            );
            db.Organizations.Add(adminOrg);

            // Seed Shipping Agent (needed to create VVNs) - WITH representative for user tracking
            if (!db.Organizations.Any(o => o.Type == OrganizationType.SHIPPING_AGENT))
            {
                var rep = new Representative(
                    name: "Alice Maritime",
                    citizenId: "CIT123456",
                    nationality: "PT",
                    email: "alice@globalship.com",
                    phone: "+351910000000"
                );

                var sa = new Organization(
                    id: Guid.Parse("bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb"),
                    identifier: "SHIPAG01",
                    legalName: "Global Shipping Ltd",
                    alternativeName: "Global Ship",
                    addressLine: "123 Harbor Road, Lisbon",
                    taxNumber: "PT123456789",
                    type: OrganizationType.SHIPPING_AGENT,
                    representatives: new[] { rep }
                );
                db.Organizations.Add(sa);

                // Seed Shipping Agent 2: Shipping Cheap Uni
                var rep2_org2 = new Representative(
                    name: "Arigato",
                    citizenId: "231231131",
                    nationality: "JP",
                    email: "arigato@japan.jp",
                    phone: "+81131231313"
                );

                var rep3_org2 = new Representative(
                    name: "Arigata",
                    citizenId: "3284283482",
                    nationality: "JP",
                    email: "arigata@japan.jp",
                    phone: "+81311231313"
                );

                var sa2 = new Organization(
                    id: Guid.Parse("cccccccc-cccc-cccc-cccc-cccccccccccc"),
                    identifier: "SHIPAG02",
                    legalName: "Shipping Cheap Uni",
                    alternativeName: "ShipCheap",
                    addressLine: "987 Street, Tokio, Japan",
                    taxNumber: "JP987654321",
                    type: OrganizationType.SHIPPING_AGENT,
                    representatives: new[] { rep2_org2, rep3_org2 }
                );
                db.Organizations.Add(sa2);

                // Seed Shipping Agent 3: Shipping Xip Lda
                var rep1_org3 = new Representative(
                    name: "José Obrigado",
                    citizenId: "231231122",
                    nationality: "MC",
                    email: "obrigado@macau.mc",
                    phone: "+853131232222"
                );

                var sa3 = new Organization(
                    id: Guid.Parse("dddddddd-dddd-dddd-dddd-dddddddddddd"),
                    identifier: "SHIPAG03",
                    legalName: "Shipping Xip Lda",
                    alternativeName: "ShipXip",
                    addressLine: "987 Street, Macau, Mau",
                    taxNumber: "MC927654321",
                    type: OrganizationType.SHIPPING_AGENT,
                    representatives: new[] { rep1_org3 }
                );
                db.Organizations.Add(sa3);
            }

            await db.SaveChangesAsync();
        }

        // Seed Users for representatives (needed for X-User-Id headers)
        // Note: Only Shipping Agents require user tracking; Port Authority uses role-based auth
        if (!db.Users.Any())
        {
            // Administrator User
            var adminOrg = await db.Organizations
                .FirstOrDefaultAsync(o => o.Type == OrganizationType.ADMINISTRATOR);
            
            if (adminOrg != null)
            {
                // Hash the default admin password: "Admin@123"
                var passwordHash = DDDNetCore.Application.Services.AuthenticationService.HashPassword("Admin@123");
                
                var adminUser = DDDNetCore.Domain.Users.User.CreateVerifiedAdmin(
                    Guid.Parse("11111111-1111-1111-1111-111111111111"),
                    "System Administrator",
                    "admin@portsystem.com",
                    adminOrg.OrganizationId,
                    passwordHash: passwordHash
                );

                db.Users.Add(adminUser);
            }

            // Shipping Agent User (for VVN creation, etc.)
            var saOrg = await db.Organizations
                .FirstOrDefaultAsync(o => o.Type == OrganizationType.SHIPPING_AGENT);

            if (saOrg != null)
            {
                var saUser = new DDDNetCore.Domain.Users.User(
                    id: Guid.Parse("22222222-2222-2222-2222-222222222222"), // Fixed GUID for convenience
                    name: "Alice Maritime",
                    email: "alice@globalship.com",
                    organizationId: saOrg.OrganizationId,
                    role: DDDNetCore.Domain.Users.UserRole.SHIPPING_AGENT_REPRESENTATIVE
                );
                db.Users.Add(saUser);

                // Shipping Agent 2 - SHIPAG02 (Shipping Cheap Uni)
                var saOrg2 = db.Organizations.FirstOrDefault(o => o.Identifier == "SHIPAG02");
                if (saOrg2 != null)
                {
                    var arigatoUser = new DDDNetCore.Domain.Users.User(
                        id: Guid.Parse("33333333-3333-3333-3333-333333333333"), // Fixed GUID for convenience
                        name: "Arigato",
                        email: "arigato@japan.jp",
                        organizationId: saOrg2.OrganizationId,
                        role: DDDNetCore.Domain.Users.UserRole.SHIPPING_AGENT_REPRESENTATIVE
                    );
                    db.Users.Add(arigatoUser);

                    var arigataUser = new DDDNetCore.Domain.Users.User(
                        id: Guid.Parse("44444444-4444-4444-4444-444444444444"), // Fixed GUID for convenience
                        name: "Arigata",
                        email: "arigata@japan.jp",
                        organizationId: saOrg2.OrganizationId,
                        role: DDDNetCore.Domain.Users.UserRole.SHIPPING_AGENT_REPRESENTATIVE
                    );
                    db.Users.Add(arigataUser);
                }

                // Shipping Agent 3 - SHIPAG03 (Shipping Xip Lda)
                var saOrg3 = db.Organizations.FirstOrDefault(o => o.Identifier == "SHIPAG03");
                if (saOrg3 != null)
                {
                    var obrigadoUser = new DDDNetCore.Domain.Users.User(
                        id: Guid.Parse("55555555-5555-5555-5555-555555555555"), // Fixed GUID for convenience
                        name: "José Obrigado",
                        email: "obrigado@macau.mc",
                        organizationId: saOrg3.OrganizationId,
                        role: DDDNetCore.Domain.Users.UserRole.SHIPPING_AGENT_REPRESENTATIVE
                    );
                    db.Users.Add(obrigadoUser);
                }
            }

            await db.SaveChangesAsync();
        }

        // Always fetch the Shipping Agent org id (outside the if-block)
        shippingAgentOrgId = await db.Organizations
            .Where(o => o.Type == OrganizationType.SHIPPING_AGENT)
            .Select(o => o.OrganizationId)
            .FirstAsync();

        // 2) VesselType
        if (!db.VesselTypes.Any())
        {
            var vt = new VesselType(Guid.NewGuid().ToString("N"), "Panamax");
            vt.Update(
                name: "Panamax",
                description: "Seed type for tests",
                capacityTeu: 5000,
                maxRows: 22,
                maxBays: 24,
                maxTiers: 10,
                operationalConstraints: "Define Length, draft and beam"
            );
            db.VesselTypes.Add(vt);
            await db.SaveChangesAsync();
        }

        // fetch the VesselTypeId (string) for use below
        var vtId = await db.VesselTypes
            .Select(vt => vt.VesselTypeId)
            .FirstAsync();

        // 3) Vessel (owned/operated by the SHIPPING AGENT org)
        const string seedImo = "9319466";  // valid 7-digit IMO
        if (!db.Vessels.Any(v => v.ImoNumber == seedImo))
        {
            var vessel = new Vessel(
                imoNumber: seedImo,
                name: "SeedVessel-9319466",
                vesselTypeId: vtId,
                organizationId: shippingAgentOrgId!,   // Shipping Agent org
                capacityTeu: 5000
            );
            db.Vessels.Add(vessel);
            await db.SaveChangesAsync();
        }

        // 4) Staff Members
        if (!db.StaffMembers.Any())
        {
            var staff1 = new DDDNetCore.Domain.HumanResources.StaffMember(
                mecanographicNumber: 1001,
                shortName: "John Doe",
                email: "john.doe@example.com",
                phone: "123456789",
                status: DDDNetCore.Domain.HumanResources.HumanResourceStatus.AVAILABLE,
                startHour: TimeSpan.FromHours(9),
                endHour: TimeSpan.FromHours(17)
            );

            var staff2 = new DDDNetCore.Domain.HumanResources.StaffMember(
                mecanographicNumber: 1002,
                shortName: "Jane Smith",
                email: "jane.smith@example.com",
                phone: "987654321",
                status: DDDNetCore.Domain.HumanResources.HumanResourceStatus.UNAVAILABLE,
                startHour: TimeSpan.FromHours(8),
                endHour: TimeSpan.FromHours(16)
            );

            db.StaffMembers.Add(staff1);
            db.StaffMembers.Add(staff2);
            await db.SaveChangesAsync();
        }

        // 5) Staff Qualifications
        if (!db.Qualifications.Any())
        {
            var qualification1 = new StaffMemberQualification("QUAL001", "STS Crane Operator");
            var qualification2 = new StaffMemberQualification("QUAL002", "Forklift Driver");

            db.Qualifications.Add(qualification1);
            db.Qualifications.Add(qualification2);
            await db.SaveChangesAsync();
        }

        // 6) Docks (needed before Physical Resources)
        if (!db.Docks.Any())
        {
            var dock01 = new DDDNetCore.Domain.Docks.Dock(
                code: "DOCK01",
                name: "Main Container Terminal",
                location: "Port of Lei XõesSection A",
                lengthM: 300.0,
                depthM: 12.0,
                maxDraftM: 11.5
            );
            db.Docks.Add(dock01);

            var dock02 = new DDDNetCore.Domain.Docks.Dock(
                code: "DOCK02",
                name: "Secondary dock",
                location: "Porto de Leixões, terminal 2",
                lengthM: 200.0,
                depthM: 11.0,
                maxDraftM: 10.0
            );
            db.Docks.Add(dock02);

            var dock03 = new DDDNetCore.Domain.Docks.Dock(
                code: "DOCK03",
                name: "Secondary dock",
                location: "Porto de Leixões, terminal 3",
                lengthM: 190.0,
                depthM: 10.0,
                maxDraftM: 8.0
            );
            db.Docks.Add(dock03);

            await db.SaveChangesAsync();
        }

        // 7) VVN in IN_PROGRESS for quick testing
        var seedVvnGuid = Guid.Parse("aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"); // fixed GUID for convenience in Scalar
        if (!db.VesselVisitNotifications.Any(v => v.VvnGuid == seedVvnGuid))
        {
            var vvn = new VesselVisitNotification(
                vvnBusinessId: "2025-PTLEI-000001",    // First VVN for 2025
                vesselImo: seedImo,                    // ties to the seeded vessel
                purpose: VisitPurpose.MAINTENANCE,     // or LOAD/UNLOAD
                etaUtc: new DateTime(2025, 11, 03, 8, 0, 0, DateTimeKind.Utc),
                etdUtc: new DateTime(2025, 11, 04, 2, 0, 0, DateTimeKind.Utc),
                captainName: "Capt. Seed",
                captainCitizenId: "12345678",
                captainNationality: "PT",
                crewCount: 12,
                orgId: shippingAgentOrgId!             // same Shipping Agent org
            );
            
            // make the GUID predictable
            typeof(VesselVisitNotification)
                .GetProperty(nameof(VesselVisitNotification.VvnGuid))!
                .SetValue(vvn, seedVvnGuid);

            db.VesselVisitNotifications.Add(vvn);
            await db.SaveChangesAsync();
        }

        // 8) Physical Resources (STSCrane and MobileEquipment)
        if (!db.PhysicalResources.Any())
        {
            // Fetch existing qualifications
            var stsCraneQualification = await db.Qualifications.FirstOrDefaultAsync(q => q.QualificationId == "QUAL001");
            var forkliftQualification = await db.Qualifications.FirstOrDefaultAsync(q => q.QualificationId == "QUAL002");

            // Example STSCrane
            var stsCrane = new DDDNetCore.Domain.Resources.STSCrane(
                code: "STS001",
                description: "STS Crane for Container Handling",
                setupTimeSeconds: 600,
                avgContainersPerHour: 30,
                installedAtDockCode: "DOCK01"
            );
            if (stsCraneQualification != null)
            {
                stsCrane.SetRequiredQualifications(new[] { stsCraneQualification });
            }
            db.PhysicalResources.Add(stsCrane);

            // Example MobileEquipment (Truck)
            var truck = new DDDNetCore.Domain.Resources.MobileEquipment(
                code: "TRUCK001",
                description: "Truck for Container Transport",
                setupTimeSeconds: 300,
                type: DDDNetCore.Domain.Resources.MobileEquipmentType.TRUCK,
                maxSpeedKph: 80,
                containersPerTrip: 2
            );
            if (forkliftQualification != null)
            {
                truck.SetRequiredQualifications(new[] { forkliftQualification });
            }
            db.PhysicalResources.Add(truck);

            // Example MobileEquipment (Yard Gantry Crane)
            var yardGantryCrane = new DDDNetCore.Domain.Resources.MobileEquipment(
                code: "YGC001",
                description: "Yard Gantry Crane for Container Stacking",
                setupTimeSeconds: 500,
                type: DDDNetCore.Domain.Resources.MobileEquipmentType.YARD_GANTRY_CRANE,
                avgContainersPerHour: 25
            );
            if (stsCraneQualification != null && forkliftQualification != null)
            {
                yardGantryCrane.SetRequiredQualifications(new[] { stsCraneQualification, forkliftQualification });
            }
            db.PhysicalResources.Add(yardGantryCrane);

            await db.SaveChangesAsync();
        }

        // --- Seed Privacy Policies (GDPR - US 4.5.1 / US 4.5.2) ---
        if (!db.PrivacyPolicies.Any())
        {
            var systemUserId = Guid.Parse("00000000-0000-0000-0000-000000000001");
            var now = DateTime.UtcNow;

            // Portuguese version
            var ptPolicy = new PrivacyPolicy(
                id: Guid.NewGuid(),
                version: "1.0",
                content: @"# Política de Privacidade

## 1. Introdução

O Sistema de Gestão Portuária (""Sistema"") está comprometido em proteger a sua privacidade e dados pessoais em conformidade com o Regulamento Geral sobre a Proteção de Dados (RGPD) da União Europeia.

## 2. Dados Recolhidos

Recolhemos os seguintes tipos de dados pessoais:
- **Dados de identificação**: Nome, email, identificador de utilizador
- **Dados profissionais**: Organização, cargo, número mecanográfico
- **Dados de acesso**: Logs de autenticação, endereço IP, informações do navegador
- **Dados operacionais**: Ações realizadas no sistema relacionadas com operações portuárias

## 3. Finalidade do Tratamento

Os seus dados pessoais são tratados para:
- Gestão de acesso e autenticação no sistema
- Gestão de operações portuárias e logísticas
- Cumprimento de obrigações legais e regulamentares
- Auditoria e segurança do sistema

## 4. Base Legal

O tratamento dos seus dados baseia-se em:
- Execução de contrato ou diligências pré-contratuais
- Cumprimento de obrigações legais
- Interesses legítimos para segurança e auditoria

## 5. Retenção de Dados

Os dados pessoais são retidos pelo período necessário para cumprir as finalidades descritas, respeitando os prazos legais aplicáveis.

## 6. Seus Direitos

Ao abrigo do RGPD, tem direito a:
- Aceder aos seus dados pessoais
- Retificar dados incorretos
- Solicitar a eliminação dos dados (direito ao esquecimento)
- Opor-se ao tratamento
- Portabilidade dos dados
- Retirar o consentimento

## 7. Informação para Tripulantes, Capitães e Representantes

### Dados Recolhidos via VVN (Vessel Visit Notification)

Se é tripulante, capitão ou representante de uma embarcação, os seus dados pessoais podem ser tratados através das Notificações de Visita de Embarcação (VVN) submetidas ao nosso sistema.

**Dados que podem ser recolhidos:**
- Nome completo e função a bordo
- Informações de contacto
- Dados de identificação (passaporte, documento de marítimo)
- Dados relacionados com a operação portuária

### Base Legal para Tratamento de Dados de Tripulação

O tratamento destes dados baseia-se em:
- **Obrigação legal** - Cumprimento da Convenção FAL da IMO (International Maritime Organization) que exige a notificação prévia de visitas de embarcações
- **Interesse legítimo** - Gestão segura e eficiente das operações portuárias
- **Segurança marítima** - Cumprimento dos requisitos de segurança portuária

### Exercício de Direitos para Não-Utilizadores

Se não é utilizador registado no sistema mas os seus dados foram processados, pode exercer os seus direitos RGPD contactando a Autoridade Portuária:
- **Email**: gdpr-requests@portmanagement.pt
- **Telefone**: +351 XXX XXX XXX
- **Formulário de contacto**: Disponível em /privacy-policy#contact-form

Na sua solicitação, inclua:
- Nome completo
- Referência da embarcação ou VVN (se conhecida)
- Descrição do pedido

## 8. Contacto

Para exercer os seus direitos ou esclarecer dúvidas sobre privacidade, contacte o Encarregado de Proteção de Dados através do email: dpo@portmanagement.pt

## 9. Alterações à Política

Esta política pode ser atualizada periodicamente. Será notificado de alterações significativas através do sistema.

*Última atualização: Janeiro 2026*",
                createdBy: systemUserId,
                effectiveDate: now,
                languageCode: "pt",
                changeSummary: "Versão inicial da política de privacidade conforme RGPD"
            );
            ptPolicy.Activate();
            db.PrivacyPolicies.Add(ptPolicy);

            // English version
            var enPolicy = new PrivacyPolicy(
                id: Guid.NewGuid(),
                version: "1.0",
                content: @"# Privacy Policy

## 1. Introduction

The Port Management System (""System"") is committed to protecting your privacy and personal data in compliance with the European Union General Data Protection Regulation (GDPR).

## 2. Data Collected

We collect the following types of personal data:
- **Identification data**: Name, email, user identifier
- **Professional data**: Organization, role, employee number
- **Access data**: Authentication logs, IP address, browser information
- **Operational data**: Actions performed in the system related to port operations

## 3. Purpose of Processing

Your personal data is processed for:
- Access management and system authentication
- Management of port and logistics operations
- Compliance with legal and regulatory obligations
- System audit and security

## 4. Legal Basis

The processing of your data is based on:
- Performance of a contract or pre-contractual measures
- Compliance with legal obligations
- Legitimate interests for security and audit

## 5. Data Retention

Personal data is retained for the period necessary to fulfill the purposes described, respecting applicable legal deadlines.

## 6. Your Rights

Under GDPR, you have the right to:
- Access your personal data
- Rectify incorrect data
- Request data deletion (right to be forgotten)
- Object to processing
- Data portability
- Withdraw consent

## 7. Information for Crew Members, Captains, and Representatives

### Data Collected via VVN (Vessel Visit Notification)

If you are a crew member, captain, or vessel representative, your personal data may be processed through Vessel Visit Notifications (VVN) submitted to our system.

**Data that may be collected:**
- Full name and role on board
- Contact information
- Identification data (passport, seafarer's document)
- Data related to port operations

### Legal Basis for Crew Data Processing

The processing of this data is based on:
- **Legal obligation** - Compliance with the IMO FAL Convention (International Maritime Organization) requiring advance notification of vessel visits
- **Legitimate interest** - Safe and efficient management of port operations
- **Maritime safety** - Compliance with port security requirements

### Exercising Rights for Non-Users

If you are not a registered user of the system but your data has been processed, you can exercise your GDPR rights by contacting the Port Authority:
- **Email**: gdpr-requests@portmanagement.pt
- **Phone**: +351 XXX XXX XXX
- **Contact form**: Available at /privacy-policy#contact-form

In your request, please include:
- Full name
- Vessel or VVN reference (if known)
- Description of your request

## 8. Contact

To exercise your rights or clarify privacy questions, contact the Data Protection Officer at: dpo@portmanagement.pt

## 9. Policy Changes

This policy may be updated periodically. You will be notified of significant changes through the system.

*Last updated: January 2026*",
                createdBy: systemUserId,
                effectiveDate: now,
                languageCode: "en",
                changeSummary: "Initial privacy policy version per GDPR requirements"
            );
            enPolicy.Activate();
            db.PrivacyPolicies.Add(enPolicy);

            await db.SaveChangesAsync();
            Console.WriteLine("✅ Privacy policies seeded (PT and EN versions).");
        }

    }
}

// --- end dev seed ---

// Enable CORS for Angular frontend (Sprint B - US 3.1.1)
app.UseCors("AllowAngularApp");

// HTTPS redirection disabled for development with HTTP-only API
// app.UseHttpsRedirection();

// Phase 6: Authorization - Extract CallerContext from headers before authorization
app.Use(async (context, next) =>
{
    var callerContext = DDDNetCore.Application.Security.CallerContextFactory.FromHeaders(
        context.Request.Headers, 
        requireUserId: false);
    context.Items["Caller"] = callerContext;
    await next();
});

// Phase 6: Authentication & Authorization - Enable middleware (authentication must come before authorization)
app.UseAuthentication();
app.UseAuthorization();

app.MapControllers();
app.Run();

// Make Program class accessible for integration tests
public partial class Program { }
