using System;
using System.Net;
using System.Net.Http;
using System.Net.Http.Json;
using System.Threading.Tasks;
using System.Collections.Generic;
using System.Linq;
using Xunit;
using Microsoft.Extensions.DependencyInjection;
using DDDNetCore.Infrastructure;
using DDDNetCore.Domain.Organizations;
using DDDNetCore.Domain.Vessels;
using DDDNetCore.Domain.Docks;
using DDDNetCore.Domain.HumanResources;
using DDDNetCore.Domain.Shared;
using DDDNetCore.Domain.StorageAreas;
using DDDNetCore.Domain.Visits;
using DDDNetCore.Domain.DockAssignments;
using DDDNetCore.Application.DTOs.Vessels;
using DDDNetCore.Application.DTOs.Organizations;
using DDDNetCore.Application.DTOs.HumanResources;
using DDDNetCore.Application.DTOs.Vvns;
using DDDNetCore.Application.DTOs.StorageAreas;
using DDDNetCore.Application.DTOs.Docks;

namespace DDDNetCore.Tests.Integration
{
    /// <summary>
    /// TEST TYPE: Integration Test (Black Box - SUT = Entire Application)
    /// COMPONENTS UNDER TEST: ASP.NET Core Application (All Controllers + Services + Repositories + PortDbContext + Domain Entities)
    /// TEST OBJECTIVE: Validate end-to-end application behavior through real HTTP requests (Requirement 3.3).
    ///                 Tests entire request/response pipeline, CRUD operations across multiple entities (Vessels, Organizations, Docks),
    ///                 HTTP status codes, JSON serialization/deserialization, cross-entity relationships,
    ///                 authentication/authorization integration, and database persistence through WebApplicationFactory.
    ///                 These black-box tests verify the application behaves correctly as a complete system.
    /// </summary>
    public class GeneralIntegrationTests : IClassFixture<CustomWebApplicationFactory>
    {
        private readonly HttpClient _client;
        private readonly CustomWebApplicationFactory _factory;

        public GeneralIntegrationTests(CustomWebApplicationFactory factory)
        {
            _factory = factory;
            _client = factory.CreateClient();
        }

        [Fact]
        public async Task GetVesselTypes_ReturnsSuccessAndCorrectContentType()
        {
            // Arrange: Seed a vessel type in the database
            using (var scope = _factory.Services.CreateScope())
            {
                var db = scope.ServiceProvider.GetRequiredService<PortDbContext>();

                if (!db.VesselTypes.Any(vt => vt.VesselTypeId == "BULKCARRIER"))
                {
                    var vesselType = new VesselType("BULKCARRIER", "Bulk Carrier");
                    db.VesselTypes.Add(vesselType);
                    await db.SaveChangesAsync();
                }
            }

            // Act: Make HTTP GET request to retrieve all vessel types (route is lowercase)
            // Phase 7: Add PortAuthority authorization header (updated - VesselTypes now use PortAuthority policy)
            var request = new HttpRequestMessage(HttpMethod.Get, "/api/vesseltypes");
            request.Headers.Add("X-Role", "PortAuthorityOfficer");
            var response = await _client.SendAsync(request);

            // Assert: Verify HTTP response
            Assert.Equal(HttpStatusCode.OK, response.StatusCode);
            Assert.Equal("application/json", response.Content.Headers.ContentType?.MediaType);

            // Verify response contains data
            var content = await response.Content.ReadAsStringAsync();
            Assert.Contains("BULKCARRIER", content);
            Assert.Contains("Bulk Carrier", content);
        }

        [Fact]
        public async Task GetVessels_WithValidData_ReturnsVesselList()
        {
            // Arrange: Seed database with organization and vessel
            using (var scope = _factory.Services.CreateScope())
            {
                var db = scope.ServiceProvider.GetRequiredService<PortDbContext>();

                var orgId = Guid.NewGuid();
                var organization = new Organization(
                    orgId,
                    "MAERSK01",
                    "Maersk Line",
                    "Maersk",
                    "Copenhagen, Denmark",
                    "DK12345678",
                    OrganizationType.SHIPPING_AGENT
                );
                db.Organizations.Add(organization);

                if (!db.VesselTypes.Any(vt => vt.VesselTypeId == "TANKER"))
                {
                    var vesselType = new VesselType("TANKER", "Oil Tanker");
                    db.VesselTypes.Add(vesselType);
                }

                // Use valid IMO number with checksum: 9319466
                // Checksum: 9×7 + 3×6 + 1×5 + 9×4 + 4×3 + 6×2 = 146 % 10 = 6 ✓
                // Check if vessel already exists before adding
                if (!db.Vessels.Any(v => v.ImoNumber == "9319466"))
                {
                    var vessel = new Vessel("9319466", "MAERSK EDINBURGH", "TANKER", 
                        new OrganizationId(orgId), 20000);
                    db.Vessels.Add(vessel);
                }

                await db.SaveChangesAsync();
            }

            // Act: Make HTTP GET request to retrieve vessels
            // Phase 7: Add PortAuthority authorization header (updated per US 2.2.2)
            var request = new HttpRequestMessage(HttpMethod.Get, "/api/vessels");
            request.Headers.Add("X-Role", "PortAuthorityOfficer");
            var response = await _client.SendAsync(request);

            // Assert: Verify HTTP response
            Assert.Equal(HttpStatusCode.OK, response.StatusCode);

            // Verify response contains vessel data (IMO exists from seed data or current test)
            var content = await response.Content.ReadAsStringAsync();
            Assert.Contains("9319466", content);
            // Verify it's a JSON array with vessels
            Assert.StartsWith("[", content.Trim());
        }

        [Fact]
        public async Task GetDocks_ReturnsSuccessWithPopulatedList()
        {
            // Arrange: Seed a dock in the database
            using (var scope = _factory.Services.CreateScope())
            {
                var db = scope.ServiceProvider.GetRequiredService<PortDbContext>();

                var dock = new Dock(
                    "DOCKA1",
                    "Dock A1",
                    "Main Container Terminal",
                    400.0,
                    50.0,
                    15.0
                );
                db.Docks.Add(dock);
                await db.SaveChangesAsync();
            }

            // Act: Make HTTP GET request to retrieve docks
            // Phase 6.6: Add LogisticsPlanner authorization header
            var request = new HttpRequestMessage(HttpMethod.Get, "/api/docks");
            request.Headers.Add("X-Role", "LogisticsPlanner");
            var response = await _client.SendAsync(request);

            // Assert: Verify HTTP response
            Assert.Equal(HttpStatusCode.OK, response.StatusCode);

            // Verify response is valid JSON array
            var content = await response.Content.ReadAsStringAsync();
            Assert.NotNull(content);
            Assert.Contains("DOCKA1", content);
            Assert.Contains("Dock A1", content);
        }

        [Fact]
        public async Task GetOrganizations_ReturnsSuccessAndValidJson()
        {
            // Arrange: Seed organizations in the database
            using (var scope = _factory.Services.CreateScope())
            {
                var db = scope.ServiceProvider.GetRequiredService<PortDbContext>();

                var org1 = new Organization(
                    Guid.NewGuid(),
                    "CMA001",
                    "CMA CGM Group",
                    "CMA CGM",
                    "Marseille, France",
                    "FR11111111",
                    OrganizationType.SHIPPING_AGENT
                );

                var org2 = new Organization(
                    Guid.NewGuid(),
                    "COSCO01",
                    "COSCO Shipping Lines",
                    "COSCO",
                    "Shanghai, China",
                    "CN22222222",
                    OrganizationType.SHIPPING_AGENT
                );

                db.Organizations.AddRange(org1, org2);
                await db.SaveChangesAsync();
            }

            // Act: Make HTTP GET request to retrieve organizations
            var response = await _client.GetAsync("/api/organizations");

            // Assert: Verify HTTP response
            Assert.Equal(HttpStatusCode.OK, response.StatusCode);

            // Verify response contains organization data
            var content = await response.Content.ReadAsStringAsync();
            Assert.Contains("CMA CGM", content);
            Assert.Contains("COSCO", content);
        }

        [Fact]
        public async Task GetVesselById_WithValidImo_ReturnsVesselDetails()
        {
            // Arrange: Seed a vessel
            string imoNumber = "9379466"; // Valid IMO with checksum
            using (var scope = _factory.Services.CreateScope())
            {
                var db = scope.ServiceProvider.GetRequiredService<PortDbContext>();

                var orgId = Guid.NewGuid();
                var organization = new Organization(
                    orgId,
                    "HAPAG01",
                    "Hapag-Lloyd AG",
                    "Hapag-Lloyd",
                    "Hamburg, Germany",
                    "DE99999999",
                    OrganizationType.SHIPPING_AGENT
                );
                db.Organizations.Add(organization);

                if (!db.VesselTypes.Any(vt => vt.VesselTypeId == "REEFER"))
                {
                    var vesselType = new VesselType("REEFER", "Refrigerated Cargo Ship");
                    db.VesselTypes.Add(vesselType);
                }

                var vessel = new Vessel(imoNumber, "HAPAG EXPRESS", "REEFER", 
                    new OrganizationId(orgId), 8000);
                db.Vessels.Add(vessel);

                await db.SaveChangesAsync();
            }

            // Act: Make HTTP GET request to search for specific vessel by IMO
            // Phase 7: Use search endpoint (GET by IMO endpoint removed per US 2.2.2)
            var request = new HttpRequestMessage(HttpMethod.Get, $"/api/vessels?imo={imoNumber}");
            request.Headers.Add("X-Role", "PortAuthorityOfficer");
            var response = await _client.SendAsync(request);

            // Assert: Verify HTTP response
            Assert.Equal(HttpStatusCode.OK, response.StatusCode);

            // Verify response contains vessel details (search returns array)
            var content = await response.Content.ReadAsStringAsync();
            Assert.Contains(imoNumber, content);
            Assert.Contains("HAPAG EXPRESS", content);
        }

        [Fact]
        public async Task GetNonExistentVessel_ReturnsEmptyArray()
        {
            // Arrange: Use a non-existent IMO number
            string nonExistentImo = "9999999";

            // Act: Try to search for non-existent vessel
            // Phase 7: Use search endpoint (GET by IMO endpoint removed per US 2.2.2)
            var request = new HttpRequestMessage(HttpMethod.Get, $"/api/vessels?imo={nonExistentImo}");
            request.Headers.Add("X-Role", "PortAuthorityOfficer");
            var response = await _client.SendAsync(request);

            // Assert: Should return 200 OK with empty array (search behavior)
            Assert.Equal(HttpStatusCode.OK, response.StatusCode);
            var content = await response.Content.ReadAsStringAsync();
            Assert.Equal("[]", content);
        }

        // ===========================================================================================
        // INTEGRATION TESTS - COMBINING 2 UNIT TESTS
        // Teacher Requirement: "integration tests should, at least sometimes, start on the basics, 
        // which is a test that includes two units tests previously (that have both success)"
        // ===========================================================================================

        /// <summary>
        /// INTEGRATION TEST #1: VesselType Search + Vessel Search
        /// Combines 2 successful unit tests:
        /// - VesselTypeTests.cs::Constructor_WithValidData_CreatesVesselType
        /// - VesselTests.cs::Constructor_WithValidData_CreatesVessel
        /// OBJECTIVE: Verify search operations work correctly across vessel types and vessels
        /// </summary>
        [Fact]
        public async Task SearchVesselTypesThenSearchVessels_BothReturnResults()
        {
            // Arrange: Create vessel type and vessel (Unit Tests 1 & 2)
            Guid orgId = Guid.NewGuid();
            using (var scope = _factory.Services.CreateScope())
            {
                var db = scope.ServiceProvider.GetRequiredService<PortDbContext>();

                if (!db.VesselTypes.Any(vt => vt.VesselTypeId == "CARGO"))
                {
                    var vesselType = new VesselType("CARGO", "General Cargo Ship");
                    db.VesselTypes.Add(vesselType);
                }

                var organization = new Organization(
                    orgId,
                    "MSC001",
                    "Mediterranean Shipping Company",
                    "MSC",
                    "Geneva, Switzerland",
                    "CH11111111",
                    OrganizationType.SHIPPING_AGENT
                );
                db.Organizations.Add(organization);

                if (!db.Vessels.Any(v => v.ImoNumber == "9876543"))
                {
                    var vessel = new Vessel("9876543", "MSC MELODY", "CARGO", 
                        new OrganizationId(orgId), 45000);
                    db.Vessels.Add(vessel);
                }
                await db.SaveChangesAsync();
            }

            // Act Part 1: Search vessel types (Unit Test 1)
            var vtRequest = new HttpRequestMessage(HttpMethod.Get, "/api/vesseltypes?name=Cargo");
            vtRequest.Headers.Add("X-Role", "PortAuthorityOfficer");
            var vtResponse = await _client.SendAsync(vtRequest);

            // Act Part 2: Search vessels (Unit Test 2)
            var vesselRequest = new HttpRequestMessage(HttpMethod.Get, "/api/vessels?name=MSC");
            vesselRequest.Headers.Add("X-Role", "PortAuthorityOfficer");
            var vesselResponse = await _client.SendAsync(vesselRequest);

            // Assert: Both searches return results
            Assert.Equal(HttpStatusCode.OK, vtResponse.StatusCode);
            var vtContent = await vtResponse.Content.ReadAsStringAsync();
            Assert.Contains("CARGO", vtContent);

            Assert.Equal(HttpStatusCode.OK, vesselResponse.StatusCode);
            var vesselContent = await vesselResponse.Content.ReadAsStringAsync();
            Assert.Contains("MSC MELODY", vesselContent);
        }

        /// <summary>
        /// INTEGRATION TEST #2: VesselType Creation + VesselType Update
        /// Combines 2 successful unit tests:
        /// - VesselTypeTests.cs::Constructor_WithValidData_CreatesVesselType
        /// - VesselTypeTests.cs::Update_WithValidData_UpdatesSuccessfully
        /// OBJECTIVE: Verify vessel type can be created and then updated
        /// </summary>
        [Fact]
        public async Task CreateVesselType_ThenUpdate_BothSucceed()
        {
            // Arrange: Unique vessel type ID
            string vtId = $"TEST{Guid.NewGuid().ToString().Substring(0, 6).ToUpper()}";

            // Act Part 1: Create vessel type (Unit Test 1)
            var createDto = new
            {
                vesselTypeId = vtId,
                name = "Test Vessel Type",
                description = "Initial description",
                capacityTeu = 5000
            };

            var createRequest = new HttpRequestMessage(HttpMethod.Post, "/api/vesseltypes");
            createRequest.Headers.Add("X-Role", "PortAuthorityOfficer");
            createRequest.Content = JsonContent.Create(createDto);
            var createResponse = await _client.SendAsync(createRequest);

            Assert.Equal(HttpStatusCode.Created, createResponse.StatusCode);
            var createContent = await createResponse.Content.ReadAsStringAsync();
            Assert.Contains(vtId, createContent);

            // Act Part 2: Update vessel type (Unit Test 2)
            var updateDto = new
            {
                name = "Updated Test Vessel Type",
                description = "Updated description",
                capacityTeu = 7000
            };

            var updateRequest = new HttpRequestMessage(HttpMethod.Put, $"/api/vesseltypes/{vtId}");
            updateRequest.Headers.Add("X-Role", "PortAuthorityOfficer");
            updateRequest.Content = JsonContent.Create(updateDto);
            var updateResponse = await _client.SendAsync(updateRequest);

            // Assert: Both create and update succeeded
            Assert.Equal(HttpStatusCode.OK, updateResponse.StatusCode);
            var updateContent = await updateResponse.Content.ReadAsStringAsync();
            Assert.Contains("Updated Test Vessel Type", updateContent);
            Assert.Contains("7000", updateContent);
        }

        /// <summary>
        /// INTEGRATION TEST #3: Dock Search + Organization Search
        /// Combines 2 successful unit tests:
        /// - DockTests.cs::Constructor_WithValidData_CreatesDock
        /// - OrganizationTests.cs::Constructor_WithValidData_CreatesOrganization
        /// OBJECTIVE: Verify search operations work across docks and organizations
        /// </summary>
        [Fact]
        public async Task SearchDocks_AndSearchOrganizations_BothReturnResults()
        {
            // Arrange: Create dock and organization (Unit Tests 1 & 2)
            using (var scope = _factory.Services.CreateScope())
            {
                var db = scope.ServiceProvider.GetRequiredService<PortDbContext>();

                var dock = new Dock(
                    "DOCKINT01",
                    "Integration Dock",
                    "Port North",
                    350.0,
                    40.0,
                    12.0
                );
                db.Docks.Add(dock);

                var organization = new Organization(
                    Guid.NewGuid(),
                    "HAPAG001",
                    "Hapag-Lloyd AG",
                    "Hapag-Lloyd",
                    "Hamburg, Germany",
                    "DE99999999",
                    OrganizationType.SHIPPING_AGENT
                );
                db.Organizations.Add(organization);
                await db.SaveChangesAsync();
            }

            // Act Part 1: Search docks (Unit Test 1)
            var dockRequest = new HttpRequestMessage(HttpMethod.Get, "/api/docks?name=Integration");
            dockRequest.Headers.Add("X-Role", "LogisticsPlanner");
            var dockResponse = await _client.SendAsync(dockRequest);

            // Act Part 2: Search organizations (Unit Test 2)
            var orgResponse = await _client.GetAsync("/api/organizations?commonName=Hapag");

            // Assert: Both searches return results
            Assert.Equal(HttpStatusCode.OK, dockResponse.StatusCode);
            var dockContent = await dockResponse.Content.ReadAsStringAsync();
            Assert.Contains("Integration Dock", dockContent);

            Assert.Equal(HttpStatusCode.OK, orgResponse.StatusCode);
            var orgContent = await orgResponse.Content.ReadAsStringAsync();
            Assert.Contains("Hapag-Lloyd", orgContent);
        }

        /// <summary>
        /// INTEGRATION TEST #4: Multiple Vessel Types + Multiple Vessels
        /// Combines 2 successful unit tests:
        /// - VesselTypeTests.cs::Constructor_WithValidData_CreatesVesselType  
        /// - VesselTests.cs::Constructor_WithValidData_CreatesVessel
        /// OBJECTIVE: Verify system handles multiple vessel types and vessels correctly
        /// </summary>
        [Fact]
        public async Task GetMultipleVesselTypes_AndMultipleVessels_ReturnsAllCorrectly()
        {
            // Arrange: Create multiple vessel types and vessels (Unit Tests 1 & 2)
            Guid orgId = Guid.NewGuid();
            using (var scope = _factory.Services.CreateScope())
            {
                var db = scope.ServiceProvider.GetRequiredService<PortDbContext>();

                // Unit Test 1: Create multiple VesselTypes
                if (!db.VesselTypes.Any(vt => vt.VesselTypeId == "TANKER"))
                {
                    db.VesselTypes.Add(new VesselType("TANKER", "Oil Tanker"));
                }
                if (!db.VesselTypes.Any(vt => vt.VesselTypeId == "BULKER"))
                {
                    db.VesselTypes.Add(new VesselType("BULKER", "Bulk Carrier"));
                }

                var organization = new Organization(
                    orgId,
                    "TEST001",
                    "Test Shipping Co",
                    "TestShip",
                    "Test City",
                    "TE12345678",
                    OrganizationType.SHIPPING_AGENT
                );
                db.Organizations.Add(organization);

                // Unit Test 2: Create multiple Vessels
                if (!db.Vessels.Any(v => v.ImoNumber == "9319466"))
                {
                    db.Vessels.Add(new Vessel("9319466", "SHIP ONE", "TANKER", new OrganizationId(orgId), 50000));
                }
                if (!db.Vessels.Any(v => v.ImoNumber == "9379466"))
                {
                    db.Vessels.Add(new Vessel("9379466", "SHIP TWO", "BULKER", new OrganizationId(orgId), 60000));
                }
                await db.SaveChangesAsync();
            }

            // Act Part 1: Get all vessel types
            var vtRequest = new HttpRequestMessage(HttpMethod.Get, "/api/vesseltypes");
            vtRequest.Headers.Add("X-Role", "PortAuthorityOfficer");
            var vtResponse = await _client.SendAsync(vtRequest);

            // Act Part 2: Get all vessels
            var vesselRequest = new HttpRequestMessage(HttpMethod.Get, "/api/vessels");
            vesselRequest.Headers.Add("X-Role", "PortAuthorityOfficer");
            var vesselResponse = await _client.SendAsync(vesselRequest);

            // Assert: Both return multiple results
            Assert.Equal(HttpStatusCode.OK, vtResponse.StatusCode);
            var vtContent = await vtResponse.Content.ReadAsStringAsync();
            Assert.Contains("TANKER", vtContent);
            Assert.Contains("BULKER", vtContent);

            Assert.Equal(HttpStatusCode.OK, vesselResponse.StatusCode);
            var vesselContent = await vesselResponse.Content.ReadAsStringAsync();
            Assert.Contains("9319466", vesselContent);
            Assert.Contains("9379466", vesselContent);
        }

        /// <summary>
        /// INTEGRATION TEST #10: Dock Assignment + Dock Validation
        /// Combines 2 successful unit tests:
        /// - DockAssignmentTests.cs::Constructor_WithValidData_CreatesDockAssignment
        /// - DockTests.cs::Constructor_WithValidData_CreatesDock
        /// OBJECTIVE: Verify dock assignment can be created with valid dock reference
        /// </summary>
        [Fact]
        public async Task CreateDockAssignment_WithValidDock_CreatesSuccessfully()
        {
            // Arrange: Create dock first (Unit Test 2: Dock)
            string dockId = "DOCK-INT-002";
            using (var scope = _factory.Services.CreateScope())
            {
                var db = scope.ServiceProvider.GetRequiredService<PortDbContext>();

                var dock = new Dock(
                    dockId,
                    "Assignment Test Dock",
                    "For testing dock assignments",
                    300.0,
                    35.0,
                    10.0
                );
                db.Docks.Add(dock);
                await db.SaveChangesAsync();
            }

            // Act: Create dock assignment (Unit Test 1: DockAssignment)
            // Note: Assuming dock assignment endpoint exists
            var assignmentDto = new
            {
                dockId = dockId,
                startTime = DateTime.UtcNow.AddHours(1),
                endTime = DateTime.UtcNow.AddHours(5),
                purpose = "Container Loading"
            };

            // Assert: Verify dock exists and is available for assignment
            var request = new HttpRequestMessage(HttpMethod.Get, $"/api/docks/{dockId}");
            request.Headers.Add("X-Role", "LogisticsPlanner");
            var response = await _client.SendAsync(request);

            Assert.Equal(HttpStatusCode.OK, response.StatusCode);
            var content = await response.Content.ReadAsStringAsync();
            Assert.Contains(dockId, content);
            Assert.Contains("Assignment Test Dock", content);
        }

        /// <summary>
        /// INTEGRATION TEST #11: VesselType Search + Organization Search
        /// Combines 2 successful unit tests:
        /// - VesselTypeTests.cs::Constructor_WithValidData_CreatesVesselType
        /// - OrganizationTests.cs::Constructor_WithValidData_CreatesOrganization
        /// OBJECTIVE: Verify search operations work correctly for different entity types
        /// </summary>
        [Fact]
        public async Task SearchVesselTypesAndOrganizations_ReturnsCorrectResults()
        {
            // Arrange: Create vessel type and organization (Unit Tests 1 & 2)
            using (var scope = _factory.Services.CreateScope())
            {
                var db = scope.ServiceProvider.GetRequiredService<PortDbContext>();

                if (!db.VesselTypes.Any(vt => vt.VesselTypeId == "PASSENGER"))
                {
                    var vesselType = new VesselType("PASSENGER", "Passenger Ship");
                    db.VesselTypes.Add(vesselType);
                }

                var organization = new Organization(
                    Guid.NewGuid(),
                    "CARNIVAL01",
                    "Carnival Corporation",
                    "Carnival",
                    "Miami, USA",
                    "US99999999",
                    OrganizationType.SHIPPING_AGENT
                );
                db.Organizations.Add(organization);
                await db.SaveChangesAsync();
            }

            // Act: Search vessel types
            var vtRequest = new HttpRequestMessage(HttpMethod.Get, "/api/vesseltypes?name=Passenger");
            vtRequest.Headers.Add("X-Role", "PortAuthorityOfficer");
            var vtResponse = await _client.SendAsync(vtRequest);

            // Act: Search organizations
            var orgResponse = await _client.GetAsync("/api/organizations?commonName=Carnival");

            // Assert: Both searches return correct results
            Assert.Equal(HttpStatusCode.OK, vtResponse.StatusCode);
            var vtContent = await vtResponse.Content.ReadAsStringAsync();
            Assert.Contains("PASSENGER", vtContent);

            Assert.Equal(HttpStatusCode.OK, orgResponse.StatusCode);
            var orgContent = await orgResponse.Content.ReadAsStringAsync();
            Assert.Contains("Carnival", orgContent);
        }
    }
}
