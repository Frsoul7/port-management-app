using System;
using System.Linq;
using System.Threading.Tasks;
using Xunit;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.Primitives;
using DDDNetCore.Presentation.Controllers;
using DDDNetCore.Application.Services;
using DDDNetCore.Domain.Visits;
using DDDNetCore.Domain.Organizations;
using DDDNetCore.Domain.Vessels;
using DDDNetCore.Domain.DockAssignments;
using DDDNetCore.Domain.Users;
using DDDNetCore.Infrastructure;
using DDDNetCore.Application.DTOs.Vvns;
using DDDNetCore.Domain.Visits.Policies;

namespace DDDNetCore.Tests.VesselVisitNotifications
{
    /// <summary>
    /// TEST TYPE: Integration Test
    /// COMPONENTS UNDER TEST: VesselVisitNotificationsController, VesselVisitNotificationRepository, PortDbContext, VesselVisitNotification (Domain Entity)
    /// TEST OBJECTIVE: Validate VVN status viewing functionality with authorization (US 2.2.10).
    ///                 Tests HTTP GET requests with authentication, authorization policies (Shipping Agent Representatives can view their own VVNs),
    ///                 filtering by status (Draft, Submitted, Approved, Rejected), date range filtering,
    ///                 vessel/organization-based filtering, and proper return of VVN status collections.
    /// </summary>
    public class ViewVvnStatusTests : IDisposable
    {
        private readonly PortDbContext _dbContext;
        private readonly VesselVisitService _service;
        private readonly VesselVisitNotificationsController _controller;

        // Test data IDs
        private readonly Guid _shippingAgentOrgId = Guid.NewGuid();
        private readonly Guid _anotherAgentOrgId = Guid.NewGuid();
        private readonly Guid _portAuthorityOrgId = Guid.NewGuid();
        private readonly Guid _rep1UserId = Guid.NewGuid();
        private readonly Guid _rep2UserId = Guid.NewGuid();
        private Guid _vvnSubmittedId;
        private Guid _vvnApprovedId;
        private Guid _vvnRejectedId;
        private Guid _vvnInProgressId;

        public ViewVvnStatusTests()
        {
            var options = new DbContextOptionsBuilder<PortDbContext>()
                .UseInMemoryDatabase(databaseName: Guid.NewGuid().ToString())
                .Options;

            _dbContext = new PortDbContext(options);
            
            // Mock dependencies - use simple implementations for testing
            var unitOfWork = new DDDNetCore.Infrastructure.UnitOfWork(_dbContext);
            ICrewCompliancePolicy mockCrewPolicy = null!; // Not used in these tests
            VvnIdGenerator mockVvnIdGenerator = null!; // Not used in these tests
            
            _service = new VesselVisitService(unitOfWork, mockCrewPolicy, mockVvnIdGenerator);
            _controller = new VesselVisitNotificationsController(_service);

            SeedTestData().Wait();
        }

        private async Task SeedTestData()
        {
            // Create Organizations
            var shippingAgent = new Organization(
                _shippingAgentOrgId,
                "MSC001",
                "Mediterranean Shipping Company",
                "MSC",
                "Address 1",
                "PT123456789",
                OrganizationType.SHIPPING_AGENT
            );

            var anotherAgent = new Organization(
                _anotherAgentOrgId,
                "CMA001",
                "CMA CGM",
                "CMA",
                "Address 2",
                "PT987654321",
                OrganizationType.SHIPPING_AGENT
            );

            var portAuthority = new Organization(
                _portAuthorityOrgId,
                "PORTLX",
                "Port Authority of Lisbon",
                "PORTLX",
                "Address 3",
                "PT555555555",
                OrganizationType.PORT_AUTHORITY
            );

            _dbContext.Organizations.AddRange(shippingAgent, anotherAgent, portAuthority);

            // Create Vessel Type
            var vesselTypeId = Guid.NewGuid().ToString();
            var vesselType = new VesselType(vesselTypeId, "Container Ship");
            _dbContext.VesselTypes.Add(vesselType);

            // Create Vessel
            var vessel = new Vessel("9074729", "MSC OSCAR", vesselTypeId, new OrganizationId(_shippingAgentOrgId), 18000);
            _dbContext.Vessels.Add(vessel);

            // Create Dock
            var dock = new DDDNetCore.Domain.Docks.Dock("DOCK01", "Main Dock", "Berth 1", 300.0, 15.0, 12.0);
            _dbContext.Docks.Add(dock);

            await _dbContext.SaveChangesAsync();

            // Create VVNs in different states
            
            // 1. IN_PROGRESS VVN
            var vvnInProgress = new VesselVisitNotification(
                $"2025-PTLEI-001",
                "9074729",
                VisitPurpose.LOAD,
                DateTime.UtcNow.AddDays(10),
                DateTime.UtcNow.AddDays(12),
                "John Smith",
                "12345678",
                "US",
                25,
                new OrganizationId(_shippingAgentOrgId)
            );
            _dbContext.VesselVisitNotifications.Add(vvnInProgress);
            await _dbContext.SaveChangesAsync();
            _vvnInProgressId = vvnInProgress.VvnGuid;

            // 2. SUBMITTED VVN (submitted by rep1)
            var vvnSubmitted = new VesselVisitNotification(
                $"2025-PTLEI-002",
                "9074729",
                VisitPurpose.UNLOAD,
                DateTime.UtcNow.AddDays(15),
                DateTime.UtcNow.AddDays(17),
                "Jane Doe",
                "87654321",
                "GB",
                30,
                new OrganizationId(_shippingAgentOrgId)
            );
            vvnSubmitted.Submit(new UserId(_rep1UserId));
            _dbContext.VesselVisitNotifications.Add(vvnSubmitted);
            await _dbContext.SaveChangesAsync();
            _vvnSubmittedId = vvnSubmitted.VvnGuid;

            // 3. APPROVED VVN with dock assignment
            var vvnApproved = new VesselVisitNotification(
                $"2025-PTLEI-003",
                "9074729",
                VisitPurpose.BOTH,
                DateTime.UtcNow.AddDays(20),
                DateTime.UtcNow.AddDays(22),
                "Bob Captain",
                "11223344",
                "PT",
                28,
                new OrganizationId(_shippingAgentOrgId)
            );
            vvnApproved.Submit(new UserId(_rep2UserId));
            _dbContext.VesselVisitNotifications.Add(vvnApproved);
            await _dbContext.SaveChangesAsync();
            _vvnApprovedId = vvnApproved.VvnGuid;
            
            var dockAssignment = DockAssignment.Create(
                vvnApproved.VvnGuid,
                "DOCK01",
                vvnApproved.Eta,
                vvnApproved.Etd,
                _portAuthorityOrgId.ToString(),
                DateTime.UtcNow
            );
            _dbContext.DockAssignments.Add(dockAssignment);
            await _dbContext.SaveChangesAsync();
            
            vvnApproved.Approve(new UserId(_portAuthorityOrgId), dockAssignment.DockAssignmentId);
            await _dbContext.SaveChangesAsync();

            // 4. REJECTED VVN with rejection reason
            var vvnRejected = new VesselVisitNotification(
                $"2025-PTLEI-004",
                "9074729",
                VisitPurpose.LOAD,
                DateTime.UtcNow.AddDays(25),
                DateTime.UtcNow.AddDays(27),
                "Alice Master",
                "99887766",
                "FR",
                20,
                new OrganizationId(_shippingAgentOrgId)
            );
            vvnRejected.Submit(new UserId(_rep1UserId));
            vvnRejected.Reject(new UserId(_portAuthorityOrgId), "Dock unavailable during requested period");
            _dbContext.VesselVisitNotifications.Add(vvnRejected);
            await _dbContext.SaveChangesAsync();
            _vvnRejectedId = vvnRejected.VvnGuid;
        }

        private void SetupControllerContext(Guid organizationId, Guid userId, string role = "ShippingAgentRep")
        {
            var headers = new HeaderDictionary
            {
                { "X-Org-Id", new StringValues(organizationId.ToString()) },
                { "X-User-Id", new StringValues(userId.ToString()) },
                { "X-Role", new StringValues(role) }
            };

            _controller.ControllerContext = new ControllerContext
            {
                HttpContext = new DefaultHttpContext()
            };

            foreach (var header in headers)
            {
                _controller.ControllerContext.HttpContext.Request.Headers[header.Key] = header.Value;
            }
        }

        /// <summary>
        /// Test 1: Shipping Agent Representative can view all VVNs from their organization
        /// AC1: View all VVNs submitted by themselves or other reps from same organization
        /// </summary>
        [Fact]
        public async Task GetMyOrganizationVvns_WithNoFilters_ReturnsAllVvnsFromOrganization()
        {
            // Arrange
            SetupControllerContext(_shippingAgentOrgId, _rep1UserId);

            // Act
            var result = await _controller.GetMyOrganizationVvns();

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result.Result);
            var vvns = Assert.IsAssignableFrom<System.Collections.Generic.IEnumerable<VvnStatusResponse>>(okResult.Value);
            var vvnList = vvns.ToList();

            Assert.Equal(4, vvnList.Count); // Should see all 4 VVNs from their organization
            Assert.Contains(vvnList, v => v.Id == _vvnInProgressId);
            Assert.Contains(vvnList, v => v.Id == _vvnSubmittedId);
            Assert.Contains(vvnList, v => v.Id == _vvnApprovedId);
            Assert.Contains(vvnList, v => v.Id == _vvnRejectedId);
        }

        /// <summary>
        /// Test 2: Filter VVNs by vessel IMO number
        /// AC2: VVNs must be searchable by vessel IMO
        /// </summary>
        [Fact]
        public async Task GetMyOrganizationVvns_FilterByVesselImo_ReturnsMatchingVvns()
        {
            // Arrange
            SetupControllerContext(_shippingAgentOrgId, _rep1UserId);

            // Act
            var result = await _controller.GetMyOrganizationVvns(vesselImo: "9074729");

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result.Result);
            var vvns = Assert.IsAssignableFrom<System.Collections.Generic.IEnumerable<VvnStatusResponse>>(okResult.Value);
            var vvnList = vvns.ToList();

            Assert.Equal(4, vvnList.Count); // All test VVNs use the same vessel
            Assert.All(vvnList, v => Assert.Equal("9074729", v.VesselImo));
        }

        /// <summary>
        /// Test 3: Filter VVNs by status (APPROVED)
        /// AC3: VVNs must be filterable by status
        /// AC6: Approved VVNs must display dock assignment details
        /// </summary>
        [Fact]
        public async Task GetMyOrganizationVvns_FilterByApprovedStatus_ReturnsOnlyApprovedWithDockDetails()
        {
            // Arrange
            SetupControllerContext(_shippingAgentOrgId, _rep1UserId);

            // Act
            var result = await _controller.GetMyOrganizationVvns(status: "APPROVED");

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result.Result);
            var vvns = Assert.IsAssignableFrom<System.Collections.Generic.IEnumerable<VvnStatusResponse>>(okResult.Value);
            var vvnList = vvns.ToList();

            Assert.Single(vvnList);
            var approvedVvn = vvnList.First();
            Assert.Equal(_vvnApprovedId, approvedVvn.Id);
            Assert.Equal("APPROVED", approvedVvn.Status);
            
            // AC6: Verify dock assignment details
            Assert.NotNull(approvedVvn.DockAssignment);
            Assert.Equal("DOCK01", approvedVvn.DockAssignment.DockCode);
            Assert.NotNull(approvedVvn.ApprovedAt);
            Assert.NotNull(approvedVvn.ApprovedById);
        }

        /// <summary>
        /// Test 4: Filter VVNs by status (REJECTED)
        /// AC3: VVNs must be filterable by status
        /// AC7: Rejected VVNs must display rejection reason
        /// </summary>
        [Fact]
        public async Task GetMyOrganizationVvns_FilterByRejectedStatus_ReturnsOnlyRejectedWithReason()
        {
            // Arrange
            SetupControllerContext(_shippingAgentOrgId, _rep1UserId);

            // Act
            var result = await _controller.GetMyOrganizationVvns(status: "REJECTED");

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result.Result);
            var vvns = Assert.IsAssignableFrom<System.Collections.Generic.IEnumerable<VvnStatusResponse>>(okResult.Value);
            var vvnList = vvns.ToList();

            Assert.Single(vvnList);
            var rejectedVvn = vvnList.First();
            Assert.Equal(_vvnRejectedId, rejectedVvn.Id);
            Assert.Equal("REJECTED", rejectedVvn.Status);
            
            // AC7: Verify rejection reason and timestamp
            Assert.NotNull(rejectedVvn.RejectionReason);
            Assert.Equal("Dock unavailable during requested period", rejectedVvn.RejectionReason);
            Assert.NotNull(rejectedVvn.RejectedAt);
            Assert.NotNull(rejectedVvn.RejectedById);
        }

        /// <summary>
        /// Test 5: Filter VVNs by submitter (representative)
        /// AC4: VVNs must be filterable by representative/submitter
        /// </summary>
        [Fact]
        public async Task GetMyOrganizationVvns_FilterBySubmitter_ReturnsOnlyVvnsFromThatRep()
        {
            // Arrange
            SetupControllerContext(_shippingAgentOrgId, _rep1UserId);

            // Act
            var result = await _controller.GetMyOrganizationVvns(submittedById: _rep1UserId);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result.Result);
            var vvns = Assert.IsAssignableFrom<System.Collections.Generic.IEnumerable<VvnStatusResponse>>(okResult.Value);
            var vvnList = vvns.ToList();

            Assert.Equal(2, vvnList.Count); // rep1 submitted SUBMITTED and REJECTED VVNs
            Assert.All(vvnList, v => Assert.Equal(_rep1UserId, v.SubmittedById));
        }

        /// <summary>
        /// Test 6: Filter VVNs by time range
        /// AC5: VVNs must be filterable by time range
        /// </summary>
        [Fact]
        public async Task GetMyOrganizationVvns_FilterByTimeRange_ReturnsVvnsInRange()
        {
            // Arrange
            SetupControllerContext(_shippingAgentOrgId, _rep1UserId);
            var fromDate = DateTime.UtcNow.AddMinutes(-5); // All VVNs created recently
            var toDate = DateTime.UtcNow.AddDays(1);

            // Act
            var result = await _controller.GetMyOrganizationVvns(fromDate: fromDate, toDate: toDate);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result.Result);
            var vvns = Assert.IsAssignableFrom<System.Collections.Generic.IEnumerable<VvnStatusResponse>>(okResult.Value);
            var vvnList = vvns.ToList();

            Assert.Equal(4, vvnList.Count); // All VVNs created in this time range
            Assert.All(vvnList, v => Assert.InRange(v.CreatedAt, fromDate, toDate));
        }

        /// <summary>
        /// Test 7: Cannot view VVNs from other organizations
        /// AC1: Only see VVNs from own organization
        /// </summary>
        [Fact]
        public async Task GetMyOrganizationVvns_DifferentOrganization_ReturnsEmpty()
        {
            // Arrange - Use different organization ID
            SetupControllerContext(_anotherAgentOrgId, Guid.NewGuid());

            // Act
            var result = await _controller.GetMyOrganizationVvns();

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result.Result);
            var vvns = Assert.IsAssignableFrom<System.Collections.Generic.IEnumerable<VvnStatusResponse>>(okResult.Value);
            var vvnList = vvns.ToList();

            Assert.Empty(vvnList); // No VVNs from this organization
        }

        /// <summary>
        /// Test 8: Reopen rejected VVN back to IN_PROGRESS
        /// AC8: Shipping Agent Representatives can reopen rejected VVNs
        /// </summary>
        [Fact]
        public async Task ReopenToDraft_RejectedVvn_ChangesStatusToInProgress()
        {
            // Arrange
            SetupControllerContext(_shippingAgentOrgId, _rep1UserId);

            // Act
            var result = await _controller.ReopenToDraft(_vvnRejectedId);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            
            // Verify VVN status changed
            var vvn = await _dbContext.VesselVisitNotifications.FindAsync(_vvnRejectedId);
            Assert.NotNull(vvn);
            Assert.Equal(VVNState.IN_PROGRESS, vvn.State);
            
            // Verify rejection info cleared
            Assert.Null(vvn.RejectionReason);
            Assert.Null(vvn.RejectedById);
            Assert.Null(vvn.RejectedAt);
        }

        public void Dispose()
        {
            _dbContext?.Dispose();
        }
    }
}
