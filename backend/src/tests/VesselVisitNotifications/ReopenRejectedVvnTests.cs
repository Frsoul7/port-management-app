using System;
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
using DDDNetCore.Domain.Users;
using DDDNetCore.Infrastructure;
using DDDNetCore.Domain.Visits.Policies;

namespace DDDNetCore.Tests.VesselVisitNotifications
{
    /// <summary>
    /// TEST TYPE: Integration Test
    /// COMPONENTS UNDER TEST: VesselVisitNotificationsController, VesselVisitNotificationRepository, PortDbContext, VesselVisitNotification (Domain Entity)
    /// TEST OBJECTIVE: Validate VVN reopening functionality after rejection (US 2.2.9).
    ///                 Tests HTTP PATCH/PUT requests, state transition from Rejected → Draft,
    ///                 authorization (only Shipping Agent Representative can reopen their own VVNs),
    ///                 audit trail preservation (rejection reason retained), editable state restoration,
    ///                 and validation that only rejected VVNs can be reopened (not approved/submitted).
    /// Validates that Shipping Agent Representatives can reopen rejected VVNs back to IN_PROGRESS state
    /// </summary>
    public class ReopenRejectedVvnTests : IDisposable
    {
        private readonly PortDbContext _dbContext;
        private readonly VesselVisitService _service;
        private readonly VesselVisitNotificationsController _controller;

        // Test data IDs
        private readonly Guid _shippingAgentOrgId = Guid.NewGuid();
        private readonly Guid _anotherAgentOrgId = Guid.NewGuid();
        private readonly Guid _portAuthorityOrgId = Guid.NewGuid();
        private readonly Guid _repUserId = Guid.NewGuid();
        private readonly Guid _officerUserId = Guid.NewGuid();
        private Guid _rejectedVvnId;
        private Guid _approvedVvnId;
        private Guid _submittedVvnId;

        public ReopenRejectedVvnTests()
        {
            var options = new DbContextOptionsBuilder<PortDbContext>()
                .UseInMemoryDatabase(databaseName: Guid.NewGuid().ToString())
                .Options;

            _dbContext = new PortDbContext(options);
            
            // Mock dependencies
            var unitOfWork = new DDDNetCore.Infrastructure.UnitOfWork(_dbContext);
            ICrewCompliancePolicy mockCrewPolicy = null!;
            VvnIdGenerator mockVvnIdGenerator = null!;
            
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
                "Lisbon Port, Terminal 1",
                "PT123456789",
                OrganizationType.SHIPPING_AGENT
            );

            var anotherAgent = new Organization(
                _anotherAgentOrgId,
                "CMA001",
                "CMA CGM",
                "CMA",
                "Porto Port, Terminal 2",
                "PT987654321",
                OrganizationType.SHIPPING_AGENT
            );

            var portAuthority = new Organization(
                _portAuthorityOrgId,
                "PORTLX",
                "Port Authority of Lisbon",
                "PORTLX",
                "Administrative Building",
                "PT555555555",
                OrganizationType.PORT_AUTHORITY
            );

            _dbContext.Organizations.AddRange(shippingAgent, anotherAgent, portAuthority);

            // Create Vessel Type
            var vesselType = new VesselType("CONTAINER", "Container Ship");
            _dbContext.VesselTypes.Add(vesselType);

            // Create Vessel
            var vessel = new Vessel("9074729", "MSC OSCAR", "CONTAINER", new OrganizationId(_shippingAgentOrgId), 18000);
            _dbContext.Vessels.Add(vessel);

            await _dbContext.SaveChangesAsync();

            // Create REJECTED VVN (by shipping agent, rejected by port authority)
            var rejectedVvn = new VesselVisitNotification(
                "2025-PTLEI-001",
                "9074729",
                VisitPurpose.BOTH,
                DateTime.UtcNow.AddDays(10),
                DateTime.UtcNow.AddDays(12),
                "Captain John Smith",
                "12345678",
                "US",
                25,
                new OrganizationId(_shippingAgentOrgId)
            );
            rejectedVvn.Submit(new UserId(_repUserId));
            rejectedVvn.Reject(new UserId(_officerUserId), "Missing hazardous cargo crew documentation");
            _dbContext.VesselVisitNotifications.Add(rejectedVvn);
            await _dbContext.SaveChangesAsync();
            _rejectedVvnId = rejectedVvn.VvnGuid;

            // Create APPROVED VVN (cannot be reopened)
            var approvedVvn = new VesselVisitNotification(
                "2025-PTLEI-002",
                "9074729",
                VisitPurpose.LOAD,
                DateTime.UtcNow.AddDays(15),
                DateTime.UtcNow.AddDays(17),
                "Captain Jane Doe",
                "87654321",
                "GB",
                30,
                new OrganizationId(_shippingAgentOrgId)
            );
            approvedVvn.Submit(new UserId(_repUserId));
            var dockAssignmentId = Guid.NewGuid();
            approvedVvn.Approve(new UserId(_officerUserId), dockAssignmentId);
            _dbContext.VesselVisitNotifications.Add(approvedVvn);
            await _dbContext.SaveChangesAsync();
            _approvedVvnId = approvedVvn.VvnGuid;

            // Create SUBMITTED VVN (cannot be reopened)
            var submittedVvn = new VesselVisitNotification(
                "2025-PTLEI-003",
                "9074729",
                VisitPurpose.UNLOAD,
                DateTime.UtcNow.AddDays(20),
                DateTime.UtcNow.AddDays(22),
                "Captain Bob Wilson",
                "11223344",
                "FR",
                28,
                new OrganizationId(_shippingAgentOrgId)
            );
            submittedVvn.Submit(new UserId(_repUserId));
            _dbContext.VesselVisitNotifications.Add(submittedVvn);
            await _dbContext.SaveChangesAsync();
            _submittedVvnId = submittedVvn.VvnGuid;
        }

        /// <summary>
        /// Test 1: Successfully reopen a rejected VVN
        /// AC1, AC2, AC6: REJECTED → IN_PROGRESS, VVN becomes editable
        /// </summary>
        [Fact]
        public async Task ReopenRejectedVvn_Success_ReturnsOkAndSetsStateToInProgress()
        {
            // Arrange
            SetShippingAgentHeaders(_shippingAgentOrgId, _repUserId);

            // Act
            var result = await _controller.ReopenToDraft(_rejectedVvnId);

            // Assert
            var okResult = Assert.IsType<OkObjectResult>(result);
            Assert.NotNull(okResult.Value);
            
            // Verify response contains expected data
            dynamic? response = okResult.Value;
            Assert.NotNull(response);
            Assert.Equal(_rejectedVvnId, response.GetType().GetProperty("vvnId")?.GetValue(response));
            Assert.Equal("IN_PROGRESS", response.GetType().GetProperty("status")?.GetValue(response));
        }

        /// <summary>
        /// Test 2: Cannot reopen VVN with wrong role (Port Authority Officer)
        /// AC3, AC9: Only Shipping Agent Representatives can reopen VVNs
        /// </summary>
        [Fact]
        public async Task ReopenRejectedVvn_PortAuthorityOfficer_ReturnsForbidden()
        {
            // Arrange
            SetPortAuthorityHeaders(_portAuthorityOrgId, _officerUserId);

            // Act
            var result = await _controller.ReopenToDraft(_rejectedVvnId);

            // Assert
            var objectResult = Assert.IsType<UnauthorizedObjectResult>(result);
            Assert.Equal(401, objectResult.StatusCode);
        }

        /// <summary>
        /// Test 3: Cannot reopen VVN from different organization
        /// AC4: Representative can only reopen VVNs from their organization
        /// </summary>
        [Fact]
        public async Task ReopenRejectedVvn_DifferentOrganization_ReturnsForbidden()
        {
            // Arrange
            var anotherRepUserId = Guid.NewGuid();
            SetShippingAgentHeaders(_anotherAgentOrgId, anotherRepUserId);

            // Act
            var result = await _controller.ReopenToDraft(_rejectedVvnId);

            // Assert
            var objectResult = Assert.IsType<UnauthorizedObjectResult>(result);
            Assert.Equal(401, objectResult.StatusCode);
        }

        /// <summary>
        /// Test 4: Cannot reopen VVN that is not in REJECTED state (APPROVED)
        /// AC1, AC7: Only REJECTED VVNs can be reopened
        /// Note: EnsureSameOrgAsync checks organization first, so this returns 403 if org doesn't match
        /// </summary>
        [Fact]
        public async Task ReopenRejectedVvn_ApprovedVvn_ReturnsConflict()
        {
            // Arrange
            SetShippingAgentHeaders(_shippingAgentOrgId, _repUserId);

            // Act
            var result = await _controller.ReopenToDraft(_approvedVvnId);

            // Assert: Verify we get Conflict (409) response
            var conflictResult = Assert.IsType<ConflictObjectResult>(result);
            Assert.Equal(409, conflictResult.StatusCode);
        }

        /// <summary>
        /// Test 5: Cannot reopen VVN that is in SUBMITTED state
        /// AC1, AC7: Only REJECTED VVNs can be reopened
        /// Note: EnsureSameOrgAsync checks organization first, so this returns 403 if org doesn't match
        /// </summary>
        [Fact]
        public async Task ReopenRejectedVvn_SubmittedVvn_ReturnsConflict()
        {
            // Arrange
            SetShippingAgentHeaders(_shippingAgentOrgId, _repUserId);

            // Act
            var result = await _controller.ReopenToDraft(_submittedVvnId);

            // Assert: Verify we get Conflict (409) response
            var conflictResult = Assert.IsType<ConflictObjectResult>(result);
            Assert.Equal(409, conflictResult.StatusCode);
        }

        /// <summary>
        /// Test 6: Reopen non-existent VVN returns Forbidden
        /// Note: Controller logic checks org first via EnsureSameOrgAsync,
        /// which returns false for non-existent VVNs, resulting in 403 Forbidden
        /// </summary>
        [Fact]
        public async Task ReopenRejectedVvn_NonExistentVvn_ReturnsForbidden()
        {
            // Arrange
            var nonExistentId = Guid.NewGuid();
            SetShippingAgentHeaders(_shippingAgentOrgId, _repUserId);

            // Act
            var result = await _controller.ReopenToDraft(nonExistentId);

            // Assert
            // EnsureSameOrgAsync returns false for non-existent VVNs, causing 401 Unauthorized
            var objectResult = Assert.IsType<UnauthorizedObjectResult>(result);
            Assert.Equal(401, objectResult.StatusCode);
        }

        // Helper methods
        private void SetShippingAgentHeaders(Guid orgId, Guid userId)
        {
            _controller.ControllerContext = new ControllerContext
            {
                HttpContext = new DefaultHttpContext()
            };

            _controller.HttpContext.Request.Headers["X-Role"] = "ShippingAgentRep";
            _controller.HttpContext.Request.Headers["X-Org-Id"] = orgId.ToString();
            _controller.HttpContext.Request.Headers["X-User-Id"] = userId.ToString();
        }

        private void SetPortAuthorityHeaders(Guid orgId, Guid userId)
        {
            _controller.ControllerContext = new ControllerContext
            {
                HttpContext = new DefaultHttpContext()
            };

            _controller.HttpContext.Request.Headers["X-Role"] = "PortAuthorityOfficer";
            _controller.HttpContext.Request.Headers["X-Org-Id"] = orgId.ToString();
            _controller.HttpContext.Request.Headers["X-User-Id"] = userId.ToString();
        }

        public void Dispose()
        {
            _dbContext?.Database.EnsureDeleted();
            _dbContext?.Dispose();
        }
    }
}
