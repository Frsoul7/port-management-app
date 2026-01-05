using System;
using System.Linq;
using Xunit;
using DDDNetCore.Domain.Visits;
using DDDNetCore.Domain.Organizations;
using DDDNetCore.Domain.Users;
using DDDNetCore.Domain.Visits.Manifests;

namespace DDDNetCore.Tests.Unit.Visits
{
    /// <summary>
    /// TEST TYPE: Unit Test
    /// ENTITY UNDER TEST: VesselVisitNotification (Domain Aggregate Root)
    /// TEST OBJECTIVE: Validate VVN domain logic including constructor validation, state transitions (IN_PROGRESS → SUBMITTED → APPROVED/REJECTED),
    ///                 manifest management (loading/unloading), crew summary updates, and business rules enforcement
    ///                 (ETD > ETA, editable states, IMO validation on submit, approval/rejection requirements).
    /// </summary>
    public class VesselVisitNotificationTests
    {
        #region Test Data Helpers

        private OrganizationId CreateTestOrganizationId() => new OrganizationId(Guid.NewGuid());
        private UserId CreateTestUserId() => new UserId(Guid.NewGuid());

        private VesselVisitNotification CreateValidVvn()
        {
            return new VesselVisitNotification(
                vvnBusinessId: "2025-PTLEI-000001",
                vesselImo: "9176187",
                purpose: VisitPurpose.BOTH,
                etaUtc: DateTime.UtcNow.AddDays(7),
                etdUtc: DateTime.UtcNow.AddDays(9),
                captainName: "Captain Smith",
                captainCitizenId: "123456789",
                captainNationality: "US",
                crewCount: 20,
                orgId: CreateTestOrganizationId()
            );
        }

        #endregion

        #region Constructor Tests - Valid Cases

        [Fact]
        public void Constructor_WithValidData_CreatesVvn()
        {
            // Arrange
            var vvnId = "2025-PTLEI-000001";
            var vesselImo = "9176187";
            var purpose = VisitPurpose.LOAD;
            var eta = DateTime.UtcNow.AddDays(5);
            var etd = DateTime.UtcNow.AddDays(7);
            var captainName = "John Doe";
            var captainCitizenId = "987654321";
            var captainNationality = "PT";
            var crewCount = 15;
            var orgId = CreateTestOrganizationId();

            // Act
            var vvn = new VesselVisitNotification(vvnId, vesselImo, purpose, eta, etd, captainName, captainCitizenId, captainNationality, crewCount, orgId);

            // Assert
            Assert.NotEqual(Guid.Empty, vvn.VvnGuid);
            Assert.Equal(vvnId.ToUpperInvariant(), vvn.VvnBusinessId);
            Assert.Equal(vesselImo, vvn.VesselImo);
            Assert.Equal(purpose, vvn.VisitPurpose);
            Assert.Equal(eta, vvn.Eta);
            Assert.Equal(etd, vvn.Etd);
            Assert.Equal(captainName.Trim(), vvn.CaptainName);
            Assert.Equal(captainCitizenId.Trim(), vvn.CaptainCitizenId);
            Assert.Equal(captainNationality.ToUpperInvariant(), vvn.CaptainNationality);
            Assert.Equal(crewCount, vvn.CrewCount);
            Assert.Equal(orgId, vvn.OrganizationId);
            Assert.Equal(VVNState.IN_PROGRESS, vvn.State);
        }

        [Theory]
        [InlineData("2025-ptlei-000001", "2025-PTLEI-000001")]
        [InlineData("2025-PTLEI-000002", "2025-PTLEI-000002")]
        [InlineData("  2025-ptlei-000003  ", "2025-PTLEI-000003")]
        public void Constructor_WithVariousVvnIds_NormalizesToUppercase(string inputId, string expectedId)
        {
            // Arrange & Act
            var vvn = new VesselVisitNotification(
                inputId, "9176187", VisitPurpose.LOAD, 
                DateTime.UtcNow.AddDays(5), DateTime.UtcNow.AddDays(7),
                "Captain", "123", "US", 10, CreateTestOrganizationId()
            );

            // Assert
            Assert.Equal(expectedId, vvn.VvnBusinessId);
        }

        [Fact]
        public void Constructor_WithWhitespaceInCaptainFields_TrimsValues()
        {
            // Arrange & Act
            var vvn = new VesselVisitNotification(
                "2025-PTLEI-000001", "9176187", VisitPurpose.LOAD,
                DateTime.UtcNow.AddDays(5), DateTime.UtcNow.AddDays(7),
                "  Captain Smith  ", "  987654321  ", "  us  ", 10, CreateTestOrganizationId()
            );

            // Assert
            Assert.Equal("Captain Smith", vvn.CaptainName);
            Assert.Equal("987654321", vvn.CaptainCitizenId);
            Assert.Equal("US", vvn.CaptainNationality);
        }

        [Theory]
        [InlineData(0)]
        [InlineData(5)]
        [InlineData(100)]
        public void Constructor_WithValidCrewCounts_SetsCrewCount(int crewCount)
        {
            // Arrange & Act
            var vvn = new VesselVisitNotification(
                "2025-PTLEI-000001", "9176187", VisitPurpose.LOAD,
                DateTime.UtcNow.AddDays(5), DateTime.UtcNow.AddDays(7),
                "Captain", "123", "US", crewCount, CreateTestOrganizationId()
            );

            // Assert
            Assert.Equal(crewCount, vvn.CrewCount);
        }

        #endregion

        #region Constructor Tests - Invalid Cases

        [Theory]
        [InlineData(null)]
        [InlineData("")]
        [InlineData("   ")]
        public void Constructor_WithInvalidVvnBusinessId_ThrowsArgumentException(string? invalidId)
        {
            // Act & Assert
            Assert.Throws<ArgumentException>(() => new VesselVisitNotification(
                invalidId, "9176187", VisitPurpose.LOAD,
                DateTime.UtcNow.AddDays(5), DateTime.UtcNow.AddDays(7),
                "Captain", "123", "US", 10, CreateTestOrganizationId()
            ));
        }

        [Theory]
        [InlineData(null)]
        [InlineData("")]
        [InlineData("   ")]
        public void Constructor_WithInvalidVesselImo_ThrowsArgumentException(string? invalidImo)
        {
            // Act & Assert
            Assert.Throws<ArgumentException>(() => new VesselVisitNotification(
                "2025-PTLEI-000001", invalidImo, VisitPurpose.LOAD,
                DateTime.UtcNow.AddDays(5), DateTime.UtcNow.AddDays(7),
                "Captain", "123", "US", 10, CreateTestOrganizationId()
            ));
        }

        [Theory]
        [InlineData(null)]
        [InlineData("")]
        [InlineData("   ")]
        public void Constructor_WithInvalidCaptainName_ThrowsArgumentException(string? invalidName)
        {
            // Act & Assert
            Assert.Throws<ArgumentException>(() => new VesselVisitNotification(
                "2025-PTLEI-000001", "9176187", VisitPurpose.LOAD,
                DateTime.UtcNow.AddDays(5), DateTime.UtcNow.AddDays(7),
                invalidName, "123", "US", 10, CreateTestOrganizationId()
            ));
        }

        [Theory]
        [InlineData("U")]     // Too short
        [InlineData("USA")]   // Too long
        [InlineData("")]
        [InlineData("   ")]
        public void Constructor_WithInvalidCaptainNationality_ThrowsArgumentException(string invalidNationality)
        {
            // Act & Assert
            Assert.Throws<ArgumentException>(() => new VesselVisitNotification(
                "2025-PTLEI-000001", "9176187", VisitPurpose.LOAD,
                DateTime.UtcNow.AddDays(5), DateTime.UtcNow.AddDays(7),
                "Captain", "123", invalidNationality, 10, CreateTestOrganizationId()
            ));
        }

        [Fact]
        public void Constructor_WithNegativeCrewCount_ThrowsArgumentOutOfRangeException()
        {
            // Act & Assert
            Assert.Throws<ArgumentOutOfRangeException>(() => new VesselVisitNotification(
                "2025-PTLEI-000001", "9176187", VisitPurpose.LOAD,
                DateTime.UtcNow.AddDays(5), DateTime.UtcNow.AddDays(7),
                "Captain", "123", "US", -1, CreateTestOrganizationId()
            ));
        }

        [Fact]
        public void Constructor_WithEtdBeforeEta_ThrowsArgumentException()
        {
            // Arrange
            var eta = DateTime.UtcNow.AddDays(7);
            var etd = DateTime.UtcNow.AddDays(5); // Earlier than ETA

            // Act & Assert
            var ex = Assert.Throws<ArgumentException>(() => new VesselVisitNotification(
                "2025-PTLEI-000001", "9176187", VisitPurpose.LOAD,
                eta, etd, "Captain", "123", "US", 10, CreateTestOrganizationId()
            ));
            Assert.Contains("ETD must be after ETA", ex.Message);
        }

        [Fact]
        public void Constructor_WithNullOrganizationId_ThrowsArgumentNullException()
        {
            // Act & Assert
            Assert.Throws<ArgumentNullException>(() => new VesselVisitNotification(
                "2025-PTLEI-000001", "9176187", VisitPurpose.LOAD,
                DateTime.UtcNow.AddDays(5), DateTime.UtcNow.AddDays(7),
                "Captain", "123", "US", 10, null!
            ));
        }

        #endregion

        #region State Transition Tests

        [Fact]
        public void Submit_WithValidState_TransitionsToSubmitted()
        {
            // Arrange
            var vvn = CreateValidVvn();
            var submitterId = CreateTestUserId();

            // Act
            vvn.Submit(submitterId);

            // Assert
            Assert.Equal(VVNState.SUBMITTED, vvn.State);
            Assert.NotNull(vvn.SubmittedAt);
            Assert.Equal(submitterId, vvn.SubmittedById);
        }

        [Fact]
        public void Submit_FromApprovedState_ThrowsInvalidOperationException()
        {
            // Arrange
            var vvn = CreateValidVvn();
            vvn.Submit(CreateTestUserId());
            vvn.Approve(CreateTestUserId(), Guid.NewGuid());

            // Act & Assert
            Assert.Throws<InvalidOperationException>(() => vvn.Submit(CreateTestUserId()));
        }

        [Fact]
        public void Approve_FromSubmittedState_TransitionsToApproved()
        {
            // Arrange
            var vvn = CreateValidVvn();
            vvn.Submit(CreateTestUserId());
            var approverId = CreateTestUserId();
            var dockAssignmentId = Guid.NewGuid();

            // Act
            vvn.Approve(approverId, dockAssignmentId);

            // Assert
            Assert.Equal(VVNState.APPROVED, vvn.State);
            Assert.NotNull(vvn.ApprovedAt);
            Assert.Equal(approverId, vvn.ApprovedById);
            Assert.Equal(dockAssignmentId, vvn.DockAssignmentId);
        }

        [Fact]
        public void Approve_FromInProgressState_ThrowsInvalidOperationException()
        {
            // Arrange
            var vvn = CreateValidVvn();

            // Act & Assert
            Assert.Throws<InvalidOperationException>(() => vvn.Approve(CreateTestUserId(), Guid.NewGuid()));
        }

        [Fact]
        public void Reject_FromSubmittedState_TransitionsToRejected()
        {
            // Arrange
            var vvn = CreateValidVvn();
            vvn.Submit(CreateTestUserId());
            var rejectorId = CreateTestUserId();
            var reason = "Missing required documentation";

            // Act
            vvn.Reject(rejectorId, reason);

            // Assert
            Assert.Equal(VVNState.REJECTED, vvn.State);
            Assert.NotNull(vvn.RejectedAt);
            Assert.Equal(rejectorId, vvn.RejectedById);
            Assert.Equal(reason.Trim(), vvn.RejectionReason);
        }

        [Theory]
        [InlineData(null)]
        [InlineData("")]
        [InlineData("   ")]
        public void Reject_WithEmptyReason_ThrowsArgumentException(string? invalidReason)
        {
            // Arrange
            var vvn = CreateValidVvn();
            vvn.Submit(CreateTestUserId());

            // Act & Assert
            Assert.Throws<ArgumentException>(() => vvn.Reject(CreateTestUserId(), invalidReason));
        }

        [Fact]
        public void ReopenToDraft_FromRejectedState_TransitionsToInProgress()
        {
            // Arrange
            var vvn = CreateValidVvn();
            vvn.Submit(CreateTestUserId());
            vvn.Reject(CreateTestUserId(), "Incomplete information");

            // Act
            vvn.ReopenToDraft();

            // Assert
            Assert.Equal(VVNState.IN_PROGRESS, vvn.State);
            Assert.Null(vvn.RejectedAt);
            Assert.Null(vvn.RejectedById);
            Assert.Null(vvn.RejectionReason);
            Assert.Null(vvn.SubmittedAt);
        }

        [Fact]
        public void ReopenToDraft_FromApprovedState_ThrowsInvalidOperationException()
        {
            // Arrange
            var vvn = CreateValidVvn();
            vvn.Submit(CreateTestUserId());
            vvn.Approve(CreateTestUserId(), Guid.NewGuid());

            // Act & Assert
            Assert.Throws<InvalidOperationException>(() => vvn.ReopenToDraft());
        }

        #endregion

        #region Crew Summary Tests

        [Fact]
        public void SetCrewSummary_WithValidData_UpdatesCrew()
        {
            // Arrange
            var vvn = CreateValidVvn();
            var newCaptainName = "Captain Jones";
            var newCitizenId = "111222333";
            var newNationality = "GB";
            var newCrewCount = 25;

            // Act
            vvn.SetCrewSummary(newCaptainName, newCitizenId, newNationality, newCrewCount);

            // Assert
            Assert.Equal(newCaptainName.Trim(), vvn.CaptainName);
            Assert.Equal(newCitizenId.Trim(), vvn.CaptainCitizenId);
            Assert.Equal(newNationality.ToUpperInvariant(), vvn.CaptainNationality);
            Assert.Equal(newCrewCount, vvn.CrewCount);
        }

        [Fact]
        public void SetCrewSummary_InSubmittedState_ThrowsInvalidOperationException()
        {
            // Arrange
            var vvn = CreateValidVvn();
            vvn.Submit(CreateTestUserId());

            // Act & Assert
            Assert.Throws<InvalidOperationException>(() => 
                vvn.SetCrewSummary("Captain", "123", "US", 10)
            );
        }

        [Theory]
        [InlineData(-1)]
        [InlineData(-100)]
        public void SetCrewSummary_WithNegativeCrewCount_ThrowsArgumentOutOfRangeException(int invalidCount)
        {
            // Arrange
            var vvn = CreateValidVvn();

            // Act & Assert
            Assert.Throws<ArgumentOutOfRangeException>(() => 
                vvn.SetCrewSummary("Captain", "123", "US", invalidCount)
            );
        }

        #endregion

        #region Manifest Management Tests

        [Fact]
        public void AddLoadingEntry_WithValidEntry_AddsToManifest()
        {
            // Arrange
            var vvn = CreateValidVvn();
            var entry = ManifestEntry.Create("ABCU0000022", false, 1, 2, 3, "Steel Beams");

            // Act
            vvn.AddLoadingEntry(entry);

            // Assert
            Assert.NotNull(vvn.LoadingManifest);
            Assert.Single(vvn.LoadingManifest.Entries);
            Assert.Equal(1, vvn.LoadingCount);
        }

        [Fact]
        public void AddUnloadingEntry_WithValidEntry_AddsToManifest()
        {
            // Arrange
            var vvn = CreateValidVvn();
            var entry = ManifestEntry.Create("ABCU0000022", false, 5, 4, 2, "Grain");

            // Act
            vvn.AddUnloadingEntry(entry);

            // Assert
            Assert.NotNull(vvn.UnloadingManifest);
            Assert.Single(vvn.UnloadingManifest.Entries);
            Assert.Equal(1, vvn.UnloadingCount);
        }

        [Fact]
        public void AddEntry_WithLoadType_CreatesLoadingManifest()
        {
            // Arrange
            var vvn = CreateValidVvn();
            var entry = ManifestEntry.Create("ABCU0000022", false, 3, 2, 1, "Machinery");

            // Act
            vvn.AddEntry(ManifestType.Load, entry);

            // Assert
            Assert.NotNull(vvn.LoadingManifest);
            Assert.Single(vvn.LoadingManifest.Entries);
            Assert.Equal(1, vvn.LoadingCount);
        }

        [Fact]
        public void RemoveEntry_FromLoadingManifest_RemovesEntry()
        {
            // Arrange
            var vvn = CreateValidVvn();
            var entry = ManifestEntry.Create("ABCU0000022", false, 1, 1, 1, "Cargo");
            vvn.AddLoadingEntry(entry);
            var entryId = vvn.LoadingManifest!.Entries.First().Id;

            // Act
            vvn.RemoveEntry(ManifestType.Load, entryId);

            // Assert
            Assert.Empty(vvn.LoadingManifest.Entries);
            Assert.Equal(0, vvn.LoadingCount);
        }

        [Fact]
        public void AddLoadingEntry_InSubmittedState_ThrowsInvalidOperationException()
        {
            // Arrange
            var vvn = CreateValidVvn();
            vvn.Submit(CreateTestUserId());
            var entry = ManifestEntry.Create("ABCU0000022", false, 1, 1, 1, "Cargo");

            // Act & Assert
            Assert.Throws<InvalidOperationException>(() => vvn.AddLoadingEntry(entry));
        }

        #endregion

        #region ETA/ETD Update Tests

        [Fact]
        public void SetEta_InEditableState_UpdatesEta()
        {
            // Arrange
            var vvn = CreateValidVvn();
            var newEta = DateTime.UtcNow.AddDays(10);

            // Act
            vvn.SetEta(newEta);

            // Assert
            Assert.Equal(newEta, vvn.Eta);
        }

        [Fact]
        public void SetEtd_InEditableState_UpdatesEtd()
        {
            // Arrange
            var vvn = CreateValidVvn();
            var newEtd = DateTime.UtcNow.AddDays(15);

            // Act
            vvn.SetEtd(newEtd);

            // Assert
            Assert.Equal(newEtd, vvn.Etd);
        }

        [Fact]
        public void SetEta_InSubmittedState_ThrowsInvalidOperationException()
        {
            // Arrange
            var vvn = CreateValidVvn();
            vvn.Submit(CreateTestUserId());

            // Act & Assert
            Assert.Throws<InvalidOperationException>(() => vvn.SetEta(DateTime.UtcNow.AddDays(10)));
        }

        [Fact]
        public void SetVesselImo_InEditableState_UpdatesImo()
        {
            // Arrange
            var vvn = CreateValidVvn();
            var newImo = "9321483";

            // Act
            vvn.SetVesselImo(newImo);

            // Assert
            Assert.Equal(newImo, vvn.VesselImo);
        }

        #endregion

        #region Integration Tests

        [Fact]
        public void CreateVvnWithBothManifests_FullLifecycle_Succeeds()
        {
            // Arrange
            var vvn = CreateValidVvn();
            var loadEntry = ManifestEntry.Create("ABCU0000022", false, 1, 2, 3, "Export Goods");
            var unloadEntry = ManifestEntry.Create("ABCU0000022", false, 4, 5, 6, "Import Goods");

            // Act
            vvn.AddLoadingEntry(loadEntry);
            vvn.AddUnloadingEntry(unloadEntry);
            vvn.Submit(CreateTestUserId());
            vvn.Approve(CreateTestUserId(), Guid.NewGuid());

            // Assert
            Assert.Equal(VVNState.APPROVED, vvn.State);
            Assert.Equal(1, vvn.LoadingCount);
            Assert.Equal(1, vvn.UnloadingCount);
            Assert.NotNull(vvn.ApprovedAt);
            Assert.NotNull(vvn.DockAssignmentId);
        }

        [Fact]
        public void RejectAndReopenWorkflow_ReturnsToInProgress()
        {
            // Arrange
            var vvn = CreateValidVvn();
            vvn.Submit(CreateTestUserId());

            // Act
            vvn.Reject(CreateTestUserId(), "Missing captain license");
            vvn.ReopenToDraft();
            vvn.SetCrewSummary("Updated Captain", "999888777", "FR", 18);
            vvn.Submit(CreateTestUserId());

            // Assert
            Assert.Equal(VVNState.SUBMITTED, vvn.State);
            Assert.Equal("Updated Captain", vvn.CaptainName);
            Assert.Equal("FR", vvn.CaptainNationality);
            Assert.Null(vvn.RejectionReason);
        }

        #endregion
    }
}
