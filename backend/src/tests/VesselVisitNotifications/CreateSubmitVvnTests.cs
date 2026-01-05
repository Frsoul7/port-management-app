using System;
using System.Linq;
using Xunit;
using DDDNetCore.Domain.Visits;
using DDDNetCore.Domain.Visits.Manifests;
using DDDNetCore.Domain.Organizations;
using DDDNetCore.Domain.Users;
using DDDNetCore.Domain.Shared;

namespace DDDNetCore.Tests.VesselVisitNotifications
{
    /// <summary>
    /// TEST TYPE: Unit Test
    /// ENTITY UNDER TEST: VesselVisitNotification (Domain Aggregate Root)
    /// TEST OBJECTIVE: Validate VVN lifecycle operations and state transitions (US 2.2.8).
    ///                 Tests VVN creation, draft editing, submission, manifest management (cargo/containers),
    ///                 crew member handling, state machine transitions (Draft → Submitted),
    ///                 validation rules enforcement, and business invariants for VVN workflow.
    /// </summary>
    public class CreateSubmitVvnTests
    {
        private readonly OrganizationId _validOrgId;
        private readonly UserId _validUserId;

        public CreateSubmitVvnTests()
        {
            _validOrgId = new OrganizationId(Guid.NewGuid());
            _validUserId = new UserId(Guid.NewGuid());
        }

        #region Test 1: Create VVN with Valid Data - Happy Path

        [Fact]
        public void CreateVvn_WithValidData_SetsPropertiesCorrectly()
        {
            // Arrange
            var vvnId = "2025-PTLEI-000001";
            var vesselImo = "9319466"; // Valid IMO
            var purpose = VisitPurpose.BOTH;
            var eta = DateTime.UtcNow.AddDays(5);
            var etd = DateTime.UtcNow.AddDays(7);
            var captainName = "John Smith";
            var captainCitizenId = "PT123456789";
            var captainNationality = "PT";
            var crewCount = 25;

            // Act
            var vvn = new VesselVisitNotification(
                vvnId,
                vesselImo,
                purpose,
                eta,
                etd,
                captainName,
                captainCitizenId,
                captainNationality,
                crewCount,
                _validOrgId
            );

            // Assert
            Assert.Equal(vvnId.ToUpperInvariant(), vvn.VvnBusinessId);
            Assert.Equal(vesselImo, vvn.VesselImo);
            Assert.Equal(purpose, vvn.VisitPurpose);
            Assert.Equal(eta, vvn.Eta);
            Assert.Equal(etd, vvn.Etd);
            Assert.Equal(captainName.Trim(), vvn.CaptainName);
            Assert.Equal(captainCitizenId.Trim(), vvn.CaptainCitizenId);
            Assert.Equal(captainNationality.ToUpperInvariant(), vvn.CaptainNationality);
            Assert.Equal(crewCount, vvn.CrewCount);
            Assert.Equal(VVNState.IN_PROGRESS, vvn.State);
            Assert.NotEqual(Guid.Empty, vvn.VvnGuid);
            Assert.Equal(_validOrgId, vvn.OrganizationId);
        }

        #endregion

        #region Test 2: Create VVN with Missing VVN Business ID - Should Throw

        [Fact]
        public void CreateVvn_WithEmptyBusinessId_ThrowsArgumentException()
        {
            // Arrange
            var eta = DateTime.UtcNow.AddDays(5);
            var etd = DateTime.UtcNow.AddDays(7);

            // Act & Assert
            var exception = Assert.Throws<ArgumentException>(() =>
                new VesselVisitNotification(
                    "",  // Empty business ID
                    "9319466",
                    VisitPurpose.LOAD,
                    eta,
                    etd,
                    "Captain Name",
                    "PT123456",
                    "PT",
                    20,
                    _validOrgId
                )
            );

            Assert.Contains("VVN Business ID required", exception.Message);
        }

        #endregion

        #region Test 3: Create VVN with Invalid ETA/ETD (ETD before ETA) - Should Throw

        [Fact]
        public void CreateVvn_WithEtdBeforeEta_ThrowsArgumentException()
        {
            // Arrange
            var eta = DateTime.UtcNow.AddDays(7);
            var etd = DateTime.UtcNow.AddDays(5); // ETD before ETA - invalid!

            // Act & Assert
            var exception = Assert.Throws<ArgumentException>(() =>
                new VesselVisitNotification(
                    "2025-PTLEI-000002",
                    "9319466",
                    VisitPurpose.UNLOAD,
                    eta,
                    etd,
                    "Captain Name",
                    "PT123456",
                    "PT",
                    15,
                    _validOrgId
                )
            );

            Assert.Contains("ETD must be after ETA", exception.Message);
        }

        #endregion

        #region Test 4: Add Loading Manifest Entry with Valid ISO 6346 Container Code

        [Fact]
        public void AddLoadingEntry_WithValidIso6346Code_AddsSuccessfully()
        {
            // Arrange
            var vvn = CreateValidVvn();
            // Valid ISO 6346: ABCU0000017 (ABC + U + 000001 + check digit 7)
            var containerCode = "ABCU0000017"; 
            var entry = ManifestEntry.Create(containerCode, hazardous: false, bay: 1, row: 2, tier: 3, goods: "Electronics");

            // Act
            vvn.AddLoadingEntry(entry);

            // Assert
            Assert.NotNull(vvn.LoadingManifest);
            Assert.Single(vvn.LoadingManifest.Entries);
            Assert.Equal(1, vvn.LoadingCount);
            Assert.Equal(containerCode.ToUpperInvariant(), vvn.LoadingManifest.Entries.First().ContainerUniqueId);
        }

        #endregion

        #region Test 5: Add Manifest Entry with Invalid ISO 6346 Code - Should Throw

        [Fact]
        public void CreateManifestEntry_WithInvalidIso6346Code_ThrowsArgumentException()
        {
            // Arrange
            var invalidCode = "INVALID123"; // Not a valid ISO 6346 code

            // Act & Assert
            var exception = Assert.Throws<ArgumentException>(() =>
                ManifestEntry.Create(invalidCode, hazardous: false, bay: 1, row: 1, tier: 1)
            );

            Assert.Contains("not a valid ISO 6346 code", exception.Message);
        }

        #endregion

        #region Test 6: Add Unloading Manifest Entry - Should Create Unloading Manifest

        [Fact]
        public void AddUnloadingEntry_CreatesUnloadingManifestAndAddsEntry()
        {
            // Arrange
            var vvn = CreateValidVvn();
            // Valid ISO 6346: ABCU0000022 (ABC + U + 000002 + check digit 2)
            var containerCode = "ABCU0000022";
            var entry = ManifestEntry.Create(containerCode, hazardous: true, bay: 5, row: 3, tier: 2, goods: "Chemicals");

            // Act
            vvn.AddUnloadingEntry(entry);

            // Assert
            Assert.NotNull(vvn.UnloadingManifest);
            Assert.Single(vvn.UnloadingManifest.Entries);
            Assert.Equal(1, vvn.UnloadingCount);
            Assert.True(vvn.UnloadingManifest.Entries.First().HazardousGoods);
        }

        #endregion

        #region Test 7: Submit VVN in IN_PROGRESS State - Should Change to SUBMITTED

        [Fact]
        public void Submit_VvnInProgress_ChangesStateToSubmitted()
        {
            // Arrange
            var vvn = CreateValidVvn();
            var submitterId = new UserId(Guid.NewGuid());

            // Act
            vvn.Submit(submitterId);

            // Assert
            Assert.Equal(VVNState.SUBMITTED, vvn.State);
            Assert.NotNull(vvn.SubmittedAt);
            Assert.Equal(submitterId, vvn.SubmittedById);
            Assert.Null(vvn.RejectedAt);
            Assert.Null(vvn.RejectionReason);
        }

        #endregion

        #region Test 8: Submit VVN with Invalid IMO - Should Throw

        [Fact]
        public void Submit_VvnWithInvalidImo_ThrowsInvalidOperationException()
        {
            // Arrange
            var vvn = CreateValidVvn();
            // "9999999" has invalid checksum (should be 9999993, not 9999999)
            vvn.SetVesselImo("9999999"); 
            var submitterId = new UserId(Guid.NewGuid());

            // Act & Assert
            var exception = Assert.Throws<InvalidOperationException>(() =>
                vvn.Submit(submitterId)
            );

            Assert.Contains("Vessel IMO is invalid", exception.Message);
        }

        #endregion

        #region Test 9: Edit VVN in SUBMITTED State - Should Throw

        [Fact]
        public void AddLoadingEntry_WhenSubmitted_ThrowsInvalidOperationException()
        {
            // Arrange
            var vvn = CreateValidVvn();
            vvn.Submit(_validUserId);
            var entry = ManifestEntry.Create("ABCU0000017", hazardous: false, bay: 1, row: 1, tier: 1);

            // Act & Assert
            var exception = Assert.Throws<InvalidOperationException>(() =>
                vvn.AddLoadingEntry(entry)
            );

            Assert.Contains("not editable", exception.Message);
        }

        #endregion

        #region Test 10: Set Crew Summary with Invalid Captain Nationality - Should Throw

        [Fact]
        public void SetCrewSummary_WithInvalidNationalityCode_ThrowsArgumentException()
        {
            // Arrange
            var vvn = CreateValidVvn();

            // Act & Assert (using 3-letter code instead of 2-letter ISO 3166-1 alpha-2)
            var exception = Assert.Throws<ArgumentException>(() =>
                vvn.SetCrewSummary("New Captain", "PT987654", "PRT", 30) // "PRT" is 3 letters
            );

            Assert.Contains("2-letter ISO 3166-1 alpha-2 code", exception.Message);
        }

        #endregion

        #region Test 11: Add Multiple Entries with Same Container ID - Should Throw

        [Fact]
        public void AddEntry_WithDuplicateContainerId_ThrowsInvalidOperationException()
        {
            // Arrange
            var vvn = CreateValidVvn();
            var containerCode = "ABCU0000017";
            var entry1 = ManifestEntry.Create(containerCode, hazardous: false, bay: 1, row: 1, tier: 1);
            var entry2 = ManifestEntry.Create(containerCode, hazardous: false, bay: 2, row: 2, tier: 2);

            vvn.AddLoadingEntry(entry1);

            // Act & Assert
            var exception = Assert.Throws<InvalidOperationException>(() =>
                vvn.AddLoadingEntry(entry2)
            );

            Assert.Contains("Duplicate container", exception.Message);
        }

        #endregion

        #region Test 12: Update ETA/ETD While in IN_PROGRESS - Should Update Successfully

        [Fact]
        public void SetEtaEtd_WhileInProgress_UpdatesSuccessfully()
        {
            // Arrange
            var vvn = CreateValidVvn();
            var newEta = DateTime.UtcNow.AddDays(10);
            var newEtd = DateTime.UtcNow.AddDays(12);

            // Act
            vvn.SetEta(newEta);
            vvn.SetEtd(newEtd);

            // Assert
            Assert.Equal(newEta, vvn.Eta);
            Assert.Equal(newEtd, vvn.Etd);
        }

        #endregion

        #region Helper Methods

        /// <summary>
        /// Creates a valid VVN for testing purposes
        /// </summary>
        private VesselVisitNotification CreateValidVvn()
        {
            return new VesselVisitNotification(
                vvnBusinessId: "2025-PTLEI-TEST001",
                vesselImo: "9319466", // Valid IMO with correct checksum
                purpose: VisitPurpose.BOTH,
                etaUtc: DateTime.UtcNow.AddDays(5),
                etdUtc: DateTime.UtcNow.AddDays(7),
                captainName: "Captain John Doe",
                captainCitizenId: "PT123456789",
                captainNationality: "PT",
                crewCount: 20,
                orgId: _validOrgId
            );
        }

        #endregion
    }
}
