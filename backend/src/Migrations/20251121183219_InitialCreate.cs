using System;
using Microsoft.EntityFrameworkCore.Migrations;

#nullable disable

namespace DDDNetCore.Migrations
{
    /// <inheritdoc />
    public partial class InitialCreate : Migration
    {
        /// <inheritdoc />
        protected override void Up(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.CreateTable(
                name: "DecisionLogs",
                columns: table => new
                {
                    DecisionLogId = table.Column<Guid>(type: "uniqueidentifier", nullable: false),
                    VvnGuid = table.Column<Guid>(type: "uniqueidentifier", nullable: false),
                    Outcome = table.Column<int>(type: "int", nullable: false),
                    OfficerUserId = table.Column<Guid>(type: "uniqueidentifier", nullable: false),
                    AtUtc = table.Column<DateTime>(type: "datetime2", nullable: false),
                    DockAssignmentId = table.Column<Guid>(type: "uniqueidentifier", nullable: true),
                    Reason = table.Column<string>(type: "nvarchar(max)", nullable: true),
                    Notes = table.Column<string>(type: "nvarchar(max)", nullable: true)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_DecisionLogs", x => x.DecisionLogId);
                });

            migrationBuilder.CreateTable(
                name: "Docks",
                columns: table => new
                {
                    Code = table.Column<string>(type: "nvarchar(20)", maxLength: 20, nullable: false),
                    Name = table.Column<string>(type: "nvarchar(200)", maxLength: 200, nullable: false),
                    Location = table.Column<string>(type: "nvarchar(500)", maxLength: 500, nullable: false),
                    LengthM = table.Column<double>(type: "float", nullable: false),
                    DepthM = table.Column<double>(type: "float", nullable: false),
                    MaxDraftM = table.Column<double>(type: "float", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_Docks", x => x.Code);
                });

            migrationBuilder.CreateTable(
                name: "Organizations",
                columns: table => new
                {
                    Id = table.Column<Guid>(type: "uniqueidentifier", nullable: false),
                    OrganizationId = table.Column<Guid>(type: "uniqueidentifier", nullable: false),
                    Identifier = table.Column<string>(type: "nvarchar(10)", maxLength: 10, nullable: false),
                    LegalName = table.Column<string>(type: "nvarchar(200)", maxLength: 200, nullable: false),
                    AlternativeName = table.Column<string>(type: "nvarchar(200)", maxLength: 200, nullable: false),
                    AddressLine = table.Column<string>(type: "nvarchar(500)", maxLength: 500, nullable: false),
                    TaxNumber = table.Column<string>(type: "nvarchar(64)", maxLength: 64, nullable: false),
                    Type = table.Column<string>(type: "nvarchar(max)", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_Organizations", x => x.Id);
                    table.UniqueConstraint("AK_Organizations_OrganizationId", x => x.OrganizationId);
                });

            migrationBuilder.CreateTable(
                name: "StaffMembers",
                columns: table => new
                {
                    MecanographicNumber = table.Column<long>(type: "bigint", nullable: false)
                        .Annotation("SqlServer:Identity", "1, 1"),
                    ShortName = table.Column<string>(type: "nvarchar(max)", nullable: false),
                    Email = table.Column<string>(type: "nvarchar(max)", nullable: false),
                    Phone = table.Column<string>(type: "nvarchar(max)", nullable: false),
                    Status = table.Column<int>(type: "int", nullable: false),
                    ActivityStatus = table.Column<int>(type: "int", nullable: false),
                    StartHour = table.Column<TimeSpan>(type: "time", nullable: false),
                    EndHour = table.Column<TimeSpan>(type: "time", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_StaffMembers", x => x.MecanographicNumber);
                });

            migrationBuilder.CreateTable(
                name: "StorageAreas",
                columns: table => new
                {
                    StorageAreaId = table.Column<string>(type: "nvarchar(64)", maxLength: 64, nullable: false),
                    Name = table.Column<string>(type: "nvarchar(200)", maxLength: 200, nullable: false),
                    Location = table.Column<string>(type: "nvarchar(500)", maxLength: 500, nullable: false),
                    MaxCapacityTEU = table.Column<int>(type: "int", nullable: false),
                    CurrentOccupancyTEU = table.Column<int>(type: "int", nullable: false),
                    ServesAllDocks = table.Column<bool>(type: "bit", nullable: false),
                    Type = table.Column<string>(type: "nvarchar(450)", nullable: false),
                    YardSpec_YardSpecId = table.Column<int>(type: "int", nullable: true),
                    YardSpec_Notes = table.Column<string>(type: "nvarchar(1000)", maxLength: 1000, nullable: true),
                    WarehouseSpec_WarehouseSpecId = table.Column<int>(type: "int", nullable: true),
                    WarehouseSpec_Notes = table.Column<string>(type: "nvarchar(1000)", maxLength: 1000, nullable: true)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_StorageAreas", x => x.StorageAreaId);
                });

            migrationBuilder.CreateTable(
                name: "VesselTypes",
                columns: table => new
                {
                    VesselTypeId = table.Column<string>(type: "nvarchar(64)", maxLength: 64, nullable: false),
                    Name = table.Column<string>(type: "nvarchar(200)", maxLength: 200, nullable: false),
                    Description = table.Column<string>(type: "nvarchar(2000)", maxLength: 2000, nullable: true),
                    CapacityTEU = table.Column<int>(type: "int", nullable: true),
                    MaxRows = table.Column<int>(type: "int", nullable: true),
                    MaxBays = table.Column<int>(type: "int", nullable: true),
                    MaxTiers = table.Column<int>(type: "int", nullable: true),
                    OperationalConstraints = table.Column<string>(type: "nvarchar(2000)", maxLength: 2000, nullable: true)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_VesselTypes", x => x.VesselTypeId);
                });

            migrationBuilder.CreateTable(
                name: "PhysicalResources",
                columns: table => new
                {
                    ResourceId = table.Column<string>(type: "nvarchar(450)", nullable: false),
                    Code = table.Column<string>(type: "nvarchar(20)", maxLength: 20, nullable: false),
                    Description = table.Column<string>(type: "nvarchar(500)", maxLength: 500, nullable: true),
                    Availability = table.Column<string>(type: "nvarchar(450)", nullable: false),
                    SetupTimeSeconds = table.Column<int>(type: "int", nullable: false),
                    CreatedAt = table.Column<DateTime>(type: "datetime2", nullable: false),
                    DeactivatedAt = table.Column<DateTime>(type: "datetime2", nullable: true),
                    DeactivationReason = table.Column<string>(type: "nvarchar(500)", maxLength: 500, nullable: true),
                    ResourceType = table.Column<string>(type: "nvarchar(21)", maxLength: 21, nullable: false),
                    MobileEquipmentType = table.Column<string>(type: "nvarchar(max)", nullable: true),
                    MaxSpeedKph = table.Column<int>(type: "int", nullable: true),
                    ContainersPerTrip = table.Column<int>(type: "int", nullable: true),
                    AvgContainersPerHour = table.Column<int>(type: "int", nullable: true),
                    CurrentDockCode = table.Column<string>(type: "nvarchar(20)", maxLength: 20, nullable: true),
                    STSCrane_AvgContainersPerHour = table.Column<int>(type: "int", nullable: true),
                    InstalledAtDockCode = table.Column<string>(type: "nvarchar(20)", maxLength: 20, nullable: true)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_PhysicalResources", x => x.ResourceId);
                    table.ForeignKey(
                        name: "FK_PhysicalResources_Docks_CurrentDockCode",
                        column: x => x.CurrentDockCode,
                        principalTable: "Docks",
                        principalColumn: "Code",
                        onDelete: ReferentialAction.SetNull);
                    table.ForeignKey(
                        name: "FK_PhysicalResources_Docks_InstalledAtDockCode",
                        column: x => x.InstalledAtDockCode,
                        principalTable: "Docks",
                        principalColumn: "Code",
                        onDelete: ReferentialAction.Restrict);
                });

            migrationBuilder.CreateTable(
                name: "Representative",
                columns: table => new
                {
                    Id = table.Column<Guid>(type: "uniqueidentifier", nullable: false),
                    RepresentativeId = table.Column<Guid>(type: "uniqueidentifier", nullable: false),
                    Name = table.Column<string>(type: "nvarchar(150)", maxLength: 150, nullable: false),
                    CitizenId = table.Column<string>(type: "nvarchar(64)", maxLength: 64, nullable: false),
                    Nationality = table.Column<string>(type: "nvarchar(2)", maxLength: 2, nullable: false),
                    Email = table.Column<string>(type: "nvarchar(254)", maxLength: 254, nullable: false),
                    Phone = table.Column<string>(type: "nvarchar(64)", maxLength: 64, nullable: false),
                    IsActive = table.Column<bool>(type: "bit", nullable: false),
                    CreatedAt = table.Column<DateTime>(type: "datetime2", nullable: false),
                    OrganizationId = table.Column<Guid>(type: "uniqueidentifier", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_Representative", x => x.Id);
                    table.ForeignKey(
                        name: "FK_Representative_Organizations_OrganizationId",
                        column: x => x.OrganizationId,
                        principalTable: "Organizations",
                        principalColumn: "Id",
                        onDelete: ReferentialAction.Cascade);
                });

            migrationBuilder.CreateTable(
                name: "Users",
                columns: table => new
                {
                    UserId = table.Column<Guid>(type: "uniqueidentifier", nullable: false),
                    Name = table.Column<string>(type: "nvarchar(200)", maxLength: 200, nullable: false),
                    Email = table.Column<string>(type: "nvarchar(255)", maxLength: 255, nullable: false),
                    ProfilePictureUrl = table.Column<string>(type: "nvarchar(500)", maxLength: 500, nullable: true),
                    PasswordHash = table.Column<string>(type: "nvarchar(255)", maxLength: 255, nullable: true),
                    Role = table.Column<string>(type: "nvarchar(max)", nullable: true),
                    IsActive = table.Column<bool>(type: "bit", nullable: false, defaultValue: false),
                    EmailVerified = table.Column<bool>(type: "bit", nullable: false, defaultValue: false),
                    ActivationToken = table.Column<string>(type: "nvarchar(255)", maxLength: 255, nullable: true),
                    ActivationTokenExpiry = table.Column<DateTime>(type: "datetime2", nullable: true),
                    CreatedAt = table.Column<DateTime>(type: "datetime2", nullable: false),
                    OrganizationId = table.Column<Guid>(type: "uniqueidentifier", nullable: false),
                    Id = table.Column<Guid>(type: "uniqueidentifier", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_Users", x => x.UserId);
                    table.ForeignKey(
                        name: "FK_Users_Organizations_OrganizationId",
                        column: x => x.OrganizationId,
                        principalTable: "Organizations",
                        principalColumn: "OrganizationId",
                        onDelete: ReferentialAction.Restrict);
                });

            migrationBuilder.CreateTable(
                name: "Qualifications",
                columns: table => new
                {
                    QualificationId = table.Column<string>(type: "nvarchar(450)", nullable: false),
                    Name = table.Column<string>(type: "nvarchar(200)", maxLength: 200, nullable: false),
                    Description = table.Column<string>(type: "nvarchar(max)", nullable: true),
                    StaffMemberMecanographicNumber = table.Column<long>(type: "bigint", nullable: true)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_Qualifications", x => x.QualificationId);
                    table.ForeignKey(
                        name: "FK_Qualifications_StaffMembers_StaffMemberMecanographicNumber",
                        column: x => x.StaffMemberMecanographicNumber,
                        principalTable: "StaffMembers",
                        principalColumn: "MecanographicNumber");
                });

            migrationBuilder.CreateTable(
                name: "DockStorageArea",
                columns: table => new
                {
                    DocksCode = table.Column<string>(type: "nvarchar(20)", nullable: false),
                    ServedStorageAreasStorageAreaId = table.Column<string>(type: "nvarchar(64)", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_DockStorageArea", x => new { x.DocksCode, x.ServedStorageAreasStorageAreaId });
                    table.ForeignKey(
                        name: "FK_DockStorageArea_Docks_DocksCode",
                        column: x => x.DocksCode,
                        principalTable: "Docks",
                        principalColumn: "Code",
                        onDelete: ReferentialAction.Cascade);
                    table.ForeignKey(
                        name: "FK_DockStorageArea_StorageAreas_ServedStorageAreasStorageAreaId",
                        column: x => x.ServedStorageAreasStorageAreaId,
                        principalTable: "StorageAreas",
                        principalColumn: "StorageAreaId",
                        onDelete: ReferentialAction.Cascade);
                });

            migrationBuilder.CreateTable(
                name: "DockStorageDistance",
                columns: table => new
                {
                    DockStorageDistanceId = table.Column<int>(type: "int", nullable: false)
                        .Annotation("SqlServer:Identity", "1, 1"),
                    DistanceMeters = table.Column<int>(type: "int", nullable: false),
                    DockCode = table.Column<string>(type: "nvarchar(20)", nullable: false),
                    StorageAreaId = table.Column<string>(type: "nvarchar(64)", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_DockStorageDistance", x => x.DockStorageDistanceId);
                    table.ForeignKey(
                        name: "FK_DockStorageDistance_Docks_DockCode",
                        column: x => x.DockCode,
                        principalTable: "Docks",
                        principalColumn: "Code",
                        onDelete: ReferentialAction.Cascade);
                    table.ForeignKey(
                        name: "FK_DockStorageDistance_StorageAreas_StorageAreaId",
                        column: x => x.StorageAreaId,
                        principalTable: "StorageAreas",
                        principalColumn: "StorageAreaId",
                        onDelete: ReferentialAction.Cascade);
                });

            migrationBuilder.CreateTable(
                name: "DockVesselType",
                columns: table => new
                {
                    AllowedDocksCode = table.Column<string>(type: "nvarchar(20)", nullable: false),
                    AllowedVesselTypesVesselTypeId = table.Column<string>(type: "nvarchar(64)", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_DockVesselType", x => new { x.AllowedDocksCode, x.AllowedVesselTypesVesselTypeId });
                    table.ForeignKey(
                        name: "FK_DockVesselType_Docks_AllowedDocksCode",
                        column: x => x.AllowedDocksCode,
                        principalTable: "Docks",
                        principalColumn: "Code",
                        onDelete: ReferentialAction.Cascade);
                    table.ForeignKey(
                        name: "FK_DockVesselType_VesselTypes_AllowedVesselTypesVesselTypeId",
                        column: x => x.AllowedVesselTypesVesselTypeId,
                        principalTable: "VesselTypes",
                        principalColumn: "VesselTypeId",
                        onDelete: ReferentialAction.Cascade);
                });

            migrationBuilder.CreateTable(
                name: "Vessels",
                columns: table => new
                {
                    ImoNumber = table.Column<string>(type: "nvarchar(7)", maxLength: 7, nullable: false),
                    Name = table.Column<string>(type: "nvarchar(200)", maxLength: 200, nullable: false),
                    CapacityTEU = table.Column<int>(type: "int", nullable: true),
                    VesselTypeId = table.Column<string>(type: "nvarchar(64)", maxLength: 64, nullable: false),
                    OwnerOrganizationId = table.Column<Guid>(type: "uniqueidentifier", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_Vessels", x => x.ImoNumber);
                    table.ForeignKey(
                        name: "FK_Vessels_Organizations_OwnerOrganizationId",
                        column: x => x.OwnerOrganizationId,
                        principalTable: "Organizations",
                        principalColumn: "OrganizationId",
                        onDelete: ReferentialAction.Restrict);
                    table.ForeignKey(
                        name: "FK_Vessels_VesselTypes_VesselTypeId",
                        column: x => x.VesselTypeId,
                        principalTable: "VesselTypes",
                        principalColumn: "VesselTypeId",
                        onDelete: ReferentialAction.Restrict);
                });

            migrationBuilder.CreateTable(
                name: "PhysicalResourceQualifications",
                columns: table => new
                {
                    PhysicalResourceResourceId = table.Column<string>(type: "nvarchar(450)", nullable: false),
                    RequiredQualificationsQualificationId = table.Column<string>(type: "nvarchar(450)", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_PhysicalResourceQualifications", x => new { x.PhysicalResourceResourceId, x.RequiredQualificationsQualificationId });
                    table.ForeignKey(
                        name: "FK_PhysicalResourceQualifications_PhysicalResources_PhysicalResourceResourceId",
                        column: x => x.PhysicalResourceResourceId,
                        principalTable: "PhysicalResources",
                        principalColumn: "ResourceId",
                        onDelete: ReferentialAction.Cascade);
                    table.ForeignKey(
                        name: "FK_PhysicalResourceQualifications_Qualifications_RequiredQualificationsQualificationId",
                        column: x => x.RequiredQualificationsQualificationId,
                        principalTable: "Qualifications",
                        principalColumn: "QualificationId",
                        onDelete: ReferentialAction.Cascade);
                });

            migrationBuilder.CreateTable(
                name: "VesselVisitNotifications",
                columns: table => new
                {
                    VvnGuid = table.Column<Guid>(type: "uniqueidentifier", nullable: false),
                    VvnBusinessId = table.Column<string>(type: "nvarchar(20)", maxLength: 20, nullable: false),
                    State = table.Column<int>(type: "int", nullable: false),
                    VisitPurpose = table.Column<int>(type: "int", nullable: false),
                    VesselImo = table.Column<string>(type: "nvarchar(7)", maxLength: 7, nullable: false),
                    Eta = table.Column<DateTime>(type: "datetime2", nullable: false),
                    Etd = table.Column<DateTime>(type: "datetime2", nullable: false),
                    CreatedAt = table.Column<DateTime>(type: "datetime2", nullable: false),
                    SubmittedAt = table.Column<DateTime>(type: "datetime2", nullable: true),
                    ApprovedAt = table.Column<DateTime>(type: "datetime2", nullable: true),
                    RejectedAt = table.Column<DateTime>(type: "datetime2", nullable: true),
                    RejectionReason = table.Column<string>(type: "nvarchar(max)", nullable: true),
                    VesselImoNumber = table.Column<string>(type: "nvarchar(7)", nullable: true),
                    OrganizationId = table.Column<Guid>(type: "uniqueidentifier", nullable: false),
                    SubmittedById = table.Column<Guid>(type: "uniqueidentifier", nullable: true),
                    ApprovedById = table.Column<Guid>(type: "uniqueidentifier", nullable: true),
                    RejectedById = table.Column<Guid>(type: "uniqueidentifier", nullable: true),
                    DockAssignmentId = table.Column<Guid>(type: "uniqueidentifier", nullable: true),
                    LoadingCount = table.Column<int>(type: "int", nullable: false),
                    UnloadingCount = table.Column<int>(type: "int", nullable: false),
                    CaptainName = table.Column<string>(type: "nvarchar(max)", nullable: false),
                    CaptainCitizenId = table.Column<string>(type: "nvarchar(max)", nullable: false),
                    CaptainNationality = table.Column<string>(type: "nvarchar(2)", maxLength: 2, nullable: false),
                    CrewCount = table.Column<int>(type: "int", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_VesselVisitNotifications", x => x.VvnGuid);
                    table.ForeignKey(
                        name: "FK_VesselVisitNotifications_Organizations_OrganizationId",
                        column: x => x.OrganizationId,
                        principalTable: "Organizations",
                        principalColumn: "OrganizationId",
                        onDelete: ReferentialAction.Restrict);
                    table.ForeignKey(
                        name: "FK_VesselVisitNotifications_Users_ApprovedById",
                        column: x => x.ApprovedById,
                        principalTable: "Users",
                        principalColumn: "UserId");
                    table.ForeignKey(
                        name: "FK_VesselVisitNotifications_Users_RejectedById",
                        column: x => x.RejectedById,
                        principalTable: "Users",
                        principalColumn: "UserId");
                    table.ForeignKey(
                        name: "FK_VesselVisitNotifications_Users_SubmittedById",
                        column: x => x.SubmittedById,
                        principalTable: "Users",
                        principalColumn: "UserId");
                    table.ForeignKey(
                        name: "FK_VesselVisitNotifications_Vessels_VesselImoNumber",
                        column: x => x.VesselImoNumber,
                        principalTable: "Vessels",
                        principalColumn: "ImoNumber");
                });

            migrationBuilder.CreateTable(
                name: "CargoManifests",
                columns: table => new
                {
                    Id = table.Column<Guid>(type: "uniqueidentifier", nullable: false),
                    VvnGuid = table.Column<Guid>(type: "uniqueidentifier", nullable: false),
                    ManifestType = table.Column<int>(type: "int", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_CargoManifests", x => x.Id);
                    table.ForeignKey(
                        name: "FK_CargoManifests_VesselVisitNotifications_VvnGuid",
                        column: x => x.VvnGuid,
                        principalTable: "VesselVisitNotifications",
                        principalColumn: "VvnGuid",
                        onDelete: ReferentialAction.Cascade);
                });

            migrationBuilder.CreateTable(
                name: "CrewMembers",
                columns: table => new
                {
                    CrewMemberId = table.Column<Guid>(type: "uniqueidentifier", nullable: false),
                    Name = table.Column<string>(type: "nvarchar(200)", maxLength: 200, nullable: false),
                    CitizenId = table.Column<string>(type: "nvarchar(100)", maxLength: 100, nullable: false),
                    Nationality = table.Column<string>(type: "nvarchar(50)", maxLength: 50, nullable: false),
                    VesselVisitNotificationId = table.Column<Guid>(type: "uniqueidentifier", nullable: false),
                    VesselVisitNotificationVvnGuid = table.Column<Guid>(type: "uniqueidentifier", nullable: true)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_CrewMembers", x => x.CrewMemberId);
                    table.ForeignKey(
                        name: "FK_CrewMembers_VesselVisitNotifications_VesselVisitNotificationId",
                        column: x => x.VesselVisitNotificationId,
                        principalTable: "VesselVisitNotifications",
                        principalColumn: "VvnGuid",
                        onDelete: ReferentialAction.Cascade);
                    table.ForeignKey(
                        name: "FK_CrewMembers_VesselVisitNotifications_VesselVisitNotificationVvnGuid",
                        column: x => x.VesselVisitNotificationVvnGuid,
                        principalTable: "VesselVisitNotifications",
                        principalColumn: "VvnGuid");
                });

            migrationBuilder.CreateTable(
                name: "DockAssignments",
                columns: table => new
                {
                    DockAssignmentId = table.Column<Guid>(type: "uniqueidentifier", nullable: false),
                    BerthFrom = table.Column<DateTime>(type: "datetime2", nullable: false),
                    BerthTo = table.Column<DateTime>(type: "datetime2", nullable: false),
                    Status = table.Column<int>(type: "int", nullable: false),
                    DockCode = table.Column<string>(type: "nvarchar(20)", nullable: false),
                    CreatedAt = table.Column<DateTime>(type: "datetime2", nullable: false),
                    VesselVisitNotificationId = table.Column<Guid>(type: "uniqueidentifier", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_DockAssignments", x => x.DockAssignmentId);
                    table.ForeignKey(
                        name: "FK_DockAssignments_Docks_DockCode",
                        column: x => x.DockCode,
                        principalTable: "Docks",
                        principalColumn: "Code",
                        onDelete: ReferentialAction.Restrict);
                    table.ForeignKey(
                        name: "FK_DockAssignments_VesselVisitNotifications_VesselVisitNotificationId",
                        column: x => x.VesselVisitNotificationId,
                        principalTable: "VesselVisitNotifications",
                        principalColumn: "VvnGuid",
                        onDelete: ReferentialAction.Restrict);
                });

            migrationBuilder.CreateTable(
                name: "CargoManifestEntries",
                columns: table => new
                {
                    Id = table.Column<Guid>(type: "uniqueidentifier", nullable: false),
                    ManifestId = table.Column<Guid>(type: "uniqueidentifier", nullable: false),
                    ContainerUniqueId = table.Column<string>(type: "nvarchar(20)", maxLength: 20, nullable: false),
                    HazardousGoods = table.Column<bool>(type: "bit", nullable: false),
                    ContainerBayNr = table.Column<int>(type: "int", nullable: false),
                    ContainerRowNr = table.Column<int>(type: "int", nullable: false),
                    ContainerTierNr = table.Column<int>(type: "int", nullable: false),
                    GoodsDescription = table.Column<string>(type: "nvarchar(max)", nullable: true)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_CargoManifestEntries", x => x.Id);
                    table.ForeignKey(
                        name: "FK_CargoManifestEntries_CargoManifests_ManifestId",
                        column: x => x.ManifestId,
                        principalTable: "CargoManifests",
                        principalColumn: "Id",
                        onDelete: ReferentialAction.Cascade);
                });

            migrationBuilder.CreateTable(
                name: "DockAssignmentEvents",
                columns: table => new
                {
                    DockAssignmentEventId = table.Column<int>(type: "int", nullable: false)
                        .Annotation("SqlServer:Identity", "1, 1"),
                    AssignedAt = table.Column<DateTime>(type: "datetime2", nullable: false),
                    AssignedById = table.Column<string>(type: "nvarchar(max)", nullable: false),
                    FromDockCode = table.Column<string>(type: "nvarchar(max)", nullable: true),
                    ToDockCode = table.Column<string>(type: "nvarchar(max)", nullable: false),
                    Reason = table.Column<string>(type: "nvarchar(max)", nullable: true),
                    DockAssignmentId = table.Column<Guid>(type: "uniqueidentifier", nullable: true)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_DockAssignmentEvents", x => x.DockAssignmentEventId);
                    table.ForeignKey(
                        name: "FK_DockAssignmentEvents_DockAssignments_DockAssignmentId",
                        column: x => x.DockAssignmentId,
                        principalTable: "DockAssignments",
                        principalColumn: "DockAssignmentId",
                        onDelete: ReferentialAction.Cascade);
                });

            migrationBuilder.CreateIndex(
                name: "IX_CargoManifestEntries_ManifestId",
                table: "CargoManifestEntries",
                column: "ManifestId");

            migrationBuilder.CreateIndex(
                name: "IX_CargoManifestEntries_ManifestId_ContainerUniqueId",
                table: "CargoManifestEntries",
                columns: new[] { "ManifestId", "ContainerUniqueId" },
                unique: true);

            migrationBuilder.CreateIndex(
                name: "IX_CargoManifests_VvnGuid",
                table: "CargoManifests",
                column: "VvnGuid",
                unique: true);

            migrationBuilder.CreateIndex(
                name: "IX_CargoManifests_VvnGuid_ManifestType",
                table: "CargoManifests",
                columns: new[] { "VvnGuid", "ManifestType" },
                unique: true);

            migrationBuilder.CreateIndex(
                name: "IX_CrewMembers_VesselVisitNotificationId",
                table: "CrewMembers",
                column: "VesselVisitNotificationId");

            migrationBuilder.CreateIndex(
                name: "IX_CrewMembers_VesselVisitNotificationVvnGuid",
                table: "CrewMembers",
                column: "VesselVisitNotificationVvnGuid");

            migrationBuilder.CreateIndex(
                name: "IX_DecisionLogs_VvnGuid_AtUtc",
                table: "DecisionLogs",
                columns: new[] { "VvnGuid", "AtUtc" });

            migrationBuilder.CreateIndex(
                name: "IX_DockAssignmentEvents_DockAssignmentId",
                table: "DockAssignmentEvents",
                column: "DockAssignmentId");

            migrationBuilder.CreateIndex(
                name: "IX_DockAssignments_DockCode_BerthFrom",
                table: "DockAssignments",
                columns: new[] { "DockCode", "BerthFrom" });

            migrationBuilder.CreateIndex(
                name: "IX_DockAssignments_VesselVisitNotificationId",
                table: "DockAssignments",
                column: "VesselVisitNotificationId",
                unique: true);

            migrationBuilder.CreateIndex(
                name: "IX_Docks_Code",
                table: "Docks",
                column: "Code",
                unique: true);

            migrationBuilder.CreateIndex(
                name: "IX_Docks_Name",
                table: "Docks",
                column: "Name");

            migrationBuilder.CreateIndex(
                name: "IX_DockStorageArea_ServedStorageAreasStorageAreaId",
                table: "DockStorageArea",
                column: "ServedStorageAreasStorageAreaId");

            migrationBuilder.CreateIndex(
                name: "IX_DockStorageDistance_DockCode",
                table: "DockStorageDistance",
                column: "DockCode");

            migrationBuilder.CreateIndex(
                name: "IX_DockStorageDistance_StorageAreaId",
                table: "DockStorageDistance",
                column: "StorageAreaId");

            migrationBuilder.CreateIndex(
                name: "IX_DockVesselType_AllowedVesselTypesVesselTypeId",
                table: "DockVesselType",
                column: "AllowedVesselTypesVesselTypeId");

            migrationBuilder.CreateIndex(
                name: "IX_Organizations_Identifier",
                table: "Organizations",
                column: "Identifier",
                unique: true);

            migrationBuilder.CreateIndex(
                name: "IX_PhysicalResourceQualifications_RequiredQualificationsQualificationId",
                table: "PhysicalResourceQualifications",
                column: "RequiredQualificationsQualificationId");

            migrationBuilder.CreateIndex(
                name: "IX_PhysicalResources_Availability",
                table: "PhysicalResources",
                column: "Availability");

            migrationBuilder.CreateIndex(
                name: "IX_PhysicalResources_Code",
                table: "PhysicalResources",
                column: "Code",
                unique: true);

            migrationBuilder.CreateIndex(
                name: "IX_PhysicalResources_CurrentDockCode",
                table: "PhysicalResources",
                column: "CurrentDockCode");

            migrationBuilder.CreateIndex(
                name: "IX_PhysicalResources_InstalledAtDockCode",
                table: "PhysicalResources",
                column: "InstalledAtDockCode",
                unique: true,
                filter: "[InstalledAtDockCode] IS NOT NULL");

            migrationBuilder.CreateIndex(
                name: "IX_Qualifications_StaffMemberMecanographicNumber",
                table: "Qualifications",
                column: "StaffMemberMecanographicNumber");

            migrationBuilder.CreateIndex(
                name: "IX_Representative_Email",
                table: "Representative",
                column: "Email");

            migrationBuilder.CreateIndex(
                name: "IX_Representative_OrganizationId",
                table: "Representative",
                column: "OrganizationId");

            migrationBuilder.CreateIndex(
                name: "IX_StorageAreas_Name",
                table: "StorageAreas",
                column: "Name");

            migrationBuilder.CreateIndex(
                name: "IX_StorageAreas_Type",
                table: "StorageAreas",
                column: "Type");

            migrationBuilder.CreateIndex(
                name: "IX_Users_Email",
                table: "Users",
                column: "Email",
                unique: true);

            migrationBuilder.CreateIndex(
                name: "IX_Users_OrganizationId",
                table: "Users",
                column: "OrganizationId");

            migrationBuilder.CreateIndex(
                name: "IX_Vessels_ImoNumber",
                table: "Vessels",
                column: "ImoNumber",
                unique: true);

            migrationBuilder.CreateIndex(
                name: "IX_Vessels_Name",
                table: "Vessels",
                column: "Name",
                unique: true);

            migrationBuilder.CreateIndex(
                name: "IX_Vessels_OwnerOrganizationId",
                table: "Vessels",
                column: "OwnerOrganizationId");

            migrationBuilder.CreateIndex(
                name: "IX_Vessels_VesselTypeId",
                table: "Vessels",
                column: "VesselTypeId");

            migrationBuilder.CreateIndex(
                name: "IX_VesselVisitNotifications_ApprovedById",
                table: "VesselVisitNotifications",
                column: "ApprovedById");

            migrationBuilder.CreateIndex(
                name: "IX_VesselVisitNotifications_OrganizationId",
                table: "VesselVisitNotifications",
                column: "OrganizationId");

            migrationBuilder.CreateIndex(
                name: "IX_VesselVisitNotifications_RejectedById",
                table: "VesselVisitNotifications",
                column: "RejectedById");

            migrationBuilder.CreateIndex(
                name: "IX_VesselVisitNotifications_SubmittedById",
                table: "VesselVisitNotifications",
                column: "SubmittedById");

            migrationBuilder.CreateIndex(
                name: "IX_VesselVisitNotifications_VesselImoNumber",
                table: "VesselVisitNotifications",
                column: "VesselImoNumber");
        }

        /// <inheritdoc />
        protected override void Down(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.DropTable(
                name: "CargoManifestEntries");

            migrationBuilder.DropTable(
                name: "CrewMembers");

            migrationBuilder.DropTable(
                name: "DecisionLogs");

            migrationBuilder.DropTable(
                name: "DockAssignmentEvents");

            migrationBuilder.DropTable(
                name: "DockStorageArea");

            migrationBuilder.DropTable(
                name: "DockStorageDistance");

            migrationBuilder.DropTable(
                name: "DockVesselType");

            migrationBuilder.DropTable(
                name: "PhysicalResourceQualifications");

            migrationBuilder.DropTable(
                name: "Representative");

            migrationBuilder.DropTable(
                name: "CargoManifests");

            migrationBuilder.DropTable(
                name: "DockAssignments");

            migrationBuilder.DropTable(
                name: "StorageAreas");

            migrationBuilder.DropTable(
                name: "PhysicalResources");

            migrationBuilder.DropTable(
                name: "Qualifications");

            migrationBuilder.DropTable(
                name: "VesselVisitNotifications");

            migrationBuilder.DropTable(
                name: "Docks");

            migrationBuilder.DropTable(
                name: "StaffMembers");

            migrationBuilder.DropTable(
                name: "Users");

            migrationBuilder.DropTable(
                name: "Vessels");

            migrationBuilder.DropTable(
                name: "Organizations");

            migrationBuilder.DropTable(
                name: "VesselTypes");
        }
    }
}
