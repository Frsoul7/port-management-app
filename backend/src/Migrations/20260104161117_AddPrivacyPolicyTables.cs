using System;
using Microsoft.EntityFrameworkCore.Migrations;

#nullable disable

namespace DDDNetCore.Migrations
{
    /// <inheritdoc />
    public partial class AddPrivacyPolicyTables : Migration
    {
        /// <inheritdoc />
        protected override void Up(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.CreateTable(
                name: "PrivacyPolicies",
                columns: table => new
                {
                    PolicyId = table.Column<Guid>(type: "uniqueidentifier", nullable: false),
                    Version = table.Column<string>(type: "nvarchar(50)", maxLength: 50, nullable: false),
                    Content = table.Column<string>(type: "nvarchar(max)", nullable: false),
                    ChangeSummary = table.Column<string>(type: "nvarchar(2000)", maxLength: 2000, nullable: true),
                    EffectiveDate = table.Column<DateTime>(type: "datetime2", nullable: false),
                    CreatedBy = table.Column<Guid>(type: "uniqueidentifier", nullable: false),
                    CreatedAt = table.Column<DateTime>(type: "datetime2", nullable: false),
                    IsActive = table.Column<bool>(type: "bit", nullable: false),
                    LanguageCode = table.Column<string>(type: "nvarchar(10)", maxLength: 10, nullable: false, defaultValue: "pt")
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_PrivacyPolicies", x => x.PolicyId);
                });

            migrationBuilder.CreateTable(
                name: "PrivacyPolicyAcknowledgments",
                columns: table => new
                {
                    AcknowledgmentId = table.Column<Guid>(type: "uniqueidentifier", nullable: false),
                    UserId = table.Column<Guid>(type: "uniqueidentifier", nullable: false),
                    PolicyId = table.Column<Guid>(type: "uniqueidentifier", nullable: false),
                    PolicyVersion = table.Column<string>(type: "nvarchar(50)", maxLength: 50, nullable: false),
                    AcknowledgedAt = table.Column<DateTime>(type: "datetime2", nullable: false),
                    IpAddress = table.Column<string>(type: "nvarchar(50)", maxLength: 50, nullable: true),
                    UserAgent = table.Column<string>(type: "nvarchar(500)", maxLength: 500, nullable: true)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_PrivacyPolicyAcknowledgments", x => x.AcknowledgmentId);
                    table.ForeignKey(
                        name: "FK_PrivacyPolicyAcknowledgments_PrivacyPolicies_PolicyId",
                        column: x => x.PolicyId,
                        principalTable: "PrivacyPolicies",
                        principalColumn: "PolicyId",
                        onDelete: ReferentialAction.Restrict);
                });

            migrationBuilder.CreateIndex(
                name: "IX_PrivacyPolicies_EffectiveDate",
                table: "PrivacyPolicies",
                column: "EffectiveDate");

            migrationBuilder.CreateIndex(
                name: "IX_PrivacyPolicies_IsActive",
                table: "PrivacyPolicies",
                column: "IsActive");

            migrationBuilder.CreateIndex(
                name: "IX_PrivacyPolicies_LanguageCode",
                table: "PrivacyPolicies",
                column: "LanguageCode");

            migrationBuilder.CreateIndex(
                name: "IX_PrivacyPolicies_LanguageCode_IsActive",
                table: "PrivacyPolicies",
                columns: new[] { "LanguageCode", "IsActive" });

            migrationBuilder.CreateIndex(
                name: "IX_PrivacyPolicyAcknowledgments_AcknowledgedAt",
                table: "PrivacyPolicyAcknowledgments",
                column: "AcknowledgedAt");

            migrationBuilder.CreateIndex(
                name: "IX_PrivacyPolicyAcknowledgments_PolicyId",
                table: "PrivacyPolicyAcknowledgments",
                column: "PolicyId");

            migrationBuilder.CreateIndex(
                name: "IX_PrivacyPolicyAcknowledgments_UserId",
                table: "PrivacyPolicyAcknowledgments",
                column: "UserId");

            migrationBuilder.CreateIndex(
                name: "IX_PrivacyPolicyAcknowledgments_UserId_PolicyId",
                table: "PrivacyPolicyAcknowledgments",
                columns: new[] { "UserId", "PolicyId" },
                unique: true);
        }

        /// <inheritdoc />
        protected override void Down(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.DropTable(
                name: "PrivacyPolicyAcknowledgments");

            migrationBuilder.DropTable(
                name: "PrivacyPolicies");
        }
    }
}
