using System;
using Microsoft.EntityFrameworkCore.Migrations;

#nullable disable

namespace DDDNetCore.Migrations
{
    /// <inheritdoc />
    public partial class ExtendDataRequestForUserRights : Migration
    {
        /// <inheritdoc />
        protected override void Up(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.AddColumn<string>(
                name: "RequestData",
                table: "DataRequests",
                type: "nvarchar(max)",
                maxLength: 8000,
                nullable: true);

            migrationBuilder.AddColumn<string>(
                name: "Source",
                table: "DataRequests",
                type: "nvarchar(20)",
                maxLength: 20,
                nullable: false,
                defaultValue: "");

            migrationBuilder.AddColumn<Guid>(
                name: "UserId",
                table: "DataRequests",
                type: "uniqueidentifier",
                nullable: true);

            migrationBuilder.CreateIndex(
                name: "IX_DataRequests_Source",
                table: "DataRequests",
                column: "Source");

            migrationBuilder.CreateIndex(
                name: "IX_DataRequests_UserId",
                table: "DataRequests",
                column: "UserId");
        }

        /// <inheritdoc />
        protected override void Down(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.DropIndex(
                name: "IX_DataRequests_Source",
                table: "DataRequests");

            migrationBuilder.DropIndex(
                name: "IX_DataRequests_UserId",
                table: "DataRequests");

            migrationBuilder.DropColumn(
                name: "RequestData",
                table: "DataRequests");

            migrationBuilder.DropColumn(
                name: "Source",
                table: "DataRequests");

            migrationBuilder.DropColumn(
                name: "UserId",
                table: "DataRequests");
        }
    }
}
