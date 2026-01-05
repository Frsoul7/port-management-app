using DDDNetCore.Domain.DataRequests;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace DDDNetCore.Infrastructure.Configurations
{
    /// <summary>
    /// EF Core configuration for DataRequest entity.
    /// US 4.5.3: User Data Rights (SAR)
    /// US 4.5.4: Non-User Data Rights
    /// </summary>
    public class DataRequestConfiguration : IEntityTypeConfiguration<DataRequest>
    {
        public void Configure(EntityTypeBuilder<DataRequest> builder)
        {
            builder.ToTable("DataRequests");

            // Primary key (Guid from base Entity)
            builder.HasKey(r => r.Id);
            builder.Property(r => r.Id)
                .IsRequired();

            // RequestId - strongly typed ID, ignore for EF (computed from Id)
            builder.Ignore(r => r.RequestId);

            // Source - required, stored as string (USER or EXTERNAL)
            builder.Property(r => r.Source)
                .IsRequired()
                .HasConversion<string>()
                .HasMaxLength(20);
            builder.HasIndex(r => r.Source);

            // UserId - optional (only for USER source requests)
            builder.Property(r => r.UserId);
            builder.HasIndex(r => r.UserId);

            // Full name - required
            builder.Property(r => r.FullName)
                .IsRequired()
                .HasMaxLength(200);

            // Email - required, indexed for lookups
            builder.Property(r => r.Email)
                .IsRequired()
                .HasMaxLength(255);
            builder.HasIndex(r => r.Email);

            // Phone - optional
            builder.Property(r => r.Phone)
                .HasMaxLength(50);

            // Request type - required, stored as string
            builder.Property(r => r.RequestType)
                .IsRequired()
                .HasConversion<string>()
                .HasMaxLength(50);

            // Status - required, stored as string, indexed
            builder.Property(r => r.Status)
                .IsRequired()
                .HasConversion<string>()
                .HasMaxLength(50);
            builder.HasIndex(r => r.Status);

            // Vessel reference - optional
            builder.Property(r => r.VesselReference)
                .HasMaxLength(200);

            // VVN reference - optional
            builder.Property(r => r.VvnReference)
                .HasMaxLength(100);

            // Description - required
            builder.Property(r => r.Description)
                .IsRequired()
                .HasMaxLength(4000);

            // Reference number - required, unique, indexed
            builder.Property(r => r.ReferenceNumber)
                .IsRequired()
                .HasMaxLength(50);
            builder.HasIndex(r => r.ReferenceNumber).IsUnique();

            // RequestData - optional, JSON for rectification requests
            builder.Property(r => r.RequestData)
                .HasMaxLength(8000);

            // Consent given at - required
            builder.Property(r => r.ConsentGivenAt)
                .IsRequired();

            // Submitted at - required, indexed
            builder.Property(r => r.SubmittedAt)
                .IsRequired();
            builder.HasIndex(r => r.SubmittedAt);

            // Updated at - optional
            builder.Property(r => r.UpdatedAt);

            // Completed at - optional
            builder.Property(r => r.CompletedAt);

            // Admin notes - optional
            builder.Property(r => r.AdminNotes)
                .HasMaxLength(4000);

            // Processed by - optional
            builder.Property(r => r.ProcessedBy);

            // IP address - optional
            builder.Property(r => r.IpAddress)
                .HasMaxLength(50);

            // Composite index for status check by reference and email
            builder.HasIndex(r => new { r.ReferenceNumber, r.Email });
        }
    }
}
