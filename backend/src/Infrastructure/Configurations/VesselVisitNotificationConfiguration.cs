using DDDNetCore.Domain.Visits;
using DDDNetCore.Domain.Visits.Manifests;
using DDDNetCore.Domain.Organizations;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace DDDNetCore.Infrastructure.Configurations.Visits
{
    public class VesselVisitNotificationConfiguration : IEntityTypeConfiguration<VesselVisitNotification>
    {
        public void Configure(EntityTypeBuilder<VesselVisitNotification> b)
        {
            b.ToTable("VesselVisitNotifications");
            b.HasKey(v => v.VvnGuid);

            // Business ID (2025-PTLEI-000001)
            b.Property(v => v.VvnBusinessId)
             .IsRequired()
             .HasMaxLength(20);

            // Vessel
            b.Property(v => v.VesselImo)
             .IsRequired()
             .HasMaxLength(7);

            b.Property(v => v.CaptainName).IsRequired();
            b.Property(v => v.CaptainCitizenId).IsRequired();
            b.Property(v => v.CaptainNationality).IsRequired().HasMaxLength(2);
            b.Property(v => v.CrewCount).IsRequired();

            b.Property(v => v.VisitPurpose).IsRequired();
            b.Property(v => v.State).IsRequired();

            b.Property(v => v.Eta).IsRequired();
            b.Property(v => v.Etd).IsRequired();
            b.Property(v => v.CreatedAt).IsRequired();

            // Counters
            b.Property(v => v.LoadingCount).IsRequired();
            b.Property(v => v.UnloadingCount).IsRequired();

            // OrganizationId value object conversion
            b.Property(v => v.OrganizationId)
             .HasConversion(
                 v => v.Value,                    // VO -> Guid for database
                 v => new OrganizationId(v)       // Guid -> VO when reading
             )
             .IsRequired();

            // Relationship to Organization (explicit FK configuration to avoid shadow property)
            b.HasOne(v => v.Organization)
             .WithMany()
             .HasForeignKey(v => v.OrganizationId)
             .HasPrincipalKey(o => o.OrganizationId)
             .OnDelete(DeleteBehavior.Restrict);

            // Optional 1:1 to Loading manifest
            b.HasOne(v => v.LoadingManifest)
             .WithOne()
             .HasForeignKey<LoadingCargoManifest>(m => m.VvnGuid)
             .OnDelete(DeleteBehavior.Cascade)
             .IsRequired(false);

            // Optional 1:1 to Unloading manifest
            b.HasOne(v => v.UnloadingManifest)
             .WithOne()
             .HasForeignKey<UnloadingCargoManifest>(m => m.VvnGuid)
             .OnDelete(DeleteBehavior.Cascade)
             .IsRequired(false);


            // The discriminator belongs in CargoManifestConfiguration on the CargoManifest base type.
        }
    }
}
