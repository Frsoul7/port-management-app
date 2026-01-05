using DDDNetCore.Domain.Visits;
using DDDNetCore.Domain.Visits.Manifests;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace DDDNetCore.Infrastructure.Configurations.Visits.Manifests
{
    public class CargoManifestConfiguration : IEntityTypeConfiguration<CargoManifest>
    {
        public void Configure(EntityTypeBuilder<CargoManifest> b)
        {
            b.ToTable("CargoManifests");
            b.HasKey(m => m.Id);

            // FK back to VVN (set on dependent)
            b.Property(m => m.VvnGuid).IsRequired();

            // TPH discriminator defined ONLY here
            b.HasDiscriminator<ManifestType>("ManifestType")
             .HasValue<LoadingCargoManifest>(ManifestType.Load)
             .HasValue<UnloadingCargoManifest>(ManifestType.Unload);

            // Unique per (VVN, ManifestType)
            // Use string-based variant; EF doesn't accept EF.Property in HasIndex expressions
            b.HasIndex("VvnGuid", "ManifestType").IsUnique();

            // Entries collection (mapped via property)
            b.HasMany(m => m.Entries)
             .WithOne()
             .HasForeignKey(e => e.ManifestId)
             .OnDelete(DeleteBehavior.Cascade);

        }
    }
}
