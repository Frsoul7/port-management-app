using DDDNetCore.Domain.Visits.Manifests;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace DDDNetCore.Infrastructure.Configurations
{
    public class ManifestEntryConfiguration : IEntityTypeConfiguration<ManifestEntry>
    {
        public void Configure(EntityTypeBuilder<ManifestEntry> b)
        {
            b.ToTable("CargoManifestEntries");
            b.HasKey(e => e.Id);

            // Foreign key to CargoManifest
            b.Property(e => e.ManifestId).IsRequired();
            b.HasIndex(e => e.ManifestId);

            // Container identification
            b.Property(e => e.ContainerUniqueId)
             .HasMaxLength(20)
             .IsRequired();

            // Avoid duplicate container codes within the same manifest
            b.HasIndex(e => new { e.ManifestId, e.ContainerUniqueId }).IsUnique();

            // Core cargo positioning & description
            b.Property(e => e.ContainerBayNr).IsRequired();
            b.Property(e => e.ContainerRowNr).IsRequired();
            b.Property(e => e.ContainerTierNr).IsRequired();

            b.Property(e => e.HazardousGoods).IsRequired();
            b.Property(e => e.GoodsDescription);

            // No navigation back to manifest in this aggregate model
            // (entries are owned via aggregate methods in CargoManifest)
        }
    }
}
