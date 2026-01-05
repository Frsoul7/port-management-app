using DDDNetCore.Domain.Docks;
using DDDNetCore.Domain.Vessels;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace DDDNetCore.Infrastructure.Configurations;

public class DockConfiguration : IEntityTypeConfiguration<Dock>
{
    public void Configure(EntityTypeBuilder<Dock> b)
    {
        // Key
        b.HasKey(d => d.Code);

        b.Property(d => d.Code)
            .IsRequired()
            .HasMaxLength(20);

        b.Property(d => d.Name)
            .IsRequired()
            .HasMaxLength(200);

        b.Property(d => d.Location)
            .IsRequired()
            .HasMaxLength(500);

        b.Property(d => d.LengthM)
            .IsRequired();

        b.Property(d => d.DepthM)
            .IsRequired();

        b.Property(d => d.MaxDraftM)
            .IsRequired();

        // Many-to-many relationship with VesselType
        // (which vessel types can berth here)
        b.HasMany(d => d.AllowedVesselTypes)
            .WithMany(vt => vt.AllowedDocks);

        // Many-to-many relationship with StorageArea
        // (configured from StorageArea side in StorageAreaConfiguration)

        // One-to-one relationship with STSCrane
        // (configured from STSCrane side with HasForeignKey)

        // One-to-many relationship with MobileEquipment
        // (configured from MobileEquipment side)

        // Optional: Add indexes for common queries
        b.HasIndex(d => d.Name);
        b.HasIndex(d => d.Code).IsUnique();
    }
}
