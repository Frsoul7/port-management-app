using DDDNetCore.Domain.StorageAreas;
using DDDNetCore.Domain.Docks;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace DDDNetCore.Infrastructure.Configurations;

public class StorageAreaConfiguration : IEntityTypeConfiguration<StorageArea>
{
    public void Configure(EntityTypeBuilder<StorageArea> b)
    {
        // Key
        b.HasKey(sa => sa.StorageAreaId);

        b.Property(sa => sa.StorageAreaId)
            .IsRequired()
            .HasMaxLength(64);

        b.Property(sa => sa.Name)
            .IsRequired()
            .HasMaxLength(200);

        b.Property(sa => sa.Location)
            .IsRequired()
            .HasMaxLength(500);

        b.Property(sa => sa.MaxCapacityTEU)
            .IsRequired();

        b.Property(sa => sa.CurrentOccupancyTEU)
            .IsRequired();

        b.Property(sa => sa.ServesAllDocks)
            .IsRequired();

        b.Property(sa => sa.Type)
            .IsRequired()
            .HasConversion<string>(); // Store enum as string

        // Owned entities (XOR constraint enforced at domain level)
        b.OwnsOne(sa => sa.YardSpec, ys =>
        {
            ys.Property(y => y.Notes).HasMaxLength(1000);
        });

        b.OwnsOne(sa => sa.WarehouseSpec, ws =>
        {
            ws.Property(w => w.Notes).HasMaxLength(1000);
        });

        // Many-to-many relationship with Dock
        // (when ServesAllDocks = false, specific docks are linked)
        b.HasMany(sa => sa.Docks)
            .WithMany(d => d.ServedStorageAreas);

        // Optional: Add index for common queries
        b.HasIndex(sa => sa.Name);
        b.HasIndex(sa => sa.Type);
    }
}
