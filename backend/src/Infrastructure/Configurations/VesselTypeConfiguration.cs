using DDDNetCore.Domain.Vessels;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace DDDNetCore.Infrastructure.Configurations;

public class VesselTypeConfiguration : IEntityTypeConfiguration<VesselType>
{
    public void Configure(EntityTypeBuilder<VesselType> b)
    {
        // Key + columns
        b.HasKey(x => x.VesselTypeId);
        b.Property(x => x.VesselTypeId).HasMaxLength(64);
        b.Property(x => x.Name).IsRequired().HasMaxLength(200);
        b.Property(x => x.Description).HasMaxLength(2000);
        b.Property(x => x.CapacityTEU);
        b.Property(x => x.MaxRows);
        b.Property(x => x.MaxBays);
        b.Property(x => x.MaxTiers);
        b.Property(x => x.OperationalConstraints).HasMaxLength(2000);

        // Relationships
        b.HasMany(x => x.Vessels)
         .WithOne(v => v.VesselType!)
         .HasForeignKey(v => v.VesselTypeId)
         .OnDelete(DeleteBehavior.Restrict);

        // Many-to-many with Dock (the reverse nav lives on Dock)
        b.HasMany(x => x.AllowedDocks)
         .WithMany(d => d.AllowedVesselTypes);
    }
}
