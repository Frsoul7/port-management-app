using DDDNetCore.Domain.Vessels;
using DDDNetCore.Domain.Organizations;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace DDDNetCore.Infrastructure.Configurations;

public class VesselConfiguration : IEntityTypeConfiguration<Vessel>
{
    public void Configure(EntityTypeBuilder<Vessel> b)
    {
        // Key
        b.HasKey(v => v.ImoNumber);

        b.Property(v => v.ImoNumber)
         .IsRequired()
         .HasMaxLength(7); // "1234567"

        b.Property(v => v.Name)
         .IsRequired()
         .HasMaxLength(200);

        // Optional
        b.Property(v => v.CapacityTEU);

        // FK to VesselType
        b.Property(v => v.VesselTypeId)
         .IsRequired()
         .HasMaxLength(64);

        b.HasOne(v => v.VesselType)
         .WithMany(t => t.Vessels)
         .HasForeignKey(v => v.VesselTypeId)
         .OnDelete(DeleteBehavior.Restrict);

        // Value converter for OwnerOrganizationId (VO <-> Guid)
        b.Property(v => v.OwnerOrganizationId)
         .HasConversion(
             id   => id.Value,                    // to provider
             guid => new OrganizationId(guid)     // from provider
         )
         .IsRequired();

        // FK to Organization (many vessels per org)
        b.HasOne(v => v.OwnerOrganization)
         .WithMany()
         .HasForeignKey(v => v.OwnerOrganizationId)
         .HasPrincipalKey(o => o.OrganizationId)
         .OnDelete(DeleteBehavior.Restrict);

        // Optional: unique index for IMO (redundant with PK)
        b.HasIndex(v => v.ImoNumber).IsUnique();

        // Unique index for Name (InMemory ignores uniqueness; will work on relational)
        b.HasIndex(v => v.Name).IsUnique();
    }
}
