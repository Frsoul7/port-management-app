using DDDNetCore.Domain.Organizations;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace DDDNetCore.Infrastructure.Configurations
{
    public class OrganizationConfiguration : IEntityTypeConfiguration<Organization>
    {
        public void Configure(EntityTypeBuilder<Organization> b)
        {
            // Primary key is Id (Guid)
            b.HasKey(o => o.Id);

            b.HasAlternateKey(o => o.OrganizationId);
            
            b.Property(v => v.OrganizationId)
                .HasConversion(v => v.Value, v => new OrganizationId(v));

            b.Property(o => o.Identifier)
                .IsRequired()
                .HasMaxLength(10);
            b.HasIndex(o => o.Identifier).IsUnique();

            b.Property(o => o.LegalName)
                .IsRequired()
                .HasMaxLength(200);

            b.Property(o => o.AlternativeName)
                .HasMaxLength(200);

            b.Property(o => o.AddressLine)
                .IsRequired()
                .HasMaxLength(500);

            b.Property(o => o.TaxNumber)
                .IsRequired()
                .HasMaxLength(64);

            b.Property(o => o.Type)
                .HasConversion<string>()   // store as "SHIPPING_AGENT", etc.
                .IsRequired();

            // -----------------------------
            // Representatives (owned collection)
            // -----------------------------
            b.OwnsMany(o => o.Representatives, rb =>
            {
                // FK back to Organization (shadow)
                rb.WithOwner().HasForeignKey("OrganizationId");

                // EF PK for the owned entity
                rb.HasKey(r => r.Id);

                // We generate Guid Id in the domain ctor, so tell EF not to generate it
                rb.Property(r => r.Id)
                  .ValueGeneratedNever();

                // Map the DDD VO RepresentativeId as a scalar with conversion
                rb.Property(r => r.RepresentativeId)
                  .HasConversion(
                      v => v.Value,                // to provider (Guid)
                      v => new RepresentativeId(v) // from provider
                  )
                  .IsRequired();

                rb.Property(r => r.Name).IsRequired().HasMaxLength(150);
                rb.Property(r => r.CitizenId).IsRequired().HasMaxLength(64);
                rb.Property(r => r.Nationality).IsRequired().HasMaxLength(2);
                rb.Property(r => r.Email).IsRequired().HasMaxLength(254);
                rb.Property(r => r.Phone).HasMaxLength(64);
                rb.Property(r => r.IsActive).IsRequired();
                rb.Property(r => r.CreatedAt).IsRequired();

                // Helpful for lookups; not unique at DB level for now (controller enforces uniqueness)
                rb.HasIndex(r => r.Email);
                // If later you want DB-enforced uniqueness, switch to: .IsUnique();
                // rb.HasIndex(r => r.Phone);
            });

            // Backing field mapping for the collection
            b.Navigation(o => o.Representatives)
             .HasField("_representatives")
             .UsePropertyAccessMode(PropertyAccessMode.Field);
        }
    }
}
