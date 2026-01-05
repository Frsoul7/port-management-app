using DDDNetCore.Domain.Resources;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace DDDNetCore.Infrastructure.Configurations
{
    public class PhysicalResourceConfiguration : IEntityTypeConfiguration<PhysicalResource>
    {
        public void Configure(EntityTypeBuilder<PhysicalResource> b)
        {
            b.HasKey(r => r.ResourceId);

            b.Property(r => r.Code)
                .IsRequired()
                .HasMaxLength(20);

            b.Property(r => r.Description)
                .HasMaxLength(500);

            b.Property(r => r.Availability)
                .IsRequired()
                .HasConversion<string>();

            b.Property(r => r.SetupTimeSeconds)
                .IsRequired();

            b.Property(r => r.CreatedAt)
                .IsRequired();

            b.Property(r => r.DeactivationReason)
                .HasMaxLength(500);

            // TPH discriminator
            b.HasDiscriminator<string>("ResourceType")
                .HasValue<STSCrane>("STS_CRANE")
                .HasValue<MobileEquipment>("MOBILE_EQUIPMENT");

            // Many-to-many with Qualifications
            b.HasMany("RequiredQualifications")
                .WithMany()
                .UsingEntity(j => j.ToTable("PhysicalResourceQualifications"));

            b.HasIndex(r => r.Code).IsUnique();
            b.HasIndex(r => r.Availability);
        }
    }

    public class STSCraneConfiguration : IEntityTypeConfiguration<STSCrane>
    {
        public void Configure(EntityTypeBuilder<STSCrane> b)
        {
            b.Property(c => c.AvgContainersPerHour)
                .IsRequired();

            b.Property(c => c.InstalledAtDockCode)
                .HasMaxLength(20);

            b.HasOne(c => c.InstalledAt)
                .WithOne(d => d.StsCrane)
                .HasForeignKey<STSCrane>(c => c.InstalledAtDockCode)
                .OnDelete(DeleteBehavior.Restrict);
        }
    }

    public class MobileEquipmentConfiguration : IEntityTypeConfiguration<MobileEquipment>
    {
        public void Configure(EntityTypeBuilder<MobileEquipment> b)
        {
            b.Property(m => m.MobileEquipmentType)
                .IsRequired()
                .HasConversion<string>();

            b.Property(m => m.CurrentDockCode)
                .HasMaxLength(20);

            b.HasOne(m => m.CurrentAllocation)
                .WithMany(d => d.MobileEquipments)
                .HasForeignKey(m => m.CurrentDockCode)
                .OnDelete(DeleteBehavior.SetNull);
        }
    }
}
