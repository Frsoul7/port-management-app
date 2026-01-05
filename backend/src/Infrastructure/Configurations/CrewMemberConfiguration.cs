using DDDNetCore.Domain.Visits.Crew;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace DDDNetCore.Infrastructure.Configurations
{
    public class CrewMemberConfiguration : IEntityTypeConfiguration<CrewMember>
    {
        public void Configure(EntityTypeBuilder<CrewMember> b)
        {
            b.ToTable("CrewMembers");

            b.HasKey(x => x.CrewMemberId);

            b.Property(x => x.Name).IsRequired().HasMaxLength(200);
            b.Property(x => x.CitizenId).IsRequired().HasMaxLength(100);
            b.Property(x => x.Nationality).IsRequired().HasMaxLength(50);

            b.HasIndex(x => x.VesselVisitNotificationId);

            // Relationship: CrewMember -> VVN
            b.HasOne<Domain.Visits.VesselVisitNotification>()
             .WithMany() // not exposing collection off VVN for simplicity
             .HasForeignKey(x => x.VesselVisitNotificationId)
             .OnDelete(DeleteBehavior.Cascade);
        }
    }
}
