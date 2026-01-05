using DDDNetCore.Domain.Visits;
using DDDNetCore.Domain.Users;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace DDDNetCore.Infrastructure.Configurations.Visits
{
    public class DecisionLogConfiguration : IEntityTypeConfiguration<DecisionLog>
    {
        public void Configure(EntityTypeBuilder<DecisionLog> b)
        {
            b.ToTable("DecisionLogs");
            b.HasKey(x => x.DecisionLogId);

            b.Property(x => x.VvnGuid).IsRequired();
            b.Property(x => x.AtUtc).IsRequired();

            // map UserId VO <-> Guid
            b.Property(x => x.OfficerUserId)
             .HasConversion(
                 toProvider => toProvider.Value,     // UserId -> Guid
                 fromProvider => new UserId(fromProvider)  // Guid -> UserId
             )
             .IsRequired();

            // optional: quick index for audit queries
            b.HasIndex(x => new { x.VvnGuid, x.AtUtc });
        }
    }
}
