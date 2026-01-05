using DDDNetCore.Domain.DockAssignments;
using DDDNetCore.Domain.Docks;
using DDDNetCore.Domain.Visits;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace DDDNetCore.Infrastructure.Configurations
{
    public class DockAssignmentConfiguration : IEntityTypeConfiguration<DockAssignment>
    {
        public void Configure(EntityTypeBuilder<DockAssignment> b)
        {
            b.ToTable("DockAssignments");

            b.HasKey(x => x.DockAssignmentId);

            b.Property(x => x.DockCode)
                .IsRequired();

            b.HasIndex(x => x.VesselVisitNotificationId);
            b.HasIndex(x => new { x.DockCode, x.BerthFrom });

            // Relationship: DockAssignment -> Dock (by DockCode)
            b.HasOne(x => x.Dock)
                .WithMany()
                .HasForeignKey("DockCode")
                .OnDelete(DeleteBehavior.Restrict);

            // Relationship: DockAssignment (1) <-> (1) VVN (FK on DockAssignment)
            b.HasOne<VesselVisitNotification>() // no nav on DA side, VVN has nullable nav
                .WithOne(vvn => vvn.DockAssignment)
                .HasForeignKey<DockAssignment>(x => x.VesselVisitNotificationId)
                .OnDelete(DeleteBehavior.Restrict);

            // History events (separate entity/table)
            b.HasMany(x => x.AssignmentHistory)
                .WithOne()
                .HasForeignKey("DockAssignmentId")
                .OnDelete(DeleteBehavior.Cascade);
        }
    }

    public class DockAssignmentEventConfiguration : IEntityTypeConfiguration<DockAssignmentEvent>
    {
        public void Configure(EntityTypeBuilder<DockAssignmentEvent> b)
        {
            b.ToTable("DockAssignmentEvents");

            b.HasKey(e => e.DockAssignmentEventId);

            b.Property(e => e.AssignedById).IsRequired();
            b.Property(e => e.ToDockCode).IsRequired();
            // FromDockCode is nullable by design (first assignment)
        }
    }
}
