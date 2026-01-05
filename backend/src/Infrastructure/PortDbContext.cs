using Microsoft.EntityFrameworkCore;
using DDDNetCore.Domain.Organizations;
using DDDNetCore.Domain.Users;
using DDDNetCore.Domain.Vessels;
using DDDNetCore.Domain.Visits;
using DDDNetCore.Domain.Visits.Manifests;    // 🔁 moved manifests under Visits
using DDDNetCore.Domain.DockAssignments;      // 🔁 new aggregate (replaces ScheduledVisit)
using DDDNetCore.Domain.Docks;
using DDDNetCore.Domain.Resources;
using DDDNetCore.Domain.StorageAreas;
using DDDNetCore.Domain.PrivacyPolicy;       // 🔁 GDPR - Privacy Policy entities
using DDDNetCore.Domain.DataRequests;        // 🔁 GDPR - Data Request entities (US 4.5.4)
using System;
using DDDNetCore.Domain.HumanResources;

namespace DDDNetCore.Infrastructure
{
    public class PortDbContext : DbContext
    {
        public PortDbContext(DbContextOptions<PortDbContext> options) : base(options) { }

        public DbSet<Organization> Organizations => Set<Organization>();
        public DbSet<User> Users => Set<User>();
        public DbSet<Vessel> Vessels => Set<Vessel>();
        public DbSet<VesselType> VesselTypes => Set<VesselType>();

        public DbSet<VesselVisitNotification> VesselVisitNotifications => Set<VesselVisitNotification>();

        // DockAssignment (+ events)
        public DbSet<DockAssignment> DockAssignments => Set<DockAssignment>();
        public DbSet<DockAssignmentEvent> DockAssignmentEvents => Set<DockAssignmentEvent>();

        public DbSet<CargoManifest> CargoManifests => Set<CargoManifest>();
        public DbSet<ManifestEntry> ManifestEntries => Set<ManifestEntry>();

        public DbSet<Dock> Docks => Set<Dock>();
        public DbSet<StorageArea> StorageAreas => Set<StorageArea>();
        public DbSet<MobileEquipment> MobileEquipments => Set<MobileEquipment>();
        public DbSet<StaffMember> StaffMembers => Set<StaffMember>();
        public DbSet<StaffMemberQualification> Qualifications => Set<StaffMemberQualification>();

        public DbSet<PhysicalResource> PhysicalResources => Set<PhysicalResource>();

        public DbSet<DecisionLog> DecisionLogs => Set<DecisionLog>();

        // GDPR - Privacy Policy
        public DbSet<PrivacyPolicy> PrivacyPolicies => Set<PrivacyPolicy>();
        public DbSet<PrivacyPolicyAcknowledgment> PrivacyPolicyAcknowledgments => Set<PrivacyPolicyAcknowledgment>();

        // GDPR - Data Requests (US 4.5.4)
        public DbSet<DataRequest> DataRequests => Set<DataRequest>();


        protected override void OnModelCreating(ModelBuilder modelBuilder)
        {
            base.OnModelCreating(modelBuilder);

            // Auto-discover IEntityTypeConfiguration<T> (VVN, DockAssignment, Manifests, etc.)
            modelBuilder.ApplyConfigurationsFromAssembly(typeof(PortDbContext).Assembly);

            // ===== PRIMARY (and alternate) KEYS (only for types NOT covered by separate configs) =====
            modelBuilder.Entity<Organization>().HasAlternateKey(o => o.OrganizationId);
            modelBuilder.Entity<User>().HasKey(u => u.UserId);
            modelBuilder.Entity<Vessel>().HasKey(v => v.ImoNumber);

            // StorageArea / Docks / Resources & rules
            modelBuilder.Entity<DockStorageDistance>().HasKey(d => d.DockStorageDistanceId);
            
            // StaffMember and Qualifications
            modelBuilder.Entity<StaffMember>().HasKey(sm => sm.MecanographicNumber);
            modelBuilder.Entity<StaffMember>()
                .Property(sm => sm.MecanographicNumber)
                .ValueGeneratedNever(); // Allow manual control over mecanographic number
            modelBuilder.Entity<StaffMember>()
                .Property(sm => sm.StartHour)
                .IsRequired();
            modelBuilder.Entity<StaffMember>()
                .Property(sm => sm.EndHour)
                .IsRequired();

            modelBuilder.Entity<StaffMemberQualification>().HasKey(q => q.QualificationId);
            modelBuilder.Entity<StaffMemberQualification>()
                .Property(q => q.Name)
                .IsRequired()
                .HasMaxLength(200);
            
            // ===== VALUE CONVERSIONS (keep only those not already in per-entity configs) =====
            modelBuilder.Entity<Organization>()
                .Property(o => o.OrganizationId)
                .HasConversion(v => v.Value, v => new OrganizationId(v));

            modelBuilder.Entity<User>()
                .Property(u => u.UserId)
                .HasConversion(v => v.Value, v => new UserId(v));

            modelBuilder.Entity<User>()
                .Property(u => u.OrganizationId)
                .HasConversion(v => v.Value, v => new OrganizationId(v));

            modelBuilder.Entity<User>()
                .Property(u => u.Role)
                .HasConversion<string>();

            // Remove VVN OrganizationId conversion - now handled in VesselVisitNotificationConfiguration
            // modelBuilder.Entity<VesselVisitNotification>()
            //     .Property(v => v.OrganizationId)
            //     .HasConversion(v => v.Value, v => new OrganizationId(v));

            modelBuilder.Entity<VesselVisitNotification>()
                .Property(v => v.SubmittedById)
                .HasConversion(v => v == null ? Guid.Empty : v.Value, v => v == Guid.Empty ? null : new UserId(v));

            modelBuilder.Entity<VesselVisitNotification>()
                .Property(v => v.ApprovedById)
                .HasConversion(v => v == null ? Guid.Empty : v.Value, v => v == Guid.Empty ? null : new UserId(v));

            modelBuilder.Entity<VesselVisitNotification>()
                .Property(v => v.RejectedById)
                .HasConversion(v => v == null ? Guid.Empty : v.Value, v => v == Guid.Empty ? null : new UserId(v));


            // ===== RELATIONSHIPS (only those not already in per-entity configs) =====

            // Organization - User
            modelBuilder.Entity<User>()
                .HasOne(u => u.Organization)
                .WithMany()
                .HasForeignKey(u => u.OrganizationId)
                .HasPrincipalKey(o => o.OrganizationId)
                .OnDelete(DeleteBehavior.Restrict);

            // Remove VVN - Organization relationship - now handled in VesselVisitNotificationConfiguration
            // modelBuilder.Entity<VesselVisitNotification>()
            //     .HasOne(v => v.Organization)
            //     .WithMany()
            //     .HasForeignKey(v => v.OrganizationId)
            //     .HasPrincipalKey(o => o.OrganizationId)
            //     .OnDelete(DeleteBehavior.Restrict);

            // STSCrane - Dock (one-to-one)
            modelBuilder.Entity<STSCrane>()
                .HasOne(s => s.InstalledAt)
                .WithOne(d => d.StsCrane)
                .HasForeignKey<STSCrane>(s => s.InstalledAtDockCode)
                .OnDelete(DeleteBehavior.Restrict);

            // MobileEquipment - Dock (current allocation)
            modelBuilder.Entity<MobileEquipment>()
                .HasOne(me => me.CurrentAllocation)
                .WithMany(d => d.MobileEquipments)
                .HasForeignKey("CurrentDockCode")
                .OnDelete(DeleteBehavior.SetNull);

            // DockStorageDistance - Dock
            modelBuilder.Entity<DockStorageDistance>()
                .HasOne(dsd => dsd.Dock)
                .WithMany()
                .HasForeignKey(dsd => dsd.DockCode)
                .OnDelete(DeleteBehavior.Cascade);

            // DockStorageDistance - StorageArea
            modelBuilder.Entity<DockStorageDistance>()
                .HasOne(dsd => dsd.StorageArea)
                .WithMany()
                .HasForeignKey(dsd => dsd.StorageAreaId)
                .OnDelete(DeleteBehavior.Cascade);

            // NOTE:
            // - VVN user/vessel relationships, userId conversions on VVN,
            // - VVN-Organization relationship (now in VesselVisitNotificationConfiguration)
            // - DockAssignment mappings (incl. history and VVN link),
            // - CargoManifest TPH and ManifestEntry mapping,
            // are all configured in the dedicated configuration classes.

        }
    }
}
