using DDDNetCore.Domain.PrivacyPolicy;
using DDDNetCore.Domain.Users;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace DDDNetCore.Infrastructure.Configurations
{
    /// <summary>
    /// EF Core configuration for PrivacyPolicy entity.
    /// </summary>
    public class PrivacyPolicyConfiguration : IEntityTypeConfiguration<PrivacyPolicy>
    {
        public void Configure(EntityTypeBuilder<PrivacyPolicy> builder)
        {
            builder.ToTable("PrivacyPolicies");

            // Ignore base entity Id since we use PolicyId
            builder.Ignore(p => p.Id);

            // Primary key
            builder.HasKey(p => p.PolicyId);

            // Value conversion for strongly-typed ID
            builder.Property(p => p.PolicyId)
                .HasConversion(
                    id => id.Value,
                    value => new PrivacyPolicyId(value))
                .IsRequired();

            // Version - required, indexed for queries
            builder.Property(p => p.Version)
                .IsRequired()
                .HasMaxLength(50);

            // Content - required, large text field
            builder.Property(p => p.Content)
                .IsRequired()
                .HasColumnType("nvarchar(max)");

            // Change summary - optional
            builder.Property(p => p.ChangeSummary)
                .HasMaxLength(2000);

            // Effective date - required, indexed
            builder.Property(p => p.EffectiveDate)
                .IsRequired();
            builder.HasIndex(p => p.EffectiveDate);

            // Created by - required
            builder.Property(p => p.CreatedBy)
                .IsRequired();

            // Created at - required
            builder.Property(p => p.CreatedAt)
                .IsRequired();

            // Is active - required, indexed for current policy queries
            builder.Property(p => p.IsActive)
                .IsRequired();
            builder.HasIndex(p => p.IsActive);

            // Language code - required, indexed for multi-language support
            builder.Property(p => p.LanguageCode)
                .IsRequired()
                .HasMaxLength(10)
                .HasDefaultValue("pt");
            builder.HasIndex(p => p.LanguageCode);

            // Composite index for quick current policy lookup by language
            builder.HasIndex(p => new { p.LanguageCode, p.IsActive });
        }
    }

    /// <summary>
    /// EF Core configuration for PrivacyPolicyAcknowledgment entity.
    /// </summary>
    public class PrivacyPolicyAcknowledgmentConfiguration : IEntityTypeConfiguration<PrivacyPolicyAcknowledgment>
    {
        public void Configure(EntityTypeBuilder<PrivacyPolicyAcknowledgment> builder)
        {
            builder.ToTable("PrivacyPolicyAcknowledgments");

            // Ignore base entity Id since we use AcknowledgmentId
            builder.Ignore(a => a.Id);

            // Primary key
            builder.HasKey(a => a.AcknowledgmentId);

            // Value conversion for strongly-typed ID
            builder.Property(a => a.AcknowledgmentId)
                .HasConversion(
                    id => id.Value,
                    value => new PrivacyPolicyAcknowledgmentId(value))
                .IsRequired();

            // User ID - required, indexed for user's acknowledgment history
            builder.Property(a => a.UserId)
                .HasConversion(
                    id => id.Value,
                    value => new UserId(value))
                .IsRequired();
            builder.HasIndex(a => a.UserId);

            // Policy ID - required, foreign key
            builder.Property(a => a.PolicyId)
                .HasConversion(
                    id => id.Value,
                    value => new PrivacyPolicyId(value))
                .IsRequired();

            // Policy version - required, denormalized for audit trail
            builder.Property(a => a.PolicyVersion)
                .IsRequired()
                .HasMaxLength(50);

            // Acknowledged at - required, indexed for compliance reporting
            builder.Property(a => a.AcknowledgedAt)
                .IsRequired();
            builder.HasIndex(a => a.AcknowledgedAt);

            // IP address - optional, for compliance/audit
            builder.Property(a => a.IpAddress)
                .HasMaxLength(50);

            // User agent - optional, for compliance/audit
            builder.Property(a => a.UserAgent)
                .HasMaxLength(500);

            // Relationship to PrivacyPolicy using the Policy navigation property
            builder.HasOne(a => a.Policy)
                .WithMany()
                .HasForeignKey(a => a.PolicyId)
                .OnDelete(DeleteBehavior.Restrict);

            // Unique constraint: one acknowledgment per user per policy
            builder.HasIndex(a => new { a.UserId, a.PolicyId })
                .IsUnique();
        }
    }
}
