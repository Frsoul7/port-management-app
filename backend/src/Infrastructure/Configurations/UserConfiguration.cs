using DDDNetCore.Domain.Users;
using DDDNetCore.Domain.Organizations;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace DDDNetCore.Infrastructure.Configurations
{
    public class UserConfiguration : IEntityTypeConfiguration<User>
    {
        public void Configure(EntityTypeBuilder<User> builder)
        {
            builder.ToTable("Users");

            builder.HasKey(u => u.Id);

            builder.Property(u => u.Name)
                .IsRequired()
                .HasMaxLength(200);

            builder.Property(u => u.Email)
                .IsRequired()
                .HasMaxLength(255);

            builder.HasIndex(u => u.Email)
                .IsUnique();

            builder.Property(u => u.ProfilePictureUrl)
                .HasMaxLength(500);

            builder.Property(u => u.PasswordHash)
                .HasMaxLength(255);

            builder.Property(u => u.Role)
                .HasConversion<string>()
                .IsRequired(false); // Role is nullable until assigned by admin

            builder.Property(u => u.IsActive)
                .IsRequired()
                .HasDefaultValue(false);

            builder.Property(u => u.EmailVerified)
                .IsRequired()
                .HasDefaultValue(false);

            builder.Property(u => u.ActivationToken)
                .HasMaxLength(255);

            builder.Property(u => u.ActivationTokenExpiry);

            builder.Property(u => u.CreatedAt)
                .IsRequired();

            // Note: UserId and OrganizationId conversions are handled in PortDbContext
            // Navigation to Organization is also handled in PortDbContext
        }
    }
}
