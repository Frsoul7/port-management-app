using System;
using System.Linq;
using DDDNetCore.Domain.Common;
using DDDNetCore.Domain.HumanResources;
using Xunit;

namespace DDDNetCore.Tests.Unit.HumanResources
{
    /// <summary>
    /// TEST TYPE: Unit Test
    /// ENTITY UNDER TEST: StaffMember (Domain Entity)
    /// TEST OBJECTIVE: Validate StaffMember entity creation, validation rules, and business logic in isolation.
    ///                 Tests constructor validation, contact info updates, working hours management,
    ///                 qualification management, and status transitions.
    /// </summary>
    public class StaffMemberTests
    {
        private const long VALID_MECANOGRAPHIC_NUMBER = 123456;
        private const string VALID_SHORT_NAME = "John Doe";
        private const string VALID_EMAIL = "john.doe@example.com";
        private const string VALID_PHONE = "+351912345678";
        private readonly TimeSpan VALID_START_HOUR = new TimeSpan(9, 0, 0);  // 09:00
        private readonly TimeSpan VALID_END_HOUR = new TimeSpan(17, 0, 0);   // 17:00

        #region Constructor Valid Cases

        [Fact]
        public void Constructor_WithValidData_ShouldCreateStaffMember()
        {
            // Arrange & Act
            var staffMember = new StaffMember(
                VALID_MECANOGRAPHIC_NUMBER,
                VALID_SHORT_NAME,
                VALID_EMAIL,
                VALID_PHONE,
                HumanResourceStatus.AVAILABLE,
                VALID_START_HOUR,
                VALID_END_HOUR
            );

            // Assert
            Assert.Equal(VALID_MECANOGRAPHIC_NUMBER, staffMember.MecanographicNumber);
            Assert.Equal(VALID_SHORT_NAME, staffMember.ShortName);
            Assert.Equal(VALID_EMAIL, staffMember.Email);
            Assert.Equal(VALID_PHONE, staffMember.Phone);
            Assert.Equal(HumanResourceStatus.AVAILABLE, staffMember.Status);
            Assert.Equal(VALID_START_HOUR, staffMember.StartHour);
            Assert.Equal(VALID_END_HOUR, staffMember.EndHour);
            Assert.Equal(EntityActiveStatus.ACTIVE, staffMember.ActivityStatus); // Default value
            Assert.NotNull(staffMember.Qualifications);
            Assert.Empty(staffMember.Qualifications);
        }

        [Fact]
        public void Constructor_WithCustomActivityStatus_ShouldCreateStaffMember()
        {
            // Arrange & Act
            var staffMember = new StaffMember(
                VALID_MECANOGRAPHIC_NUMBER,
                VALID_SHORT_NAME,
                VALID_EMAIL,
                VALID_PHONE,
                HumanResourceStatus.UNAVAILABLE,
                VALID_START_HOUR,
                VALID_END_HOUR,
                EntityActiveStatus.INACTIVE
            );

            // Assert
            Assert.Equal(EntityActiveStatus.INACTIVE, staffMember.ActivityStatus);
            Assert.Equal(HumanResourceStatus.UNAVAILABLE, staffMember.Status);
        }

        [Fact]
        public void Constructor_WithWhitespaceInFields_ShouldTrimValues()
        {
            // Arrange
            var nameWithSpaces = "  Jane Smith  ";
            var emailWithSpaces = "  jane@example.com  ";
            var phoneWithSpaces = "  +351987654321  ";

            // Act
            var staffMember = new StaffMember(
                VALID_MECANOGRAPHIC_NUMBER,
                nameWithSpaces,
                emailWithSpaces,
                phoneWithSpaces,
                HumanResourceStatus.AVAILABLE,
                VALID_START_HOUR,
                VALID_END_HOUR
            );

            // Assert
            Assert.Equal("Jane Smith", staffMember.ShortName);
            Assert.Equal("jane@example.com", staffMember.Email);
            Assert.Equal("+351987654321", staffMember.Phone);
        }

        [Fact]
        public void Constructor_WithAllHumanResourceStatuses_ShouldAcceptValidStatuses()
        {
            // Arrange & Act
            var available = new StaffMember(1, VALID_SHORT_NAME, VALID_EMAIL, VALID_PHONE,
                HumanResourceStatus.AVAILABLE, VALID_START_HOUR, VALID_END_HOUR);
            var unavailable = new StaffMember(2, VALID_SHORT_NAME, VALID_EMAIL, VALID_PHONE,
                HumanResourceStatus.UNAVAILABLE, VALID_START_HOUR, VALID_END_HOUR);
            var reassigned = new StaffMember(3, VALID_SHORT_NAME, VALID_EMAIL, VALID_PHONE,
                HumanResourceStatus.TEMPORARILY_REASSIGNED, VALID_START_HOUR, VALID_END_HOUR);

            // Assert
            Assert.Equal(HumanResourceStatus.AVAILABLE, available.Status);
            Assert.Equal(HumanResourceStatus.UNAVAILABLE, unavailable.Status);
            Assert.Equal(HumanResourceStatus.TEMPORARILY_REASSIGNED, reassigned.Status);
        }

        #endregion

        #region Constructor Invalid Cases

        [Theory]
        [InlineData(0)]
        [InlineData(-1)]
        [InlineData(-100)]
        public void Constructor_WithInvalidMecanographicNumber_ShouldThrowArgumentException(long invalidNumber)
        {
            // Arrange & Act & Assert
            var exception = Assert.Throws<ArgumentException>(() =>
                new StaffMember(invalidNumber, VALID_SHORT_NAME, VALID_EMAIL, VALID_PHONE,
                    HumanResourceStatus.AVAILABLE, VALID_START_HOUR, VALID_END_HOUR)
            );

            Assert.Contains("Mecanographic number must be positive", exception.Message);
        }

        [Theory]
        [InlineData(null)]
        [InlineData("")]
        [InlineData("   ")]
        public void Constructor_WithInvalidShortName_ShouldThrowArgumentException(string? invalidName)
        {
            // Arrange & Act & Assert
            var exception = Assert.Throws<ArgumentException>(() =>
                new StaffMember(VALID_MECANOGRAPHIC_NUMBER, invalidName, VALID_EMAIL, VALID_PHONE,
                    HumanResourceStatus.AVAILABLE, VALID_START_HOUR, VALID_END_HOUR)
            );

            Assert.Contains("Short name is required", exception.Message);
        }

        [Theory]
        [InlineData(null)]
        [InlineData("")]
        [InlineData("   ")]
        public void Constructor_WithInvalidEmail_ShouldThrowArgumentException(string? invalidEmail)
        {
            // Arrange & Act & Assert
            var exception = Assert.Throws<ArgumentException>(() =>
                new StaffMember(VALID_MECANOGRAPHIC_NUMBER, VALID_SHORT_NAME, invalidEmail, VALID_PHONE,
                    HumanResourceStatus.AVAILABLE, VALID_START_HOUR, VALID_END_HOUR)
            );

            Assert.Contains("Email is required", exception.Message);
        }

        [Theory]
        [InlineData(null)]
        [InlineData("")]
        [InlineData("   ")]
        public void Constructor_WithInvalidPhone_ShouldThrowArgumentException(string? invalidPhone)
        {
            // Arrange & Act & Assert
            var exception = Assert.Throws<ArgumentException>(() =>
                new StaffMember(VALID_MECANOGRAPHIC_NUMBER, VALID_SHORT_NAME, VALID_EMAIL, invalidPhone,
                    HumanResourceStatus.AVAILABLE, VALID_START_HOUR, VALID_END_HOUR)
            );

            Assert.Contains("Phone is required", exception.Message);
        }

        [Fact]
        public void Constructor_WithStartHourEqualToEndHour_ShouldThrowArgumentException()
        {
            // Arrange
            var sameTime = new TimeSpan(9, 0, 0);

            // Act & Assert
            var exception = Assert.Throws<ArgumentException>(() =>
                new StaffMember(VALID_MECANOGRAPHIC_NUMBER, VALID_SHORT_NAME, VALID_EMAIL, VALID_PHONE,
                    HumanResourceStatus.AVAILABLE, sameTime, sameTime)
            );

            Assert.Contains("Start hour must be earlier than end hour", exception.Message);
        }

        [Fact]
        public void Constructor_WithStartHourAfterEndHour_ShouldThrowArgumentException()
        {
            // Arrange
            var startHour = new TimeSpan(17, 0, 0);
            var endHour = new TimeSpan(9, 0, 0);

            // Act & Assert
            var exception = Assert.Throws<ArgumentException>(() =>
                new StaffMember(VALID_MECANOGRAPHIC_NUMBER, VALID_SHORT_NAME, VALID_EMAIL, VALID_PHONE,
                    HumanResourceStatus.AVAILABLE, startHour, endHour)
            );

            Assert.Contains("Start hour must be earlier than end hour", exception.Message);
        }

        #endregion

        #region Qualification Management

        [Fact]
        public void AddQualification_WithValidQualification_ShouldAddToCollection()
        {
            // Arrange
            var staffMember = CreateValidStaffMember();
            var qualification = new StaffMemberQualification("Q001", "Crane Operator");

            // Act
            staffMember.AddQualification(qualification);

            // Assert
            Assert.Single(staffMember.Qualifications);
            Assert.Contains(qualification, staffMember.Qualifications);
        }

        [Fact]
        public void AddQualification_WithMultipleQualifications_ShouldAddAll()
        {
            // Arrange
            var staffMember = CreateValidStaffMember();
            var qual1 = new StaffMemberQualification("Q001", "Crane Operator");
            var qual2 = new StaffMemberQualification("Q002", "Forklift Driver");
            var qual3 = new StaffMemberQualification("Q003", "Safety Inspector");

            // Act
            staffMember.AddQualification(qual1);
            staffMember.AddQualification(qual2);
            staffMember.AddQualification(qual3);

            // Assert
            Assert.Equal(3, staffMember.Qualifications.Count);
            Assert.Contains(qual1, staffMember.Qualifications);
            Assert.Contains(qual2, staffMember.Qualifications);
            Assert.Contains(qual3, staffMember.Qualifications);
        }

        [Fact]
        public void AddQualification_WithNullQualification_ShouldThrowArgumentNullException()
        {
            // Arrange
            var staffMember = CreateValidStaffMember();

            // Act & Assert
            Assert.Throws<ArgumentNullException>(() => staffMember.AddQualification(null!));
        }

        [Fact]
        public void AddQualification_WithDuplicateQualification_ShouldThrowInvalidOperationException()
        {
            // Arrange
            var staffMember = CreateValidStaffMember();
            var qualification = new StaffMemberQualification("Q001", "Crane Operator");
            staffMember.AddQualification(qualification);

            // Act & Assert
            var exception = Assert.Throws<InvalidOperationException>(() =>
                staffMember.AddQualification(qualification)
            );

            Assert.Contains("Qualification already assigned", exception.Message);
        }

        [Fact]
        public void RemoveQualification_WithExistingQualification_ShouldRemoveFromCollection()
        {
            // Arrange
            var staffMember = CreateValidStaffMember();
            var qualification = new StaffMemberQualification("Q001", "Crane Operator");
            staffMember.AddQualification(qualification);

            // Act
            staffMember.RemoveQualification(qualification);

            // Assert
            Assert.Empty(staffMember.Qualifications);
            Assert.DoesNotContain(qualification, staffMember.Qualifications);
        }

        [Fact]
        public void RemoveQualification_WithNullQualification_ShouldThrowArgumentNullException()
        {
            // Arrange
            var staffMember = CreateValidStaffMember();

            // Act & Assert
            Assert.Throws<ArgumentNullException>(() => staffMember.RemoveQualification(null!));
        }

        [Fact]
        public void RemoveQualification_WithNonExistentQualification_ShouldThrowInvalidOperationException()
        {
            // Arrange
            var staffMember = CreateValidStaffMember();
            var qualification = new StaffMemberQualification("Q001", "Crane Operator");

            // Act & Assert
            var exception = Assert.Throws<InvalidOperationException>(() =>
                staffMember.RemoveQualification(qualification)
            );

            Assert.Contains("Qualification not found", exception.Message);
        }

        #endregion

        #region Contact Info Updates

        [Fact]
        public void UpdateContactInfo_WithValidData_ShouldUpdateEmailAndPhone()
        {
            // Arrange
            var staffMember = CreateValidStaffMember();
            var newEmail = "newemail@example.com";
            var newPhone = "+351999888777";

            // Act
            staffMember.UpdateContactInfo(newEmail, newPhone);

            // Assert
            Assert.Equal(newEmail, staffMember.Email);
            Assert.Equal(newPhone, staffMember.Phone);
        }

        [Fact]
        public void UpdateContactInfo_WithWhitespace_ShouldTrimValues()
        {
            // Arrange
            var staffMember = CreateValidStaffMember();
            var newEmail = "  updated@example.com  ";
            var newPhone = "  +351111222333  ";

            // Act
            staffMember.UpdateContactInfo(newEmail, newPhone);

            // Assert
            Assert.Equal("updated@example.com", staffMember.Email);
            Assert.Equal("+351111222333", staffMember.Phone);
        }

        [Theory]
        [InlineData(null, "+351912345678")]
        [InlineData("", "+351912345678")]
        [InlineData("   ", "+351912345678")]
        public void UpdateContactInfo_WithInvalidEmail_ShouldThrowArgumentException(string? invalidEmail, string validPhone)
        {
            // Arrange
            var staffMember = CreateValidStaffMember();

            // Act & Assert
            var exception = Assert.Throws<ArgumentException>(() =>
                staffMember.UpdateContactInfo(invalidEmail, validPhone)
            );

            Assert.Contains("Email is required", exception.Message);
        }

        [Theory]
        [InlineData("valid@example.com", null)]
        [InlineData("valid@example.com", "")]
        [InlineData("valid@example.com", "   ")]
        public void UpdateContactInfo_WithInvalidPhone_ShouldThrowArgumentException(string validEmail, string? invalidPhone)
        {
            // Arrange
            var staffMember = CreateValidStaffMember();

            // Act & Assert
            var exception = Assert.Throws<ArgumentException>(() =>
                staffMember.UpdateContactInfo(validEmail, invalidPhone)
            );

            Assert.Contains("Phone is required", exception.Message);
        }

        #endregion

        #region Status Updates

        [Fact]
        public void UpdateOperationalStatus_WithValidStatus_ShouldUpdateStatus()
        {
            // Arrange
            var staffMember = CreateValidStaffMember();
            Assert.Equal(HumanResourceStatus.AVAILABLE, staffMember.Status);

            // Act
            staffMember.UpdateOperationalStatus(HumanResourceStatus.TEMPORARILY_REASSIGNED);

            // Assert
            Assert.Equal(HumanResourceStatus.TEMPORARILY_REASSIGNED, staffMember.Status);
        }

        [Fact]
        public void UpdateActiveStatus_WithValidStatus_ShouldUpdateActivityStatus()
        {
            // Arrange
            var staffMember = CreateValidStaffMember();
            Assert.Equal(EntityActiveStatus.ACTIVE, staffMember.ActivityStatus);

            // Act
            staffMember.UpdateActiveStatus(EntityActiveStatus.INACTIVE);

            // Assert
            Assert.Equal(EntityActiveStatus.INACTIVE, staffMember.ActivityStatus);
        }

        #endregion

        #region Working Hours Management

        [Fact]
        public void UpdateWorkingHours_WithValidHours_ShouldUpdateStartAndEndHours()
        {
            // Arrange
            var staffMember = CreateValidStaffMember();
            var newStartHour = new TimeSpan(8, 0, 0);   // 08:00
            var newEndHour = new TimeSpan(16, 0, 0);    // 16:00

            // Act
            staffMember.UpdateWorkingHours(newStartHour, newEndHour);

            // Assert
            Assert.Equal(newStartHour, staffMember.StartHour);
            Assert.Equal(newEndHour, staffMember.EndHour);
        }

        [Fact]
        public void UpdateWorkingHours_WithStartHourEqualToEndHour_ShouldThrowArgumentException()
        {
            // Arrange
            var staffMember = CreateValidStaffMember();
            var sameTime = new TimeSpan(10, 0, 0);

            // Act & Assert
            var exception = Assert.Throws<ArgumentException>(() =>
                staffMember.UpdateWorkingHours(sameTime, sameTime)
            );

            Assert.Contains("Start hour must be earlier than end hour", exception.Message);
        }

        [Fact]
        public void UpdateWorkingHours_WithStartHourAfterEndHour_ShouldThrowArgumentException()
        {
            // Arrange
            var staffMember = CreateValidStaffMember();
            var startHour = new TimeSpan(18, 0, 0);
            var endHour = new TimeSpan(10, 0, 0);

            // Act & Assert
            var exception = Assert.Throws<ArgumentException>(() =>
                staffMember.UpdateWorkingHours(startHour, endHour)
            );

            Assert.Contains("Start hour must be earlier than end hour", exception.Message);
        }

        [Fact]
        public void IsWithinWorkingHours_WithTimeWithinRange_ShouldReturnTrue()
        {
            // Arrange
            var staffMember = CreateValidStaffMember(); // 09:00 - 17:00
            var withinHours = new DateTime(2025, 11, 12, 12, 30, 0); // 12:30

            // Act
            var result = staffMember.IsWithinWorkingHours(withinHours);

            // Assert
            Assert.True(result);
        }

        [Fact]
        public void IsWithinWorkingHours_WithTimeBeforeStart_ShouldReturnFalse()
        {
            // Arrange
            var staffMember = CreateValidStaffMember(); // 09:00 - 17:00
            var beforeHours = new DateTime(2025, 11, 12, 8, 30, 0); // 08:30

            // Act
            var result = staffMember.IsWithinWorkingHours(beforeHours);

            // Assert
            Assert.False(result);
        }

        [Fact]
        public void IsWithinWorkingHours_WithTimeAfterEnd_ShouldReturnFalse()
        {
            // Arrange
            var staffMember = CreateValidStaffMember(); // 09:00 - 17:00
            var afterHours = new DateTime(2025, 11, 12, 18, 0, 0); // 18:00

            // Act
            var result = staffMember.IsWithinWorkingHours(afterHours);

            // Assert
            Assert.False(result);
        }

        [Fact]
        public void IsWithinWorkingHours_WithTimeAtStartBoundary_ShouldReturnTrue()
        {
            // Arrange
            var staffMember = CreateValidStaffMember(); // 09:00 - 17:00
            var atStart = new DateTime(2025, 11, 12, 9, 0, 0); // 09:00

            // Act
            var result = staffMember.IsWithinWorkingHours(atStart);

            // Assert
            Assert.True(result);
        }

        [Fact]
        public void IsWithinWorkingHours_WithTimeAtEndBoundary_ShouldReturnTrue()
        {
            // Arrange
            var staffMember = CreateValidStaffMember(); // 09:00 - 17:00
            var atEnd = new DateTime(2025, 11, 12, 17, 0, 0); // 17:00

            // Act
            var result = staffMember.IsWithinWorkingHours(atEnd);

            // Assert
            Assert.True(result);
        }

        #endregion

        #region Helper Methods

        private StaffMember CreateValidStaffMember()
        {
            return new StaffMember(
                VALID_MECANOGRAPHIC_NUMBER,
                VALID_SHORT_NAME,
                VALID_EMAIL,
                VALID_PHONE,
                HumanResourceStatus.AVAILABLE,
                VALID_START_HOUR,
                VALID_END_HOUR
            );
        }

        #endregion
    }
}
