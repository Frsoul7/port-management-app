using System;
using Xunit;
using DDDNetCore.Domain.HumanResources;
using DDDNetCore.Tests.HumanResources.Base;

namespace DDDNetCore.Tests.HumanResources
{
    /// <summary>
    /// TEST TYPE: Unit Test
    /// ENTITY UNDER TEST: StaffMemberQualification (Domain Entity - Value Object)
    /// TEST OBJECTIVE: Validate StaffMemberQualification update operations and business rules.
    ///                 Tests name/description updates, whitespace handling, empty string rejection,
    ///                 and immutable ID constraint (qualification ID cannot be changed after creation).
    /// </summary>
    public class StaffMemberQualificationUpdateTests : StaffMemberTestBase
    {
        [Fact]
        public void UpdateQualification_WithValidData_ShouldSucceed()
        {
            // Arrange
            var qualification = CreateQualification("FORKLIFT_OP", "Forklift Operator", "Old description");
            var newName = "Advanced Forklift Operator";
            var newDescription = "Advanced forklift operation with safety certification";

            // Act
            qualification.Update(newName, newDescription);

            // Assert
            Assert.Equal("Advanced Forklift Operator", qualification.Name);
            Assert.Equal("Advanced forklift operation with safety certification", qualification.Description);
            Assert.Equal("FORKLIFT_OP", qualification.QualificationId); // ID should not change
        }

        [Fact]
        public void UpdateQualification_WithoutDescription_ShouldUpdateNameOnly()
        {
            // Arrange
            var qualification = CreateQualification("CRANE_OP", "Crane Operator", "Original description");
            var newName = "Senior Crane Operator";

            // Act
            qualification.Update(newName);

            // Assert
            Assert.Equal("Senior Crane Operator", qualification.Name);
            Assert.Null(qualification.Description);
        }

        [Fact]
        public void UpdateQualification_WithNullDescription_ShouldSetDescriptionToNull()
        {
            // Arrange
            var qualification = CreateQualification("CRANE_OP", "Crane Operator", "Original description");
            var newName = "Senior Crane Operator";

            // Act
            qualification.Update(newName, null);

            // Assert
            Assert.Equal("Senior Crane Operator", qualification.Name);
            Assert.Null(qualification.Description);
        }

        [Fact]
        public void UpdateQualification_WithWhitespaceInFields_ShouldTrim()
        {
            // Arrange
            var qualification = CreateQualification("FORKLIFT_OP", "Forklift Operator");
            var newName = "  Advanced Forklift Operator  ";
            var newDescription = "  Advanced operation  ";

            // Act
            qualification.Update(newName, newDescription);

            // Assert
            Assert.Equal("Advanced Forklift Operator", qualification.Name);
            Assert.Equal("Advanced operation", qualification.Description);
        }

        [Theory]
        [InlineData("")]
        [InlineData("   ")]
        public void UpdateQualification_WithInvalidName_ShouldThrowException(string invalidName)
        {
            // Arrange
            var qualification = CreateQualification("FORKLIFT_OP", "Forklift Operator");

            // Act & Assert
            var exception = Assert.Throws<ArgumentException>(() => 
                qualification.Update(invalidName));
            Assert.Equal("name", exception.ParamName);
            Assert.Contains("Qualification name is required", exception.Message);
        }

        [Fact]
        public void UpdateQualification_ShouldNotChangeQualificationId()
        {
            // Arrange
            var originalId = "FORKLIFT_OP";
            var qualification = CreateQualification(originalId, "Forklift Operator");

            // Act
            qualification.Update("New Name", "New Description");

            // Assert
            Assert.Equal(originalId, qualification.QualificationId);
        }

        [Fact]
        public void UpdateQualification_MultipleUpdates_ShouldKeepLatestValues()
        {
            // Arrange
            var qualification = CreateQualification("CRANE_OP", "Crane Operator", "Initial description");

            // Act
            qualification.Update("Updated Name 1", "Updated description 1");
            qualification.Update("Updated Name 2", "Updated description 2");
            qualification.Update("Final Name", "Final description");

            // Assert
            Assert.Equal("Final Name", qualification.Name);
            Assert.Equal("Final description", qualification.Description);
        }

        [Fact]
        public void UpdateQualification_FromDescriptionToNull_ShouldSucceed()
        {
            // Arrange
            var qualification = CreateQualification("FORKLIFT_OP", "Forklift Operator", "Some description");

            // Act
            qualification.Update("Updated Name", null);

            // Assert
            Assert.Equal("Updated Name", qualification.Name);
            Assert.Null(qualification.Description);
        }

        [Fact]
        public void UpdateQualification_FromNullToDescription_ShouldSucceed()
        {
            // Arrange
            var qualification = CreateQualification("FORKLIFT_OP", "Forklift Operator", null);

            // Act
            qualification.Update("Updated Name", "New description");

            // Assert
            Assert.Equal("Updated Name", qualification.Name);
            Assert.Equal("New description", qualification.Description);
        }
    }
}
