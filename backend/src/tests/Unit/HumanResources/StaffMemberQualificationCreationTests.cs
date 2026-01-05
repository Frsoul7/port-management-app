using System;
using Xunit;
using DDDNetCore.Domain.HumanResources;
using DDDNetCore.Tests.HumanResources.Base;

namespace DDDNetCore.Tests.HumanResources
{
    /// <summary>
    /// TEST TYPE: Unit Test
    /// ENTITY UNDER TEST: StaffMemberQualification (Domain Entity - Value Object)
    /// TEST OBJECTIVE: Validate StaffMemberQualification entity creation and validation rules.
    ///                 Tests qualification ID normalization (uppercase), name/description validation,
    ///                 whitespace trimming, and required field constraints.
    /// </summary>
    public class StaffMemberQualificationCreationTests : StaffMemberTestBase
    {
        [Fact]
        public void CreateQualification_WithValidData_ShouldSucceed()
        {
            // Arrange
            var qualificationId = "FORKLIFT_OP";
            var name = "Forklift Operator";
            var description = "Licensed to operate forklift equipment";

            // Act
            var qualification = new StaffMemberQualification(qualificationId, name, description);

            // Assert
            Assert.NotNull(qualification);
            Assert.Equal("FORKLIFT_OP", qualification.QualificationId);
            Assert.Equal("Forklift Operator", qualification.Name);
            Assert.Equal("Licensed to operate forklift equipment", qualification.Description);
        }

        [Fact]
        public void CreateQualification_WithoutDescription_ShouldSucceed()
        {
            // Arrange
            var qualificationId = "CRANE_OP";
            var name = "Crane Operator";

            // Act
            var qualification = new StaffMemberQualification(qualificationId, name);

            // Assert
            Assert.NotNull(qualification);
            Assert.Equal("CRANE_OP", qualification.QualificationId);
            Assert.Equal("Crane Operator", qualification.Name);
            Assert.Null(qualification.Description);
        }

        [Fact]
        public void CreateQualification_WithLowercaseId_ShouldConvertToUppercase()
        {
            // Arrange
            var qualificationId = "forklift_op";
            var name = "Forklift Operator";

            // Act
            var qualification = new StaffMemberQualification(qualificationId, name);

            // Assert
            Assert.Equal("FORKLIFT_OP", qualification.QualificationId);
        }

        [Fact]
        public void CreateQualification_WithWhitespaceInFields_ShouldTrim()
        {
            // Arrange
            var qualificationId = "  FORKLIFT_OP  ";
            var name = "  Forklift Operator  ";
            var description = "  Licensed forklift operator  ";

            // Act
            var qualification = new StaffMemberQualification(qualificationId, name, description);

            // Assert
            Assert.Equal("FORKLIFT_OP", qualification.QualificationId);
            Assert.Equal("Forklift Operator", qualification.Name);
            Assert.Equal("Licensed forklift operator", qualification.Description);
        }

        [Theory]
        [InlineData("")]
        [InlineData("   ")]
        public void CreateQualification_WithInvalidQualificationId_ShouldThrowException(string invalidId)
        {
            // Arrange
            var name = "Forklift Operator";

            // Act & Assert
            var exception = Assert.Throws<ArgumentException>(() => 
                new StaffMemberQualification(invalidId, name));
            Assert.Equal("qualificationId", exception.ParamName);
            Assert.Contains("Qualification ID is required", exception.Message);
        }

        [Theory]
        [InlineData("")]
        [InlineData("   ")]
        public void CreateQualification_WithInvalidName_ShouldThrowException(string invalidName)
        {
            // Arrange
            var qualificationId = "FORKLIFT_OP";

            // Act & Assert
            var exception = Assert.Throws<ArgumentException>(() => 
                new StaffMemberQualification(qualificationId, invalidName));
            Assert.Equal("name", exception.ParamName);
            Assert.Contains("Qualification name is required", exception.Message);
        }

        [Fact]
        public void CreateQualification_WithNullDescription_ShouldSucceed()
        {
            // Arrange
            var qualificationId = "CRANE_OP";
            var name = "Crane Operator";
            string? description = null;

            // Act
            var qualification = new StaffMemberQualification(qualificationId, name, description);

            // Assert
            Assert.NotNull(qualification);
            Assert.Null(qualification.Description);
        }

        [Fact]
        public void CreateQualification_WithEmptyDescription_ShouldSetToEmptyString()
        {
            // Arrange
            var qualificationId = "CRANE_OP";
            var name = "Crane Operator";
            var description = "";

            // Act
            var qualification = new StaffMemberQualification(qualificationId, name, description);

            // Assert
            Assert.NotNull(qualification);
            Assert.Equal("", qualification.Description);
        }
    }
}
