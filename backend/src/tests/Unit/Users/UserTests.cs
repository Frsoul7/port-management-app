using System;
using Xunit;
using DDDNetCore.Domain.Users;
using DDDNetCore.Domain.Organizations;

namespace DDDNetCore.Tests.Unit.Users
{
    /************************************************************************************************
     * TEST TYPE: Unit Tests
     * ENTITY UNDER TEST: User (Aggregate Root)
     * TEST OBJECTIVE: Verify user creation, identity management, and organization association
     ************************************************************************************************/

    public class UserTests
    {
        #region Constructor - Valid Cases

        [Fact]
        public void Constructor_WithValidData_CreatesUser()
        {
            // Arrange
            var id = Guid.NewGuid();
            var name = "John Doe";
            var email = "john.doe@example.com";
            var orgId = new OrganizationId(Guid.NewGuid());

            // Act
            var user = new User(
                id: id,
                name: name,
                email: email,
                organizationId: orgId,
                role: UserRole.SHIPPING_AGENT_REPRESENTATIVE,
                profilePictureUrl: null,
                passwordHash: null
            );

            // Assert
            Assert.Equal(id, user.Id);
            Assert.Equal(id, user.UserId.Value);
            Assert.Equal(name, user.Name);
            Assert.Equal(email, user.Email);
            Assert.Equal(orgId, user.OrganizationId);
        }

        [Fact]
        public void Constructor_WithValidData_CreatesUserIdValueObject()
        {
            // Arrange
            var id = Guid.NewGuid();
            var name = "Jane Smith";
            var email = "jane.smith@example.com";
            var orgId = new OrganizationId(Guid.NewGuid());

            // Act
            var user = new User(
                id: id,
                name: name,
                email: email,
                organizationId: orgId,
                role: UserRole.SHIPPING_AGENT_REPRESENTATIVE,
                profilePictureUrl: null,
                passwordHash: null
            );

            // Assert
            Assert.NotNull(user.UserId);
            Assert.Equal(id, user.UserId.Value);
        }

        [Fact]
        public void Constructor_WithDifferentEmails_CreatesDistinctUsers()
        {
            // Arrange
            var id1 = Guid.NewGuid();
            var id2 = Guid.NewGuid();
            var orgId = new OrganizationId(Guid.NewGuid());
            var email1 = "user1@example.com";
            var email2 = "user2@example.com";

            // Act
            var user1 = new User(
                id: id1,
                name: "User One",
                email: email1,
                organizationId: orgId,
                role: UserRole.SHIPPING_AGENT_REPRESENTATIVE,
                profilePictureUrl: null,
                passwordHash: null
            );
            var user2 = new User(
                id: id2,
                name: "User Two",
                email: email2,
                organizationId: orgId,
                role: UserRole.PORT_AUTHORITY_OFFICER,
                profilePictureUrl: null,
                passwordHash: null
            );

            // Assert
            Assert.NotEqual(user1.Id, user2.Id);
            Assert.NotEqual(user1.Email, user2.Email);
            Assert.Equal(user1.OrganizationId, user2.OrganizationId);
        }

        [Fact]
        public void Constructor_WithLongName_AcceptsName()
        {
            // Arrange
            var id = Guid.NewGuid();
            var longName = "Dr. Alexander Christopher Montgomery-Wellington III";
            var email = "alexander@example.com";
            var orgId = new OrganizationId(Guid.NewGuid());

            // Act
            var user = new User(
                id: id,
                name: longName,
                email: email,
                organizationId: orgId,
                role: UserRole.SHIPPING_AGENT_REPRESENTATIVE,
                profilePictureUrl: null,
                passwordHash: null
            );

            // Assert
            Assert.Equal(longName, user.Name);
        }

        [Fact]
        public void Constructor_WithComplexEmail_AcceptsEmail()
        {
            // Arrange
            var id = Guid.NewGuid();
            var name = "Test User";
            var complexEmail = "test.user+tag@subdomain.example.co.uk";
            var orgId = new OrganizationId(Guid.NewGuid());

            // Act
            var user = new User(
                id: id,
                name: name,
                email: complexEmail,
                organizationId: orgId,
                role: UserRole.SHIPPING_AGENT_REPRESENTATIVE,
                profilePictureUrl: null,
                passwordHash: null
            );

            // Assert
            Assert.Equal(complexEmail, user.Email);
        }

        #endregion

        #region UserId Value Object Tests

        [Fact]
        public void UserId_ToString_ReturnsGuidString()
        {
            // Arrange
            var id = Guid.NewGuid();
            var user = CreateValidUser(id);

            // Act
            var userIdString = user.UserId.ToString();

            // Assert
            Assert.Equal(id.ToString(), userIdString);
        }

        [Fact]
        public void UserId_WithSameGuid_AreEqual()
        {
            // Arrange
            var id = Guid.NewGuid();
            var userId1 = new UserId(id);
            var userId2 = new UserId(id);

            // Act & Assert
            Assert.Equal(userId1, userId2);
        }

        [Fact]
        public void UserId_WithDifferentGuid_AreNotEqual()
        {
            // Arrange
            var userId1 = new UserId(Guid.NewGuid());
            var userId2 = new UserId(Guid.NewGuid());

            // Act & Assert
            Assert.NotEqual(userId1, userId2);
        }

        [Fact]
        public void UserId_NewId_CreatesUniqueId()
        {
            // Act
            var userId1 = UserId.NewId();
            var userId2 = UserId.NewId();

            // Assert
            Assert.NotEqual(userId1, userId2);
            Assert.NotEqual(Guid.Empty, userId1.Value);
            Assert.NotEqual(Guid.Empty, userId2.Value);
        }

        #endregion

        #region Organization Association

        [Fact]
        public void Constructor_SetsOrganizationId()
        {
            // Arrange
            var id = Guid.NewGuid();
            var orgId = new OrganizationId(Guid.NewGuid());

            // Act
            var user = new User(
                id: id,
                name: "Maritime Officer",
                email: "officer@example.com",
                organizationId: orgId,
                role: UserRole.SHIPPING_AGENT_REPRESENTATIVE,
                profilePictureUrl: null,
                passwordHash: null
            );

            // Assert
            Assert.Equal(orgId, user.OrganizationId);
        }

        [Fact]
        public void Constructor_OrganizationNavigationProperty_InitiallyNull()
        {
            // Arrange
            var id = Guid.NewGuid();
            var orgId = new OrganizationId(Guid.NewGuid());

            // Act
            var user = new User(
                id: id,
                name: "Port Manager",
                email: "manager@example.com",
                organizationId: orgId,
                role: UserRole.SHIPPING_AGENT_REPRESENTATIVE,
                profilePictureUrl: null,
                passwordHash: null
            );

            // Assert
            Assert.Null(user.Organization);
        }

        [Fact]
        public void UsersWithSameOrganization_ShareOrganizationId()
        {
            // Arrange
            var orgId = new OrganizationId(Guid.NewGuid());
            var user1 = new User(Guid.NewGuid(), "User A", "a@example.com", orgId, UserRole.SHIPPING_AGENT_REPRESENTATIVE, null, null);
            var user2 = new User(Guid.NewGuid(), "User B", "b@example.com", orgId, UserRole.PORT_AUTHORITY_OFFICER, null, null);

            // Assert
            Assert.Equal(user1.OrganizationId, user2.OrganizationId);
        }

        #endregion

        #region Entity Identity Tests

        [Fact]
        public void Users_WithSameId_AreConsideredSame()
        {
            // Arrange
            var id = Guid.NewGuid();
            var orgId = new OrganizationId(Guid.NewGuid());
            var user1 = new User(id, "Name A", "email1@example.com", orgId, UserRole.SHIPPING_AGENT_REPRESENTATIVE, null, null);
            var user2 = new User(id, "Name B", "email2@example.com", orgId, UserRole.PORT_AUTHORITY_OFFICER, null, null);

            // Act & Assert
            Assert.Equal(user1.Id, user2.Id);
            Assert.Equal(user1.UserId, user2.UserId);
        }

        [Fact]
        public void Users_WithDifferentIds_AreDifferent()
        {
            // Arrange
            var orgId = new OrganizationId(Guid.NewGuid());
            var user1 = new User(Guid.NewGuid(), "Same Name", "same@example.com", orgId, UserRole.SHIPPING_AGENT_REPRESENTATIVE, null, null);
            var user2 = new User(Guid.NewGuid(), "Same Name", "same@example.com", orgId, UserRole.PORT_AUTHORITY_OFFICER, null, null);

            // Act & Assert
            Assert.NotEqual(user1.Id, user2.Id);
            Assert.NotEqual(user1.UserId, user2.UserId);
        }

        #endregion

        #region Edge Cases

        [Fact]
        public void Constructor_WithEmptyGuid_CreatesUserWithEmptyId()
        {
            // Arrange
            var id = Guid.Empty;
            var name = "Empty ID User";
            var email = "empty@example.com";
            var orgId = new OrganizationId(Guid.Empty);

            // Act
            var user = new User(
                id: id,
                name: name,
                email: email,
                organizationId: orgId,
                role: UserRole.SHIPPING_AGENT_REPRESENTATIVE,
                profilePictureUrl: null,
                passwordHash: null
            );

            // Assert
            Assert.Equal(Guid.Empty, user.Id);
            Assert.Equal(Guid.Empty, user.UserId.Value);
        }

        [Fact]
        public void Constructor_WithEmptyStrings_AcceptsEmptyValues()
        {
            // Arrange
            var id = Guid.NewGuid();
            var name = "";
            var email = "";
            var orgId = new OrganizationId(Guid.NewGuid());

            // Act
            var user = new User(
                id: id,
                name: name,
                email: email,
                organizationId: orgId,
                role: UserRole.SHIPPING_AGENT_REPRESENTATIVE,
                profilePictureUrl: null,
                passwordHash: null
            );

            // Assert
            Assert.Equal(string.Empty, user.Name);
            Assert.Equal(string.Empty, user.Email);
        }

        [Fact]
        public void Constructor_WithSpecialCharactersInName_AcceptsName()
        {
            // Arrange
            var id = Guid.NewGuid();
            var name = "José María O'Brien-Müller";
            var email = "jose@example.com";
            var orgId = new OrganizationId(Guid.NewGuid());

            // Act
            var user = new User(
                id: id,
                name: name,
                email: email,
                organizationId: orgId,
                role: UserRole.SHIPPING_AGENT_REPRESENTATIVE,
                profilePictureUrl: null,
                passwordHash: null
            );

            // Assert
            Assert.Equal(name, user.Name);
        }

        [Fact]
        public void Constructor_WithUnicodeCharacters_HandlesCorrectly()
        {
            // Arrange
            var id = Guid.NewGuid();
            var name = "李明 (Li Ming)";
            var email = "li.ming@example.com";
            var orgId = new OrganizationId(Guid.NewGuid());

            // Act
            var user = new User(
                id: id,
                name: name,
                email: email,
                organizationId: orgId,
                role: UserRole.SHIPPING_AGENT_REPRESENTATIVE,
                profilePictureUrl: null,
                passwordHash: null
            );

            // Assert
            Assert.Equal(name, user.Name);
        }

        #endregion

        #region Helper Methods

        private static User CreateValidUser(Guid? id = null, string name = "Test User", 
            string email = "test@example.com")
        {
            var userId = id ?? Guid.NewGuid();
            var orgId = new OrganizationId(Guid.NewGuid());
            return new User(
                id: userId,
                name: name,
                email: email,
                organizationId: orgId,
                role: UserRole.SHIPPING_AGENT_REPRESENTATIVE,
                profilePictureUrl: null,
                passwordHash: null
            );
        }

        #endregion
    }
}
