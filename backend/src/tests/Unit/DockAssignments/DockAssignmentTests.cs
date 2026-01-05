using System;
using Xunit;
using DDDNetCore.Domain.DockAssignments;

namespace DDDNetCore.Tests.Unit.DockAssignments
{
    /************************************************************************************************
     * TEST TYPE: Unit Tests
     * ENTITY UNDER TEST: DockAssignment (Aggregate Root)
     * TEST OBJECTIVE: Verify creation, state transitions, reassignment, rescheduling, and event history
     ************************************************************************************************/

    public class DockAssignmentTests
    {
        #region Constructor - Valid Cases

        [Fact]
        public void Create_WithValidData_CreatesAssignment()
        {
            // Arrange
            var vvnId = Guid.NewGuid();
            var dockCode = "DOCK-A1";
            var berthFrom = DateTime.UtcNow;
            var berthTo = berthFrom.AddHours(12);
            var userId = "user123";
            var assignedAt = DateTime.UtcNow;

            // Act
            var assignment = DockAssignment.Create(vvnId, dockCode, berthFrom, berthTo, userId, assignedAt);

            // Assert
            Assert.NotEqual(Guid.Empty, assignment.DockAssignmentId);
            Assert.Equal(vvnId, assignment.VesselVisitNotificationId);
            Assert.Equal(dockCode, assignment.DockCode);
            Assert.Equal(berthFrom, assignment.BerthFrom);
            Assert.Equal(berthTo, assignment.BerthTo);
            Assert.Equal(DockAssignmentStatus.SCHEDULED, assignment.Status);
        }

        [Fact]
        public void Create_WithValidData_AddsInitialEvent()
        {
            // Arrange
            var vvnId = Guid.NewGuid();
            var dockCode = "DOCK-B2";
            var berthFrom = DateTime.UtcNow;
            var berthTo = berthFrom.AddDays(1);
            var userId = "user456";
            var assignedAt = DateTime.UtcNow;

            // Act
            var assignment = DockAssignment.Create(vvnId, dockCode, berthFrom, berthTo, userId, assignedAt);

            // Assert
            Assert.Single(assignment.AssignmentHistory);
            var evt = Assert.Single(assignment.AssignmentHistory);
            Assert.Equal(assignedAt, evt.AssignedAt);
            Assert.Equal(userId, evt.AssignedById);
            Assert.Null(evt.FromDockCode);
            Assert.Equal(dockCode, evt.ToDockCode);
            Assert.Equal("Initial assignment on approval", evt.Reason);
        }

        [Fact]
        public void Create_WithWhitespaceDockCode_TrimsCode()
        {
            // Arrange
            var vvnId = Guid.NewGuid();
            var dockCode = "  DOCK-C3  ";
            var berthFrom = DateTime.UtcNow;
            var berthTo = berthFrom.AddHours(8);
            var userId = "user789";
            var assignedAt = DateTime.UtcNow;

            // Act
            var assignment = DockAssignment.Create(vvnId, dockCode, berthFrom, berthTo, userId, assignedAt);

            // Assert
            Assert.Equal("DOCK-C3", assignment.DockCode);
        }

        #endregion

        #region Constructor - Invalid Cases

        [Fact]
        public void Create_WithEmptyVvnId_ThrowsArgumentException()
        {
            // Arrange
            var vvnId = Guid.Empty;
            var dockCode = "DOCK-A1";
            var berthFrom = DateTime.UtcNow;
            var berthTo = berthFrom.AddHours(12);

            // Act & Assert
            var ex = Assert.Throws<ArgumentException>(() =>
                DockAssignment.Create(vvnId, dockCode, berthFrom, berthTo, "user", DateTime.UtcNow));
            Assert.Contains("VVN id is required", ex.Message);
        }

        [Theory]
        [InlineData(null)]
        [InlineData("")]
        [InlineData("   ")]
        public void Create_WithInvalidDockCode_ThrowsArgumentException(string? invalidDockCode)
        {
            // Arrange
            var vvnId = Guid.NewGuid();
            var berthFrom = DateTime.UtcNow;
            var berthTo = berthFrom.AddHours(12);

            // Act & Assert
            var ex = Assert.Throws<ArgumentException>(() =>
                DockAssignment.Create(vvnId, invalidDockCode, berthFrom, berthTo, "user", DateTime.UtcNow));
            Assert.Contains("Dock code is required", ex.Message);
        }

        [Fact]
        public void Create_WithBerthToBeforeFrom_ThrowsArgumentException()
        {
            // Arrange
            var vvnId = Guid.NewGuid();
            var dockCode = "DOCK-A1";
            var berthFrom = DateTime.UtcNow;
            var berthTo = berthFrom.AddHours(-1); // Invalid: before berthFrom

            // Act & Assert
            var ex = Assert.Throws<ArgumentException>(() =>
                DockAssignment.Create(vvnId, dockCode, berthFrom, berthTo, "user", DateTime.UtcNow));
            Assert.Contains("BerthTo must be after BerthFrom", ex.Message);
        }

        [Fact]
        public void Create_WithBerthToEqualToFrom_ThrowsArgumentException()
        {
            // Arrange
            var vvnId = Guid.NewGuid();
            var dockCode = "DOCK-A1";
            var berthFrom = DateTime.UtcNow;
            var berthTo = berthFrom; // Invalid: equal to berthFrom

            // Act & Assert
            var ex = Assert.Throws<ArgumentException>(() =>
                DockAssignment.Create(vvnId, dockCode, berthFrom, berthTo, "user", DateTime.UtcNow));
            Assert.Contains("BerthTo must be after BerthFrom", ex.Message);
        }

        #endregion

        #region ReassignDock

        [Fact]
        public void ReassignDock_WithValidData_UpdatesDockCode()
        {
            // Arrange
            var assignment = CreateValidAssignment("DOCK-A1");
            var newDockCode = "DOCK-B2";
            var userId = "reassigner";
            var reassignedAt = DateTime.UtcNow;

            // Act
            assignment.ReassignDock(newDockCode, userId, reassignedAt, "Vessel size change");

            // Assert
            Assert.Equal(newDockCode, assignment.DockCode);
            Assert.Equal(2, assignment.AssignmentHistory.Count); // Initial + reassignment
        }

        [Fact]
        public void ReassignDock_WithValidData_AddsEvent()
        {
            // Arrange
            var assignment = CreateValidAssignment("DOCK-A1");
            var newDockCode = "DOCK-C3";
            var userId = "admin";
            var reassignedAt = DateTime.UtcNow;
            var reason = "Weather conditions";

            // Act
            assignment.ReassignDock(newDockCode, userId, reassignedAt, reason);

            // Assert
            Assert.Equal(2, assignment.AssignmentHistory.Count);
            var lastEvent = GetLastEvent(assignment);
            Assert.Equal(reassignedAt, lastEvent.AssignedAt);
            Assert.Equal(userId, lastEvent.AssignedById);
            Assert.Equal("DOCK-A1", lastEvent.FromDockCode);
            Assert.Equal(newDockCode, lastEvent.ToDockCode);
            Assert.Equal(reason, lastEvent.Reason);
        }

        [Fact]
        public void ReassignDock_WithWhitespaceDockCode_TrimsCode()
        {
            // Arrange
            var assignment = CreateValidAssignment("DOCK-A1");

            // Act
            assignment.ReassignDock("  DOCK-D4  ", "user", DateTime.UtcNow);

            // Assert
            Assert.Equal("DOCK-D4", assignment.DockCode);
        }

        [Theory]
        [InlineData(null)]
        [InlineData("")]
        [InlineData("   ")]
        public void ReassignDock_WithInvalidDockCode_ThrowsArgumentException(string? invalidDockCode)
        {
            // Arrange
            var assignment = CreateValidAssignment("DOCK-A1");

            // Act & Assert
            var ex = Assert.Throws<ArgumentException>(() =>
                assignment.ReassignDock(invalidDockCode, "user", DateTime.UtcNow));
            Assert.Contains("Target dock code is required", ex.Message);
        }

        [Fact]
        public void ReassignDock_WhenCancelled_ThrowsInvalidOperationException()
        {
            // Arrange
            var assignment = CreateValidAssignment("DOCK-A1");
            assignment.Cancel("user", DateTime.UtcNow, "Test cancellation");

            // Act & Assert
            Assert.Throws<InvalidOperationException>(() =>
                assignment.ReassignDock("DOCK-B2", "user", DateTime.UtcNow));
        }

        [Fact]
        public void ReassignDock_WhenCompleted_ThrowsInvalidOperationException()
        {
            // Arrange
            var assignment = CreateValidAssignment("DOCK-A1");
            assignment.MarkInProgress();
            assignment.Complete();

            // Act & Assert
            Assert.Throws<InvalidOperationException>(() =>
                assignment.ReassignDock("DOCK-B2", "user", DateTime.UtcNow));
        }

        #endregion

        #region Reschedule

        [Fact]
        public void Reschedule_WithValidData_UpdatesBerthWindow()
        {
            // Arrange
            var assignment = CreateValidAssignment("DOCK-A1");
            var newFrom = DateTime.UtcNow.AddDays(1);
            var newTo = newFrom.AddHours(24);
            var userId = "scheduler";
            var rescheduledAt = DateTime.UtcNow;

            // Act
            assignment.Reschedule(newFrom, newTo, userId, rescheduledAt, "Delayed arrival");

            // Assert
            Assert.Equal(newFrom, assignment.BerthFrom);
            Assert.Equal(newTo, assignment.BerthTo);
        }

        [Fact]
        public void Reschedule_WithValidData_AddsEvent()
        {
            // Arrange
            var assignment = CreateValidAssignment("DOCK-A1");
            var newFrom = DateTime.UtcNow.AddHours(6);
            var newTo = newFrom.AddHours(18);
            var userId = "planner";
            var rescheduledAt = DateTime.UtcNow;
            var reason = "Port congestion";

            // Act
            assignment.Reschedule(newFrom, newTo, userId, rescheduledAt, reason);

            // Assert
            Assert.Equal(2, assignment.AssignmentHistory.Count);
            var lastEvent = GetLastEvent(assignment);
            Assert.Equal(rescheduledAt, lastEvent.AssignedAt);
            Assert.Equal(userId, lastEvent.AssignedById);
            Assert.Equal("DOCK-A1", lastEvent.FromDockCode);
            Assert.Equal("DOCK-A1", lastEvent.ToDockCode);
            Assert.Equal(reason, lastEvent.Reason);
        }

        [Fact]
        public void Reschedule_WithNoReason_UsesDefaultReason()
        {
            // Arrange
            var assignment = CreateValidAssignment("DOCK-A1");
            var newFrom = DateTime.UtcNow.AddDays(2);
            var newTo = newFrom.AddHours(10);

            // Act
            assignment.Reschedule(newFrom, newTo, "user", DateTime.UtcNow);

            // Assert
            var lastEvent = GetLastEvent(assignment);
            Assert.Equal("Reschedule", lastEvent.Reason);
        }

        [Fact]
        public void Reschedule_WithNewToBeforeFrom_ThrowsArgumentException()
        {
            // Arrange
            var assignment = CreateValidAssignment("DOCK-A1");
            var newFrom = DateTime.UtcNow;
            var newTo = newFrom.AddHours(-1);

            // Act & Assert
            var ex = Assert.Throws<ArgumentException>(() =>
                assignment.Reschedule(newFrom, newTo, "user", DateTime.UtcNow));
            Assert.Contains("BerthTo must be after BerthFrom", ex.Message);
        }

        [Fact]
        public void Reschedule_WhenCancelled_ThrowsInvalidOperationException()
        {
            // Arrange
            var assignment = CreateValidAssignment("DOCK-A1");
            assignment.Cancel("user", DateTime.UtcNow, "Test cancellation");
            var newFrom = DateTime.UtcNow.AddDays(1);
            var newTo = newFrom.AddHours(12);

            // Act & Assert
            Assert.Throws<InvalidOperationException>(() =>
                assignment.Reschedule(newFrom, newTo, "user", DateTime.UtcNow));
        }

        [Fact]
        public void Reschedule_WhenCompleted_ThrowsInvalidOperationException()
        {
            // Arrange
            var assignment = CreateValidAssignment("DOCK-A1");
            assignment.MarkInProgress();
            assignment.Complete();
            var newFrom = DateTime.UtcNow.AddDays(1);
            var newTo = newFrom.AddHours(12);

            // Act & Assert
            Assert.Throws<InvalidOperationException>(() =>
                assignment.Reschedule(newFrom, newTo, "user", DateTime.UtcNow));
        }

        #endregion

        #region Status Transitions

        [Fact]
        public void MarkInProgress_FromScheduled_UpdatesStatus()
        {
            // Arrange
            var assignment = CreateValidAssignment("DOCK-A1");

            // Act
            assignment.MarkInProgress();

            // Assert
            Assert.Equal(DockAssignmentStatus.IN_PROGRESS, assignment.Status);
        }

        [Fact]
        public void MarkInProgress_WhenCancelled_ThrowsInvalidOperationException()
        {
            // Arrange
            var assignment = CreateValidAssignment("DOCK-A1");
            assignment.Cancel("user", DateTime.UtcNow);

            // Act & Assert
            Assert.Throws<InvalidOperationException>(() => assignment.MarkInProgress());
        }

        [Fact]
        public void MarkInProgress_WhenCompleted_ThrowsInvalidOperationException()
        {
            // Arrange
            var assignment = CreateValidAssignment("DOCK-A1");
            assignment.MarkInProgress();
            assignment.Complete();

            // Act & Assert
            Assert.Throws<InvalidOperationException>(() => assignment.MarkInProgress());
        }

        [Fact]
        public void Complete_FromScheduled_UpdatesStatus()
        {
            // Arrange
            var assignment = CreateValidAssignment("DOCK-A1");

            // Act
            assignment.Complete();

            // Assert
            Assert.Equal(DockAssignmentStatus.COMPLETED, assignment.Status);
        }

        [Fact]
        public void Complete_FromInProgress_UpdatesStatus()
        {
            // Arrange
            var assignment = CreateValidAssignment("DOCK-A1");
            assignment.MarkInProgress();

            // Act
            assignment.Complete();

            // Assert
            Assert.Equal(DockAssignmentStatus.COMPLETED, assignment.Status);
        }

        [Fact]
        public void Complete_WhenCancelled_ThrowsInvalidOperationException()
        {
            // Arrange
            var assignment = CreateValidAssignment("DOCK-A1");
            assignment.Cancel("user", DateTime.UtcNow);

            // Act & Assert
            Assert.Throws<InvalidOperationException>(() => assignment.Complete());
        }

        [Fact]
        public void Cancel_FromScheduled_UpdatesStatusAndAddsEvent()
        {
            // Arrange
            var assignment = CreateValidAssignment("DOCK-A1");
            var userId = "canceller";
            var cancelledAt = DateTime.UtcNow;
            var reason = "VVN rejected";

            // Act
            assignment.Cancel(userId, cancelledAt, reason);

            // Assert
            Assert.Equal(DockAssignmentStatus.CANCELLED, assignment.Status);
            Assert.Equal(2, assignment.AssignmentHistory.Count);
            var lastEvent = GetLastEvent(assignment);
            Assert.Equal(cancelledAt, lastEvent.AssignedAt);
            Assert.Equal(userId, lastEvent.AssignedById);
            Assert.Equal(reason, lastEvent.Reason);
        }

        [Fact]
        public void Cancel_FromInProgress_UpdatesStatus()
        {
            // Arrange
            var assignment = CreateValidAssignment("DOCK-A1");
            assignment.MarkInProgress();

            // Act
            assignment.Cancel("user", DateTime.UtcNow, "Emergency");

            // Assert
            Assert.Equal(DockAssignmentStatus.CANCELLED, assignment.Status);
        }

        [Fact]
        public void Cancel_WithNoReason_UsesDefaultReason()
        {
            // Arrange
            var assignment = CreateValidAssignment("DOCK-A1");

            // Act
            assignment.Cancel("user", DateTime.UtcNow);

            // Assert
            var lastEvent = GetLastEvent(assignment);
            Assert.Equal("Cancelled", lastEvent.Reason);
        }

        [Fact]
        public void Cancel_WhenCompleted_ThrowsInvalidOperationException()
        {
            // Arrange
            var assignment = CreateValidAssignment("DOCK-A1");
            assignment.MarkInProgress();
            assignment.Complete();

            // Act & Assert
            Assert.Throws<InvalidOperationException>(() =>
                assignment.Cancel("user", DateTime.UtcNow));
        }

        #endregion

        #region Complex Scenarios

        [Fact]
        public void MultipleReassignments_TracksFullHistory()
        {
            // Arrange
            var assignment = CreateValidAssignment("DOCK-A1");

            // Act
            assignment.ReassignDock("DOCK-B2", "user1", DateTime.UtcNow, "Size mismatch");
            assignment.ReassignDock("DOCK-C3", "user2", DateTime.UtcNow, "Better position");
            assignment.ReassignDock("DOCK-D4", "user3", DateTime.UtcNow, "Final allocation");

            // Assert
            Assert.Equal("DOCK-D4", assignment.DockCode);
            Assert.Equal(4, assignment.AssignmentHistory.Count); // Initial + 3 reassignments
        }

        [Fact]
        public void ReassignAndReschedule_BothOperationsSucceed()
        {
            // Arrange
            var assignment = CreateValidAssignment("DOCK-A1");
            var newDock = "DOCK-E5";
            var newFrom = DateTime.UtcNow.AddDays(1);
            var newTo = newFrom.AddHours(20);

            // Act
            assignment.ReassignDock(newDock, "user1", DateTime.UtcNow, "Dock change");
            assignment.Reschedule(newFrom, newTo, "user2", DateTime.UtcNow, "Time change");

            // Assert
            Assert.Equal(newDock, assignment.DockCode);
            Assert.Equal(newFrom, assignment.BerthFrom);
            Assert.Equal(newTo, assignment.BerthTo);
            Assert.Equal(3, assignment.AssignmentHistory.Count); // Initial + reassign + reschedule
        }

        [Fact]
        public void FullLifecycle_ScheduledToInProgressToCompleted_Succeeds()
        {
            // Arrange
            var assignment = CreateValidAssignment("DOCK-A1");

            // Act
            Assert.Equal(DockAssignmentStatus.SCHEDULED, assignment.Status);
            
            assignment.MarkInProgress();
            Assert.Equal(DockAssignmentStatus.IN_PROGRESS, assignment.Status);
            
            assignment.Complete();

            // Assert
            Assert.Equal(DockAssignmentStatus.COMPLETED, assignment.Status);
        }

        [Fact]
        public void FullLifecycle_ScheduledToCancelled_Succeeds()
        {
            // Arrange
            var assignment = CreateValidAssignment("DOCK-A1");

            // Act
            assignment.Cancel("user", DateTime.UtcNow, "VVN withdrawn");

            // Assert
            Assert.Equal(DockAssignmentStatus.CANCELLED, assignment.Status);
            Assert.Equal(2, assignment.AssignmentHistory.Count);
        }

        #endregion

        #region Helper Methods

        private static DockAssignment CreateValidAssignment(string dockCode = "DOCK-TEST")
        {
            var vvnId = Guid.NewGuid();
            var berthFrom = DateTime.UtcNow;
            var berthTo = berthFrom.AddHours(12);
            return DockAssignment.Create(vvnId, dockCode, berthFrom, berthTo, "testUser", DateTime.UtcNow);
        }

        private static DockAssignmentEvent GetLastEvent(DockAssignment assignment)
        {
            var events = new System.Collections.Generic.List<DockAssignmentEvent>(assignment.AssignmentHistory);
            return events[events.Count - 1];
        }

        #endregion
    }
}
