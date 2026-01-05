using System;
using System.Collections.Generic;
using DDDNetCore.Domain.Docks;

namespace DDDNetCore.Domain.DockAssignments
{
    public enum DockAssignmentStatus { SCHEDULED, IN_PROGRESS, COMPLETED, CANCELLED }

    /// <summary>
    /// Aggregate root representing the berth allocation for a VVN on a specific dock and time window.
    /// All date-times are expected to be UTC.
    /// </summary>
    public class DockAssignment
    {
        public Guid DockAssignmentId { get; private set; }

        /// <summary>Inclusive start of the allocated berth window (UTC).</summary>
        public DateTime BerthFrom { get; private set; }

        /// <summary>Exclusive end of the allocated berth window (UTC).</summary>
        public DateTime BerthTo { get; private set; }

        public DockAssignmentStatus Status { get; private set; } = DockAssignmentStatus.SCHEDULED;

        /// <summary>Current dock code for this allocation.</summary>
        public string DockCode { get; private set; } = null!;
        public Dock? Dock { get; private set; }

        /// <summary>Top-level creation timestamp (UTC).</summary>
        public DateTime CreatedAt { get; private set; }

        /// <summary>Back-reference to the VVN that this allocation services.</summary>
        public Guid VesselVisitNotificationId { get; private set; }

        // Read-only projection; mutate via methods that append events
        private readonly List<DockAssignmentEvent> _assignmentHistory = new();
        public IReadOnlyCollection<DockAssignmentEvent> AssignmentHistory => _assignmentHistory.AsReadOnly();

        private DockAssignment() { }

        /// <summary>
        /// Factory used when approving a VVN. Requires VVN id, target dock, and berth window.
        /// </summary>
        public static DockAssignment Create(Guid vvnId, string dockCode, DateTime berthFromUtc, DateTime berthToUtc, string assignedByUserId, DateTime assignedAtUtc)
        {
            if (vvnId == Guid.Empty) throw new ArgumentException("VVN id is required.", nameof(vvnId));
            if (string.IsNullOrWhiteSpace(dockCode)) throw new ArgumentException("Dock code is required.", nameof(dockCode));
            dockCode = dockCode.Trim();
            if (berthToUtc <= berthFromUtc) throw new ArgumentException("BerthTo must be after BerthFrom.");

            var da = new DockAssignment
            {
                DockAssignmentId = Guid.NewGuid(),
                VesselVisitNotificationId = vvnId,
                DockCode = dockCode,
                BerthFrom = berthFromUtc,
                BerthTo = berthToUtc,
                Status = DockAssignmentStatus.SCHEDULED,
                CreatedAt = DateTime.UtcNow
            };

            da._assignmentHistory.Add(new DockAssignmentEvent(
                assignedAtUtc,
                assignedByUserId,
                fromDockCode: null,
                toDockCode: dockCode,
                reason: "Initial assignment on approval"));

            return da;
        }

        /// <summary>
        /// Reassign to a different dock (or same dock with reason) and record an event.
        /// </summary>
        public void ReassignDock(string toDockCode, string assignedByUserId, DateTime atUtc, string? reason = null)
        {
            if (Status == DockAssignmentStatus.CANCELLED || Status == DockAssignmentStatus.COMPLETED)
                throw new InvalidOperationException("Cannot reassign a completed or cancelled dock assignment.");

            if (string.IsNullOrWhiteSpace(toDockCode)) throw new ArgumentException("Target dock code is required.", nameof(toDockCode));
            toDockCode = toDockCode.Trim();

            var from = DockCode;
            DockCode = toDockCode;
            _assignmentHistory.Add(new DockAssignmentEvent(atUtc, assignedByUserId, from, toDockCode, reason));
        }

        /// <summary>Adjust the berth window.</summary>
        public void Reschedule(DateTime newFromUtc, DateTime newToUtc, string adjustedByUserId, DateTime atUtc, string? reason = null)
        {
            if (Status == DockAssignmentStatus.CANCELLED || Status == DockAssignmentStatus.COMPLETED)
                throw new InvalidOperationException("Cannot reschedule a completed or cancelled dock assignment.");

            if (newToUtc <= newFromUtc) throw new ArgumentException("BerthTo must be after BerthFrom.");

            BerthFrom = newFromUtc;
            BerthTo = newToUtc;
            _assignmentHistory.Add(new DockAssignmentEvent(atUtc, adjustedByUserId, DockCode, DockCode, reason ?? "Reschedule"));
        }

        public void MarkInProgress()
        {
            if (Status == DockAssignmentStatus.CANCELLED || Status == DockAssignmentStatus.COMPLETED)
                throw new InvalidOperationException("Invalid transition.");
            Status = DockAssignmentStatus.IN_PROGRESS;
        }

        public void Complete()
        {
            if (Status == DockAssignmentStatus.CANCELLED)
                throw new InvalidOperationException("Invalid transition.");
            Status = DockAssignmentStatus.COMPLETED;
        }

        public void Cancel(string cancelledByUserId, DateTime atUtc, string? reason = null)
        {
            if (Status == DockAssignmentStatus.COMPLETED)
                throw new InvalidOperationException("Cannot cancel a completed dock assignment.");

            Status = DockAssignmentStatus.CANCELLED;
            _assignmentHistory.Add(new DockAssignmentEvent(atUtc, cancelledByUserId, DockCode, DockCode, reason ?? "Cancelled"));
        }
    }

    public class DockAssignmentEvent
    {
        public int DockAssignmentEventId { get; private set; }
        public DateTime AssignedAt { get; private set; }          // UTC
        public string AssignedById { get; private set; } = null!;
        public string? FromDockCode { get; private set; }
        public string ToDockCode { get; private set; } = null!;
        public string? Reason { get; private set; }

        private DockAssignmentEvent() { }

        public DockAssignmentEvent(DateTime assignedAtUtc, string assignedById, string? fromDockCode, string toDockCode, string? reason)
        {
            AssignedAt = assignedAtUtc;
            AssignedById = assignedById;
            FromDockCode = fromDockCode;
            ToDockCode = toDockCode;
            Reason = reason;
        }
    }
}
