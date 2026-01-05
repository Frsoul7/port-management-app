using System;
using DDDNetCore.Domain.Users;  // <- for UserId

namespace DDDNetCore.Domain.Visits
{
    public enum DecisionOutcome { Approved = 1, Rejected = 2 }

    public class DecisionLog
    {
        public Guid DecisionLogId { get; private set; }
        public Guid VvnGuid { get; private set; }
        public DecisionOutcome Outcome { get; private set; }

        // keep VO
        public UserId OfficerUserId { get; private set; } = null!;

        public DateTime AtUtc { get; private set; }
        public Guid? DockAssignmentId { get; private set; }
        public string? Reason { get; private set; }
        public string? Notes { get; private set; }

        private DecisionLog() { }

        public static DecisionLog Approved(Guid vvnId, UserId officer, Guid dockAssignmentId, DateTime atUtc, string? notes = null)
            => new DecisionLog
            {
                DecisionLogId = Guid.NewGuid(),
                VvnGuid = vvnId,
                Outcome = DecisionOutcome.Approved,
                OfficerUserId = officer,
                AtUtc = atUtc,
                DockAssignmentId = dockAssignmentId,
                Notes = notes
            };

        public static DecisionLog Rejected(Guid vvnId, UserId officer, string reason, DateTime atUtc, string? notes = null)
            => new DecisionLog
            {
                DecisionLogId = Guid.NewGuid(),
                VvnGuid = vvnId,
                Outcome = DecisionOutcome.Rejected,
                OfficerUserId = officer,
                AtUtc = atUtc,
                Reason = reason,
                Notes = notes
            };
    }
}
