namespace DDDNetCore.Domain.Visits.Policies
{
    public interface ICrewCompliancePolicy
    {
        /// Throws InvalidOperationException if crew is required and missing.
        void EnsureSatisfied(Visits.VesselVisitNotification vvn);
    }
}
