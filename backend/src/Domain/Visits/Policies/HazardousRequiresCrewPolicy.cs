using System;
using System.Linq;
using DDDNetCore.Domain.Visits.Manifests;

namespace DDDNetCore.Domain.Visits.Policies
{
    public class HazardousRequiresCrewPolicy : ICrewCompliancePolicy
    {
        public void EnsureSatisfied(Visits.VesselVisitNotification vvn)
        {
            // ALWAYS require captain + non-negative crew count
            if (string.IsNullOrWhiteSpace(vvn.CaptainName))
                throw new InvalidOperationException("Captain name is required.");
            if (vvn.CrewCount < 0)
                throw new InvalidOperationException("Crew count must be >= 0.");

            // If any hazardous cargo exists, require at least one specific handler (crew member)
            bool hasHaz = (vvn.LoadingManifest?.Entries?.Any(e => e.HazardousGoods) == true)
                       || (vvn.UnloadingManifest?.Entries?.Any(e => e.HazardousGoods) == true);

            if (hasHaz && (vvn.CrewMembers?.Count ?? 0) == 0)
                throw new InvalidOperationException(
                    "This VVN contains at least one container with hazardous goods, so at least one specific crew handler must be defined."
                );
        }
    }
}
