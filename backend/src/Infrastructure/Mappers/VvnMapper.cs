using DDDNetCore.Application.DTOs.Vvns;
using DDDNetCore.Domain.Visits;
using DDDNetCore.Domain.Organizations;
using DDDNetCore.API.Contracts;

namespace DDDNetCore.Infrastructure.Mappers;

public static class VvnMapper
{
    /// <summary>
    /// Maps a VesselVisitNotification domain entity to VvnSummaryResponse
    /// </summary>
    public static VvnSummaryResponse ToSummaryDto(VesselVisitNotification vvn)
    {
        return new VvnSummaryResponse
        {
            Id = vvn.VvnGuid,
            VvnBusinessId = vvn.VvnBusinessId,
            Status = vvn.State.ToString(),
            Purpose = vvn.VisitPurpose.ToString(),
            Eta = vvn.Eta,
            Etd = vvn.Etd,
            SubmittedAt = vvn.SubmittedAt,
            VesselImo = vvn.VesselImo,
            VesselName = vvn.Vessel?.Name,
            VesselType = vvn.Vessel?.VesselType?.Name,
            AssignedDock = vvn.DockAssignment?.DockCode
        };
    }

    /// <summary>
    /// Maps a CreateVvnRequest to a VesselVisitNotification domain entity
    /// </summary>
    public static VesselVisitNotification ToEntity(CreateVvnRequest request, OrganizationId organizationId, string businessId)
    {
        return new VesselVisitNotification(
            vvnBusinessId: businessId,
            vesselImo: request.VesselImo,
            purpose: ParseVisitPurpose(request.Purpose),
            etaUtc: request.Eta,
            etdUtc: request.Etd,
            captainName: request.CaptainName,
            captainCitizenId: request.CaptainCitizenId,
            captainNationality: request.CaptainNationality,
            crewCount: request.CrewCount,
            orgId: organizationId
        );
    }

    private static VisitPurpose ParseVisitPurpose(string purpose)
    {
        return purpose.ToUpperInvariant() switch
        {
            "LOAD" => VisitPurpose.LOAD,
            "UNLOAD" => VisitPurpose.UNLOAD,
            "BOTH" => VisitPurpose.BOTH,
            "MAINTENANCE" => VisitPurpose.MAINTENANCE,
            _ => throw new ArgumentException($"Invalid visit purpose: {purpose}")
        };
    }
}
