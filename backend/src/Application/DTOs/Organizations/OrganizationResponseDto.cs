using System;
using System.Collections.Generic;
using DDDNetCore.Domain.Organizations;

namespace DDDNetCore.Application.DTOs.Organizations
{
    public record OrganizationResponseDto(
        string Id,
        string OrganizationId,
        string Identifier,
        string LegalName,
        string AlternativeName,
        string Address,
        string TaxNumber,
        string Type,
        List<RepresentativeResponseDto> Representatives
    )
    {
        public static OrganizationResponseDto From(Organization o)
        {
            return new OrganizationResponseDto(
                Id: o.Id.ToString(),
                OrganizationId: o.OrganizationId.ToString(),
                Identifier: o.Identifier,
                LegalName: o.LegalName,
                AlternativeName: o.AlternativeName,
                Address: o.AddressLine,
                TaxNumber: o.TaxNumber,
                Type: o.Type.ToString(),
                Representatives: o.Representatives.Select(r => new RepresentativeResponseDto(
                    Id: r.RepresentativeId.ToString(),
                    Name: r.Name,
                    CitizenId: r.CitizenId,
                    Nationality: r.Nationality,
                    Email: r.Email,
                    Phone: r.Phone,
                    IsActive: r.IsActive,
                    CreatedAt: r.CreatedAt
                )).ToList()
            );
        }
    }

    public record RepresentativeResponseDto(
        string Id,
        string Name,
        string CitizenId,
        string Nationality,
        string Email,
        string Phone,
        bool IsActive,
        DateTime CreatedAt
    );
}
