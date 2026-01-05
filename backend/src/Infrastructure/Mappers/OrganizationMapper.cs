using DDDNetCore.Application.DTOs.Organizations;
using DDDNetCore.Domain.Organizations;

namespace DDDNetCore.Infrastructure.Mappers;

public static class OrganizationMapper
{
    /// <summary>
    /// Maps an Organization domain entity to OrganizationResponseDto
    /// </summary>
    public static OrganizationResponseDto ToDto(Organization organization)
    {
        return new OrganizationResponseDto(
            Id: organization.Id.ToString(),
            OrganizationId: organization.OrganizationId.ToString(),
            Identifier: organization.Identifier,
            LegalName: organization.LegalName,
            AlternativeName: organization.AlternativeName,
            Address: organization.AddressLine,
            TaxNumber: organization.TaxNumber,
            Type: organization.Type.ToString(),
            Representatives: organization.Representatives.Select(RepresentativeMapper.ToDto).ToList()
        );
    }

    /// <summary>
    /// Maps a CreateOrganizationDto to an Organization domain entity
    /// </summary>
    public static Organization ToEntity(CreateOrganizationDto dto)
    {
        var representatives = dto.Representatives?
            .Select(RepresentativeMapper.ToEntity)
            .ToList();

        return new Organization(
            id: Guid.NewGuid(),
            identifier: dto.Identifier,
            legalName: dto.LegalName,
            alternativeName: dto.AlternativeName ?? string.Empty,
            addressLine: dto.Address,
            taxNumber: dto.TaxNumber,
            type: dto.Type,
            representatives: representatives
        );
    }
}

public static class RepresentativeMapper
{
    /// <summary>
    /// Maps a Representative domain entity to RepresentativeResponseDto
    /// </summary>
    public static RepresentativeResponseDto ToDto(Representative representative)
    {
        return new RepresentativeResponseDto(
            Id: representative.RepresentativeId.ToString(),
            Name: representative.Name,
            CitizenId: representative.CitizenId,
            Nationality: representative.Nationality,
            Email: representative.Email,
            Phone: representative.Phone,
            IsActive: representative.IsActive,
            CreatedAt: representative.CreatedAt
        );
    }

    /// <summary>
    /// Maps a RepresentativeInputDto to a Representative domain entity
    /// </summary>
    public static Representative ToEntity(RepresentativeInputDto dto)
    {
        return new Representative(
            name: dto.Name,
            citizenId: dto.CitizenId,
            nationality: dto.Nationality,
            email: dto.Email,
            phone: dto.Phone ?? string.Empty
        );
    }
}
