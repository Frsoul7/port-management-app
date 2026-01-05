using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDNetCore.Domain.IRepository;
using DDDNetCore.Application.Interfaces;
using DDDNetCore.Domain.Organizations;
using DDDNetCore.Domain.Shared;
using DDDNetCore.Domain.Vessels;
using DDDNetCore.Application.DTOs.Vessels;
using DDDNetCore.Infrastructure.Mappers;
using Microsoft.EntityFrameworkCore;

namespace DDDNetCore.Application.Services
{
    /// <summary>
    /// Application service for vessel business logic.
    /// Orchestrates domain operations and enforces business rules for vessels.
    /// </summary>
    public class VesselService : IVesselService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly DDDNetCore.Domain.Factory.IVesselFactory _vesselFactory;

        public VesselService(IUnitOfWork unitOfWork, DDDNetCore.Domain.Factory.IVesselFactory vesselFactory)
        {
            _unitOfWork = unitOfWork;
            _vesselFactory = vesselFactory;
        }

        public async Task<VesselResponseDto> CreateVesselAsync(CreateVesselDto dto, string callerRole, Guid? callerOrgId)
        {
            // Authorization: Only Port Authority Officers can register vessels
            if (callerRole != "PortAuthorityOfficer" && callerRole != "Administrator")
            {
                throw new UnauthorizedAccessException("Only Port Authority Officers can register vessels.");
            }

            // Optional: Validate organization type if callerOrgId is provided (Port Authority doesn't require it)
            if (callerOrgId.HasValue)
            {
                var callerOrg = await _unitOfWork.Organizations.GetByIdAsync(callerOrgId.Value.ToString());
                if (callerOrg == null)
                {
                    throw new ArgumentException("Organization not found.", nameof(callerOrgId));
                }

                if (callerOrg.Type != OrganizationType.PORT_AUTHORITY)
                {
                    throw new UnauthorizedAccessException("The specified organization is not a Port Authority.");
                }
            }
            // Note: X-Org-Id is optional for Port Authority officers (they perform system-wide operations)

            // Validate required fields
            if (string.IsNullOrWhiteSpace(dto.ImoNumber))
                throw new ArgumentException("ImoNumber is required", nameof(dto.ImoNumber));
            if (string.IsNullOrWhiteSpace(dto.Name))
                throw new ArgumentException("Name is required", nameof(dto.Name));
            if (string.IsNullOrWhiteSpace(dto.VesselTypeId))
                throw new ArgumentException("VesselTypeId is required", nameof(dto.VesselTypeId));
            if (string.IsNullOrWhiteSpace(dto.OrganizationId))
                throw new ArgumentException("OrganizationId is required", nameof(dto.OrganizationId));

            // Normalize IMO for uniqueness check
            var normImo = ImoValidator.Normalize(dto.ImoNumber);

            // Check IMO uniqueness
            var imoExists = await _unitOfWork.Vessels.ExistsAsync(normImo);
            if (imoExists)
                throw new InvalidOperationException("A vessel with this IMO already exists.");

            // Check name uniqueness (case-insensitive)
            var nameNorm = dto.Name.Trim().ToUpperInvariant();
            var allVessels = await _unitOfWork.Vessels.GetAllAsync();
            var nameExists = allVessels.Any(v => v.Name.Trim().ToUpper() == nameNorm);
            if (nameExists)
                throw new InvalidOperationException($"A vessel named '{dto.Name}' already exists.");

            // Validate vessel type exists
            var vesselType = await _unitOfWork.VesselTypes.GetByIdAsync(dto.VesselTypeId);
            if (vesselType == null)
                throw new ArgumentException("Unknown VesselTypeId. Create the vessel type first.", nameof(dto.VesselTypeId));

            // Validate organization exists
            if (!Guid.TryParse(dto.OrganizationId, out var orgGuid))
                throw new ArgumentException("OrganizationId must be a GUID", nameof(dto.OrganizationId));

            var org = await _unitOfWork.Organizations.GetByIdAsync(dto.OrganizationId);
            if (org == null)
                throw new ArgumentException("Unknown OrganizationId. Create the organization first.", nameof(dto.OrganizationId));

            // Validate organization type is SHIPPING_AGENT
            if (org.Type != OrganizationType.SHIPPING_AGENT)
                throw new ArgumentException("Vessel can only be assigned to a SHIPPING_AGENT organization.", nameof(dto.OrganizationId));

            var orgIdVo = new OrganizationId(orgGuid);

            // Create vessel using factory (validates IMO format and normalizes data)
            var vessel = _vesselFactory.Create(dto.ImoNumber, dto.Name.Trim(), dto.VesselTypeId, orgIdVo, dto.CapacityTEU);

            await _unitOfWork.Vessels.AddAsync(vessel);
            await _unitOfWork.CommitAsync();

            return VesselMapper.ToDto(vessel);
        }

        public async Task<VesselResponseDto> UpdateVesselAsync(string imo, UpdateVesselDto dto)
        {
            var normImo = ImoValidator.Normalize(imo);
            var vessel = await _unitOfWork.Vessels.GetByImoNumberAsync(normImo);
            
            if (vessel == null)
                throw new KeyNotFoundException("Vessel not found.");

            // Verify new vessel type exists
            var vesselType = await _unitOfWork.VesselTypes.GetByIdAsync(dto.VesselTypeId);
            if (vesselType == null)
                throw new ArgumentException("Unknown VesselTypeId.", nameof(dto.VesselTypeId));

            // Verify new organization exists
            if (!Guid.TryParse(dto.OrganizationId, out var orgGuid))
                throw new ArgumentException("OrganizationId must be a GUID", nameof(dto.OrganizationId));

            var org = await _unitOfWork.Organizations.GetByIdAsync(dto.OrganizationId);
            if (org == null)
                throw new ArgumentException("Unknown OrganizationId.", nameof(dto.OrganizationId));

            // Validate organization type is SHIPPING_AGENT
            if (org.Type != OrganizationType.SHIPPING_AGENT)
                throw new ArgumentException("Vessel can only be assigned to a SHIPPING_AGENT organization.", nameof(dto.OrganizationId));

            // Check name uniqueness on update (exclude current vessel)
            var newNameNorm = dto.Name.Trim().ToUpperInvariant();
            var allVessels = await _unitOfWork.Vessels.GetAllAsync();
            var nameClash = allVessels.Any(v => v.ImoNumber != vessel.ImoNumber &&
                               v.Name.Trim().ToUpper() == newNameNorm);
            if (nameClash)
                throw new InvalidOperationException($"A vessel named '{dto.Name}' already exists.");

            vessel.UpdateBasics(dto.Name.Trim(), dto.VesselTypeId, new OrganizationId(orgGuid), dto.CapacityTEU);
            _unitOfWork.Vessels.Update(vessel);
            await _unitOfWork.CommitAsync();

            return VesselMapper.ToDto(vessel);
        }

        public async Task<VesselResponseDto?> GetVesselByImoAsync(string imo)
        {
            var normImo = ImoValidator.Normalize(imo);
            var vessel = await _unitOfWork.Vessels.GetByImoNumberAsync(normImo);
            return vessel == null ? null : VesselMapper.ToDto(vessel);
        }

        public async Task<List<VesselResponseDto>> SearchVesselsAsync(
            string? imo,
            string? name,
            string? organizationName)
        {
            // Normalize search term
            string? searchTerm = null;
            if (!string.IsNullOrWhiteSpace(imo))
            {
                searchTerm = ImoValidator.Normalize(imo);
            }
            else if (!string.IsNullOrWhiteSpace(name))
            {
                searchTerm = name.Trim();
            }

            // Use repository search method (removed typeId and organizationId filters)
            var vessels = await _unitOfWork.Vessels.SearchAsync(searchTerm, null, null);

            // Apply additional filters in memory if needed
            if (!string.IsNullOrWhiteSpace(organizationName))
            {
                var on = organizationName.Trim().ToLower();
                vessels = vessels.Where(v => v.OwnerOrganization != null &&
                                 (
                                     v.OwnerOrganization.LegalName.ToLower().Contains(on) ||
                                     (v.OwnerOrganization.AlternativeName ?? string.Empty).ToLower().Contains(on) ||
                                     (v.OwnerOrganization.Identifier ?? string.Empty).ToLower().Contains(on)
                                 ));
            }

            return vessels.OrderBy(v => v.Name).Select(VesselMapper.ToDto).ToList();
        }
    }
}
