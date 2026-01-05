using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDNetCore.Domain.IRepository;
using DDDNetCore.Application.Interfaces;
using DDDNetCore.Domain.Vessels;
using DDDNetCore.Application.DTOs.VesselTypes;
using DDDNetCore.Infrastructure.Mappers;

namespace DDDNetCore.Application.Services
{
    /// <summary>
    /// Application service for vessel type business logic.
    /// Orchestrates domain operations and enforces business rules for vessel types.
    /// </summary>
    public class VesselTypeService : IVesselTypeService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly DDDNetCore.Domain.Factory.IVesselTypeFactory _vesselTypeFactory;

        public VesselTypeService(IUnitOfWork unitOfWork, DDDNetCore.Domain.Factory.IVesselTypeFactory vesselTypeFactory)
        {
            _unitOfWork = unitOfWork;
            _vesselTypeFactory = vesselTypeFactory;
        }

        public async Task<VesselTypeResponseDto> CreateVesselTypeAsync(CreateVesselTypeDto dto)
        {
            if (string.IsNullOrWhiteSpace(dto.Name))
                throw new ArgumentException("Name is required.", nameof(dto.Name));

            var name = dto.Name.Trim();

            // Check uniqueness (case-insensitive)
            if (await _unitOfWork.VesselTypes.ExistsByNameAsync(name))
                throw new InvalidOperationException("Vessel type name already exists.");

            var id = dto.VesselTypeId ?? Guid.NewGuid().ToString("N");
            
            // Create vessel type using factory (validates and normalizes data)
            var vesselType = _vesselTypeFactory.Create(id, name);
            
            vesselType.Update(
                dto.Name, 
                dto.Description, 
                dto.CapacityTEU,
                dto.MaxRows, 
                dto.MaxBays, 
                dto.MaxTiers, 
                dto.OperationalConstraints
            );

            await _unitOfWork.VesselTypes.AddAsync(vesselType);
            await _unitOfWork.CommitAsync();

            return VesselTypeMapper.ToDto(vesselType);
        }

        public async Task<VesselTypeResponseDto> UpdateVesselTypeAsync(string id, UpdateVesselTypeDto dto)
        {
            var vesselType = await _unitOfWork.VesselTypes.GetByIdAsync(id);
            if (vesselType == null)
                throw new KeyNotFoundException("Vessel type not found.");

            // Check name uniqueness when changing the name
            if (!string.IsNullOrWhiteSpace(dto.Name))
            {
                var newName = dto.Name.Trim();
                if (await _unitOfWork.VesselTypes.ExistsByNameAsync(newName, excludeId: id))
                    throw new InvalidOperationException("Vessel type name already exists.");
            }

            vesselType.Update(
                dto.Name ?? vesselType.Name,
                dto.Description ?? vesselType.Description,
                dto.CapacityTEU ?? vesselType.CapacityTEU,
                dto.MaxRows ?? vesselType.MaxRows,
                dto.MaxBays ?? vesselType.MaxBays,
                dto.MaxTiers ?? vesselType.MaxTiers,
                dto.OperationalConstraints ?? vesselType.OperationalConstraints
            );

            _unitOfWork.VesselTypes.Update(vesselType);
            await _unitOfWork.CommitAsync();

            return VesselTypeMapper.ToDto(vesselType);
        }

        public async Task<VesselTypeResponseDto?> GetVesselTypeByIdAsync(string id)
        {
            var vesselType = await _unitOfWork.VesselTypes.GetByIdAsync(id);
            return vesselType == null ? null : VesselTypeMapper.ToDto(vesselType);
        }

        public async Task<(List<VesselTypeResponseDto> Items, int Total)> SearchVesselTypesAsync(string? name, string? description, int page, int pageSize)
        {
            // Validate pagination parameters
            if (page < 1) page = 1;
            if (pageSize < 1 || pageSize > 200) pageSize = 20;

            var (items, total) = await _unitOfWork.VesselTypes.SearchAsync(name, description, page, pageSize);
            var dtoItems = items.Select(VesselTypeMapper.ToDto).ToList();
            return (dtoItems, total);
        }
    }
}
