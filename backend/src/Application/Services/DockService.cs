using DDDNetCore.Application.DTOs.Docks;
using DDDNetCore.Application.Interfaces;
using DDDNetCore.Domain.Docks;
using DDDNetCore.Domain.IRepository;
using DDDNetCore.Infrastructure.Mappers;

namespace DDDNetCore.Application.Services;

public class DockService : IDockService
{
    private readonly IDockRepository _dockRepo;
    private readonly IVesselTypeRepository _vesselTypeRepo;
    private readonly IUnitOfWork _unitOfWork;

    public DockService(
        IDockRepository dockRepo,
        IVesselTypeRepository vesselTypeRepo,
        IUnitOfWork unitOfWork)
    {
        _dockRepo = dockRepo;
        _vesselTypeRepo = vesselTypeRepo;
        _unitOfWork = unitOfWork;
    }

    public async Task<List<DockResponseDto>> GetAllAsync(string? name = null, string? location = null, string? vesselTypeId = null)
    {
        List<Dock> docks;

        if (!string.IsNullOrWhiteSpace(name) || !string.IsNullOrWhiteSpace(location) || !string.IsNullOrWhiteSpace(vesselTypeId))
        {
            docks = await _dockRepo.SearchAsync(name, location, vesselTypeId);
        }
        else
        {
            docks = await _dockRepo.GetAllAsync(includeVesselTypes: true);
        }

        return docks.Select(DockMapper.ToDto).ToList();
    }

    public async Task<DockResponseDto?> GetByCodeAsync(string code)
    {
        var dock = await _dockRepo.GetByCodeAsync(code.ToUpperInvariant(), includeVesselTypes: true);
        return dock == null ? null : DockMapper.ToDto(dock);
    }

    public async Task<DockResponseDto> CreateAsync(CreateDockDto dto)
    {
        var codeUpper = dto.Code.ToUpperInvariant().Trim();

        // Check if dock already exists
        if (await _dockRepo.ExistsAsync(codeUpper))
            throw new InvalidOperationException($"Dock with code '{codeUpper}' already exists.");

        // Create dock
        var dock = new Dock(codeUpper, dto.Name, dto.Location, dto.LengthM, dto.DepthM, dto.MaxDraftM);

        // Set allowed vessel types if provided
        if (dto.AllowedVesselTypeIds != null && dto.AllowedVesselTypeIds.Any())
        {
            var vesselTypes = await _vesselTypeRepo.GetByIdsAsync(dto.AllowedVesselTypeIds);

            if (vesselTypes.Count != dto.AllowedVesselTypeIds.Count)
            {
                var missing = dto.AllowedVesselTypeIds.Except(vesselTypes.Select(vt => vt.VesselTypeId)).ToList();
                throw new ArgumentException($"Vessel type(s) not found: {string.Join(", ", missing)}");
            }

            dock.SetAllowedVesselTypes(vesselTypes);
        }

        await _dockRepo.AddAsync(dock);
        await _unitOfWork.CommitAsync();

        return DockMapper.ToDto(dock);
    }

    public async Task<DockResponseDto> UpdateAsync(string code, UpdateDockDto dto)
    {
        var codeUpper = code.ToUpperInvariant().Trim();
        var dock = await _dockRepo.GetByCodeAsync(codeUpper, includeVesselTypes: true);

        if (dock == null)
            throw new KeyNotFoundException($"Dock with code '{codeUpper}' not found.");

        // Update basic properties
        dock.Update(dto.Name, dto.Location, dto.LengthM, dto.DepthM, dto.MaxDraftM);

        // Update allowed vessel types if provided
        if (dto.AllowedVesselTypeIds != null)
        {
            if (dto.AllowedVesselTypeIds.Any())
            {
                var vesselTypes = await _vesselTypeRepo.GetByIdsAsync(dto.AllowedVesselTypeIds);

                if (vesselTypes.Count != dto.AllowedVesselTypeIds.Count)
                {
                    var missing = dto.AllowedVesselTypeIds.Except(vesselTypes.Select(vt => vt.VesselTypeId)).ToList();
                    throw new ArgumentException($"Vessel type(s) not found: {string.Join(", ", missing)}");
                }

                dock.SetAllowedVesselTypes(vesselTypes);
            }
            else
            {
                dock.SetAllowedVesselTypes(new List<Domain.Vessels.VesselType>());
            }
        }

        _dockRepo.Update(dock);
        await _unitOfWork.CommitAsync();

        return DockMapper.ToDto(dock);
    }

    public async Task DeleteAsync(string code)
    {
        var codeUpper = code.ToUpperInvariant().Trim();
        var dock = await _dockRepo.GetByCodeAsync(codeUpper);

        if (dock == null)
            throw new KeyNotFoundException($"Dock with code '{codeUpper}' not found.");

        _dockRepo.Remove(dock);
        await _unitOfWork.CommitAsync();
    }
}
