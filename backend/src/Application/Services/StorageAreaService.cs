using DDDNetCore.Application.DTOs.StorageAreas;
using DDDNetCore.Application.Interfaces;
using DDDNetCore.Domain.Docks;
using DDDNetCore.Domain.IRepository;
using DDDNetCore.Domain.StorageAreas;
using DDDNetCore.Infrastructure.Mappers;

namespace DDDNetCore.Application.Services;

public class StorageAreaService : IStorageAreaService
{
    private readonly IStorageAreaRepository _storageAreaRepo;
    private readonly IDockRepository _dockRepo;
    private readonly IUnitOfWork _unitOfWork;

    public StorageAreaService(
        IStorageAreaRepository storageAreaRepo,
        IDockRepository dockRepo,
        IUnitOfWork unitOfWork)
    {
        _storageAreaRepo = storageAreaRepo;
        _dockRepo = dockRepo;
        _unitOfWork = unitOfWork;
    }

    public async Task<List<StorageAreaResponseDto>> GetAllAsync(
        string? name = null,
        string? location = null,
        StorageAreaType? type = null,
        bool? servesAllDocks = null)
    {
        List<StorageArea> storageAreas;

        if (!string.IsNullOrWhiteSpace(name) || !string.IsNullOrWhiteSpace(location) ||
            type.HasValue || servesAllDocks.HasValue)
        {
            storageAreas = await _storageAreaRepo.SearchAsync(name, location, type, servesAllDocks);
        }
        else
        {
            storageAreas = await _storageAreaRepo.GetAllAsync(includeDocks: true);
        }

        return storageAreas.Select(StorageAreaMapper.ToDto).ToList();
    }

    public async Task<StorageAreaResponseDto?> GetByIdAsync(string id)
    {
        var storageArea = await _storageAreaRepo.GetByIdAsync(id, includeDocks: true);
        return storageArea == null ? null : StorageAreaMapper.ToDto(storageArea);
    }

    public async Task<StorageAreaResponseDto> CreateAsync(CreateStorageAreaDto dto)
    {
        // Validate XOR constraint for specs
        if (dto.Type == StorageAreaType.ORDINARY &&
            (dto.YardNotes != null || dto.WarehouseNotes != null))
        {
            throw new ArgumentException("ORDINARY storage areas cannot have Yard or Warehouse specs.");
        }

        if (dto.Type == StorageAreaType.YARD && dto.WarehouseNotes != null)
        {
            throw new ArgumentException("YARD storage areas cannot have Warehouse specs.");
        }

        if (dto.Type == StorageAreaType.WAREHOUSE && dto.YardNotes != null)
        {
            throw new ArgumentException("WAREHOUSE storage areas cannot have Yard specs.");
        }

        // Create storage area
        var storageArea = new StorageArea(
            dto.Name,
            dto.Location,
            dto.MaxCapacityTEU,
            dto.Type,
            dto.ServesAllDocks);

        // Set type-specific specs
        if (dto.Type == StorageAreaType.YARD)
        {
            storageArea.SetYardSpec(dto.YardNotes);
        }
        else if (dto.Type == StorageAreaType.WAREHOUSE)
        {
            storageArea.SetWarehouseSpec(dto.WarehouseNotes);
        }

        // Set served docks if not serving all
        if (!dto.ServesAllDocks && dto.ServedDockCodes != null && dto.ServedDockCodes.Any())
        {
            var docks = new List<Dock>();
            foreach (var code in dto.ServedDockCodes)
            {
                var dock = await _dockRepo.GetByCodeAsync(code.ToUpperInvariant());
                if (dock == null)
                    throw new ArgumentException($"Dock with code '{code}' not found.");
                docks.Add(dock);
            }
            storageArea.SetServedDocks(docks);
        }

        await _storageAreaRepo.AddAsync(storageArea);
        await _unitOfWork.CommitAsync();

        return StorageAreaMapper.ToDto(storageArea);
    }

    public async Task<StorageAreaResponseDto> UpdateAsync(string id, UpdateStorageAreaDto dto)
    {
        var storageArea = await _storageAreaRepo.GetByIdAsync(id, includeDocks: true);
        if (storageArea == null)
            throw new KeyNotFoundException($"Storage area with ID '{id}' not found.");

        // Update basic properties (includes capacity validation)
        storageArea.Update(dto.Name, dto.Location, dto.MaxCapacityTEU, dto.ServesAllDocks);

        // Update type-specific specs
        if (storageArea.Type == StorageAreaType.YARD)
        {
            storageArea.SetYardSpec(dto.YardNotes);
        }
        else if (storageArea.Type == StorageAreaType.WAREHOUSE)
        {
            storageArea.SetWarehouseSpec(dto.WarehouseNotes);
        }

        // Update served docks
        if (!dto.ServesAllDocks && dto.ServedDockCodes != null)
        {
            var docks = new List<Dock>();
            foreach (var code in dto.ServedDockCodes)
            {
                var dock = await _dockRepo.GetByCodeAsync(code.ToUpperInvariant());
                if (dock == null)
                    throw new ArgumentException($"Dock with code '{code}' not found.");
                docks.Add(dock);
            }
            storageArea.SetServedDocks(docks);
        }

        _storageAreaRepo.Update(storageArea);
        await _unitOfWork.CommitAsync();

        return StorageAreaMapper.ToDto(storageArea);
    }

    public async Task<StorageAreaResponseDto> UpdateOccupancyAsync(string id, UpdateOccupancyDto dto)
    {
        var storageArea = await _storageAreaRepo.GetByIdAsync(id);
        if (storageArea == null)
            throw new KeyNotFoundException($"Storage area with ID '{id}' not found.");

        storageArea.UpdateOccupancy(dto.NewOccupancyTEU);

        _storageAreaRepo.Update(storageArea);
        await _unitOfWork.CommitAsync();

        return StorageAreaMapper.ToDto(storageArea);
    }

    public async Task DeleteAsync(string id)
    {
        var storageArea = await _storageAreaRepo.GetByIdAsync(id);
        if (storageArea == null)
            throw new KeyNotFoundException($"Storage area with ID '{id}' not found.");

        if (storageArea.CurrentOccupancyTEU > 0)
        {
            throw new InvalidOperationException("Cannot delete storage area with current occupancy > 0.");
        }

        _storageAreaRepo.Remove(storageArea);
        await _unitOfWork.CommitAsync();
    }
}
