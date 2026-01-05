using System.Linq;
using DDDNetCore.Application.DTOs.HumanResources;
using DDDNetCore.Application.Interfaces;
using DDDNetCore.Domain.HumanResources;
using DDDNetCore.Domain.IRepository;
using DDDNetCore.Infrastructure.Mappers;

namespace DDDNetCore.Application.Services;

public class QualificationService : IQualificationService
{
    private readonly IQualificationRepository _qualificationRepo;
    private readonly IUnitOfWork _unitOfWork;

    public QualificationService(IQualificationRepository qualificationRepo, IUnitOfWork unitOfWork)
    {
        _qualificationRepo = qualificationRepo;
        _unitOfWork = unitOfWork;
    }

    public async Task<QualificationDto> CreateAsync(CreateQualificationDto dto)
    {
        // Validate required fields
        if (string.IsNullOrWhiteSpace(dto.QualificationId))
            throw new ArgumentException("QualificationId is required");
        
        if (string.IsNullOrWhiteSpace(dto.Name))
            throw new ArgumentException("Name is required");

        // Check if qualification already exists
        var exists = await _qualificationRepo.ExistsAsync(dto.QualificationId.ToUpperInvariant());
        if (exists)
            throw new InvalidOperationException("A qualification with this code already exists.");

        // Create qualification entity
        var qualification = new StaffMemberQualification(dto.QualificationId, dto.Name, dto.Description);

        // Persist
        await _qualificationRepo.AddAsync(qualification);
        await _unitOfWork.CommitAsync();

        return QualificationMapper.ToDto(qualification);
    }

    public async Task<List<QualificationDto>> SearchAsync(string? id, string? name)
    {
        var qualifications = await _qualificationRepo.SearchAsync(id, name);
        return qualifications.Select(QualificationMapper.ToDto).ToList();
    }

    public async Task<QualificationDto> UpdateAsync(string id, UpdateQualificationDto dto)
    {
        var qualification = await _qualificationRepo.GetByIdAsync(id.ToUpperInvariant());
        
        if (qualification == null)
            throw new KeyNotFoundException($"Qualification with ID '{id}' not found.");

        // Update qualification
        qualification.Update(dto.Name, dto.Description);
        _qualificationRepo.Update(qualification);
        await _unitOfWork.CommitAsync();

        return QualificationMapper.ToDto(qualification);
    }

    public async Task DeleteAsync(string id)
    {
        var qualification = await _qualificationRepo.GetByIdAsync(id.ToUpperInvariant());
        
        if (qualification == null)
            throw new KeyNotFoundException($"Qualification with ID '{id}' not found.");

        _qualificationRepo.Remove(qualification);
        await _unitOfWork.CommitAsync();
    }
}
