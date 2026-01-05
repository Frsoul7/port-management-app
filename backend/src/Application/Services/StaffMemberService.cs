using DDDNetCore.Application.DTOs.HumanResources;
using DDDNetCore.Application.Interfaces;
using DDDNetCore.Domain.HumanResources;
using DDDNetCore.Domain.IRepository;
using DDDNetCore.Infrastructure.Mappers;

namespace DDDNetCore.Application.Services;

public class StaffMemberService : IStaffMemberService
{
    private readonly IStaffMemberRepository _staffRepo;
    private readonly IQualificationRepository _qualificationRepo;
    private readonly IUnitOfWork _unitOfWork;

    public StaffMemberService(
        IStaffMemberRepository staffRepo,
        IQualificationRepository qualificationRepo,
        IUnitOfWork unitOfWork)
    {
        _staffRepo = staffRepo;
        _qualificationRepo = qualificationRepo;
        _unitOfWork = unitOfWork;
    }

    public async Task<StaffMemberResponseDto> CreateAsync(CreateStaffMemberDto dto)
    {
        // Validation
        if (string.IsNullOrWhiteSpace(dto.ShortName))
            throw new ArgumentException("ShortName is required");
        if (string.IsNullOrWhiteSpace(dto.Email))
            throw new ArgumentException("Email is required");
        if (string.IsNullOrWhiteSpace(dto.Phone))
            throw new ArgumentException("Phone is required");

        // Check if already exists
        var exists = await _staffRepo.ExistsAsync(dto.MecanographicNumber);
        if (exists)
            throw new InvalidOperationException("A staff member with this MecanographicNumber already exists.");

        // Parse TimeSpan
        if (!TimeSpan.TryParse(dto.StartHour, out TimeSpan startHour))
            throw new ArgumentException("Invalid StartHour format. Use 'HH:mm:ss' or 'HH:mm'");

        if (!TimeSpan.TryParse(dto.EndHour, out TimeSpan endHour))
            throw new ArgumentException("Invalid EndHour format. Use 'HH:mm:ss' or 'HH:mm'");

        // Create entity
        var staffMember = new StaffMember(
            mecanographicNumber: dto.MecanographicNumber,
            shortName: dto.ShortName,
            email: dto.Email,
            phone: dto.Phone,
            status: dto.Status,
            startHour: startHour,
            endHour: endHour
        );

        // Persist
        await _staffRepo.AddAsync(staffMember);
        await _unitOfWork.CommitAsync();

        // Return DTO
        return StaffMemberMapper.ToDto(staffMember);
    }

    public async Task<List<StaffMemberResponseDto>> GetAllAsync()
    {
        var staffMembers = await _staffRepo.ListAsync();
        return staffMembers.Select(StaffMemberMapper.ToDto).ToList();
    }

    public async Task<StaffMemberResponseDto?> GetByIdAsync(long mecanographicNumber)
    {
        var staffMember = await _staffRepo.GetByMecanographicNumberAsync(mecanographicNumber);
        return staffMember == null ? null : StaffMemberMapper.ToDto(staffMember);
    }

    public async Task<List<StaffMemberResponseDto>> GetByStatusAsync(HumanResourceStatus status)
    {
        var staffMembers = await _staffRepo.GetByStatusAsync(status);
        return staffMembers.Select(StaffMemberMapper.ToDto).ToList();
    }

    public async Task<List<StaffMemberResponseDto>> SearchAsync(string? name, HumanResourceStatus? status, string? qualification)
    {
        // Get all staff members
        var allStaff = await _staffRepo.ListAsync();

        // Apply filters
        if (!string.IsNullOrWhiteSpace(name))
            allStaff = allStaff.Where(sm => sm.ShortName.Contains(name, StringComparison.OrdinalIgnoreCase)).ToList();

        if (status.HasValue)
            allStaff = allStaff.Where(sm => sm.Status == status.Value).ToList();

        if (!string.IsNullOrWhiteSpace(qualification))
            allStaff = allStaff.Where(sm => sm.Qualifications.Any(q =>
                q.QualificationId.Contains(qualification.ToUpperInvariant()) ||
                q.Name.Contains(qualification, StringComparison.OrdinalIgnoreCase)
            )).ToList();

        return allStaff.Select(StaffMemberMapper.ToDto).ToList();
    }

    public async Task<StaffMemberResponseDto> UpdateAsync(long mecanographicNumber, UpdateStaffMemberDto dto)
    {
        var staffMember = await _staffRepo.GetByMecanographicNumberAsync(mecanographicNumber);
        if (staffMember == null)
            throw new KeyNotFoundException($"Staff member with ID {mecanographicNumber} not found");

        // Update contact info
        if (!string.IsNullOrWhiteSpace(dto.Email) && !string.IsNullOrWhiteSpace(dto.Phone))
        {
            staffMember.UpdateContactInfo(dto.Email, dto.Phone);
        }

        // Update operational status
        if (dto.Status.HasValue)
        {
            staffMember.UpdateOperationalStatus(dto.Status.Value);
        }

        // Update working hours
        if (dto.StartHour.HasValue && dto.EndHour.HasValue)
        {
            staffMember.UpdateWorkingHours(dto.StartHour.Value, dto.EndHour.Value);
        }

        _staffRepo.Update(staffMember);
        await _unitOfWork.CommitAsync();

        return StaffMemberMapper.ToDto(staffMember);
    }

    public async Task<StaffMemberResponseDto> PatchAsync(long mecanographicNumber, PatchStaffStatusDto dto)
    {
        var staffMember = await _staffRepo.GetByMecanographicNumberAsync(mecanographicNumber);
        if (staffMember == null)
            throw new KeyNotFoundException($"Staff member with ID {mecanographicNumber} not found");

        // Update contact info if provided
        if (!string.IsNullOrWhiteSpace(dto.Email) && !string.IsNullOrWhiteSpace(dto.Phone))
        {
            staffMember.UpdateContactInfo(dto.Email, dto.Phone);
        }

        // Update activity status if provided (handles activate/deactivate)
        if (dto.ActivityStatus.HasValue)
        {
            staffMember.UpdateActiveStatus(dto.ActivityStatus.Value);
        }

        // Update operational status if provided
        if (dto.Status.HasValue)
        {
            staffMember.UpdateOperationalStatus(dto.Status.Value);
        }

        // Update working hours if provided
        if (dto.StartHour.HasValue && dto.EndHour.HasValue)
        {
            staffMember.UpdateWorkingHours(dto.StartHour.Value, dto.EndHour.Value);
        }

        _staffRepo.Update(staffMember);
        await _unitOfWork.CommitAsync();

        return StaffMemberMapper.ToDto(staffMember);
    }

    public async Task<StaffMemberResponseDto> AddQualificationAsync(long mecanographicNumber, string qualificationId)
    {
        var staffMember = await _staffRepo.GetByMecanographicNumberAsync(mecanographicNumber);
        if (staffMember == null)
            throw new KeyNotFoundException("Staff member not found");

        var qualification = await _qualificationRepo.GetByIdAsync(qualificationId.ToUpperInvariant());
        if (qualification == null)
            throw new KeyNotFoundException("Qualification does not exist.");

        // Check if already has this qualification
        if (staffMember.Qualifications.Any(q => q.QualificationId == qualificationId.ToUpperInvariant()))
            throw new InvalidOperationException("Staff member already has this qualification.");

        staffMember.AddQualification(qualification);
        _staffRepo.Update(staffMember);
        await _unitOfWork.CommitAsync();

        return StaffMemberMapper.ToDto(staffMember);
    }

    public async Task RemoveQualificationAsync(long mecanographicNumber, string qualificationId)
    {
        var staffMember = await _staffRepo.GetByMecanographicNumberAsync(mecanographicNumber);
        if (staffMember == null)
            throw new KeyNotFoundException("Staff member not found");

        var qualification = staffMember.Qualifications
            .FirstOrDefault(q => q.QualificationId == qualificationId.ToUpperInvariant());

        if (qualification == null)
            throw new KeyNotFoundException("Staff member does not have this qualification.");

        staffMember.RemoveQualification(qualification);
        _staffRepo.Update(staffMember);
        await _unitOfWork.CommitAsync();
    }
}
