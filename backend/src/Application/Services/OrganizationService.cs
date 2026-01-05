using System;
using System.Collections.Generic;
using System.Linq;
using System.Security.Cryptography;
using System.Threading.Tasks;
using DDDNetCore.Domain.IRepository;
using DDDNetCore.Application.Interfaces;
using DDDNetCore.Domain.Organizations;
using DDDNetCore.Domain.Users;
using DDDNetCore.Application.DTOs.Organizations;
using DDDNetCore.Application.Validators;
using DDDNetCore.Infrastructure.Mappers;
using Microsoft.EntityFrameworkCore;

namespace DDDNetCore.Application.Services
{
    /// <summary>
    /// Application service for organization business logic.
    /// Orchestrates domain operations and enforces business rules for organizations.
    /// </summary>
    public class OrganizationService : IOrganizationService
    {
        private readonly IUnitOfWork _unitOfWork;
        private readonly DDDNetCore.Domain.Factory.IOrganizationFactory _organizationFactory;
        private readonly IEmailService _emailService;
        private readonly Microsoft.Extensions.Logging.ILogger<OrganizationService> _logger;
        private readonly Microsoft.Extensions.Configuration.IConfiguration _configuration;

        public OrganizationService(
            IUnitOfWork unitOfWork, 
            DDDNetCore.Domain.Factory.IOrganizationFactory organizationFactory,
            IEmailService emailService,
            Microsoft.Extensions.Logging.ILogger<OrganizationService> logger,
            Microsoft.Extensions.Configuration.IConfiguration configuration)
        {
            _unitOfWork = unitOfWork;
            _organizationFactory = organizationFactory;
            _emailService = emailService;
            _logger = logger;
            _configuration = configuration;
        }

        public async Task<List<OrganizationResponseDto>> GetAllOrganizationsAsync()
        {
            var organizations = await _unitOfWork.Organizations.ListAsync();
            return organizations.Select(OrganizationMapper.ToDto).ToList();
        }

        public async Task<OrganizationResponseDto?> GetOrganizationByIdAsync(string id)
        {
            var organization = await _unitOfWork.Organizations.GetByIdAsync(id);
            return organization == null ? null : OrganizationMapper.ToDto(organization);
        }

        public async Task<OrganizationResponseDto> CreateOrganizationAsync(CreateOrganizationDto dto)
        {
            // Validate identifier format
            if (string.IsNullOrWhiteSpace(dto.Identifier))
                throw new ArgumentException("Identifier is required.", nameof(dto.Identifier));

            var trimmedId = dto.Identifier.Trim();
            if (trimmedId.Length > 10)
                throw new ArgumentException("Identifier max length is 10.", nameof(dto.Identifier));

            if (!System.Text.RegularExpressions.Regex.IsMatch(trimmedId, "^[A-Za-z0-9]+$"))
                throw new ArgumentException("Identifier must be alphanumeric.", nameof(dto.Identifier));

            var normalizedId = trimmedId.ToUpperInvariant();

            // Must have alternative name
            if (string.IsNullOrWhiteSpace(dto.AlternativeName))
                throw new ArgumentException("AlternativeName is required.", nameof(dto.AlternativeName));

            // Check uniqueness
            if (await _unitOfWork.Organizations.ExistsWithIdentifierAsync(normalizedId))
                throw new InvalidOperationException("An organization with this identifier already exists.");

            // Shipping agent must have representatives
            if (dto.Type == OrganizationType.SHIPPING_AGENT &&
                (dto.Representatives == null || dto.Representatives.Count == 0))
            {
                throw new ArgumentException("Shipping Agent must include at least one representative.");
            }

            // Validate representatives
            if (dto.Representatives != null && dto.Representatives.Count > 0)
            {
                ValidateRepresentativesDto(dto.Representatives);
                await ValidateRepresentativesUniquenessAsync(dto.Representatives);
            }

            // Create representatives
            var reps = dto.Representatives?.Select(r => new Representative(
                r.Name.Trim(),
                r.CitizenId.Trim(),
                r.Nationality.Trim(),
                r.Email.Trim(),
                (r.Phone ?? string.Empty).Trim()
            )).ToList();

            // Create organization using factory (validates and normalizes identifier)
            var org = _organizationFactory.Create(
                Guid.NewGuid(),
                dto.Identifier,
                dto.LegalName,
                dto.AlternativeName.Trim(),
                dto.Address,
                dto.TaxNumber,
                dto.Type,
                reps
            );

            await _unitOfWork.Organizations.AddAsync(org);
            await _unitOfWork.CommitAsync();

            // Auto-create user accounts for representatives (SHIPPING_AGENT only)
            if (org.Type == OrganizationType.SHIPPING_AGENT && reps != null && reps.Any())
            {
                _logger.LogInformation("Auto-creating user accounts for {Count} representatives of organization {OrgName}", 
                    reps.Count, org.LegalName);

                var frontendUrl = Environment.GetEnvironmentVariable("FRONTEND_URL")
                    ?? _configuration["Frontend:Url"]
                    ?? "http://vm.nunoepteixeira.me";

                foreach (var rep in reps)
                {
                    try
                    {
                        // Create user with pre-assigned role and organization
                        var userId = Guid.NewGuid();
                        var user = new User(
                            id: userId,
                            name: rep.Name,
                            email: rep.Email,
                            organizationId: new OrganizationId(org.Id),
                            role: UserRole.SHIPPING_AGENT_REPRESENTATIVE,
                            profilePictureUrl: null
                        );

                        // Generate activation token (24 hours validity)
                        var activationToken = GenerateActivationToken();
                        var tokenExpiry = DateTime.UtcNow.AddHours(24);
                        user.SetActivationToken(activationToken, tokenExpiry);

                        _logger.LogInformation("Creating user account for representative {Email} with pre-assigned role SHIPPING_AGENT_REPRESENTATIVE", 
                            rep.Email);

                        await _unitOfWork.Users.AddAsync(user);

                        // Send activation email
                        var activationLink = $"{frontendUrl}/auth/activate?token={activationToken}&email={Uri.EscapeDataString(rep.Email)}";
                        var emailSent = await _emailService.SendActivationEmailAsync(rep.Email, rep.Name, activationLink);
                        
                        if (!emailSent)
                        {
                            _logger.LogWarning("Failed to send activation email to {Email}, but user was created", rep.Email);
                        }
                        else
                        {
                            _logger.LogInformation("Activation email sent successfully to {Email}", rep.Email);
                        }
                    }
                    catch (Exception ex)
                    {
                        _logger.LogError(ex, "Failed to create user account for representative {Email}", rep.Email);
                        // Continue with next representative instead of failing entire operation
                    }
                }

                await _unitOfWork.CommitAsync();
                _logger.LogInformation("Successfully created user accounts for representatives of organization {OrgName}", org.LegalName);
            }

            return OrganizationMapper.ToDto(org);
        }

        public async Task<OrganizationResponseDto> AddRepresentativesByIdAsync(Guid organizationId, AddRepresentativesDto dto)
        {
            var org = await _unitOfWork.Organizations.GetByIdWithRepresentativesAsync(organizationId.ToString());

            if (org == null)
                throw new KeyNotFoundException("Organization not found.");

            if (org.Type != OrganizationType.SHIPPING_AGENT)
                throw new ArgumentException("Representatives can only be added to SHIPPING_AGENT organizations.");

            return await AddRepresentativesCoreAsync(org, dto);
        }

        public async Task<OrganizationResponseDto> AddRepresentativesByIdentifierAsync(string identifier, AddRepresentativesDto dto)
        {
            var code = (identifier ?? string.Empty).Trim().ToUpper();

            var org = await _unitOfWork.Organizations.GetByIdentifierWithRepresentativesAsync(code);

            if (org == null)
                throw new KeyNotFoundException("Organization not found.");

            if (org.Type != OrganizationType.SHIPPING_AGENT)
                throw new ArgumentException("Representatives can only be added to SHIPPING_AGENT organizations.");

            return await AddRepresentativesCoreAsync(org, dto);
        }

        // Private helper methods

        private async Task<OrganizationResponseDto> AddRepresentativesCoreAsync(Organization org, AddRepresentativesDto dto)
        {
            if (dto.Representatives == null || dto.Representatives.Count == 0)
                throw new ArgumentException("At least one representative is required.");

            ValidateRepresentativesDto(dto.Representatives);
            await ValidateRepresentativesUniquenessAsync(dto.Representatives);

            // Add representatives to organization
            var newReps = dto.Representatives.Select(repDto => new Representative(
                repDto.Name.Trim(),
                repDto.CitizenId.Trim(),
                repDto.Nationality.Trim(),
                repDto.Email.Trim(),
                (repDto.Phone ?? string.Empty).Trim()
            ));
            
            org.AddRepresentatives(newReps);

            _unitOfWork.Organizations.Update(org);
            await _unitOfWork.CommitAsync();

            return OrganizationMapper.ToDto(org);
        }

        private void ValidateRepresentativesDto(List<RepresentativeInputDto> representatives)
        {
            // Check for missing required fields
            foreach (var r in representatives)
            {
                if (string.IsNullOrWhiteSpace(r.Name))
                    throw new ArgumentException("Representative name is required.");
                if (string.IsNullOrWhiteSpace(r.CitizenId))
                    throw new ArgumentException("Representative citizen ID is required.");
                if (string.IsNullOrWhiteSpace(r.Nationality))
                    throw new ArgumentException("Representative nationality is required.");
                if (string.IsNullOrWhiteSpace(r.Email))
                    throw new ArgumentException("Representative email is required.");

                // Validate email format using centralized validator
                EmailValidator.ValidateAndThrow(r.Email, "Representative.Email");
            }

            // Validate nationality format (2-letter ISO code)
            var invalidNat = representatives.FirstOrDefault(r =>
                string.IsNullOrWhiteSpace(r.Nationality) || r.Nationality.Trim().Length != 2);
            if (invalidNat != null)
                throw new ArgumentException("Representative nationality must be a 2-letter ISO code.");
        }

        private async Task ValidateRepresentativesUniquenessAsync(List<RepresentativeInputDto> representatives)
        {
            // EMAIL: duplicates in payload (case-insensitive)
            var payloadEmails = representatives
                .Select(r => (r.Email ?? string.Empty).Trim().ToLowerInvariant())
                .Where(e => !string.IsNullOrEmpty(e))
                .ToList();

            var dupEmailsInPayload = payloadEmails
                .GroupBy(e => e)
                .Where(g => g.Count() > 1)
                .Select(g => g.Key)
                .ToList();

            if (dupEmailsInPayload.Count > 0)
                throw new ArgumentException($"Duplicate representative emails in request: {string.Join(", ", dupEmailsInPayload)}");

            // EMAIL: duplicates system-wide
            if (payloadEmails.Count > 0)
            {
                var allOrgs = await _unitOfWork.Organizations.ListAsync();
                var existingEmails = allOrgs
                    .SelectMany(o => o.Representatives)
                    .Where(r => payloadEmails.Contains(r.Email.ToLower()))
                    .Select(r => r.Email.ToLower())
                    .Distinct()
                    .ToList();

                if (existingEmails.Count > 0)
                    throw new InvalidOperationException($"One or more representative emails are already in use: {string.Join(", ", existingEmails)}");
            }

            // PHONE: duplicates in payload
            var payloadPhones = representatives
                .Select(r => (r.Phone ?? string.Empty).Trim())
                .Where(p => !string.IsNullOrEmpty(p))
                .ToList();

            var dupPhonesInPayload = payloadPhones
                .GroupBy(p => p)
                .Where(g => g.Count() > 1)
                .Select(g => g.Key)
                .ToList();

            if (dupPhonesInPayload.Count > 0)
                throw new ArgumentException($"Duplicate representative phone numbers in request: {string.Join(", ", dupPhonesInPayload)}");

            // PHONE: duplicates system-wide
            if (payloadPhones.Count > 0)
            {
                var allOrgs = await _unitOfWork.Organizations.ListAsync();
                var existingPhones = allOrgs
                    .SelectMany(o => o.Representatives)
                    .Where(r => !string.IsNullOrWhiteSpace(r.Phone) && payloadPhones.Contains(r.Phone))
                    .Select(r => r.Phone!)
                    .Distinct()
                    .ToList();

                if (existingPhones.Count > 0)
                    throw new InvalidOperationException($"One or more representative phone numbers are already in use: {string.Join(", ", existingPhones)}");
            }
        }

        public async Task<RepresentativeResponseDto> UpdateRepresentativeAsync(Guid organizationId, Guid repId, UpdateRepresentativeDto dto)
        {
            var org = await _unitOfWork.Organizations.GetByIdWithRepresentativesAsync(organizationId.ToString());
            if (org == null)
                throw new KeyNotFoundException("Organization not found.");

            if (org.Type != OrganizationType.SHIPPING_AGENT)
                throw new ArgumentException("Only SHIPPING_AGENT organizations have representatives.");

            var rep = org.Representatives.FirstOrDefault(r => r.RepresentativeId.Value == repId);
            if (rep == null)
                throw new KeyNotFoundException("Representative not found.");

            // Basic validation
            if (string.IsNullOrWhiteSpace(dto.Nationality) || dto.Nationality.Trim().Length != 2)
                throw new ArgumentException("Nationality must be a 2-letter ISO code.");

            var newEmail = dto.Email.Trim().ToLowerInvariant();
            var newPhone = (dto.Phone ?? string.Empty).Trim();

            // EMAIL uniqueness across system, excluding this rep
            var allOrgs = await _unitOfWork.Organizations.ListAsync();
            var emailClash = allOrgs
                .SelectMany(o => o.Representatives)
                .Any(r => r.RepresentativeId.Value != repId && r.Email.ToLower() == newEmail);
            if (emailClash)
                throw new InvalidOperationException("Email already in use by another representative.");

            // PHONE uniqueness across system (if provided), excluding this rep
            if (!string.IsNullOrEmpty(newPhone))
            {
                var phoneClash = allOrgs
                    .SelectMany(o => o.Representatives)
                    .Any(r => r.RepresentativeId.Value != repId && !string.IsNullOrEmpty(r.Phone) && r.Phone == newPhone);
                if (phoneClash)
                    throw new InvalidOperationException("Phone number already in use by another representative.");
            }

            // Apply updates
            typeof(Representative).GetProperty(nameof(Representative.Name))!.SetValue(rep, dto.Name.Trim());
            typeof(Representative).GetProperty(nameof(Representative.CitizenId))!.SetValue(rep, dto.CitizenId.Trim());
            typeof(Representative).GetProperty(nameof(Representative.Nationality))!.SetValue(rep, dto.Nationality.Trim().ToUpperInvariant());
            typeof(Representative).GetProperty(nameof(Representative.Email))!.SetValue(rep, dto.Email.Trim());
            typeof(Representative).GetProperty(nameof(Representative.Phone))!.SetValue(rep, newPhone);

            _unitOfWork.Organizations.Update(org);
            await _unitOfWork.CommitAsync();

            return RepresentativeMapper.ToDto(rep);
        }

        public async Task<RepresentativeResponseDto> PatchRepresentativeStatusAsync(Guid organizationId, Guid repId, PatchRepresentativeStatusDto dto)
        {
            var org = await _unitOfWork.Organizations.GetByIdWithRepresentativesAsync(organizationId.ToString());
            if (org == null)
                throw new KeyNotFoundException("Organization not found.");

            var rep = org.Representatives.FirstOrDefault(r => r.RepresentativeId.Value == repId);
            if (rep == null)
                throw new KeyNotFoundException("Representative not found.");

            // Handle status change if provided
            if (dto.IsActive.HasValue)
            {
                if (dto.IsActive.Value)
                    rep.Activate();
                else
                    rep.Deactivate();
            }

            _unitOfWork.Organizations.Update(org);
            await _unitOfWork.CommitAsync();

            return RepresentativeMapper.ToDto(rep);
        }

        /// <summary>
        /// Generate a cryptographically secure activation token
        /// </summary>
        private string GenerateActivationToken()
        {
            return Convert.ToBase64String(RandomNumberGenerator.GetBytes(32))
                .Replace("+", "-")
                .Replace("/", "_")
                .Replace("=", "");
        }
    }
}
