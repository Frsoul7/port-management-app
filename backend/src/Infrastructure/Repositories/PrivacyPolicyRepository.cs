using DDDNetCore.Domain.IRepository;
using DDDNetCore.Domain.PrivacyPolicy;
using DDDNetCore.Domain.Users;
using Microsoft.EntityFrameworkCore;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace DDDNetCore.Infrastructure.Repositories
{
    /// <summary>
    /// Repository implementation for Privacy Policy aggregate.
    /// US 4.5.1: Privacy Policy management with versioning and acknowledgment tracking.
    /// </summary>
    public class PrivacyPolicyRepository : BaseRepository<PrivacyPolicy>, IPrivacyPolicyRepository
    {
        private readonly DbSet<PrivacyPolicyAcknowledgment> _acknowledgments;

        public PrivacyPolicyRepository(PortDbContext ctx) : base(ctx)
        {
            _acknowledgments = ctx.Set<PrivacyPolicyAcknowledgment>();
        }

        // ===== Privacy Policy Operations =====

        public async Task<PrivacyPolicy?> GetCurrentAsync(string languageCode = "pt")
        {
            return await _dbSet
                .Where(p => p.IsActive && p.LanguageCode == languageCode)
                .FirstOrDefaultAsync();
        }

        public async Task<PrivacyPolicy?> GetByIdAsync(PrivacyPolicyId policyId)
        {
            return await _dbSet
                .FirstOrDefaultAsync(p => p.PolicyId == policyId);
        }

        public async Task<List<PrivacyPolicy>> GetHistoryAsync(string? languageCode = null)
        {
            var query = _dbSet.AsQueryable();
            
            if (!string.IsNullOrEmpty(languageCode))
            {
                query = query.Where(p => p.LanguageCode == languageCode);
            }
            
            return await query
                .OrderByDescending(p => p.EffectiveDate)
                .ThenByDescending(p => p.CreatedAt)
                .ToListAsync();
        }

        public new async Task AddAsync(PrivacyPolicy policy)
        {
            await base.AddAsync(policy);
        }

        public async Task UpdateAsync(PrivacyPolicy policy)
        {
            _dbSet.Update(policy);
            await Task.CompletedTask;
        }

        public async Task DeactivateAllAsync(string languageCode)
        {
            var activePolicies = await _dbSet
                .Where(p => p.IsActive && p.LanguageCode == languageCode)
                .ToListAsync();

            foreach (var policy in activePolicies)
            {
                policy.Deactivate();
            }
        }

        // ===== Acknowledgment Operations =====

        public async Task<PrivacyPolicyAcknowledgment?> GetUserAcknowledgmentAsync(UserId userId, PrivacyPolicyId policyId)
        {
            return await _acknowledgments
                .Include(a => a.Policy)
                .FirstOrDefaultAsync(a => a.UserId == userId && a.PolicyId == policyId);
        }

        public async Task<PrivacyPolicyAcknowledgment?> GetLatestUserAcknowledgmentAsync(UserId userId)
        {
            return await _acknowledgments
                .Include(a => a.Policy)
                .Where(a => a.UserId == userId)
                .OrderByDescending(a => a.AcknowledgedAt)
                .FirstOrDefaultAsync();
        }

        public async Task<bool> HasUserAcknowledgedCurrentPolicyAsync(UserId userId, string languageCode = "pt")
        {
            var currentPolicy = await GetCurrentAsync(languageCode);
            if (currentPolicy == null)
            {
                // No active policy - no acknowledgment required
                return true;
            }

            return await _acknowledgments
                .AnyAsync(a => a.UserId == userId && a.PolicyId == currentPolicy.PolicyId);
        }

        public async Task AddAcknowledgmentAsync(PrivacyPolicyAcknowledgment acknowledgment)
        {
            await _acknowledgments.AddAsync(acknowledgment);
        }

        public async Task<List<PrivacyPolicyAcknowledgment>> GetAcknowledgmentsForPolicyAsync(PrivacyPolicyId policyId)
        {
            return await _acknowledgments
                .Where(a => a.PolicyId == policyId)
                .OrderByDescending(a => a.AcknowledgedAt)
                .ToListAsync();
        }
    }
}
