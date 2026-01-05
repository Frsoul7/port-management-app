using DDDNetCore.Domain.IRepository;
using DDDNetCore.Domain.Users;
using Microsoft.EntityFrameworkCore;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace DDDNetCore.Infrastructure.Repositories
{
    public class UserRepository : BaseRepository<User>, IUserRepository
    {
        public UserRepository(PortDbContext ctx) : base(ctx)
        {
        }

        public async Task<User?> GetByEmailAsync(string email)
        {
            return await _dbSet
                .Include(u => u.Organization)
                .FirstOrDefaultAsync(u => u.Email == email);
        }

        public async Task<User?> GetByIdAsync(UserId userId)
        {
            return await _dbSet
                .Include(u => u.Organization)
                .FirstOrDefaultAsync(u => u.UserId == userId);
        }

        public async Task<List<User>> GetAllAsync()
        {
            return await _dbSet
                .Include(u => u.Organization)
                .ToListAsync();
        }

        public new async Task AddAsync(User user)
        {
            await base.AddAsync(user);
        }

        public async Task UpdateAsync(User user)
        {
            _dbSet.Update(user);
            await Task.CompletedTask;
        }
    }
}
