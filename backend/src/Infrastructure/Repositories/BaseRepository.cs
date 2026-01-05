using Microsoft.EntityFrameworkCore;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace DDDNetCore.Infrastructure.Repositories
{
    public class BaseRepository<T> where T : class
    {
        protected readonly PortDbContext _ctx;
        protected readonly DbSet<T> _dbSet;

        public BaseRepository(PortDbContext ctx)
        {
            _ctx = ctx;
            _dbSet = ctx.Set<T>();
        }

        public virtual async Task AddAsync(T entity)
        {
            await _dbSet.AddAsync(entity);
        }

        public virtual void Update(T entity)
        {
            _dbSet.Update(entity);
        }

        public virtual void Remove(T entity)
        {
            _dbSet.Remove(entity);
        }

        public virtual async Task<T?> FindAsync(params object[] keys)
        {
            return await _dbSet.FindAsync(keys);
        }

        public virtual async Task<List<T>> ListAsync()
        {
            // Workaround for EF Core query compilation issue
            var query = _dbSet.AsQueryable();
            return await Task.FromResult(query.ToList());
        }
    }
}
