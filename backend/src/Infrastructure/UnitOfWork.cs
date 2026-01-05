using System;
using System.Threading.Tasks;
using DDDNetCore.Domain.IRepository;
using DDDNetCore.Application.Interfaces;
using DDDNetCore.Infrastructure.Repositories;
using Microsoft.EntityFrameworkCore.Storage;

namespace DDDNetCore.Infrastructure;

/// <summary>
/// Unit of Work implementation - manages repositories and database transactions.
/// Ensures all database operations are coordinated and can be committed/rolled back as a single unit.
/// </summary>
public class UnitOfWork : IUnitOfWork
{
    private readonly PortDbContext _context;
    private IDbContextTransaction? _transaction;
    
    // Lazy-loaded repository instances
    private IVesselRepository? _vessels;
    private IOrganizationRepository? _organizations;
    private IUserRepository? _users;
    private IVesselTypeRepository? _vesselTypes;
    private IDockRepository? _docks;
    private IStorageAreaRepository? _storageAreas;
    private IPhysicalResourceRepository? _physicalResources;
    private IStaffMemberRepository? _staffMembers;
    private IQualificationRepository? _qualifications;
    private IVesselVisitNotificationRepository? _vesselVisitNotifications;
    private IPrivacyPolicyRepository? _privacyPolicies;

    public UnitOfWork(PortDbContext context)
    {
        _context = context ?? throw new ArgumentNullException(nameof(context));
    }

    // ==================== Repository Properties ====================
    // Lazy initialization - repositories are created only when accessed
    
    public IVesselRepository Vessels => 
        _vessels ??= new VesselRepository(_context);
    
    public IOrganizationRepository Organizations => 
        _organizations ??= new OrganizationRepository(_context);
    
    public IUserRepository Users =>
        _users ??= new UserRepository(_context);
    
    public IVesselTypeRepository VesselTypes => 
        _vesselTypes ??= new VesselTypeRepository(_context);
    
    public IDockRepository Docks => 
        _docks ??= new DockRepository(_context);
    
    public IStorageAreaRepository StorageAreas => 
        _storageAreas ??= new StorageAreaRepository(_context);
    
    public IPhysicalResourceRepository PhysicalResources => 
        _physicalResources ??= new PhysicalResourceRepository(_context);
    
    public IStaffMemberRepository StaffMembers => 
        _staffMembers ??= new StaffMemberRepository(_context);
    
    public IQualificationRepository Qualifications => 
        _qualifications ??= new QualificationRepository(_context);
    
    public IVesselVisitNotificationRepository VesselVisitNotifications => 
        _vesselVisitNotifications ??= new VesselVisitNotificationRepository(_context);
    
    public IPrivacyPolicyRepository PrivacyPolicies => 
        _privacyPolicies ??= new PrivacyPolicyRepository(_context);
    
    /// <summary>
    /// Provides access to DbContext for entities without repositories (temporary).
    /// Use only for DockAssignments and DecisionLogs.
    /// </summary>
    public PortDbContext Context => _context;

    // ==================== Transaction Management ====================
    
    /// <summary>
    /// Commits all pending changes to the database.
    /// This is the primary method for saving changes.
    /// </summary>
    public async Task<int> CommitAsync()
    {
        return await _context.SaveChangesAsync();
    }

    /// <summary>
    /// Saves changes to the database (used within explicit transactions).
    /// For most scenarios, use CommitAsync() instead.
    /// </summary>
    public async Task<int> SaveChangesAsync()
    {
        return await _context.SaveChangesAsync();
    }

    /// <summary>
    /// Begins a new database transaction.
    /// Use when you need all-or-nothing behavior across multiple operations.
    /// Example:
    ///   await unitOfWork.BeginTransactionAsync();
    ///   try {
    ///     await unitOfWork.Vessels.AddAsync(vessel);
    ///     await unitOfWork.Docks.UpdateAsync(dock);
    ///     await unitOfWork.CommitAsync();
    ///     await unitOfWork.CommitTransactionAsync();
    ///   }
    ///   catch {
    ///     await unitOfWork.RollbackAsync();
    ///     throw;
    ///   }
    /// </summary>
    public async Task BeginTransactionAsync()
    {
        if (_transaction != null)
        {
            throw new InvalidOperationException("A transaction is already in progress.");
        }
        
        _transaction = await _context.Database.BeginTransactionAsync();
    }

    /// <summary>
    /// Commits the current transaction.
    /// Must be called after BeginTransactionAsync() and CommitAsync().
    /// </summary>
    public async Task CommitTransactionAsync()
    {
        if (_transaction == null)
        {
            throw new InvalidOperationException("No transaction in progress to commit.");
        }

        try
        {
            await _transaction.CommitAsync();
        }
        catch
        {
            await RollbackAsync();
            throw;
        }
        finally
        {
            await _transaction.DisposeAsync();
            _transaction = null;
        }
    }

    /// <summary>
    /// Rolls back the current transaction, undoing all changes.
    /// Typically called in exception handlers.
    /// </summary>
    public async Task RollbackAsync()
    {
        if (_transaction == null)
        {
            throw new InvalidOperationException("No transaction in progress to rollback.");
        }

        try
        {
            await _transaction.RollbackAsync();
        }
        finally
        {
            await _transaction.DisposeAsync();
            _transaction = null;
        }
    }

    // ==================== IDisposable ====================
    
    public void Dispose()
    {
        _transaction?.Dispose();
        _context.Dispose();
    }
}
