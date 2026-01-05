using System;
using System.Threading.Tasks;
using DDDNetCore.Domain.IRepository;

namespace DDDNetCore.Application.Interfaces;

/// <summary>
/// Unit of Work pattern - coordinates database operations and transactions.
/// Provides centralized access to all repositories and manages transaction lifecycle.
/// </summary>
public interface IUnitOfWork : IDisposable
{
    // ==================== Repository Access ====================
    // One property for each aggregate root repository
    
    /// <summary>Gets the Vessel repository</summary>
    IVesselRepository Vessels { get; }
    
    /// <summary>Gets the Organization repository</summary>
    IOrganizationRepository Organizations { get; }
    
    /// <summary>Gets the User repository</summary>
    IUserRepository Users { get; }
    
    /// <summary>Gets the VesselType repository</summary>
    IVesselTypeRepository VesselTypes { get; }
    
    /// <summary>Gets the Dock repository</summary>
    IDockRepository Docks { get; }
    
    /// <summary>Gets the StorageArea repository</summary>
    IStorageAreaRepository StorageAreas { get; }
    
    /// <summary>Gets the PhysicalResource repository</summary>
    IPhysicalResourceRepository PhysicalResources { get; }
    
    /// <summary>Gets the StaffMember repository</summary>
    IStaffMemberRepository StaffMembers { get; }
    
    /// <summary>Gets the Qualification repository</summary>
    IQualificationRepository Qualifications { get; }
    
    /// <summary>Gets the VesselVisitNotification repository</summary>
    IVesselVisitNotificationRepository VesselVisitNotifications { get; }
    
    /// <summary>Gets the PrivacyPolicy repository (GDPR compliance)</summary>
    IPrivacyPolicyRepository PrivacyPolicies { get; }
    
    /// <summary>
    /// Gets access to DbContext for entities without repositories (temporary - for refactoring).
    /// Use this only for DockAssignments and DecisionLogs until proper repositories are created.
    /// </summary>
    DDDNetCore.Infrastructure.PortDbContext Context { get; }
    
    // ==================== Transaction Management ====================
    
    /// <summary>
    /// Commits all pending changes to the database.
    /// Returns the number of entities affected.
    /// </summary>
    Task<int> CommitAsync();
    
    /// <summary>
    /// Begins a new database transaction.
    /// Use this when you need explicit transaction control (e.g., multiple operations that must succeed/fail together).
    /// </summary>
    Task BeginTransactionAsync();
    
    /// <summary>
    /// Commits the current transaction.
    /// Must be called after BeginTransactionAsync() and CommitAsync().
    /// </summary>
    Task CommitTransactionAsync();
    
    /// <summary>
    /// Rolls back the current transaction.
    /// Use this in exception handlers to undo changes.
    /// </summary>
    Task RollbackAsync();
    
    /// <summary>
    /// Saves changes without committing transaction (when inside explicit transaction).
    /// Most scenarios should use CommitAsync() instead.
    /// </summary>
    Task<int> SaveChangesAsync();
}
