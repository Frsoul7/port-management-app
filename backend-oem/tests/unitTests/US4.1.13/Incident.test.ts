import { Incident } from '@domain/entities/Incident';
import { IncidentStatus, IncidentSeverity } from '@shared/types';

/**
 * US 4.1.13 - Incident Entity Unit Tests
 * Tests for the Incident aggregate root - incident reporting and lifecycle management
 * 
 * Coverage:
 * - Incident creation and validation
 * - Status transitions (REPORTED → UNDER_INVESTIGATION → RESOLVED → CLOSED)
 * - Investigation workflow
 * - Resolution workflow
 * - External entity management
 * - Notes and documentation
 * - Business rule enforcement
 */
describe('US 4.1.13 - Incident Entity', () => {
  /**
   * Helper function to create a valid incident
   * US 4.1.13: Default incident with valid required fields
   */
  const createValidIncident = (overrides?: Partial<ConstructorParameters<typeof Incident>[0]>) => {
    return new Incident({
      incidentTypeId: 'type-001',
      title: 'Equipment Malfunction',
      description: 'Crane #3 hydraulic system failure during loading operation',
      severity: IncidentSeverity.HIGH,
      reportedBy: 'operator-123',
      ...overrides,
    });
  };

  describe('Creation and Validation', () => {
    /**
     * US 4.1.13: Test incident creation with all required fields
     * Verifies:
     * - Automatic ID generation
     * - Initial status = REPORTED
     * - Automatic timestamp
     * - Required fields are stored correctly
     */
    it('should create a valid incident with REPORTED status', () => {
      const incident = createValidIncident();

      expect(incident.incidentId).toBeDefined();
      expect(incident.incidentTypeId).toBe('type-001');
      expect(incident.title).toBe('Equipment Malfunction');
      expect(incident.description).toBe('Crane #3 hydraulic system failure during loading operation');
      expect(incident.severity).toBe(IncidentSeverity.HIGH);
      expect(incident.status).toBe(IncidentStatus.REPORTED);
      expect(incident.reportedBy).toBe('operator-123');
      expect(incident.reportedAt).toBeInstanceOf(Date);
      expect(incident.externalEntitiesInvolved).toEqual([]);
      expect(incident.notes).toEqual([]);
    });

    /**
     * US 4.1.13: Test optional VVE linking
     * Verifies:
     * - Incidents can be linked to vessel visit executions
     * - VVE ID is optional for general incidents
     */
    it('should create incident with optional VVE link', () => {
      const incident = createValidIncident({ vveId: 'VVE-001' });

      expect(incident.vveId).toBe('VVE-001');
    });

    /**
     * US 4.1.13: Test incident type ID validation
     * Business Rule: Incident must reference a valid incident type
     */
    it('should throw error if incident type ID is missing', () => {
      expect(() => {
        new Incident({
          incidentTypeId: '',
          title: 'Test',
          description: 'Test description',
          severity: IncidentSeverity.MEDIUM,
          reportedBy: 'user-123',
        });
      }).toThrow('Incident type ID is required');
    });

    /**
     * US 4.1.13: Test title validation
     * Business Rule: Title must not be empty
     */
    it('should throw error if title is empty', () => {
      expect(() => {
        new Incident({
          incidentTypeId: 'type-001',
          title: '   ',
          description: 'Test description',
          severity: IncidentSeverity.MEDIUM,
          reportedBy: 'user-123',
        });
      }).toThrow('Incident title is required');
    });

    /**
     * US 4.1.13: Test title length validation
     * Business Rule: Title must not exceed 200 characters
     */
    it('should throw error if title exceeds 200 characters', () => {
      expect(() => {
        new Incident({
          incidentTypeId: 'type-001',
          title: 'A'.repeat(201),
          description: 'Test description',
          severity: IncidentSeverity.MEDIUM,
          reportedBy: 'user-123',
        });
      }).toThrow('Incident title must not exceed 200 characters');
    });

    /**
     * US 4.1.13: Test description validation
     * Business Rule: Description must not be empty
     */
    it('should throw error if description is empty', () => {
      expect(() => {
        new Incident({
          incidentTypeId: 'type-001',
          title: 'Test',
          description: '',
          severity: IncidentSeverity.MEDIUM,
          reportedBy: 'user-123',
        });
      }).toThrow('Incident description is required');
    });

    /**
     * US 4.1.13: Test reporter validation
     * Business Rule: Reporter information must be provided
     */
    it('should throw error if reporter is missing', () => {
      expect(() => {
        new Incident({
          incidentTypeId: 'type-001',
          title: 'Test',
          description: 'Test description',
          severity: IncidentSeverity.MEDIUM,
          reportedBy: '',
        });
      }).toThrow('Reporter information is required');
    });

    /**
     * US 4.1.13: Test status transition validation on creation
     * Business Rule: Creating with UNDER_INVESTIGATION requires investigation timestamp
     */
    it('should throw error if created with UNDER_INVESTIGATION without investigation timestamp', () => {
      expect(() => {
        new Incident({
          incidentTypeId: 'type-001',
          title: 'Test',
          description: 'Test description',
          severity: IncidentSeverity.MEDIUM,
          reportedBy: 'user-123',
          status: IncidentStatus.UNDER_INVESTIGATION,
        });
      }).toThrow('Investigation timestamp required for UNDER_INVESTIGATION status');
    });

    /**
     * US 4.1.13: Test status transition validation on creation
     * Business Rule: Creating with RESOLVED requires resolution timestamp and summary
     */
    it('should throw error if created with RESOLVED without resolution data', () => {
      expect(() => {
        new Incident({
          incidentTypeId: 'type-001',
          title: 'Test',
          description: 'Test description',
          severity: IncidentSeverity.MEDIUM,
          reportedBy: 'user-123',
          status: IncidentStatus.RESOLVED,
        });
      }).toThrow('Resolution timestamp and summary required for RESOLVED status');
    });

    /**
     * US 4.1.13: Test status transition validation on creation
     * Business Rule: Creating with CLOSED requires close timestamp
     */
    it('should throw error if created with CLOSED without close timestamp', () => {
      expect(() => {
        new Incident({
          incidentTypeId: 'type-001',
          title: 'Test',
          description: 'Test description',
          severity: IncidentSeverity.MEDIUM,
          reportedBy: 'user-123',
          status: IncidentStatus.CLOSED,
        });
      }).toThrow('Close timestamp required for CLOSED status');
    });
  });

  describe('Investigation Workflow', () => {
    /**
     * US 4.1.13: Test starting investigation
     * Verifies:
     * - Status changes to UNDER_INVESTIGATION
     * - Investigation timestamp is set
     * - Investigator is recorded
     */
    it('should start investigation on REPORTED incident', () => {
      const incident = createValidIncident();

      incident.startInvestigation('investigator-456');

      expect(incident.status).toBe(IncidentStatus.UNDER_INVESTIGATION);
      expect(incident.investigatedBy).toBe('investigator-456');
      expect(incident.investigatedAt).toBeInstanceOf(Date);
    });

    /**
     * US 4.1.13: Test investigation workflow validation
     * Business Rule: Can only investigate REPORTED incidents
     */
    it('should throw error if trying to investigate non-REPORTED incident', () => {
      const incident = createValidIncident();
      incident.startInvestigation('investigator-456');

      expect(() => {
        incident.startInvestigation('another-investigator');
      }).toThrow('Cannot start investigation for incident with status UNDER_INVESTIGATION');
    });

    /**
     * US 4.1.13: Test investigator validation
     * Business Rule: Investigator information is required
     */
    it('should throw error if investigator is empty', () => {
      const incident = createValidIncident();

      expect(() => {
        incident.startInvestigation('   ');
      }).toThrow('Investigator information is required');
    });
  });

  describe('Resolution Workflow', () => {
    /**
     * US 4.1.13: Test resolving incident
     * Verifies:
     * - Status changes to RESOLVED
     * - Resolution timestamp is set
     * - Resolver and summary are recorded
     */
    it('should resolve incident UNDER_INVESTIGATION', () => {
      const incident = createValidIncident();
      incident.startInvestigation('investigator-456');

      incident.resolve('resolver-789', 'Root cause: Hydraulic fluid leak. Fixed by replacing seal.');

      expect(incident.status).toBe(IncidentStatus.RESOLVED);
      expect(incident.resolvedBy).toBe('resolver-789');
      expect(incident.resolvedAt).toBeInstanceOf(Date);
      expect(incident.resolutionSummary).toBe('Root cause: Hydraulic fluid leak. Fixed by replacing seal.');
    });

    /**
     * US 4.1.13: Test resolution workflow validation
     * Business Rule: Can only resolve incidents UNDER_INVESTIGATION
     */
    it('should throw error if trying to resolve REPORTED incident', () => {
      const incident = createValidIncident();

      expect(() => {
        incident.resolve('resolver-789', 'Test resolution');
      }).toThrow('Cannot resolve incident with status REPORTED');
    });

    /**
     * US 4.1.13: Test resolution workflow validation
     * Business Rule: Can only resolve incidents UNDER_INVESTIGATION
     */
    it('should throw error if trying to resolve already RESOLVED incident', () => {
      const incident = createValidIncident();
      incident.startInvestigation('investigator-456');
      incident.resolve('resolver-789', 'First resolution');

      expect(() => {
        incident.resolve('another-resolver', 'Second resolution');
      }).toThrow('Cannot resolve incident with status RESOLVED');
    });

    /**
     * US 4.1.13: Test resolver validation
     * Business Rule: Resolver information is required
     */
    it('should throw error if resolver is empty', () => {
      const incident = createValidIncident();
      incident.startInvestigation('investigator-456');

      expect(() => {
        incident.resolve('', 'Test resolution');
      }).toThrow('Resolver information is required');
    });

    /**
     * US 4.1.13: Test resolution summary validation
     * Business Rule: Resolution summary is required
     */
    it('should throw error if resolution summary is empty', () => {
      const incident = createValidIncident();
      incident.startInvestigation('investigator-456');

      expect(() => {
        incident.resolve('resolver-789', '   ');
      }).toThrow('Resolution summary is required');
    });

    /**
     * US 4.1.13: Test resolution summary length validation
     * Business Rule: Resolution summary must not exceed 1000 characters
     */
    it('should throw error if resolution summary exceeds 1000 characters', () => {
      const incident = createValidIncident();
      incident.startInvestigation('investigator-456');

      expect(() => {
        incident.resolve('resolver-789', 'A'.repeat(1001));
      }).toThrow('Resolution summary must not exceed 1000 characters');
    });
  });

  describe('Closure Workflow', () => {
    /**
     * US 4.1.13: Test closing incident
     * Verifies:
     * - Status changes to CLOSED
     * - Close timestamp is set
     * - Closer is recorded
     */
    it('should close RESOLVED incident', () => {
      const incident = createValidIncident();
      incident.startInvestigation('investigator-456');
      incident.resolve('resolver-789', 'Test resolution');

      incident.close('closer-012');

      expect(incident.status).toBe(IncidentStatus.CLOSED);
      expect(incident.closedBy).toBe('closer-012');
      expect(incident.closedAt).toBeInstanceOf(Date);
    });

    /**
     * US 4.1.13: Test closure workflow validation
     * Business Rule: Can only close RESOLVED incidents
     */
    it('should throw error if trying to close non-RESOLVED incident', () => {
      const incident = createValidIncident();

      expect(() => {
        incident.close('closer-012');
      }).toThrow('Cannot close incident with status REPORTED');
    });

    /**
     * US 4.1.13: Test closer validation
     * Business Rule: Closer information is required
     */
    it('should throw error if closer is empty', () => {
      const incident = createValidIncident();
      incident.startInvestigation('investigator-456');
      incident.resolve('resolver-789', 'Test resolution');

      expect(() => {
        incident.close('   ');
      }).toThrow('Closer information is required');
    });
  });

  describe('Notes Management', () => {
    /**
     * US 4.1.13: Test adding investigation notes
     * Verifies:
     * - Notes are added with timestamp
     * - Author and content are stored
     * - Multiple notes can be added
     */
    it('should add note to incident', () => {
      const incident = createValidIncident();

      incident.addNote('user-123', 'Initial assessment completed');

      expect(incident.notes).toHaveLength(1);
      expect(incident.notes[0]?.author).toBe('user-123');
      expect(incident.notes[0]?.content).toBe('Initial assessment completed');
      expect(incident.notes[0]?.timestamp).toBeInstanceOf(Date);
    });

    /**
     * US 4.1.13: Test adding multiple notes
     * Verifies: Notes are appended in chronological order
     */
    it('should add multiple notes in chronological order', () => {
      const incident = createValidIncident();

      incident.addNote('user-123', 'First note');
      incident.addNote('user-456', 'Second note');
      incident.addNote('user-789', 'Third note');

      expect(incident.notes).toHaveLength(3);
      expect(incident.notes[0]?.content).toBe('First note');
      expect(incident.notes[1]?.content).toBe('Second note');
      expect(incident.notes[2]?.content).toBe('Third note');
    });

    /**
     * US 4.1.13: Test notes on closed incidents
     * Business Rule: Cannot add notes to CLOSED incidents
     */
    it('should throw error if adding note to CLOSED incident', () => {
      const incident = createValidIncident();
      incident.startInvestigation('investigator-456');
      incident.resolve('resolver-789', 'Test resolution');
      incident.close('closer-012');

      expect(() => {
        incident.addNote('user-123', 'Cannot add this note');
      }).toThrow('Cannot add notes to closed incidents');
    });

    /**
     * US 4.1.13: Test note author validation
     * Business Rule: Note author is required
     */
    it('should throw error if note author is empty', () => {
      const incident = createValidIncident();

      expect(() => {
        incident.addNote('', 'Test content');
      }).toThrow('Note author is required');
    });

    /**
     * US 4.1.13: Test note content validation
     * Business Rule: Note content is required
     */
    it('should throw error if note content is empty', () => {
      const incident = createValidIncident();

      expect(() => {
        incident.addNote('user-123', '   ');
      }).toThrow('Note content is required');
    });

    /**
     * US 4.1.13: Test note content length validation
     * Business Rule: Note content must not exceed 500 characters
     */
    it('should throw error if note content exceeds 500 characters', () => {
      const incident = createValidIncident();

      expect(() => {
        incident.addNote('user-123', 'A'.repeat(501));
      }).toThrow('Note content must not exceed 500 characters');
    });
  });

  describe('Severity Management', () => {
    /**
     * US 4.1.13: Test updating severity
     * Verifies:
     * - Severity can be escalated or de-escalated
     * - Can update before resolution
     */
    it('should update severity on open incident', () => {
      const incident = createValidIncident({ severity: IncidentSeverity.MEDIUM });

      incident.updateSeverity(IncidentSeverity.CRITICAL);

      expect(incident.severity).toBe(IncidentSeverity.CRITICAL);
    });

    /**
     * US 4.1.13: Test severity update during investigation
     * Verifies: Severity can be updated while investigating
     */
    it('should update severity during investigation', () => {
      const incident = createValidIncident({ severity: IncidentSeverity.LOW });
      incident.startInvestigation('investigator-456');

      incident.updateSeverity(IncidentSeverity.HIGH);

      expect(incident.severity).toBe(IncidentSeverity.HIGH);
    });

    /**
     * US 4.1.13: Test severity lock after resolution
     * Business Rule: Cannot update severity of resolved incidents
     */
    it('should throw error if updating severity on RESOLVED incident', () => {
      const incident = createValidIncident();
      incident.startInvestigation('investigator-456');
      incident.resolve('resolver-789', 'Test resolution');

      expect(() => {
        incident.updateSeverity(IncidentSeverity.CRITICAL);
      }).toThrow('Cannot update severity of resolved or closed incidents');
    });

    /**
     * US 4.1.13: Test severity lock after closure
     * Business Rule: Cannot update severity of closed incidents
     */
    it('should throw error if updating severity on CLOSED incident', () => {
      const incident = createValidIncident();
      incident.startInvestigation('investigator-456');
      incident.resolve('resolver-789', 'Test resolution');
      incident.close('closer-012');

      expect(() => {
        incident.updateSeverity(IncidentSeverity.LOW);
      }).toThrow('Cannot update severity of resolved or closed incidents');
    });
  });

  describe('External Entity Management', () => {
    /**
     * US 4.1.13: Test involving external entities
     * Verifies:
     * - External entities can be added
     * - Examples: Coast Guard, Fire Department, Police
     */
    it('should involve external entity', () => {
      const incident = createValidIncident();

      incident.involveExternalEntity('Coast Guard');

      expect(incident.externalEntitiesInvolved).toContain('Coast Guard');
      expect(incident.hasExternalEntities()).toBe(true);
    });

    /**
     * US 4.1.13: Test involving multiple external entities
     * Verifies: Multiple entities can be involved
     */
    it('should involve multiple external entities', () => {
      const incident = createValidIncident();

      incident.involveExternalEntity('Coast Guard');
      incident.involveExternalEntity('Fire Department');
      incident.involveExternalEntity('Port Police');

      expect(incident.externalEntitiesInvolved).toHaveLength(3);
      expect(incident.externalEntitiesInvolved).toContain('Coast Guard');
      expect(incident.externalEntitiesInvolved).toContain('Fire Department');
      expect(incident.externalEntitiesInvolved).toContain('Port Police');
    });

    /**
     * US 4.1.13: Test duplicate external entity prevention
     * Business Rule: Same entity cannot be involved twice
     */
    it('should throw error if involving same entity twice', () => {
      const incident = createValidIncident();
      incident.involveExternalEntity('Coast Guard');

      expect(() => {
        incident.involveExternalEntity('Coast Guard');
      }).toThrow('External entity Coast Guard is already involved');
    });

    /**
     * US 4.1.13: Test external entity validation
     * Business Rule: Entity name is required
     */
    it('should throw error if external entity name is empty', () => {
      const incident = createValidIncident();

      expect(() => {
        incident.involveExternalEntity('   ');
      }).toThrow('External entity name is required');
    });

    /**
     * US 4.1.13: Test external entity modification on closed incidents
     * Business Rule: Cannot modify closed incidents
     */
    it('should throw error if involving external entity on CLOSED incident', () => {
      const incident = createValidIncident();
      incident.startInvestigation('investigator-456');
      incident.resolve('resolver-789', 'Test resolution');
      incident.close('closer-012');

      expect(() => {
        incident.involveExternalEntity('Coast Guard');
      }).toThrow('Cannot modify closed incidents');
    });

    /**
     * US 4.1.13: Test removing external entity
     * Verifies: External entities can be removed
     */
    it('should remove external entity', () => {
      const incident = createValidIncident();
      incident.involveExternalEntity('Coast Guard');
      incident.involveExternalEntity('Fire Department');

      incident.removeExternalEntity('Coast Guard');

      expect(incident.externalEntitiesInvolved).toHaveLength(1);
      expect(incident.externalEntitiesInvolved).not.toContain('Coast Guard');
      expect(incident.externalEntitiesInvolved).toContain('Fire Department');
    });

    /**
     * US 4.1.13: Test removing non-existent external entity
     * Business Rule: Cannot remove entity that is not involved
     */
    it('should throw error if removing non-involved entity', () => {
      const incident = createValidIncident();

      expect(() => {
        incident.removeExternalEntity('Coast Guard');
      }).toThrow('External entity Coast Guard is not involved');
    });

    /**
     * US 4.1.13: Test removing external entity from closed incident
     * Business Rule: Cannot modify closed incidents
     */
    it('should throw error if removing external entity from CLOSED incident', () => {
      const incident = createValidIncident();
      incident.involveExternalEntity('Coast Guard');
      incident.startInvestigation('investigator-456');
      incident.resolve('resolver-789', 'Test resolution');
      incident.close('closer-012');

      expect(() => {
        incident.removeExternalEntity('Coast Guard');
      }).toThrow('Cannot modify closed incidents');
    });
  });

  describe('Impact Description', () => {
    /**
     * US 4.1.13: Test setting impact description
     * Verifies:
     * - Impact can describe operational effects
     * - Examples: "Delayed loading by 3 hours", "Suspended operations"
     */
    it('should set impact description', () => {
      const incident = createValidIncident();

      incident.setImpactDescription('Loading operations delayed by 3 hours. Vessel departure postponed.');

      expect(incident.impactDescription).toBe('Loading operations delayed by 3 hours. Vessel departure postponed.');
    });

    /**
     * US 4.1.13: Test updating impact description
     * Verifies: Impact description can be updated
     */
    it('should update impact description', () => {
      const incident = createValidIncident();
      incident.setImpactDescription('Initial impact assessment');

      incident.setImpactDescription('Updated impact: 5-hour delay confirmed');

      expect(incident.impactDescription).toBe('Updated impact: 5-hour delay confirmed');
    });

    /**
     * US 4.1.13: Test clearing impact description
     * Verifies: Impact description can be cleared
     */
    it('should clear impact description when set to empty', () => {
      const incident = createValidIncident();
      incident.setImpactDescription('Initial impact');

      incident.setImpactDescription('');

      expect(incident.impactDescription).toBeUndefined();
    });

    /**
     * US 4.1.13: Test impact description length validation
     * Business Rule: Impact description must not exceed 1000 characters
     */
    it('should throw error if impact description exceeds 1000 characters', () => {
      const incident = createValidIncident();

      expect(() => {
        incident.setImpactDescription('A'.repeat(1001));
      }).toThrow('Impact description must not exceed 1000 characters');
    });

    /**
     * US 4.1.13: Test impact description on closed incident
     * Business Rule: Cannot modify closed incidents
     */
    it('should throw error if setting impact description on CLOSED incident', () => {
      const incident = createValidIncident();
      incident.startInvestigation('investigator-456');
      incident.resolve('resolver-789', 'Test resolution');
      incident.close('closer-012');

      expect(() => {
        incident.setImpactDescription('Cannot set this');
      }).toThrow('Cannot modify closed incidents');
    });
  });

  describe('Query Methods', () => {
    /**
     * US 4.1.13: Test critical incident detection
     * Verifies: isCritical() returns true for CRITICAL severity
     */
    it('should identify critical incidents', () => {
      const critical = createValidIncident({ severity: IncidentSeverity.CRITICAL });
      const high = createValidIncident({ severity: IncidentSeverity.HIGH });

      expect(critical.isCritical()).toBe(true);
      expect(high.isCritical()).toBe(false);
    });

    /**
     * US 4.1.13: Test resolved incident detection
     * Verifies: isResolved() returns true for RESOLVED or CLOSED
     */
    it('should identify resolved incidents', () => {
      const reported = createValidIncident();
      const underInvestigation = createValidIncident();
      underInvestigation.startInvestigation('investigator-456');
      const resolved = createValidIncident();
      resolved.startInvestigation('investigator-456');
      resolved.resolve('resolver-789', 'Test resolution');
      const closed = createValidIncident();
      closed.startInvestigation('investigator-456');
      closed.resolve('resolver-789', 'Test resolution');
      closed.close('closer-012');

      expect(reported.isResolved()).toBe(false);
      expect(underInvestigation.isResolved()).toBe(false);
      expect(resolved.isResolved()).toBe(true);
      expect(closed.isResolved()).toBe(true);
    });

    /**
     * US 4.1.13: Test closed incident detection
     * Verifies: isClosed() returns true only for CLOSED status
     */
    it('should identify closed incidents', () => {
      const reported = createValidIncident();
      const resolved = createValidIncident();
      resolved.startInvestigation('investigator-456');
      resolved.resolve('resolver-789', 'Test resolution');
      const closed = createValidIncident();
      closed.startInvestigation('investigator-456');
      closed.resolve('resolver-789', 'Test resolution');
      closed.close('closer-012');

      expect(reported.isClosed()).toBe(false);
      expect(resolved.isClosed()).toBe(false);
      expect(closed.isClosed()).toBe(true);
    });

    /**
     * US 4.1.13: Test open incident detection
     * Verifies: isOpen() returns true for REPORTED or UNDER_INVESTIGATION
     */
    it('should identify open incidents', () => {
      const reported = createValidIncident();
      const underInvestigation = createValidIncident();
      underInvestigation.startInvestigation('investigator-456');
      const resolved = createValidIncident();
      resolved.startInvestigation('investigator-456');
      resolved.resolve('resolver-789', 'Test resolution');

      expect(reported.isOpen()).toBe(true);
      expect(underInvestigation.isOpen()).toBe(true);
      expect(resolved.isOpen()).toBe(false);
    });

    /**
     * US 4.1.13: Test external entity involvement detection
     * Verifies: hasExternalEntities() returns true when entities are involved
     */
    it('should detect external entity involvement', () => {
      const withoutEntities = createValidIncident();
      const withEntities = createValidIncident();
      withEntities.involveExternalEntity('Coast Guard');

      expect(withoutEntities.hasExternalEntities()).toBe(false);
      expect(withEntities.hasExternalEntities()).toBe(true);
    });
  });

  describe('Time Calculations', () => {
    /**
     * US 4.1.13: Test time to resolution calculation
     * Verifies:
     * - Returns null for unresolved incidents
     * - Calculates hours between report and resolution
     */
    it('should return null for unresolved incident', () => {
      const incident = createValidIncident();

      expect(incident.getTimeToResolutionHours()).toBeNull();
    });

    /**
     * US 4.1.13: Test time to resolution calculation
     * Verifies: Returns hours for resolved incidents
     */
    it('should calculate time to resolution in hours', () => {
      const reportedAt = new Date('2026-01-01T10:00:00Z');
      const incident = createValidIncident({ reportedAt });
      incident.startInvestigation('investigator-456');
      
      // Manually set resolved date for testing (2 hours later)
      const resolvedIncident = new Incident({
        incidentTypeId: 'type-001',
        title: 'Test',
        description: 'Test',
        severity: IncidentSeverity.MEDIUM,
        reportedBy: 'user-123',
        reportedAt,
        status: IncidentStatus.RESOLVED,
        investigatedAt: new Date('2026-01-01T10:30:00Z'),
        investigatedBy: 'investigator-456',
        resolvedAt: new Date('2026-01-01T12:00:00Z'),
        resolvedBy: 'resolver-789',
        resolutionSummary: 'Test resolution',
      });

      const hours = resolvedIncident.getTimeToResolutionHours();
      expect(hours).toBe(2);
    });
  });

  describe('Serialization', () => {
    /**
     * US 4.1.13: Test JSON serialization
     * Verifies:
     * - All properties are included
     * - Dates are converted to ISO strings
     * - Arrays are preserved
     */
    it('should serialize to JSON correctly', () => {
      const incident = createValidIncident({ vveId: 'VVE-001' });
      incident.involveExternalEntity('Coast Guard');
      incident.addNote('user-123', 'Test note');

      const json = incident.toJSON();

      expect(json.incidentId).toBe(incident.incidentId);
      expect(json.incidentTypeId).toBe('type-001');
      expect(json.vveId).toBe('VVE-001');
      expect(json.title).toBe('Equipment Malfunction');
      expect(json.severity).toBe(IncidentSeverity.HIGH);
      expect(json.status).toBe(IncidentStatus.REPORTED);
      expect(json.reportedBy).toBe('operator-123');
      expect(json.reportedAt).toMatch(/^\d{4}-\d{2}-\d{2}T/);
      expect(json.externalEntitiesInvolved).toEqual(['Coast Guard']);
      expect(json.notes).toHaveLength(1);
      expect(json.notes[0]?.author).toBe('user-123');
      expect(json.notes[0]?.content).toBe('Test note');
      expect(json.notes[0]?.timestamp).toMatch(/^\d{4}-\d{2}-\d{2}T/);
    });

    /**
     * US 4.1.13: Test complete lifecycle serialization
     * Verifies: All lifecycle timestamps are serialized
     */
    it('should serialize complete lifecycle to JSON', () => {
      const incident = createValidIncident();
      incident.startInvestigation('investigator-456');
      incident.resolve('resolver-789', 'Test resolution');
      incident.close('closer-012');

      const json = incident.toJSON();

      expect(json.status).toBe(IncidentStatus.CLOSED);
      expect(json.investigatedAt).toBeDefined();
      expect(json.investigatedBy).toBe('investigator-456');
      expect(json.resolvedAt).toBeDefined();
      expect(json.resolvedBy).toBe('resolver-789');
      expect(json.resolutionSummary).toBe('Test resolution');
      expect(json.closedAt).toBeDefined();
      expect(json.closedBy).toBe('closer-012');
    });
  });
});
