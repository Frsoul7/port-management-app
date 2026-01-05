import { Component, OnInit, signal, inject } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { Router } from '@angular/router';
import { IncidentService, Incident, IncidentStatus, IncidentSeverity, IncidentFilters } from '../../core/services/incident.service';
import { IncidentTypeService, IncidentType } from '../../core/services/incident-type.service';

/**
 * US 4.1.13: List and Manage Incidents
 * Allows Logistics Operators to record and manage operational incidents
 */
@Component({
  selector: 'app-list-incidents',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './list-incidents.component.html',
  styleUrls: ['./list-incidents.component.scss']
})
export class ListIncidentsComponent implements OnInit {
  private incidentService = inject(IncidentService);
  private incidentTypeService = inject(IncidentTypeService);
  router = inject(Router);

  incidents = signal<Incident[]>([]);
  filteredIncidents = signal<Incident[]>([]);
  incidentTypes = signal<IncidentType[]>([]);
  loading = signal(false);
  errorMessage = signal<string | null>(null);

  // Filter options
  statuses: IncidentStatus[] = ['REPORTED', 'UNDER_INVESTIGATION', 'RESOLVED', 'CLOSED'];
  severities: IncidentSeverity[] = ['LOW', 'MEDIUM', 'HIGH', 'CRITICAL'];

  // Filter values
  filters = {
    status: '',
    severity: '',
    incidentTypeId: '',
    vveId: '',
    search: '',
    showActiveOnly: false
  };

  ngOnInit(): void {
    this.loadIncidentTypes();
    this.loadIncidents();
  }

  /**
   * Load incident types for filter
   */
  private loadIncidentTypes(): void {
    this.incidentTypeService.listIncidentTypes().subscribe({
      next: (response) => {
        this.incidentTypes.set(response.data.filter(t => t.isActive));
      },
      error: (error) => {
        console.error('Error loading incident types', error);
      }
    });
  }

  /**
   * Load all incidents
   */
  loadIncidents(): void {
    this.loading.set(true);
    this.errorMessage.set(null);

    const apiFilters: IncidentFilters = {};
    
    if (this.filters.showActiveOnly) {
      this.incidentService.getActiveIncidents().subscribe({
        next: (response) => {
          this.incidents.set(response.data);
          this.applyFilters();
          this.loading.set(false);
        },
        error: (error) => {
          console.error('Error loading incidents', error);
          this.errorMessage.set('Failed to load incidents');
          this.loading.set(false);
        }
      });
    } else {
      this.incidentService.listIncidents(apiFilters).subscribe({
        next: (response) => {
          this.incidents.set(response.data);
          this.applyFilters();
          this.loading.set(false);
        },
        error: (error) => {
          console.error('Error loading incidents', error);
          this.errorMessage.set('Failed to load incidents');
          this.loading.set(false);
        }
      });
    }
  }

  /**
   * Apply client-side filters
   */
  applyFilters(): void {
    let filtered = [...this.incidents()];

    if (this.filters.status) filtered = filtered.filter(i => i.status === this.filters.status);
    if (this.filters.severity) filtered = filtered.filter(i => i.severity === this.filters.severity);
    if (this.filters.incidentTypeId) filtered = filtered.filter(i => i.incidentTypeId === this.filters.incidentTypeId);
    if (this.filters.vveId) filtered = filtered.filter(i => i.vveId?.includes(this.filters.vveId));
    if (this.filters.search) {
      const search = this.filters.search.toLowerCase();
      filtered = filtered.filter(i => 
        i.title.toLowerCase().includes(search) || i.description.toLowerCase().includes(search)
      );
    }

    this.filteredIncidents.set(filtered);
  }

  /**
   * Clear filters
   */
  clearFilters(): void {
    this.filters = { status: '', severity: '', incidentTypeId: '', vveId: '', search: '', showActiveOnly: false };
    this.applyFilters();
  }

  /**
   * Navigate to create incident
   */
  createIncident(): void {
    this.router.navigate(['/incidents-records/create']);
  }

  viewIncident(id: string): void {
    this.router.navigate(['/incidents-records', id]);
  }

  editIncident(id: string): void {
    this.router.navigate(['/incidents-records/edit', id]);
  }

  deleteIncident(incident: Incident): void {
    if (!confirm(`Are you sure you want to delete incident "${incident.title}"?`)) return;

    this.incidentService.deleteIncident(incident.incidentId).subscribe({
      next: () => this.loadIncidents(),
      error: (error) => {
        console.error('Error deleting incident', error);
        alert('Failed to delete incident');
      }
    });
  }

  getStatusClass(status: IncidentStatus): string {
    const map: Record<IncidentStatus, string> = {
      REPORTED: 'status-reported', UNDER_INVESTIGATION: 'status-investigating',
      RESOLVED: 'status-resolved', CLOSED: 'status-closed'
    };
    return map[status];
  }

  getSeverityClass(severity: IncidentSeverity): string {
    const map: Record<IncidentSeverity, string> = {
      LOW: 'severity-low', MEDIUM: 'severity-medium', HIGH: 'severity-high', CRITICAL: 'severity-critical'
    };
    return map[severity];
  }

  formatDuration(minutes: number | null): string {
    if (!minutes) return 'N/A';
    const hours = Math.floor(minutes / 60);
    const mins = minutes % 60;
    return hours > 0 ? `${hours}h ${mins}m` : `${mins}m`;
  }

  isActive(incident: Incident): boolean {
    return incident.status === 'REPORTED' || incident.status === 'UNDER_INVESTIGATION';
  }
}
