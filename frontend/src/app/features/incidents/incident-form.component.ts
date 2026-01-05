import { Component, OnInit, signal, inject } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { Router, ActivatedRoute } from '@angular/router';
import { IncidentService, IncidentSeverity, Incident } from '../../core/services/incident.service';
import { IncidentTypeService, IncidentType } from '../../core/services/incident-type.service';

/**
 * Incident Form Component
 * US 4.1.13: Create and edit incidents
 */
@Component({
  selector: 'app-incident-form',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './incident-form.component.html',
  styleUrls: ['./incident-form.component.scss']
})
export class IncidentFormComponent implements OnInit {
  private incidentService = inject(IncidentService);
  private incidentTypeService = inject(IncidentTypeService);
  router = inject(Router);
  private route = inject(ActivatedRoute);

  isEditMode = signal(false);
  incidentId = signal<string | null>(null);
  loading = signal(false);
  saving = signal(false);
  errorMessage = signal<string | null>(null);
  successMessage = signal<string | null>(null);

  // Available options
  incidentTypes = signal<IncidentType[]>([]);
  severities: IncidentSeverity[] = ['LOW', 'MEDIUM', 'HIGH', 'CRITICAL'];
  
  // Temporary input for external entities
  newEntity = '';

  // Form data
  formData = {
    incidentTypeId: '',
    vveId: '',
    title: '',
    description: '',
    severity: 'MEDIUM' as IncidentSeverity,
    reportedBy: '',
    impactDescription: '',
    externalEntitiesInvolved: [] as string[],
    externalEntityInput: '' // Temporary input field for adding entities
  };

  ngOnInit(): void {
    // Load incident types
    this.loadIncidentTypes();

    // Check if edit mode
    const id = this.route.snapshot.paramMap.get('id');
    if (id) {
      this.isEditMode.set(true);
      this.incidentId.set(id);
      this.loadIncident(id);
    }
  }

  /**
   * Load available incident types
   */
  private loadIncidentTypes(): void {
    this.incidentTypeService.listIncidentTypes().subscribe({
      next: (response) => {
        // Filter only active, leaf types (non-parent types)
        const activeLeafTypes = response.data.filter(
          (type) => type.isActive && !type.isRootType
        );
        this.incidentTypes.set(activeLeafTypes);
      },
      error: (error) => {
        console.error('Error loading incident types', error);
        this.errorMessage.set('Failed to load incident types');
      }
    });
  }

  /**
   * Load incident for editing
   */
  private loadIncident(id: string): void {
    this.loading.set(true);
    this.incidentService.getIncidentById(id).subscribe({
      next: (response) => {
        const incident = response.data;
        this.formData = {
          incidentTypeId: incident.incidentTypeId,
          vveId: incident.vveId || '',
          title: incident.title,
          description: incident.description,
          severity: incident.severity,
          reportedBy: incident.reportedBy,
          impactDescription: incident.impactDescription || '',
          externalEntitiesInvolved: [...incident.externalEntitiesInvolved],
          externalEntityInput: ''
        };
        this.loading.set(false);
      },
      error: (error) => {
        console.error('Error loading incident', error);
        this.errorMessage.set('Failed to load incident');
        this.loading.set(false);
      }
    });
  }

  /**
   * Add external entity
   */
  addExternalEntity(): void {
    const entity = this.formData.externalEntityInput.trim();
    if (entity && !this.formData.externalEntitiesInvolved.includes(entity)) {
      this.formData.externalEntitiesInvolved.push(entity);
      this.formData.externalEntityInput = '';
    }
  }

  /**
   * Remove external entity
   */
  removeExternalEntity(index: number): void {
    this.formData.externalEntitiesInvolved.splice(index, 1);
  }

  /**
   * Validate form
   */
  private validateForm(): boolean {
    if (!this.formData.incidentTypeId) {
      this.errorMessage.set('Please select an incident type');
      return false;
    }

    if (!this.formData.title || this.formData.title.trim().length === 0) {
      this.errorMessage.set('Please enter a title');
      return false;
    }

    if (this.formData.title.trim().length > 200) {
      this.errorMessage.set('Title must not exceed 200 characters');
      return false;
    }

    if (!this.formData.description || this.formData.description.trim().length === 0) {
      this.errorMessage.set('Please enter a description');
      return false;
    }

    if (!this.formData.reportedBy || this.formData.reportedBy.trim().length === 0) {
      this.errorMessage.set('Please enter reporter information');
      return false;
    }

    return true;
  }

  /**
   * Submit form
   */
  onSubmit(): void {
    this.errorMessage.set(null);
    this.successMessage.set(null);

    if (!this.validateForm()) {
      return;
    }

    this.saving.set(true);

    if (this.isEditMode()) {
      this.updateIncident();
    } else {
      this.createIncident();
    }
  }

  /**
   * Create new incident
   */
  private createIncident(): void {
    const request = {
      incidentTypeId: this.formData.incidentTypeId,
      vveId: this.formData.vveId || undefined,
      title: this.formData.title.trim(),
      description: this.formData.description.trim(),
      severity: this.formData.severity,
      reportedBy: this.formData.reportedBy.trim(),
      impactDescription: this.formData.impactDescription.trim() || undefined,
      externalEntitiesInvolved:
        this.formData.externalEntitiesInvolved.length > 0
          ? this.formData.externalEntitiesInvolved
          : undefined
    };

    this.incidentService.createIncident(request).subscribe({
      next: (response) => {
        this.successMessage.set('Incident created successfully');
        this.saving.set(false);
        setTimeout(() => {
          this.router.navigate(['/incidents-records']);
        }, 1500);
      },
      error: (error) => {
        console.error('Error creating incident', error);
        const errorMsg = error.error?.message || 'Failed to create incident';
        this.errorMessage.set(errorMsg);
        this.saving.set(false);
      }
    });
  }

  /**
   * Update existing incident
   */
  private updateIncident(): void {
    const request = {
      title: this.formData.title.trim(),
      description: this.formData.description.trim(),
      severity: this.formData.severity,
      impactDescription: this.formData.impactDescription.trim() || undefined
    };

    this.incidentService.updateIncident(this.incidentId()!, request).subscribe({
      next: (response) => {
        this.successMessage.set('Incident updated successfully');
        this.saving.set(false);
        setTimeout(() => {
          this.router.navigate(['/incidents-records']);
        }, 1500);
      },
      error: (error) => {
        console.error('Error updating incident', error);
        const errorMsg = error.error?.message || 'Failed to update incident';
        this.errorMessage.set(errorMsg);
        this.saving.set(false);
      }
    });
  }

  /**
   * Cancel and go back
   */
  onCancel(): void {
    this.router.navigate(['/incidents-records']);
  }

  /**
   * Get severity badge class
   */
  getSeverityClass(severity: IncidentSeverity): string {
    const classes: Record<IncidentSeverity, string> = {
      LOW: 'severity-low',
      MEDIUM: 'severity-medium',
      HIGH: 'severity-high',
      CRITICAL: 'severity-critical'
    };
    return classes[severity];
  }
}
