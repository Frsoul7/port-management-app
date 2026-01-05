import { Component, OnInit, inject, signal } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { Router, ActivatedRoute } from '@angular/router';
import { IncidentTypeService, IncidentType, CreateIncidentTypeRequest, UpdateIncidentTypeRequest } from '../../core/services/incident-type.service';

/**
 * US 4.1.12: Create/Edit Incident Type Form
 * Allows Port Authority Officers to manage incident types
 */
@Component({
  selector: 'app-incident-type-form',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './incident-type-form.component.html',
  styleUrls: ['./incident-type-form.component.scss']
})
export class IncidentTypeFormComponent implements OnInit {
  private incidentTypeService = inject(IncidentTypeService);
  private router = inject(Router);
  private route = inject(ActivatedRoute);

  isEditMode = signal(false);
  loading = signal(false);
  saving = signal(false);
  error = signal<string | null>(null);

  // Available parent types for hierarchy
  availableParentTypes = signal<IncidentType[]>([]);

  // Form model
  formData = {
    code: '',
    typeName: '',
    description: '',
    defaultSeverity: 'MEDIUM' as 'LOW' | 'MEDIUM' | 'HIGH' | 'CRITICAL',
    isParentType: false,
    parentTypeId: ''
  };

  incidentTypeId: string | null = null;

  severityOptions = [
    { value: 'LOW', label: 'Low' },
    { value: 'MEDIUM', label: 'Medium' },
    { value: 'HIGH', label: 'High' },
    { value: 'CRITICAL', label: 'Critical' }
  ];

  ngOnInit(): void {
    // Check if we're in edit mode
    this.incidentTypeId = this.route.snapshot.paramMap.get('id');
    if (this.incidentTypeId) {
      this.isEditMode.set(true);
      this.loadIncidentType(this.incidentTypeId);
    }

    // Load available parent types
    this.loadAvailableParentTypes();
  }

  /**
   * Load incident type for editing
   */
  async loadIncidentType(id: string): Promise<void> {
    this.loading.set(true);
    this.error.set(null);

    try {
      const response = await this.incidentTypeService.getIncidentTypeById(id).toPromise();
      if (response?.data) {
        const type = response.data;
        this.formData = {
          code: type.code,
          typeName: type.typeName,
          description: type.description,
          defaultSeverity: type.defaultSeverity,
          isParentType: type.isRootType || !type.parentTypeId,
          parentTypeId: type.parentTypeId || ''
        };
      }
    } catch (err: any) {
      this.error.set(err.error?.message || 'Failed to load incident type');
    } finally {
      this.loading.set(false);
    }
  }

  /**
   * Load available parent types (for hierarchy)
   * Only load root types as potential parents
   */
  async loadAvailableParentTypes(): Promise<void> {
    try {
      const response = await this.incidentTypeService.listIncidentTypes({ activeOnly: true }).toPromise();
      if (response?.data) {
        // Filter to only show root types (potential parents)
        // and exclude current type if editing
        let types = response.data.filter(t => t.isRootType);
        if (this.incidentTypeId) {
          types = types.filter(t => t.incidentTypeId !== this.incidentTypeId);
        }
        this.availableParentTypes.set(types);
      }
    } catch (err: any) {
      console.error('Failed to load parent types:', err);
    }
  }

  /**
   * Handle "Is Parent Type?" checkbox change
   */
  onIsParentTypeChange(): void {
    if (this.formData.isParentType) {
      // Clear parent type selection when marking as parent
      this.formData.parentTypeId = '';
    }
  }

  /**
   * Validate form data
   */
  validateForm(): string | null {
    if (!this.formData.code.trim()) {
      return 'Code is required';
    }
    // Validate code format: T-INC### (e.g., T-INC001)
    const codePattern = /^T-INC\d{3}$/;
    if (!codePattern.test(this.formData.code)) {
      return 'Code must follow format T-INC### (e.g., T-INC001)';
    }
    if (!this.formData.typeName.trim()) {
      return 'Type name is required';
    }
    if (!this.formData.description.trim()) {
      return 'Description is required';
    }
    
    // Validate parent type logic: either is parent OR has parent selected
    if (!this.formData.isParentType && !this.formData.parentTypeId) {
      return 'Please either check "Is Parent Type?" or select a Parent Type';
    }
    if (this.formData.isParentType && this.formData.parentTypeId) {
      return 'Cannot be a parent type and have a parent type at the same time';
    }
    
    return null;
  }

  /**
   * Submit form
   */
  async onSubmit(): Promise<void> {
    // Validate
    const validationError = this.validateForm();
    if (validationError) {
      this.error.set(validationError);
      return;
    }

    this.saving.set(true);
    this.error.set(null);

    try {
      if (this.isEditMode() && this.incidentTypeId) {
        // Update existing incident type
        const updateRequest: UpdateIncidentTypeRequest = {
          typeName: this.formData.typeName,
          description: this.formData.description,
          defaultSeverity: this.formData.defaultSeverity
        };
        await this.incidentTypeService.updateIncidentType(this.incidentTypeId, updateRequest).toPromise();
      } else {
        // Create new incident type
        const createRequest: CreateIncidentTypeRequest = {
          code: this.formData.code,
          typeName: this.formData.typeName,
          description: this.formData.description,
          defaultSeverity: this.formData.defaultSeverity,
          categoryCode: 'OPS', // Default category required by backend
          estimatedResolutionTimeHours: 24, // Default value required by backend
          parentTypeId: this.formData.isParentType ? undefined : this.formData.parentTypeId
        };
        await this.incidentTypeService.createIncidentType(createRequest).toPromise();
      }

      // Navigate back to list
      this.router.navigate(['/incident-types']);
    } catch (err: any) {
      // Handle specific error messages
      let errorMessage = `Failed to ${this.isEditMode() ? 'update' : 'create'} incident type`;
      
      // Log full error for debugging
      console.error('Full error object:', err);
      console.error('Error message:', err.message);
      console.error('Error error:', err.error);
      
      // Check multiple possible error locations
      const backendMessage = err.error?.message || err.error?.error || err.message || '';
      const lowerMessage = backendMessage.toLowerCase();
      
      // Check for specific duplicate errors
      if (lowerMessage.includes('already exists') || lowerMessage.includes('duplicate')) {
        // Check if it's about the type name
        if (lowerMessage.includes('typename') || lowerMessage.includes('type name') || lowerMessage.includes('name')) {
          errorMessage = `Type name "${this.formData.typeName}" is already in use. Please choose a different unique name.`;
        } 
        // Otherwise assume it's about the code
        else if (lowerMessage.includes('code')) {
          errorMessage = `Code "${this.formData.code}" is already in use. Please choose a different unique code.`;
        }
        // Fallback if we can't determine which field
        else {
          errorMessage = `This incident type already exists. Please use unique values for both code and type name.`;
        }
      } else if (backendMessage) {
        errorMessage = backendMessage;
      }
      
      this.error.set(errorMessage);
    } finally {
      this.saving.set(false);
    }
  }

  /**
   * Cancel and go back
   */
  onCancel(): void {
    this.router.navigate(['/incident-types']);
  }
}
