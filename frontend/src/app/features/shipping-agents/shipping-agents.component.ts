import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { Router } from '@angular/router';
import { OrganizationService } from '../../core/services/organization.service';
import { Organization, CreateOrganizationRequest, AddRepresentativesRequest, UpdateRepresentativeRequest, PatchRepresentativeStatusRequest } from '../../core/models/organization.model';
import { ShippingAgentSearchBarComponent } from './components/shipping-agent-search-bar/shipping-agent-search-bar.component';
import { ShippingAgentTableComponent } from './components/shipping-agent-table/shipping-agent-table.component';
import { ShippingAgentFormModalComponent } from './components/shipping-agent-form-modal/shipping-agent-form-modal.component';
import { ShippingAgentViewModalComponent } from './components/shipping-agent-view-modal/shipping-agent-view-modal.component';
import { RepresentativeFormModalComponent } from './components/representative-form-modal/representative-form-modal.component';
import { AlertBannerComponent } from '../vessels/components/alert-banner/alert-banner.component';
import { TranslatePipe } from '../../core/pipes/translate.pipe';

@Component({
  selector: 'app-shipping-agents',
  standalone: true,
  imports: [
    CommonModule,
    ShippingAgentSearchBarComponent,
    ShippingAgentTableComponent,
    ShippingAgentFormModalComponent,
    ShippingAgentViewModalComponent,
    RepresentativeFormModalComponent,
    AlertBannerComponent,
    TranslatePipe
  ],
  templateUrl: './shipping-agents.component.html',
  styleUrls: ['./shipping-agents.component.scss']
})
export class ShippingAgentsComponent implements OnInit {
  organizations: Organization[] = [];
  filteredOrganizations: Organization[] = [];

  showFormModal = false;
  showViewModal = false;
  showRepresentativeModal = false;
  isRepresentativeEditMode = false;
  loading = false;
  saving = false;
  errorMessage = '';
  successMessage = '';

  selectedOrganization: Organization | null = null;
  selectedRepresentative: any = null;

  constructor(
    private organizationService: OrganizationService,
    private router: Router
  ) {}

  ngOnInit(): void {
    this.loadOrganizations();
  }

  loadOrganizations(): void {
    this.loading = true;
    this.errorMessage = '';
    
    this.organizationService.getAllOrganizations().subscribe({
      next: (data: any[]) => {
        // Filter only shipping agents
        const shippingAgents = data.filter(org => org.type === 'SHIPPING_AGENT');
        
        // Load full details for each shipping agent
        const detailRequests = shippingAgents.map(org => 
          this.organizationService.getOrganizationById(String(org.organizationId))
        );
        
        Promise.all(detailRequests.map(req => req.toPromise()))
          .then(details => {
            this.organizations = details.filter(d => d !== undefined) as Organization[];
            this.filteredOrganizations = [...this.organizations];
            this.loading = false;
          })
          .catch(err => {
            console.error('Error loading organization details:', err);
            this.errorMessage = 'Failed to load shipping agent details.';
            this.loading = false;
          });
      },
      error: (err: any) => {
        console.error('Error loading organizations:', err);
        this.errorMessage = err.error?.error || 'Failed to load shipping agents. Please try again.';
        this.loading = false;
      }
    });
  }

  onSearch(filters: { identifier: string; legalName: string; taxNumber: string }): void {
    let filtered = this.organizations;

    if (filters.identifier) {
      const term = filters.identifier.toLowerCase();
      filtered = filtered.filter(org =>
        org.identifier.toLowerCase().includes(term)
      );
    }

    if (filters.legalName) {
      const term = filters.legalName.toLowerCase();
      filtered = filtered.filter(org =>
        org.legalName.toLowerCase().includes(term)
      );
    }

    if (filters.taxNumber) {
      const term = filters.taxNumber.toLowerCase();
      filtered = filtered.filter(org =>
        org.taxNumber.toLowerCase().includes(term)
      );
    }

    this.filteredOrganizations = filtered;
  }

  onView(organization: Organization): void {
    this.selectedOrganization = organization;
    this.showViewModal = true;
  }

  openCreateModal(): void {
    this.selectedOrganization = null;
    this.showFormModal = true;
  }

  closeFormModal(): void {
    this.showFormModal = false;
    this.selectedOrganization = null;
  }

  closeViewModal(): void {
    this.showViewModal = false;
    this.selectedOrganization = null;
  }

  closeRepresentativeModal(): void {
    this.showRepresentativeModal = false;
    this.selectedRepresentative = null;
    this.isRepresentativeEditMode = false;
  }

  onFormCreate(formData: CreateOrganizationRequest): void {
    this.saving = true;
    this.errorMessage = '';

    this.organizationService.createOrganization(formData).subscribe({
      next: () => {
        this.successMessage = 'Shipping agent organization created successfully!';
        this.closeFormModal();
        this.loadOrganizations();
        this.saving = false;
        this.autoHideMessage();
      },
      error: (err: any) => {
        console.error('Error creating organization:', err);
        this.errorMessage = err.error?.message || err.error?.error || 'Failed to create organization. Please try again.';
        this.saving = false;
      }
    });
  }

  onAddRepresentative(data: { organizationId: string; representatives: any[] }): void {
    this.saving = true;
    this.errorMessage = '';

    const request: AddRepresentativesRequest = {
      representatives: data.representatives
    };

    this.organizationService.addRepresentativesById(data.organizationId, request).subscribe({
      next: () => {
        this.successMessage = 'Representative(s) added successfully!';
        this.closeRepresentativeModal();
        this.loadOrganizations();
        this.saving = false;
        this.autoHideMessage();
      },
      error: (err: any) => {
        console.error('Error adding representative:', err);
        this.errorMessage = err.error?.message || err.error?.error || 'Failed to add representative. Please try again.';
        this.saving = false;
      }
    });
  }

  onUpdateRepresentative(data: { organizationId: string; representativeId: string; request: UpdateRepresentativeRequest }): void {
    this.saving = true;
    this.errorMessage = '';

    this.organizationService.updateRepresentative(data.organizationId, data.representativeId, data.request).subscribe({
      next: () => {
        this.successMessage = 'Representative updated successfully!';
        this.closeRepresentativeModal();
        this.loadOrganizations();
        this.saving = false;
        this.autoHideMessage();
      },
      error: (err: any) => {
        console.error('Error updating representative:', err);
        this.errorMessage = err.error?.message || err.error?.error || 'Failed to update representative. Please try again.';
        this.saving = false;
      }
    });
  }

  onToggleRepresentativeStatus(data: { organizationId: string; representativeId: string; isActive: boolean }): void {
    const action = data.isActive ? 'activate' : 'deactivate';
    
    if (!confirm(`Are you sure you want to ${action} this representative?`)) {
      return;
    }

    const request: PatchRepresentativeStatusRequest = {
      isActive: data.isActive
    };

    this.organizationService.patchRepresentativeStatus(data.organizationId, data.representativeId, request).subscribe({
      next: () => {
        this.successMessage = `Representative ${data.isActive ? 'activated' : 'deactivated'} successfully!`;
        this.closeViewModal();
        this.loadOrganizations();
        this.autoHideMessage();
      },
      error: (err: any) => {
        console.error('Error changing representative status:', err);
        this.errorMessage = err.error?.message || err.error?.error || 'Failed to change representative status. Please try again.';
      }
    });
  }

  openAddRepresentativeModal(organization: Organization): void {
    this.selectedOrganization = organization;
    this.selectedRepresentative = null;
    this.isRepresentativeEditMode = false;
    this.showViewModal = false;
    this.showRepresentativeModal = true;
  }

  openEditRepresentativeModal(data: { organization: Organization; representative: any }): void {
    this.selectedOrganization = data.organization;
    this.selectedRepresentative = data.representative;
    this.isRepresentativeEditMode = true;
    this.showViewModal = false;
    this.showRepresentativeModal = true;
  }

  autoHideMessage(): void {
    setTimeout(() => {
      this.successMessage = '';
      this.errorMessage = '';
    }, 5000);
  }

  goBack(): void {
    this.router.navigate(['/dashboard']);
  }
}
