import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { Router } from '@angular/router';
import { VesselService } from '../../core/services/vessel.service';
import { AuthService } from '../../core/services/auth.service';
import { Vessel, CreateVesselRequest, UpdateVesselRequest, VesselType, Organization } from '../../core/models/vessel.model';
import { VesselSearchBarComponent, VesselSearchParams } from './components/vessel-search-bar/vessel-search-bar.component';
import { VesselTableComponent } from './components/vessel-table/vessel-table.component';
import { VesselFormModalComponent } from './components/vessel-form-modal/vessel-form-modal.component';
import { VesselViewModalComponent } from './components/vessel-view-modal/vessel-view-modal.component';
import { AlertBannerComponent } from './components/alert-banner/alert-banner.component';
import { TranslatePipe } from '../../core/pipes/translate.pipe';

@Component({
  selector: 'app-vessels',
  standalone: true,
  imports: [
    CommonModule,
    VesselSearchBarComponent,
    VesselTableComponent,
    VesselFormModalComponent,
    VesselViewModalComponent,
    AlertBannerComponent,
    TranslatePipe
  ],
  templateUrl: './vessels.component.html',
  styleUrls: ['./vessels.component.scss']
})
export class VesselsComponent implements OnInit {
  vessels: Vessel[] = [];
  vesselTypes: VesselType[] = [];
  organizations: Organization[] = [];
  
  showModal = false;
  showViewModal = false;
  isEditMode = false;
  loading = false;
  saving = false;
  errorMessage = '';
  successMessage = '';

  selectedVessel: Vessel | null = null;
  selectedVesselForEdit: Vessel | null = null;

  constructor(
    private vesselService: VesselService,
    private authService: AuthService,
    private router: Router
  ) {}

  ngOnInit(): void {
    this.loadInitialData();
  }

  async loadInitialData(): Promise<void> {
    this.loading = true;
    try {
      // Load all vessels on init
      await this.search({});
      
      // Load vessel types and organizations for dropdowns
      this.vesselService.getVesselTypes().subscribe({
        next: (response) => {
          this.vesselTypes = response.items;
        },
        error: (error) => {
          console.error('Error loading vessel types:', error);
        }
      });

      this.vesselService.getOrganizations().subscribe({
        next: (orgs) => {
          this.organizations = orgs;
        },
        error: (error) => {
          console.error('Error loading organizations:', error);
        }
      });
    } finally {
      this.loading = false;
    }
  }

  search(params: Partial<VesselSearchParams>): Promise<void> {
    return new Promise((resolve) => {
      this.loading = true;
      this.errorMessage = '';
      
      const searchParams: any = {};
      if (params.imo) searchParams.imo = params.imo;
      if (params.name) searchParams.name = params.name;
      if (params.organizationName) searchParams.organizationName = params.organizationName;

      this.vesselService.searchVessels(searchParams).subscribe({
        next: (vessels) => {
          this.vessels = vessels;
          this.loading = false;
          resolve();
        },
        error: (error) => {
          console.error('Error searching vessels:', error);
          this.errorMessage = error.error?.error || 'Failed to load vessels. Please try again.';
          this.loading = false;
          resolve();
        }
      });
    });
  }

  // Search Bar Event Handlers
  handleSearch(params: VesselSearchParams): void {
    this.search(params);
  }

  handleClear(): void {
    this.search({});
  }

  handleAdd(): void {
    this.isEditMode = false;
    this.selectedVesselForEdit = null;
    this.showModal = true;
  }

  // Table Event Handlers
  handleView(vessel: Vessel): void {
    this.selectedVessel = vessel;
    this.showViewModal = true;
  }

  handleEdit(vessel: Vessel): void {
    this.isEditMode = true;
    this.selectedVesselForEdit = vessel;
    this.showModal = true;
  }

  // Form Modal Event Handlers
  handleModalClose(): void {
    this.showModal = false;
    this.selectedVesselForEdit = null;
  }

  handleCreate(request: CreateVesselRequest): void {
    this.saving = true;
    this.errorMessage = '';

    this.vesselService.createVessel(request).subscribe({
      next: () => {
        this.successMessage = 'Vessel created successfully!';
        this.showModal = false;
        this.search({});
        this.saving = false;
        this.autoHideMessage();
      },
      error: (error) => {
        console.error('Error creating vessel:', error);
        this.errorMessage = error.error?.error || 'Failed to create vessel. Please try again.';
        this.saving = false;
      }
    });
  }

  handleUpdate(data: { imo: string; request: UpdateVesselRequest }): void {
    this.saving = true;
    this.errorMessage = '';

    this.vesselService.updateVessel(data.imo, data.request).subscribe({
      next: () => {
        this.successMessage = 'Vessel updated successfully!';
        this.showModal = false;
        this.search({});
        this.saving = false;
        this.autoHideMessage();
      },
      error: (error) => {
        console.error('Error updating vessel:', error);
        this.errorMessage = error.error?.error || 'Failed to update vessel. Please try again.';
        this.saving = false;
      }
    });
  }

  // View Modal Event Handlers
  handleViewModalClose(): void {
    this.showViewModal = false;
    this.selectedVessel = null;
  }

  handleEditFromView(vessel: Vessel): void {
    this.showViewModal = false;
    this.handleEdit(vessel);
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

