import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { Router } from '@angular/router';
import { PhysicalResourceSearchBarComponent } from './components/physical-resource-search-bar/physical-resource-search-bar.component';
import { PhysicalResourceTableComponent } from './components/physical-resource-table/physical-resource-table.component';
import { PhysicalResourceFormModalComponent } from './components/physical-resource-form-modal/physical-resource-form-modal.component';
import { PhysicalResourceViewModalComponent } from './components/physical-resource-view-modal/physical-resource-view-modal.component';
import { AlertBannerComponent } from '../vessels/components/alert-banner/alert-banner.component';
import { PhysicalResourceService } from '../../core/services/physical-resource.service';
import { PhysicalResource, CreatePhysicalResourceRequest, UpdatePhysicalResourceRequest, DockInfo } from '../../core/models/physical-resource.model';
import { TranslatePipe } from '../../core/pipes/translate.pipe';

@Component({
  selector: 'app-physical-resources',
  standalone: true,
  imports: [
    CommonModule,
    PhysicalResourceSearchBarComponent,
    PhysicalResourceTableComponent,
    PhysicalResourceFormModalComponent,
    PhysicalResourceViewModalComponent,
    AlertBannerComponent,
    TranslatePipe
  ],
  templateUrl: './physical-resources.component.html',
  styleUrls: ['./physical-resources.component.scss']
})
export class PhysicalResourcesComponent implements OnInit {
  physicalResources: PhysicalResource[] = [];
  filteredPhysicalResources: PhysicalResource[] = [];
  docks: DockInfo[] = [];
  
  showFormModal = false;
  showViewModal = false;
  isEditMode = false;
  loading = false;
  saving = false;
  errorMessage = '';
  successMessage = '';

  selectedPhysicalResource: PhysicalResource | null = null;

  constructor(
    private physicalResourceService: PhysicalResourceService,
    private router: Router
  ) {}

  ngOnInit(): void {
    this.loadPhysicalResources();
    this.loadDocks();
  }

  loadPhysicalResources(): void {
    this.loading = true;
    this.errorMessage = '';
    this.physicalResourceService.searchPhysicalResources().subscribe({
      next: (data: PhysicalResource[]) => {
        this.physicalResources = data;
        this.filteredPhysicalResources = data;
        this.loading = false;
      },
      error: (err: any) => {
        console.error('Error loading physical resources:', err);
        this.errorMessage = err.error?.error || 'Failed to load physical resources. Please try again.';
        this.loading = false;
      }
    });
  }

  loadDocks(): void {
    this.physicalResourceService.getDocks().subscribe({
      next: (data: DockInfo[]) => {
        this.docks = data;
      },
      error: (err: any) => {
        console.error('Error loading docks:', err);
      }
    });
  }

  onSearch(filters: { code: string; description: string; availability: string }): void {
    let filtered = this.physicalResources;

    if (filters.code) {
      const term = filters.code.toLowerCase();
      filtered = filtered.filter(pr =>
        pr.code.toLowerCase().includes(term)
      );
    }

    if (filters.description) {
      const term = filters.description.toLowerCase();
      filtered = filtered.filter(pr =>
        pr.description?.toLowerCase().includes(term)
      );
    }

    if (filters.availability) {
      filtered = filtered.filter(pr => pr.availability === filters.availability);
    }

    this.filteredPhysicalResources = filtered;
  }

  onView(physicalResource: PhysicalResource): void {
    this.selectedPhysicalResource = physicalResource;
    this.showViewModal = true;
  }

  onEdit(physicalResource: PhysicalResource): void {
    this.selectedPhysicalResource = physicalResource;
    this.isEditMode = true;
    this.showViewModal = false;
    this.showFormModal = true;
  }

  onDelete(physicalResource: PhysicalResource): void {
    if (confirm(`Are you sure you want to delete physical resource "${physicalResource.code}"?`)) {
      this.physicalResourceService.deletePhysicalResource(physicalResource.code).subscribe({
        next: () => {
          this.successMessage = 'Physical resource deleted successfully!';
          this.loadPhysicalResources();
          this.autoHideMessage();
        },
        error: (err: any) => {
          console.error('Error deleting physical resource:', err);
          this.errorMessage = err.error?.error || 'Failed to delete physical resource. Please try again.';
        }
      });
    }
  }

  openCreateModal(): void {
    this.selectedPhysicalResource = null;
    this.isEditMode = false;
    this.showFormModal = true;
  }

  closeFormModal(): void {
    this.showFormModal = false;
    this.selectedPhysicalResource = null;
    this.isEditMode = false;
  }

  closeViewModal(): void {
    this.showViewModal = false;
    this.selectedPhysicalResource = null;
  }

  onFormCreate(formData: CreatePhysicalResourceRequest): void {
    this.saving = true;
    this.errorMessage = '';

    this.physicalResourceService.createPhysicalResource(formData).subscribe({
      next: () => {
        this.successMessage = 'Physical resource created successfully!';
        this.saving = false;
        this.closeFormModal();
        this.loadPhysicalResources();
        this.autoHideMessage();
      },
      error: (err: any) => {
        console.error('Error creating physical resource:', err);
        this.errorMessage = err.error?.error || err.error?.message || 'Failed to create physical resource. Please try again.';
        this.saving = false;
      }
    });
  }

  onFormUpdate(data: { code: string; request: UpdatePhysicalResourceRequest }): void {
    this.saving = true;
    this.errorMessage = '';

    this.physicalResourceService.updatePhysicalResource(data.code, data.request).subscribe({
      next: () => {
        this.successMessage = 'Physical resource updated successfully!';
        this.saving = false;
        this.closeFormModal();
        this.loadPhysicalResources();
        this.autoHideMessage();
      },
      error: (err: any) => {
        console.error('Error updating physical resource:', err);
        this.errorMessage = err.error?.error || err.error?.message || 'Failed to update physical resource. Please try again.';
        this.saving = false;
      }
    });
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
