import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { Router } from '@angular/router';
import { StorageAreaSearchBarComponent } from './components/storage-area-search-bar/storage-area-search-bar.component';
import { StorageAreaTableComponent } from './components/storage-area-table/storage-area-table.component';
import { StorageAreaFormModalComponent } from './components/storage-area-form-modal/storage-area-form-modal.component';
import { StorageAreaViewModalComponent } from './components/storage-area-view-modal/storage-area-view-modal.component';
import { AlertBannerComponent } from '../vessels/components/alert-banner/alert-banner.component';
import { StorageAreaService } from '../../core/services/storage-area.service';
import { StorageArea, CreateStorageAreaRequest, UpdateStorageAreaRequest, DockInfo } from '../../core/models/storage-area.model';
import { TranslatePipe } from '../../core/pipes/translate.pipe';

@Component({
  selector: 'app-storage-areas',
  standalone: true,
  imports: [
    CommonModule,
    StorageAreaSearchBarComponent,
    StorageAreaTableComponent,
    StorageAreaFormModalComponent,
    StorageAreaViewModalComponent,
    AlertBannerComponent,
    TranslatePipe
  ],
  templateUrl: './storage-areas.component.html',
  styleUrls: ['./storage-areas.component.scss']
})
export class StorageAreasComponent implements OnInit {
  storageAreas: StorageArea[] = [];
  filteredStorageAreas: StorageArea[] = [];
  docks: DockInfo[] = [];
  
  showFormModal = false;
  showViewModal = false;
  isEditMode = false;
  loading = false;
  saving = false;
  errorMessage = '';
  successMessage = '';

  selectedStorageArea: StorageArea | null = null;

  constructor(
    private storageAreaService: StorageAreaService, 
    private router: Router
  ) {}

  ngOnInit(): void {
    this.loadStorageAreas();
    this.loadDocks();
  }

  loadStorageAreas(): void {
    this.loading = true;
    this.errorMessage = '';
    this.storageAreaService.searchStorageAreas().subscribe({
      next: (data: StorageArea[]) => {
        this.storageAreas = data;
        this.filteredStorageAreas = data;
        this.loading = false;
      },
      error: (err: any) => {
        console.error('Error loading storage areas:', err);
        this.errorMessage = err.error?.error || 'Failed to load storage areas. Please try again.';
        this.loading = false;
      }
    });
  }

  loadDocks(): void {
    this.storageAreaService.getDocks().subscribe({
      next: (data: DockInfo[]) => {
        this.docks = data;
      },
      error: (err: any) => {
        console.error('Error loading docks:', err);
      }
    });
  }

  onSearch(filters: { name: string; location: string; type: string }): void {
    let filtered = this.storageAreas;

    if (filters.name) {
      const term = filters.name.toLowerCase();
      filtered = filtered.filter(sa =>
        sa.name.toLowerCase().includes(term)
      );
    }

    if (filters.location) {
      const term = filters.location.toLowerCase();
      filtered = filtered.filter(sa =>
        sa.location.toLowerCase().includes(term)
      );
    }

    if (filters.type) {
      filtered = filtered.filter(sa => sa.type === filters.type);
    }

    this.filteredStorageAreas = filtered;
  }

  onView(storageArea: StorageArea): void {
    this.selectedStorageArea = storageArea;
    this.showViewModal = true;
  }

  onEdit(storageArea: StorageArea): void {
    this.selectedStorageArea = storageArea;
    this.isEditMode = true;
    this.showViewModal = false;
    this.showFormModal = true;
  }

  onDelete(storageArea: StorageArea): void {
    if (confirm(`Are you sure you want to delete storage area "${storageArea.name}"?`)) {
      this.storageAreaService.deleteStorageArea(storageArea.name).subscribe({
        next: () => {
          this.successMessage = 'Storage area deleted successfully!';
          this.loadStorageAreas();
          this.autoHideMessage();
        },
        error: (err: any) => {
          console.error('Error deleting storage area:', err);
          this.errorMessage = err.error?.error || 'Failed to delete storage area. Please try again.';
        }
      });
    }
  }

  openCreateModal(): void {
    this.selectedStorageArea = null;
    this.isEditMode = false;
    this.showFormModal = true;
  }

  closeFormModal(): void {
    this.showFormModal = false;
    this.selectedStorageArea = null;
    this.isEditMode = false;
  }

  closeViewModal(): void {
    this.showViewModal = false;
    this.selectedStorageArea = null;
  }

  onFormCreate(formData: CreateStorageAreaRequest): void {
    this.saving = true;
    this.errorMessage = '';

    this.storageAreaService.createStorageArea(formData).subscribe({
      next: () => {
        this.successMessage = 'Storage area created successfully!';
        this.closeFormModal();
        this.loadStorageAreas();
        this.saving = false;
        this.autoHideMessage();
      },
      error: (err: any) => {
        console.error('Error creating storage area:', err);
        this.errorMessage = err.error?.error || err.error?.message || 'Failed to create storage area. Please try again.';
        this.saving = false;
      }
    });
  }

  onFormUpdate(data: { id: string; request: UpdateStorageAreaRequest }): void {
    this.saving = true;
    this.errorMessage = '';

    this.storageAreaService.updateStorageArea(data.id, data.request).subscribe({
      next: () => {
        this.successMessage = 'Storage area updated successfully!';
        this.closeFormModal();
        this.loadStorageAreas();
        this.saving = false;
        this.autoHideMessage();
      },
      error: (err: any) => {
        console.error('Error updating storage area:', err);
        this.errorMessage = err.error?.error || err.error?.message || 'Failed to update storage area. Please try again.';
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
