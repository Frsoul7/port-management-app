import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { Router } from '@angular/router';
import { DockService } from '../../core/services/dock.service';
import { AuthService } from '../../core/services/auth.service';
import { Dock, CreateDockRequest, UpdateDockRequest, VesselTypeInfo } from '../../core/models/dock.model';
import { DockSearchBarComponent, DockSearchParams } from './components/dock-search-bar/dock-search-bar.component';
import { DockTableComponent } from './components/dock-table/dock-table.component';
import { DockFormModalComponent } from './components/dock-form-modal/dock-form-modal.component';
import { DockViewModalComponent } from './components/dock-view-modal/dock-view-modal.component';
import { AlertBannerComponent } from '../vessels/components/alert-banner/alert-banner.component';
import { TranslatePipe } from '../../core/pipes/translate.pipe';

@Component({
  selector: 'app-docks',
  standalone: true,
  imports: [
    CommonModule,
    DockSearchBarComponent,
    DockTableComponent,
    DockFormModalComponent,
    DockViewModalComponent,
    AlertBannerComponent,
    TranslatePipe
  ],
  templateUrl: './docks.component.html',
  styleUrls: ['./docks.component.scss']
})
export class DocksComponent implements OnInit {
  docks: Dock[] = [];
  vesselTypes: VesselTypeInfo[] = [];
  
  showModal = false;
  showViewModal = false;
  isEditMode = false;
  loading = false;
  saving = false;
  errorMessage = '';
  successMessage = '';

  selectedDock: Dock | null = null;
  selectedDockForEdit: Dock | null = null;

  constructor(
    private dockService: DockService,
    private authService: AuthService,
    private router: Router
  ) {}

  ngOnInit(): void {
    this.loadInitialData();
  }

  async loadInitialData(): Promise<void> {
    this.loading = true;
    try {
      // Load all docks on init
      await this.search({});
      
      // Load vessel types for dropdowns
      this.dockService.getVesselTypes().subscribe({
        next: (response) => {
          this.vesselTypes = response.items;
        },
        error: (error) => {
          console.error('Error loading vessel types:', error);
        }
      });
    } finally {
      this.loading = false;
    }
  }

  search(params: Partial<DockSearchParams>): Promise<void> {
    return new Promise((resolve) => {
      this.loading = true;
      this.errorMessage = '';
      
      const searchParams: any = {};
      if (params.name) searchParams.name = params.name;
      if (params.location) searchParams.location = params.location;
      if (params.vesselTypeId) searchParams.vesselTypeId = params.vesselTypeId;

      this.dockService.searchDocks(searchParams).subscribe({
        next: (docks) => {
          this.docks = docks;
          this.loading = false;
          resolve();
        },
        error: (error) => {
          console.error('Error searching docks:', error);
          this.errorMessage = error.error?.error || 'Failed to load docks. Please try again.';
          this.loading = false;
          resolve();
        }
      });
    });
  }

  // Search Bar Event Handlers
  handleSearch(params: DockSearchParams): void {
    this.search(params);
  }

  handleClear(): void {
    this.search({});
  }

  handleAdd(): void {
    this.isEditMode = false;
    this.selectedDockForEdit = null;
    this.showModal = true;
  }

  // Table Event Handlers
  handleView(dock: Dock): void {
    this.selectedDock = dock;
    this.showViewModal = true;
  }

  handleEdit(dock: Dock): void {
    this.isEditMode = true;
    this.selectedDockForEdit = dock;
    this.showModal = true;
  }

  // Form Modal Event Handlers
  handleModalClose(): void {
    this.showModal = false;
    this.selectedDockForEdit = null;
  }

  handleCreate(request: CreateDockRequest): void {
    this.saving = true;
    this.errorMessage = '';

    this.dockService.createDock(request).subscribe({
      next: () => {
        this.successMessage = 'Dock created successfully!';
        this.showModal = false;
        this.search({});
        this.saving = false;
        this.autoHideMessage();
      },
      error: (error) => {
        console.error('Error creating dock:', error);
        this.errorMessage = error.error?.error || error.error?.message || 'Failed to create dock. Please try again.';
        this.saving = false;
      }
    });
  }

  handleUpdate(data: { code: string; request: UpdateDockRequest }): void {
    this.saving = true;
    this.errorMessage = '';

    this.dockService.updateDock(data.code, data.request).subscribe({
      next: () => {
        this.successMessage = 'Dock updated successfully!';
        this.showModal = false;
        this.search({});
        this.saving = false;
        this.autoHideMessage();
      },
      error: (error) => {
        console.error('Error updating dock:', error);
        this.errorMessage = error.error?.error || error.error?.message || 'Failed to update dock. Please try again.';
        this.saving = false;
      }
    });
  }

  // View Modal Event Handlers
  handleViewModalClose(): void {
    this.showViewModal = false;
    this.selectedDock = null;
  }

  handleEditFromView(dock: Dock): void {
    this.showViewModal = false;
    this.handleEdit(dock);
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
