import { Component, OnInit, signal } from '@angular/core';
import { CommonModule } from '@angular/common';
import { Router } from '@angular/router';
import { VesselTypeService } from '../../core/services/vessel-type.service';
import { AuthService } from '../../core/services/auth.service';
import { MessageModalService } from '../../core/services/message-modal.service';
import {
  VesselType,
  CreateVesselTypeRequest,
  UpdateVesselTypeRequest,
  VesselTypeSearchParams
} from '../../core/models/vessel-type.model';
import { VesselTypeSearchBarComponent } from './components/vessel-type-search-bar/vessel-type-search-bar.component';
import { VesselTypeTableComponent } from './components/vessel-type-table/vessel-type-table.component';
import { VesselTypeFormModalComponent } from './components/vessel-type-form-modal/vessel-type-form-modal.component';
import { TranslatePipe } from '../../core/pipes/translate.pipe';

@Component({
  selector: 'app-vessel-types',
  standalone: true,
  imports: [
    CommonModule,
    VesselTypeSearchBarComponent,
    VesselTypeTableComponent,
    VesselTypeFormModalComponent,
    TranslatePipe
  ],
  templateUrl: './vessel-types.component.html',
  styleUrls: ['./vessel-types.component.scss']
})
export class VesselTypesComponent implements OnInit {
  vesselTypes = signal<VesselType[]>([]);
  loading = signal<boolean>(false);
  saving = signal<boolean>(false);

  showModal = signal<boolean>(false);
  isEditMode = signal<boolean>(false);
  selectedVesselType = signal<VesselType | null>(null);

  // Pagination
  totalItems = signal<number>(0);
  currentPage = signal<number>(1);
  pageSize = signal<number>(20);

  constructor(
    private vesselTypeService: VesselTypeService,
    private authService: AuthService,
    private messageModal: MessageModalService,
    private router: Router
  ) {}

  ngOnInit(): void {
    this.loadVesselTypes();
  }

  loadVesselTypes(params?: VesselTypeSearchParams): void {
    this.loading.set(true);

    const searchParams: VesselTypeSearchParams = {
      ...params,
      page: params?.page || this.currentPage(),
      pageSize: params?.pageSize || this.pageSize()
    };

    this.vesselTypeService.searchVesselTypes(searchParams).subscribe({
      next: (response) => {
        this.vesselTypes.set(response.items);
        this.totalItems.set(response.total);
        this.currentPage.set(response.page);
        this.pageSize.set(response.pageSize);
        this.loading.set(false);
      },
      error: (error) => {
        console.error('Error loading vessel types:', error);
        this.messageModal.showError(
          'Error',
          error.error?.error || 'Failed to load vessel types. Please try again.'
        );
        this.loading.set(false);
      }
    });
  }

  onSearch(params: VesselTypeSearchParams): void {
    this.currentPage.set(1); // Reset to first page on new search
    this.loadVesselTypes(params);
  }

  onClear(): void {
    this.currentPage.set(1);
    this.loadVesselTypes();
  }

  onPageChange(page: number): void {
    this.currentPage.set(page);
    this.loadVesselTypes({ page });
  }

  goBack(): void {
    this.router.navigate(['/dashboard']);
  }

  openCreateModal(): void {
    this.isEditMode.set(false);
    this.selectedVesselType.set(null);
    this.showModal.set(true);
  }

  openEditModal(vesselType: VesselType): void {
    this.isEditMode.set(true);
    this.selectedVesselType.set(vesselType);
    this.showModal.set(true);
  }

  closeModal(): void {
    this.showModal.set(false);
    this.selectedVesselType.set(null);
  }

  onCreate(request: CreateVesselTypeRequest): void {
    this.saving.set(true);

    this.vesselTypeService.createVesselType(request).subscribe({
      next: (vesselType) => {
        this.messageModal.showSuccess(
          'Success',
          `Vessel type "${vesselType.name}" created successfully!`,
          () => {
            this.closeModal();
            this.loadVesselTypes();
          }
        );
        this.saving.set(false);
      },
      error: (error) => {
        console.error('Error creating vessel type:', error);
        this.messageModal.showError(
          'Creation Failed',
          error.error?.error || 'Failed to create vessel type. Please try again.'
        );
        this.saving.set(false);
      }
    });
  }

  onUpdate(vesselType: VesselType, request: UpdateVesselTypeRequest): void {
    this.saving.set(true);

    this.vesselTypeService.updateVesselType(vesselType.vesselTypeId, request).subscribe({
      next: (updatedVesselType) => {
        this.messageModal.showSuccess(
          'Success',
          `Vessel type "${updatedVesselType.name}" updated successfully!`,
          () => {
            this.closeModal();
            this.loadVesselTypes();
          }
        );
        this.saving.set(false);
      },
      error: (error) => {
        console.error('Error updating vessel type:', error);
        this.messageModal.showError(
          'Update Failed',
          error.error?.error || 'Failed to update vessel type. Please try again.'
        );
        this.saving.set(false);
      }
    });
  }
}
