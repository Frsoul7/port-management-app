import { Component, Input, OnChanges, SimpleChanges } from '@angular/core';
import { CommonModule } from '@angular/common';
import { VvnService } from '../../../../core/services/vvn.service';
import { MessageModalService } from '../../../../core/services/message-modal.service';
import {
  VvnEntryResponse,
  AddManifestEntryRequest,
  ManifestType,
  VvnStatusResponse
} from '../../../../core/models/vvn.model';
import { CargoManifestEntryFormComponent } from '../cargo-manifest-entry-form/cargo-manifest-entry-form.component';
import { CargoManifestTableComponent } from '../cargo-manifest-table/cargo-manifest-table.component';

@Component({
  selector: 'app-cargo-manifest-manager',
  standalone: true,
  imports: [
    CommonModule,
    CargoManifestEntryFormComponent,
    CargoManifestTableComponent
  ],
  templateUrl: './cargo-manifest-manager.component.html',
  styleUrls: ['./cargo-manifest-manager.component.scss']
})
export class CargoManifestManagerComponent implements OnChanges {
  @Input() vvn: VvnStatusResponse | null = null;
  @Input() editable = false;

  loadingEntries: VvnEntryResponse[] = [];
  unloadingEntries: VvnEntryResponse[] = [];

  showLoadingForm = false;
  showUnloadingForm = false;

  loadingData = false;
  saving = false;

  ManifestType = ManifestType;

  constructor(
    private vvnService: VvnService,
    private modalService: MessageModalService
  ) {}

  ngOnChanges(changes: SimpleChanges): void {
    if (changes['vvn'] && this.vvn) {
      this.loadEntries();
    }
  }

  get canShowLoading(): boolean {
    if (!this.vvn) return false;
    const purpose = this.vvn.purpose;
    return purpose === 'LOAD' || purpose === 'BOTH';
  }

  get canShowUnloading(): boolean {
    if (!this.vvn) return false;
    const purpose = this.vvn.purpose;
    return purpose === 'UNLOAD' || purpose === 'BOTH';
  }

  loadEntries(): void {
    if (!this.vvn) return;

    this.loadingData = true;

    // Load both loading and unloading entries
    this.vvnService.getEntries(this.vvn.id).subscribe({
      next: (entries) => {
        this.loadingEntries = entries.filter(e => e.type === 'Load');
        this.unloadingEntries = entries.filter(e => e.type === 'Unload');
        this.loadingData = false;
      },
      error: (error) => {
        console.error('Error loading manifest entries:', error);
        const errorMsg = error.error?.value?.detail || error.error?.detail || 'Failed to load cargo manifest entries. Please try again.';
        this.modalService.showError('Load Failed', errorMsg);
        this.loadingData = false;
      }
    });
  }

  handleAddLoadingEntry(request: AddManifestEntryRequest): void {
    if (!this.vvn) return;

    this.saving = true;
    this.vvnService.addManifestEntry(this.vvn.id, request).subscribe({
      next: () => {
        this.modalService.showSuccess(
          'Container Added',
          'Loading container added successfully.'
        );
        this.showLoadingForm = false;
        this.loadEntries();
        this.saving = false;
      },
      error: (error) => {
        console.error('Error adding loading entry:', error);
        const errorMsg = error.error?.value?.detail || error.error?.detail || 'Failed to add container. Please try again.';
        this.modalService.showError('Add Failed', errorMsg);
        this.saving = false;
      }
    });
  }

  handleAddUnloadingEntry(request: AddManifestEntryRequest): void {
    if (!this.vvn) return;

    this.saving = true;
    this.vvnService.addManifestEntry(this.vvn.id, request).subscribe({
      next: () => {
        this.modalService.showSuccess(
          'Container Added',
          'Unloading container added successfully.'
        );
        this.showUnloadingForm = false;
        this.loadEntries();
        this.saving = false;
      },
      error: (error) => {
        console.error('Error adding unloading entry:', error);
        const errorMsg = error.error?.value?.detail || error.error?.detail || 'Failed to add container. Please try again.';
        this.modalService.showError('Add Failed', errorMsg);
        this.saving = false;
      }
    });
  }

  handleDeleteEntry(entry: VvnEntryResponse, type: ManifestType): void {
    if (!this.vvn) return;

    this.modalService.showConfirm(
      'Delete Container',
      `Are you sure you want to delete container ${entry.containerCode}?`,
      () => this.performDelete(entry, type),
      () => console.log('Delete cancelled')
    );
  }

  private performDelete(entry: VvnEntryResponse, type: ManifestType): void {
    if (!this.vvn) return;

    this.saving = true;
    this.vvnService.removeManifestEntry(this.vvn.id, entry.id, type).subscribe({
      next: () => {
        this.modalService.showSuccess(
          'Container Deleted',
          `Container ${entry.containerCode} has been removed from the manifest.`
        );
        this.loadEntries();
        this.saving = false;
      },
      error: (error) => {
        console.error('Error deleting entry:', error);
        const errorMsg = error.error?.value?.detail || error.error?.detail || 'Failed to delete container. Please try again.';
        this.modalService.showError('Delete Failed', errorMsg);
        this.saving = false;
      }
    });
  }

  toggleLoadingForm(): void {
    this.showLoadingForm = !this.showLoadingForm;
    if (this.showLoadingForm) {
      this.showUnloadingForm = false;
    }
  }

  toggleUnloadingForm(): void {
    this.showUnloadingForm = !this.showUnloadingForm;
    if (this.showUnloadingForm) {
      this.showLoadingForm = false;
    }
  }

  cancelLoadingForm(): void {
    this.showLoadingForm = false;
  }

  cancelUnloadingForm(): void {
    this.showUnloadingForm = false;
  }
}
