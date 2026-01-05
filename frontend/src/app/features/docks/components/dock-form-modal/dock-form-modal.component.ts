import { Component, EventEmitter, Input, Output, OnChanges, SimpleChanges } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { Dock, VesselTypeInfo, CreateDockRequest, UpdateDockRequest } from '../../../../core/models/dock.model';
import { TranslatePipe } from '../../../../core/pipes/translate.pipe';
import { LoadingComponent } from '../../../../shared/components/loading/loading.component';

@Component({
  selector: 'app-dock-form-modal',
  standalone: true,
  imports: [CommonModule, FormsModule, TranslatePipe, LoadingComponent],
  templateUrl: './dock-form-modal.component.html',
  styleUrls: ['./dock-form-modal.component.scss']
})
export class DockFormModalComponent implements OnChanges {
  @Input() show: boolean = false;
  @Input() isEditMode: boolean = false;
  @Input() dock: Dock | null = null;
  @Input() vesselTypes: VesselTypeInfo[] = [];
  @Input() saving: boolean = false;
  @Output() onClose = new EventEmitter<void>();
  @Output() onCreate = new EventEmitter<CreateDockRequest>();
  @Output() onUpdate = new EventEmitter<{ code: string; request: UpdateDockRequest }>();

  currentDock: any = {
    code: '',
    name: '',
    location: '',
    lengthM: null,
    depthM: null,
    maxDraftM: null,
    allowedVesselTypeIds: []
  };

  selectedVesselTypes: { [key: string]: boolean } = {};

  ngOnChanges(changes: SimpleChanges): void {
    if (changes['dock'] && this.dock) {
      this.currentDock = {
        code: this.dock.code,
        name: this.dock.name,
        location: this.dock.location,
        lengthM: this.dock.lengthM,
        depthM: this.dock.depthM,
        maxDraftM: this.dock.maxDraftM,
        allowedVesselTypeIds: [...this.dock.allowedVesselTypeIds]
      };
      this.updateSelectedVesselTypes();
    } else if (changes['show'] && this.show && !this.isEditMode) {
      this.resetForm();
    }
  }

  updateSelectedVesselTypes(): void {
    this.selectedVesselTypes = {};
    this.currentDock.allowedVesselTypeIds.forEach((id: string) => {
      this.selectedVesselTypes[id] = true;
    });
  }

  resetForm(): void {
    this.currentDock = {
      code: '',
      name: '',
      location: '',
      lengthM: null,
      depthM: null,
      maxDraftM: null,
      allowedVesselTypeIds: []
    };
    this.selectedVesselTypes = {};
  }

  close(): void {
    this.onClose.emit();
    this.resetForm();
  }

  toggleVesselType(vesselTypeId: string): void {
    if (this.selectedVesselTypes[vesselTypeId]) {
      delete this.selectedVesselTypes[vesselTypeId];
      const index = this.currentDock.allowedVesselTypeIds.indexOf(vesselTypeId);
      if (index > -1) {
        this.currentDock.allowedVesselTypeIds.splice(index, 1);
      }
    } else {
      this.selectedVesselTypes[vesselTypeId] = true;
      this.currentDock.allowedVesselTypeIds.push(vesselTypeId);
    }
  }

  save(): void {
    if (this.isEditMode && this.dock) {
      const updateRequest: UpdateDockRequest = {
        name: this.currentDock.name,
        location: this.currentDock.location,
        lengthM: this.currentDock.lengthM,
        depthM: this.currentDock.depthM,
        maxDraftM: this.currentDock.maxDraftM,
        allowedVesselTypeIds: this.currentDock.allowedVesselTypeIds
      };
      this.onUpdate.emit({ code: this.currentDock.code, request: updateRequest });
    } else {
      const createRequest: CreateDockRequest = {
        code: this.currentDock.code.toUpperCase(),
        name: this.currentDock.name,
        location: this.currentDock.location,
        lengthM: this.currentDock.lengthM,
        depthM: this.currentDock.depthM,
        maxDraftM: this.currentDock.maxDraftM,
        allowedVesselTypeIds: this.currentDock.allowedVesselTypeIds
      };
      this.onCreate.emit(createRequest);
    }
  }
}
