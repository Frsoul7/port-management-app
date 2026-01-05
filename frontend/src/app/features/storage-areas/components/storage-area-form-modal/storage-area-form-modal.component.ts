import { Component, EventEmitter, Input, Output, OnChanges, SimpleChanges } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { StorageArea, StorageAreaType, DockInfo, CreateStorageAreaRequest, UpdateStorageAreaRequest } from '../../../../core/models/storage-area.model';
import { TranslatePipe } from '../../../../core/pipes/translate.pipe';
import { LoadingComponent } from '../../../../shared/components/loading/loading.component';

@Component({
  selector: 'app-storage-area-form-modal',
  standalone: true,
  imports: [CommonModule, FormsModule, TranslatePipe, LoadingComponent],
  templateUrl: './storage-area-form-modal.component.html',
  styleUrls: ['./storage-area-form-modal.component.scss']
})
export class StorageAreaFormModalComponent implements OnChanges {
  @Input() show: boolean = false;
  @Input() isEditMode: boolean = false;
  @Input() storageArea: StorageArea | null = null;
  @Input() docks: DockInfo[] = [];
  @Input() saving: boolean = false;
  @Output() onClose = new EventEmitter<void>();
  @Output() onCreate = new EventEmitter<CreateStorageAreaRequest>();
  @Output() onUpdate = new EventEmitter<{ id: string; request: UpdateStorageAreaRequest }>();

  StorageAreaType = StorageAreaType;

  currentStorageArea: any = {
    name: '',
    location: '',
    maxCapacityTEU: null,
    type: StorageAreaType.ORDINARY,
    servesAllDocks: true,
    servedDockCodes: [],
    yardNotes: '',
    warehouseNotes: ''
  };

  selectedDocks: { [key: string]: boolean } = {};

  storageAreaTypes = [
    { value: StorageAreaType.ORDINARY, label: 'Ordinary' },
    { value: StorageAreaType.YARD, label: 'Yard' },
    { value: StorageAreaType.WAREHOUSE, label: 'Warehouse' }
  ];

  ngOnChanges(changes: SimpleChanges): void {
    if (changes['storageArea'] && this.storageArea) {
      this.currentStorageArea = {
        name: this.storageArea.name,
        location: this.storageArea.location,
        maxCapacityTEU: this.storageArea.maxCapacityTEU,
        type: this.storageArea.type,
        servesAllDocks: this.storageArea.servesAllDocks,
        servedDockCodes: [...this.storageArea.servedDockCodes],
        yardNotes: this.storageArea.yardNotes || '',
        warehouseNotes: this.storageArea.warehouseNotes || ''
      };
      this.updateSelectedDocks();
    } else if (changes['show'] && this.show && !this.isEditMode) {
      this.resetForm();
    }
  }

  updateSelectedDocks(): void {
    this.selectedDocks = {};
    this.currentStorageArea.servedDockCodes.forEach((code: string) => {
      this.selectedDocks[code] = true;
    });
  }

  resetForm(): void {
    this.currentStorageArea = {
      name: '',
      location: '',
      maxCapacityTEU: null,
      type: StorageAreaType.ORDINARY,
      servesAllDocks: true,
      servedDockCodes: [],
      yardNotes: '',
      warehouseNotes: ''
    };
    this.selectedDocks = {};
  }

  close(): void {
    this.onClose.emit();
    this.resetForm();
  }

  toggleDock(dockCode: string): void {
    if (this.selectedDocks[dockCode]) {
      delete this.selectedDocks[dockCode];
      const index = this.currentStorageArea.servedDockCodes.indexOf(dockCode);
      if (index > -1) {
        this.currentStorageArea.servedDockCodes.splice(index, 1);
      }
    } else {
      this.selectedDocks[dockCode] = true;
      this.currentStorageArea.servedDockCodes.push(dockCode);
    }
  }

  onServesAllDocksChange(): void {
    if (this.currentStorageArea.servesAllDocks) {
      this.currentStorageArea.servedDockCodes = [];
      this.selectedDocks = {};
    }
  }

  save(): void {
    if (this.isEditMode && this.storageArea) {
      const updateRequest: UpdateStorageAreaRequest = {
        name: this.currentStorageArea.name,
        location: this.currentStorageArea.location,
        maxCapacityTEU: this.currentStorageArea.maxCapacityTEU,
        servesAllDocks: this.currentStorageArea.servesAllDocks,
        servedDockCodes: this.currentStorageArea.servesAllDocks ? [] : this.currentStorageArea.servedDockCodes,
        yardNotes: this.currentStorageArea.type === StorageAreaType.YARD ? this.currentStorageArea.yardNotes : undefined,
        warehouseNotes: this.currentStorageArea.type === StorageAreaType.WAREHOUSE ? this.currentStorageArea.warehouseNotes : undefined
      };
      this.onUpdate.emit({ id: this.storageArea.storageAreaId, request: updateRequest });
    } else {
      const createRequest: CreateStorageAreaRequest = {
        name: this.currentStorageArea.name,
        location: this.currentStorageArea.location,
        maxCapacityTEU: this.currentStorageArea.maxCapacityTEU,
        type: this.currentStorageArea.type,
        servesAllDocks: this.currentStorageArea.servesAllDocks,
        servedDockCodes: this.currentStorageArea.servesAllDocks ? [] : this.currentStorageArea.servedDockCodes,
        yardNotes: this.currentStorageArea.type === StorageAreaType.YARD ? this.currentStorageArea.yardNotes : undefined,
        warehouseNotes: this.currentStorageArea.type === StorageAreaType.WAREHOUSE ? this.currentStorageArea.warehouseNotes : undefined
      };
      this.onCreate.emit(createRequest);
    }
  }
}
