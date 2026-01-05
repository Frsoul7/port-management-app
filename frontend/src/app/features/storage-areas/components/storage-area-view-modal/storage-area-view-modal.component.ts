import { Component, EventEmitter, Input, Output } from '@angular/core';
import { CommonModule } from '@angular/common';
import { StorageArea, StorageAreaType } from '../../../../core/models/storage-area.model';
import { TranslatePipe } from '../../../../core/pipes/translate.pipe';

@Component({
  selector: 'app-storage-area-view-modal',
  standalone: true,
  imports: [CommonModule, TranslatePipe],
  templateUrl: './storage-area-view-modal.component.html',
  styleUrls: ['./storage-area-view-modal.component.scss']
})
export class StorageAreaViewModalComponent {
  @Input() show: boolean = false;
  @Input() storageArea: StorageArea | null = null;
  @Output() onClose = new EventEmitter<void>();
  @Output() onEdit = new EventEmitter<StorageArea>();

  close(): void {
    this.onClose.emit();
  }

  edit(): void {
    if (this.storageArea) {
      this.onEdit.emit(this.storageArea);
    }
  }

  formatType(type: StorageAreaType): string {
    return type.charAt(0) + type.slice(1).toLowerCase();
  }

  getOccupancyPercentage(storageArea: StorageArea): number {
    if (storageArea.maxCapacityTEU === 0) return 0;
    return (storageArea.currentOccupancyTEU / storageArea.maxCapacityTEU) * 100;
  }
}
