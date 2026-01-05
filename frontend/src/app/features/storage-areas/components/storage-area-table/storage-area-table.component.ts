import { Component, EventEmitter, Input, Output } from '@angular/core';
import { CommonModule } from '@angular/common';
import { StorageArea, StorageAreaType } from '../../../../core/models/storage-area.model';
import { TranslatePipe } from '../../../../core/pipes/translate.pipe';
import { LoadingComponent } from '../../../../shared/components/loading/loading.component';

@Component({
  selector: 'app-storage-area-table',
  standalone: true,
  imports: [CommonModule, TranslatePipe, LoadingComponent],
  templateUrl: './storage-area-table.component.html',
  styleUrls: ['./storage-area-table.component.scss']
})
export class StorageAreaTableComponent {
  @Input() storageAreas: StorageArea[] = [];
  @Input() loading: boolean = false;
  @Output() onView = new EventEmitter<StorageArea>();
  @Output() onEdit = new EventEmitter<StorageArea>();
  @Output() onDelete = new EventEmitter<StorageArea>();

  viewStorageArea(storageArea: StorageArea): void {
    this.onView.emit(storageArea);
  }

  editStorageArea(storageArea: StorageArea): void {
    this.onEdit.emit(storageArea);
  }

  deleteStorageArea(storageArea: StorageArea): void {
    this.onDelete.emit(storageArea);
  }

  formatType(type: StorageAreaType): string {
    return type.charAt(0) + type.slice(1).toLowerCase();
  }

  getOccupancyPercentage(storageArea: StorageArea): number {
    if (storageArea.maxCapacityTEU === 0) return 0;
    return (storageArea.currentOccupancyTEU / storageArea.maxCapacityTEU) * 100;
  }

  getOccupancyClass(percentage: number): string {
    if (percentage >= 90) return 'high';
    if (percentage >= 70) return 'medium';
    return 'low';
  }
}
