import { Component, EventEmitter, Output } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { StorageAreaType } from '../../../../core/models/storage-area.model';
import { TranslatePipe } from '../../../../core/pipes/translate.pipe';

export interface StorageAreaSearchParams {
  name: string;
  location: string;
  type: string;
}

@Component({
  selector: 'app-storage-area-search-bar',
  standalone: true,
  imports: [CommonModule, FormsModule, TranslatePipe],
  templateUrl: './storage-area-search-bar.component.html',
  styleUrls: ['./storage-area-search-bar.component.scss']
})
export class StorageAreaSearchBarComponent {
  @Output() onSearch = new EventEmitter<StorageAreaSearchParams>();
  @Output() onClear = new EventEmitter<void>();
  @Output() onAdd = new EventEmitter<void>();

  searchParams: StorageAreaSearchParams = {
    name: '',
    location: '',
    type: ''
  };

  storageAreaTypes = [
    { value: '', label: 'All Types' },
    { value: StorageAreaType.ORDINARY, label: 'Ordinary' },
    { value: StorageAreaType.YARD, label: 'Yard' },
    { value: StorageAreaType.WAREHOUSE, label: 'Warehouse' }
  ];

  search(): void {
    this.onSearch.emit(this.searchParams);
  }

  clear(): void {
    this.searchParams = {
      name: '',
      location: '',
      type: ''
    };
    this.onClear.emit();
  }

  addStorageArea(): void {
    this.onAdd.emit();
  }

  handleEnter(): void {
    this.search();
  }
}
