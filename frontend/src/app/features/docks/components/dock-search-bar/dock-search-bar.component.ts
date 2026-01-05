import { Component, EventEmitter, Output } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { TranslatePipe } from '../../../../core/pipes/translate.pipe';

export interface DockSearchParams {
  name: string;
  location: string;
  vesselTypeId: string;
}

@Component({
  selector: 'app-dock-search-bar',
  standalone: true,
  imports: [CommonModule, FormsModule, TranslatePipe],
  templateUrl: './dock-search-bar.component.html',
  styleUrls: ['./dock-search-bar.component.scss']
})
export class DockSearchBarComponent {
  @Output() onSearch = new EventEmitter<DockSearchParams>();
  @Output() onClear = new EventEmitter<void>();
  @Output() onAdd = new EventEmitter<void>();

  searchParams: DockSearchParams = {
    name: '',
    location: '',
    vesselTypeId: ''
  };

  search(): void {
    this.onSearch.emit(this.searchParams);
  }

  clear(): void {
    this.searchParams = {
      name: '',
      location: '',
      vesselTypeId: ''
    };
    this.onClear.emit();
  }

  addDock(): void {
    this.onAdd.emit();
  }

  handleEnter(): void {
    this.search();
  }
}
