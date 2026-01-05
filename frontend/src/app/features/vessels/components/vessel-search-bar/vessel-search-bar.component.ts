import { Component, EventEmitter, Output } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { TranslatePipe } from '../../../../core/pipes/translate.pipe';

export interface VesselSearchParams {
  imo: string;
  name: string;
  organizationName: string;
}

@Component({
  selector: 'app-vessel-search-bar',
  standalone: true,
  imports: [CommonModule, FormsModule, TranslatePipe],
  templateUrl: './vessel-search-bar.component.html',
  styleUrls: ['./vessel-search-bar.component.scss']
})
export class VesselSearchBarComponent {
  @Output() onSearch = new EventEmitter<VesselSearchParams>();
  @Output() onClear = new EventEmitter<void>();
  @Output() onAdd = new EventEmitter<void>();

  searchParams: VesselSearchParams = {
    imo: '',
    name: '',
    organizationName: ''
  };

  search(): void {
    this.onSearch.emit(this.searchParams);
  }

  clear(): void {
    this.searchParams = {
      imo: '',
      name: '',
      organizationName: ''
    };
    this.onClear.emit();
  }

  addVessel(): void {
    this.onAdd.emit();
  }

  handleEnter(): void {
    this.search();
  }
}
