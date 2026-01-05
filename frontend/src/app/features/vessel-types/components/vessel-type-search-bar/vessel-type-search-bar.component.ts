import { Component, EventEmitter, Output } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { VesselTypeSearchParams } from '../../../../core/models/vessel-type.model';
import { TranslatePipe } from '../../../../core/pipes/translate.pipe';

@Component({
  selector: 'app-vessel-type-search-bar',
  standalone: true,
  imports: [CommonModule, FormsModule, TranslatePipe],
  templateUrl: './vessel-type-search-bar.component.html',
  styleUrls: ['./vessel-type-search-bar.component.scss']
})
export class VesselTypeSearchBarComponent {
  @Output() onSearch = new EventEmitter<VesselTypeSearchParams>();
  @Output() onClear = new EventEmitter<void>();
  @Output() onAdd = new EventEmitter<void>();

  searchParams: VesselTypeSearchParams = {
    name: '',
    description: ''
  };

  search(): void {
    this.onSearch.emit(this.searchParams);
  }

  clear(): void {
    this.searchParams = {
      name: '',
      description: ''
    };
    this.onClear.emit();
  }

  addVesselType(): void {
    this.onAdd.emit();
  }
}
