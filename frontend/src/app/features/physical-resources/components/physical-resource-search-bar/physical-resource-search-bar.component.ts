import { Component, EventEmitter, Output } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { PhysicalResourceAvailability } from '../../../../core/models/physical-resource.model';
import { TranslatePipe } from '../../../../core/pipes/translate.pipe';

export interface PhysicalResourceSearchParams {
  code: string;
  description: string;
  availability: string;
}

@Component({
  selector: 'app-physical-resource-search-bar',
  standalone: true,
  imports: [CommonModule, FormsModule, TranslatePipe],
  templateUrl: './physical-resource-search-bar.component.html',
  styleUrls: ['./physical-resource-search-bar.component.scss']
})
export class PhysicalResourceSearchBarComponent {
  @Output() onSearch = new EventEmitter<PhysicalResourceSearchParams>();
  @Output() onClear = new EventEmitter<void>();
  @Output() onAdd = new EventEmitter<void>();

  searchParams: PhysicalResourceSearchParams = {
    code: '',
    description: '',
    availability: ''
  };

  availabilityOptions = [
    { value: '', translationKey: 'ALL_AVAILABILITIES' },
    { value: PhysicalResourceAvailability.AVAILABLE, translationKey: 'AVAILABLE' },
    { value: PhysicalResourceAvailability.MAINTENANCE, translationKey: 'MAINTENANCE' },
    { value: PhysicalResourceAvailability.TEMP_OUT_OF_SERVICE, translationKey: 'OUT_OF_SERVICE' }
  ];

  search(): void {
    this.onSearch.emit(this.searchParams);
  }

  clear(): void {
    this.searchParams = {
      code: '',
      description: '',
      availability: ''
    };
    this.onClear.emit();
  }

  addPhysicalResource(): void {
    this.onAdd.emit();
  }

  handleEnter(): void {
    this.search();
  }
}
