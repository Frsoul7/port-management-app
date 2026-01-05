import { Component, EventEmitter, Output } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { TranslatePipe } from '../../../../core/pipes/translate.pipe';

@Component({
  selector: 'app-shipping-agent-search-bar',
  standalone: true,
  imports: [CommonModule, FormsModule, TranslatePipe],
  templateUrl: './shipping-agent-search-bar.component.html',
  styleUrls: ['./shipping-agent-search-bar.component.scss']
})
export class ShippingAgentSearchBarComponent {
  @Output() onSearch = new EventEmitter<{ identifier: string; legalName: string; taxNumber: string }>();
  @Output() onAdd = new EventEmitter<void>();

  searchParams = {
    identifier: '',
    legalName: '',
    taxNumber: ''
  };

  search(): void {
    this.onSearch.emit(this.searchParams);
  }

  clear(): void {
    this.searchParams = {
      identifier: '',
      legalName: '',
      taxNumber: ''
    };
    this.onSearch.emit(this.searchParams);
  }

  addOrganization(): void {
    this.onAdd.emit();
  }

  handleEnter(): void {
    this.search();
  }
}
