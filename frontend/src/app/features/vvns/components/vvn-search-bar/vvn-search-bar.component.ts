import { Component, EventEmitter, Input, Output } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { VvnSearchParams } from '../../../../core/models/vvn.model';
import { TranslatePipe } from '../../../../core/pipes/translate.pipe';

@Component({
  selector: 'app-vvn-search-bar',
  standalone: true,
  imports: [CommonModule, FormsModule, TranslatePipe],
  templateUrl: './vvn-search-bar.component.html',
  styleUrls: ['./vvn-search-bar.component.scss']
})
export class VvnSearchBarComponent {
  @Input() isShippingAgent = false;
  @Output() onSearch = new EventEmitter<VvnSearchParams>();
  @Output() onClear = new EventEmitter<void>();
  @Output() onAdd = new EventEmitter<void>();

  searchParams: VvnSearchParams = {
    vesselImo: '',
    status: '',
    submittedById: '',
    fromDate: '',
    toDate: ''
  };

  statusOptions = [
    { value: '', label: 'All Statuses' },
    { value: 'IN_PROGRESS', label: 'In Progress' },
    { value: 'SUBMITTED', label: 'Submitted' },
    { value: 'APPROVED', label: 'Approved' },
    { value: 'REJECTED', label: 'Rejected' }
  ];

  search(): void {
    const params: VvnSearchParams = {};
    
    if (this.searchParams.vesselImo?.trim()) {
      params.vesselImo = this.searchParams.vesselImo.trim();
    }
    if (this.searchParams.status) {
      params.status = this.searchParams.status;
    }
    if (this.searchParams.submittedById?.trim()) {
      params.submittedById = this.searchParams.submittedById.trim();
    }
    if (this.searchParams.fromDate) {
      params.fromDate = this.searchParams.fromDate;
    }
    if (this.searchParams.toDate) {
      params.toDate = this.searchParams.toDate;
    }
    
    this.onSearch.emit(params);
  }

  clear(): void {
    this.searchParams = {
      vesselImo: '',
      status: '',
      submittedById: '',
      fromDate: '',
      toDate: ''
    };
    this.onClear.emit();
  }

  addVvn(): void {
    this.onAdd.emit();
  }

  handleEnter(): void {
    this.search();
  }
}
