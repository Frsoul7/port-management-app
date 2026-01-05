import { Component, EventEmitter, Input, Output } from '@angular/core';
import { CommonModule } from '@angular/common';
import { VesselType } from '../../../../core/models/vessel-type.model';
import { TranslatePipe } from '../../../../core/pipes/translate.pipe';

@Component({
  selector: 'app-vessel-type-table',
  standalone: true,
  imports: [CommonModule, TranslatePipe],
  templateUrl: './vessel-type-table.component.html',
  styleUrls: ['./vessel-type-table.component.scss']
})
export class VesselTypeTableComponent {
  @Input() vesselTypes: VesselType[] = [];
  @Input() loading = false;
  @Input() totalItems = 0;
  @Input() currentPage = 1;
  @Input() pageSize = 20;

  @Output() edit = new EventEmitter<VesselType>();
  @Output() pageChange = new EventEmitter<number>();

  get totalPages(): number {
    return Math.ceil(this.totalItems / this.pageSize);
  }

  get startIndex(): number {
    return (this.currentPage - 1) * this.pageSize + 1;
  }

  get endIndex(): number {
    return Math.min(this.currentPage * this.pageSize, this.totalItems);
  }

  onEdit(vesselType: VesselType): void {
    this.edit.emit(vesselType);
  }

  onPreviousPage(): void {
    if (this.currentPage > 1) {
      this.pageChange.emit(this.currentPage - 1);
    }
  }

  onNextPage(): void {
    if (this.currentPage < this.totalPages) {
      this.pageChange.emit(this.currentPage + 1);
    }
  }

  onPageClick(page: number): void {
    this.pageChange.emit(page);
  }

  getVisiblePages(): number[] {
    const pages: number[] = [];
    const maxVisible = 5;
    let start = Math.max(1, this.currentPage - Math.floor(maxVisible / 2));
    let end = Math.min(this.totalPages, start + maxVisible - 1);

    if (end - start < maxVisible - 1) {
      start = Math.max(1, end - maxVisible + 1);
    }

    for (let i = start; i <= end; i++) {
      pages.push(i);
    }

    return pages;
  }
}
