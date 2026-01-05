import { Component, EventEmitter, Input, Output } from '@angular/core';
import { CommonModule } from '@angular/common';
import { VvnEntryResponse } from '../../../../core/models/vvn.model';

@Component({
  selector: 'app-cargo-manifest-table',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './cargo-manifest-table.component.html',
  styleUrls: ['./cargo-manifest-table.component.scss']
})
export class CargoManifestTableComponent {
  @Input() entries: VvnEntryResponse[] = [];
  @Input() editable = false;
  @Input() loading = false;
  @Output() onDelete = new EventEmitter<VvnEntryResponse>();

  handleDelete(entry: VvnEntryResponse): void {
    this.onDelete.emit(entry);
  }
}
