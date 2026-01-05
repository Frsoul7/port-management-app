import { Component, EventEmitter, Input, Output } from '@angular/core';
import { CommonModule } from '@angular/common';
import { Vessel } from '../../../../core/models/vessel.model';
import { TranslatePipe } from '../../../../core/pipes/translate.pipe';

@Component({
  selector: 'app-vessel-view-modal',
  standalone: true,
  imports: [CommonModule, TranslatePipe],
  templateUrl: './vessel-view-modal.component.html',
  styleUrls: ['./vessel-view-modal.component.scss']
})
export class VesselViewModalComponent {
  @Input() show: boolean = false;
  @Input() vessel: Vessel | null = null;
  @Output() onClose = new EventEmitter<void>();
  @Output() onEdit = new EventEmitter<Vessel>();

  close(): void {
    this.onClose.emit();
  }

  edit(): void {
    if (this.vessel) {
      this.onEdit.emit(this.vessel);
    }
  }
}
