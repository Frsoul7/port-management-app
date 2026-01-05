import { Component, EventEmitter, Input, Output } from '@angular/core';
import { CommonModule } from '@angular/common';
import { Vessel } from '../../../../core/models/vessel.model';
import { TranslatePipe } from '../../../../core/pipes/translate.pipe';
import { LoadingComponent } from '../../../../shared/components/loading/loading.component';

@Component({
  selector: 'app-vessel-table',
  standalone: true,
  imports: [CommonModule, TranslatePipe, LoadingComponent],
  templateUrl: './vessel-table.component.html',
  styleUrls: ['./vessel-table.component.scss']
})
export class VesselTableComponent {
  @Input() vessels: Vessel[] = [];
  @Input() loading: boolean = false;
  @Output() onView = new EventEmitter<Vessel>();
  @Output() onEdit = new EventEmitter<Vessel>();

  viewVessel(vessel: Vessel): void {
    this.onView.emit(vessel);
  }

  editVessel(vessel: Vessel): void {
    this.onEdit.emit(vessel);
  }
}
