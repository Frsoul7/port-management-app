import { Component, EventEmitter, Input, Output } from '@angular/core';
import { CommonModule } from '@angular/common';
import { Dock } from '../../../../core/models/dock.model';
import { TranslatePipe } from '../../../../core/pipes/translate.pipe';
import { LoadingComponent } from '../../../../shared/components/loading/loading.component';

@Component({
  selector: 'app-dock-table',
  standalone: true,
  imports: [CommonModule, TranslatePipe, LoadingComponent],
  templateUrl: './dock-table.component.html',
  styleUrls: ['./dock-table.component.scss']
})
export class DockTableComponent {
  @Input() docks: Dock[] = [];
  @Input() loading: boolean = false;
  @Output() onView = new EventEmitter<Dock>();
  @Output() onEdit = new EventEmitter<Dock>();

  viewDock(dock: Dock): void {
    this.onView.emit(dock);
  }

  editDock(dock: Dock): void {
    this.onEdit.emit(dock);
  }
}
