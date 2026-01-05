import { Component, EventEmitter, Input, Output } from '@angular/core';
import { CommonModule } from '@angular/common';
import { Dock } from '../../../../core/models/dock.model';
import { TranslatePipe } from '../../../../core/pipes/translate.pipe';

@Component({
  selector: 'app-dock-view-modal',
  standalone: true,
  imports: [CommonModule, TranslatePipe],
  templateUrl: './dock-view-modal.component.html',
  styleUrls: ['./dock-view-modal.component.scss']
})
export class DockViewModalComponent {
  @Input() show: boolean = false;
  @Input() dock: Dock | null = null;
  @Output() onClose = new EventEmitter<void>();
  @Output() onEdit = new EventEmitter<Dock>();

  close(): void {
    this.onClose.emit();
  }

  edit(): void {
    if (this.dock) {
      this.onEdit.emit(this.dock);
    }
  }
}
