import { Component, EventEmitter, Input, Output } from '@angular/core';
import { CommonModule } from '@angular/common';
import { Organization } from '../../../../core/models/organization.model';

@Component({
  selector: 'app-shipping-agent-view-modal',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './shipping-agent-view-modal.component.html',
  styleUrls: ['./shipping-agent-view-modal.component.scss']
})
export class ShippingAgentViewModalComponent {
  @Input() show: boolean = false;
  @Input() organization: Organization | null = null;
  @Output() onClose = new EventEmitter<void>();
  @Output() onAddRepresentative = new EventEmitter<Organization>();
  @Output() onEditRepresentative = new EventEmitter<{ organization: Organization; representative: any }>();
  @Output() onToggleStatus = new EventEmitter<{ organizationId: string; representativeId: string; isActive: boolean }>();

  close(): void {
    this.onClose.emit();
  }

  addRepresentative(): void {
    if (this.organization) {
      this.onAddRepresentative.emit(this.organization);
    }
  }

  editRepresentative(rep: any): void {
    if (this.organization) {
      this.onEditRepresentative.emit({ organization: this.organization, representative: rep });
    }
  }

  toggleRepresentativeStatus(rep: any): void {
    if (this.organization) {
      this.onToggleStatus.emit({
        organizationId: this.organization.organizationId,
        representativeId: rep.id,
        isActive: !rep.isActive
      });
    }
  }
}
