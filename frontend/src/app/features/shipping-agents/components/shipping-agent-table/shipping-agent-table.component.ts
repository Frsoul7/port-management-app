import { Component, EventEmitter, Input, Output } from '@angular/core';
import { CommonModule } from '@angular/common';
import { Organization } from '../../../../core/models/organization.model';
import { TranslatePipe } from '../../../../core/pipes/translate.pipe';
import { LoadingComponent } from '../../../../shared/components/loading/loading.component';

@Component({
  selector: 'app-shipping-agent-table',
  standalone: true,
  imports: [CommonModule, TranslatePipe, LoadingComponent],
  templateUrl: './shipping-agent-table.component.html',
  styleUrls: ['./shipping-agent-table.component.scss']
})
export class ShippingAgentTableComponent {
  @Input() organizations: Organization[] = [];
  @Input() loading: boolean = false;
  @Output() onView = new EventEmitter<Organization>();

  viewOrganization(org: Organization): void {
    this.onView.emit(org);
  }
}
