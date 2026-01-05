import { Component, EventEmitter, Input, Output } from '@angular/core';
import { CommonModule } from '@angular/common';
import { VvnStatusResponse, VvnSummaryResponse } from '../../../../core/models/vvn.model';
import { TranslatePipe } from '../../../../core/pipes/translate.pipe';
import { LoadingComponent } from '../../../../shared/components/loading/loading.component';

@Component({
  selector: 'app-vvn-table',
  standalone: true,
  imports: [CommonModule, TranslatePipe, LoadingComponent],
  templateUrl: './vvn-table.component.html',
  styleUrls: ['./vvn-table.component.scss']
})
export class VvnTableComponent {
  @Input() vvns: (VvnStatusResponse | VvnSummaryResponse)[] = [];
  @Input() loading: boolean = false;
  @Input() isShippingAgent: boolean = false;
  @Input() isPortAuthority: boolean = false;
  @Output() onView = new EventEmitter<VvnStatusResponse | VvnSummaryResponse>();
  @Output() onEdit = new EventEmitter<VvnStatusResponse | VvnSummaryResponse>();
  @Output() onSubmit = new EventEmitter<VvnStatusResponse | VvnSummaryResponse>();
  @Output() onReopen = new EventEmitter<VvnStatusResponse | VvnSummaryResponse>();

  viewVvn(vvn: VvnStatusResponse | VvnSummaryResponse): void {
    this.onView.emit(vvn);
  }

  editVvn(vvn: VvnStatusResponse | VvnSummaryResponse): void {
    this.onEdit.emit(vvn);
  }

  submitVvn(vvn: VvnStatusResponse | VvnSummaryResponse): void {
    this.onSubmit.emit(vvn);
  }

  reopenVvn(vvn: VvnStatusResponse | VvnSummaryResponse): void {
    this.onReopen.emit(vvn);
  }

  canEdit(vvn: VvnStatusResponse | VvnSummaryResponse): boolean {
    return this.isShippingAgent && (vvn.status === 'IN_PROGRESS' || vvn.status === 'REJECTED');
  }

  canSubmit(vvn: VvnStatusResponse | VvnSummaryResponse): boolean {
    return this.isShippingAgent && vvn.status === 'IN_PROGRESS';
  }

  canReopen(vvn: VvnStatusResponse | VvnSummaryResponse): boolean {
    return this.isShippingAgent && vvn.status === 'REJECTED';
  }

  getStatusClass(status: string): string {
    switch (status) {
      case 'IN_PROGRESS':
        return 'status-in-progress';
      case 'SUBMITTED':
        return 'status-submitted';
      case 'APPROVED':
        return 'status-approved';
      case 'REJECTED':
        return 'status-rejected';
      default:
        return '';
    }
  }

  getStatusLabel(status: string): string {
    switch (status) {
      case 'IN_PROGRESS':
        return 'In Progress';
      case 'SUBMITTED':
        return 'Submitted';
      case 'APPROVED':
        return 'Approved';
      case 'REJECTED':
        return 'Rejected';
      default:
        return status;
    }
  }

  formatDate(dateString: string | undefined): string {
    if (!dateString) return 'N/A';
    const date = new Date(dateString);
    return date.toLocaleString('en-US', {
      year: 'numeric',
      month: '2-digit',
      day: '2-digit',
      hour: '2-digit',
      minute: '2-digit'
    });
  }

  getVesselName(vvn: VvnStatusResponse | VvnSummaryResponse): string {
    return (vvn as VvnSummaryResponse).vesselName || 'N/A';
  }

  getAssignedDock(vvn: VvnStatusResponse | VvnSummaryResponse): string {
    return (vvn as VvnSummaryResponse).assignedDock || 'N/A';
  }
}
