import { Component, EventEmitter, Input, Output, OnChanges, SimpleChanges } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import {
  VvnStatusResponse,
  VvnSummaryResponse,
  VvnApprovalRequest,
  VvnRejectionRequest
} from '../../../../core/models/vvn.model';
import { DockService } from '../../../../core/services/dock.service';
import { MessageModalService } from '../../../../core/services/message-modal.service';
import { Dock } from '../../../../core/models/dock.model';
import { TranslatePipe } from '../../../../core/pipes/translate.pipe';
import { LoadingComponent } from '../../../../shared/components/loading/loading.component';
import { CargoManifestManagerComponent } from '../cargo-manifest-manager/cargo-manifest-manager.component';
import { CrewMembersManagerComponent } from '../crew-members-manager/crew-members-manager.component';

@Component({
  selector: 'app-vvn-view-modal',
  standalone: true,
  imports: [CommonModule, FormsModule, TranslatePipe, LoadingComponent, CargoManifestManagerComponent, CrewMembersManagerComponent],
  templateUrl: './vvn-view-modal.component.html',
  styleUrls: ['./vvn-view-modal.component.scss']
})
export class VvnViewModalComponent implements OnChanges {
  @Input() show = false;
  @Input() vvn: VvnStatusResponse | VvnSummaryResponse | null = null;
  @Input() isShippingAgent = false;
  @Input() isPortAuthority = false;
  @Input() saving = false;
  @Output() onClose = new EventEmitter<void>();
  @Output() onEdit = new EventEmitter<VvnStatusResponse | VvnSummaryResponse>();
  @Output() onApprove = new EventEmitter<{ id: string; request: VvnApprovalRequest }>();
  @Output() onReject = new EventEmitter<{ id: string; request: VvnRejectionRequest }>();

  showApprovalForm = false;
  showRejectionForm = false;
  availableDocks: Dock[] = [];
  loadingDocks = false;

  approvalData = {
    dockCode: '',
    berthFrom: '',
    berthTo: '',
    notes: ''
  };

  rejectionData = {
    reason: '',
    notes: ''
  };

  constructor(private dockService: DockService, private modalService: MessageModalService) {}

  ngOnChanges(changes: SimpleChanges): void {
    if (changes['show'] && !this.show) {
      this.resetForms();
    }
  }

  resetForms(): void {
    this.showApprovalForm = false;
    this.showRejectionForm = false;
    this.approvalData = {
      dockCode: '',
      berthFrom: '',
      berthTo: '',
      notes: ''
    };
    this.rejectionData = {
      reason: '',
      notes: ''
    };
  }

  get detailedVvn(): VvnStatusResponse | null {
    if (this.vvn && 'captainName' in this.vvn) {
      return this.vvn as VvnStatusResponse;
    }
    return null;
  }

  canEdit(): boolean {
    return !!(this.isShippingAgent && this.vvn && 
           (this.vvn.status === 'IN_PROGRESS' || this.vvn.status === 'REJECTED'));
  }

  canApproveReject(): boolean {
    return !!(this.isPortAuthority && this.vvn && this.vvn.status === 'SUBMITTED');
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

  getVesselName(): string {
    if (!this.vvn) return 'N/A';
    return (this.vvn as VvnSummaryResponse).vesselName || 'N/A';
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

  handleEdit(): void {
    if (this.vvn) {
      this.onEdit.emit(this.vvn);
    }
  }

  handleApprove(): void {
    this.showApprovalForm = true;
    this.showRejectionForm = false;
    this.loadDocks();
  }

  handleReject(): void {
    this.showRejectionForm = true;
    this.showApprovalForm = false;
  }

  loadDocks(): void {
    this.loadingDocks = true;
    this.dockService.searchDocks().subscribe({
      next: (docks) => {
        this.availableDocks = docks;
        this.loadingDocks = false;
      },
      error: (error) => {
        console.error('Error loading docks:', error);
        this.modalService.showError(
          'Load Docks Failed',
          'Failed to load available docks. Please try again.'
        );
        this.loadingDocks = false;
      }
    });
  }

  submitApproval(): void {
    if (!this.vvn) return;

    if (!this.approvalData.dockCode || !this.approvalData.berthFrom || !this.approvalData.berthTo) {
      this.modalService.showWarning(
        'Missing Information',
        'Please fill in all required fields: Dock, Berth From, and Berth To.'
      );
      return;
    }

    const request: VvnApprovalRequest = {
      dockCode: this.approvalData.dockCode.trim(),
      berthFrom: new Date(this.approvalData.berthFrom).toISOString(),
      berthTo: new Date(this.approvalData.berthTo).toISOString(),
      notes: this.approvalData.notes?.trim()
    };

    this.onApprove.emit({ id: this.vvn.id, request });
  }

  submitRejection(): void {
    if (!this.vvn) return;

    if (!this.rejectionData.reason) {
      this.modalService.showWarning(
        'Reason Required',
        'Please provide a reason for rejecting this Vessel Visit Notification.'
      );
      return;
    }

    const request: VvnRejectionRequest = {
      reason: this.rejectionData.reason.trim(),
      notes: this.rejectionData.notes?.trim()
    };

    this.onReject.emit({ id: this.vvn.id, request });
  }

  cancelApproval(): void {
    this.showApprovalForm = false;
    this.approvalData = {
      dockCode: '',
      berthFrom: '',
      berthTo: '',
      notes: ''
    };
  }

  cancelRejection(): void {
    this.showRejectionForm = false;
    this.rejectionData = {
      reason: '',
      notes: ''
    };
  }

  close(): void {
    this.onClose.emit();
  }

  handleBackdropClick(event: MouseEvent): void {
    if ((event.target as HTMLElement).classList.contains('modal-overlay')) {
      this.close();
    }
  }
}
