import { Component, OnInit } from '@angular/core';
import { CommonModule } from '@angular/common';
import { Router } from '@angular/router';
import { VvnService } from '../../core/services/vvn.service';
import { AuthService } from '../../core/services/auth.service';
import { VesselService } from '../../core/services/vessel.service';
import { MessageModalService } from '../../core/services/message-modal.service';
import { 
  VvnStatusResponse, 
  VvnSummaryResponse, 
  CreateVvnRequest, 
  UpdateVvnRequest,
  VvnSearchParams,
  VvnApprovalRequest,
  VvnRejectionRequest
} from '../../core/models/vvn.model';
import { UserRole } from '../../core/models/user.model';
import { VvnSearchBarComponent } from './components/vvn-search-bar/vvn-search-bar.component';
import { VvnTableComponent } from './components/vvn-table/vvn-table.component';
import { VvnFormModalComponent } from './components/vvn-form-modal/vvn-form-modal.component';
import { VvnViewModalComponent } from './components/vvn-view-modal/vvn-view-modal.component';
import { AlertBannerComponent } from '../../shared/components/alert-banner/alert-banner.component';
import { TranslatePipe } from '../../core/pipes/translate.pipe';

@Component({
  selector: 'app-vvns',
  standalone: true,
  imports: [
    CommonModule,
    VvnSearchBarComponent,
    VvnTableComponent,
    VvnFormModalComponent,
    VvnViewModalComponent,
    AlertBannerComponent,
    TranslatePipe
  ],
  templateUrl: './vvns.component.html',
  styleUrls: ['./vvns.component.scss']
})
export class VvnsComponent implements OnInit {
  vvns: (VvnStatusResponse | VvnSummaryResponse)[] = [];
  vessels: any[] = [];
  
  showFormModal = false;
  showViewModal = false;
  isEditMode = false;
  loading = false;
  saving = false;
  errorMessage = '';
  successMessage = '';

  selectedVvn: VvnStatusResponse | VvnSummaryResponse | null = null;
  selectedVvnForEdit: VvnStatusResponse | null = null;

  // Current user role
  currentRole: UserRole | null = null;
  isShippingAgent = false;
  isPortAuthority = false;
  isLogisticsOperator = false;

  constructor(
    private vvnService: VvnService,
    private vesselService: VesselService,
    private authService: AuthService,
    private modalService: MessageModalService,
    private router: Router
  ) {}

  ngOnInit(): void {
    this.currentRole = this.authService.getUserRole();
    this.isShippingAgent = this.authService.isShippingAgent();
    this.isPortAuthority = this.authService.hasRole(UserRole.PORT_AUTHORITY_OFFICER);
    this.isLogisticsOperator = this.authService.hasRole(UserRole.LOGISTICS_OPERATOR);
    
    this.loadInitialData();
  }

  async loadInitialData(): Promise<void> {
    this.loading = true;
    try {
      // Load VVNs based on user role
      await this.loadVvns({});
      
      // Load vessels for dropdowns
      this.vesselService.searchVessels({}).subscribe({
        next: (vessels) => {
          this.vessels = vessels;
        },
        error: (error) => {
          console.error('Error loading vessels:', error);
        }
      });
    } finally {
      this.loading = false;
    }
  }

  async loadVvns(params: Partial<VvnSearchParams>): Promise<void> {
    return new Promise((resolve) => {
      this.loading = true;
      this.errorMessage = '';
      
      // Shipping Agents see their organization's VVNs
      if (this.isShippingAgent) {
        const searchParams: VvnSearchParams = {
          vesselImo: params.vesselImo,
          status: params.status,
          submittedById: params.submittedById,
          fromDate: params.fromDate,
          toDate: params.toDate
        };

        this.vvnService.getMyOrganizationVvns(searchParams).subscribe({
          next: (vvns) => {
            this.vvns = vvns;
            this.loading = false;
            resolve();
          },
          error: (error) => {
            console.error('Error loading VVNs:', error);
            this.errorMessage = error.error?.error || 'Failed to load VVNs. Please try again.';
            this.loading = false;
            resolve();
          }
        });
      } else {
        // Port Authority and Logistics Operators see all VVNs
        this.vvnService.getAllVvns().subscribe({
          next: (vvns) => {
            this.vvns = vvns;
            this.loading = false;
            resolve();
          },
          error: (error) => {
            console.error('Error loading VVNs:', error);
            this.errorMessage = error.error?.error || 'Failed to load VVNs. Please try again.';
            this.loading = false;
            resolve();
          }
        });
      }
    });
  }

  // Search Bar Event Handlers
  handleSearch(params: VvnSearchParams): void {
    this.loadVvns(params);
  }

  handleClear(): void {
    this.loadVvns({});
  }

  handleAdd(): void {
    if (!this.isShippingAgent) {
      this.errorMessage = 'Only Shipping Agent Representatives can create VVNs.';
      this.autoHideMessage();
      return;
    }
    
    this.isEditMode = false;
    this.selectedVvnForEdit = null;
    this.showFormModal = true;
  }

  // Table Event Handlers
  handleView(vvn: VvnStatusResponse | VvnSummaryResponse): void {
    this.selectedVvn = vvn;
    this.showViewModal = true;
  }

  handleEdit(vvn: VvnStatusResponse | VvnSummaryResponse): void {
    if (!this.vvnService.canEdit(vvn)) {
      this.errorMessage = 'This VVN cannot be edited in its current state.';
      this.autoHideMessage();
      return;
    }
    
    // Load full details for editing
    this.vvnService.getMyOrganizationVvns({ vesselImo: vvn.vesselImo }).subscribe({
      next: (vvns) => {
        const fullVvn = vvns.find(v => v.id === vvn.id);
        if (fullVvn) {
          this.isEditMode = true;
          this.selectedVvnForEdit = fullVvn;
          this.showFormModal = true;
        }
      },
      error: (error) => {
        console.error('Error loading VVN details:', error);
        this.errorMessage = 'Failed to load VVN details for editing.';
        this.autoHideMessage();
      }
    });
  }

  handleSubmit(vvn: VvnStatusResponse | VvnSummaryResponse): void {
    if (!this.vvnService.canSubmit(vvn)) {
      this.modalService.showError(
        'Cannot Submit',
        'This VVN cannot be submitted in its current state.'
      );
      return;
    }

    this.modalService.showConfirm(
      'Submit VVN',
      'Are you sure you want to submit this VVN for approval by the Port Authority?',
      () => this.performSubmit(vvn),
      () => console.log('Submit cancelled')
    );
  }

  private performSubmit(vvn: VvnStatusResponse | VvnSummaryResponse): void {
    this.saving = true;
    this.vvnService.submitVvn(vvn.id).subscribe({
      next: () => {
        this.modalService.showSuccess(
          'VVN Submitted',
          'Your Vessel Visit Notification has been submitted successfully and is now pending Port Authority approval.'
        );
        this.loadVvns({});
        this.saving = false;
      },
      error: (error) => {
        console.error('Error submitting VVN:', error);
        const errorMsg = error.error?.value?.detail || error.error?.detail || 'Failed to submit VVN. Please try again.';
        this.modalService.showError('Submit Failed', errorMsg);
        this.saving = false;
      }
    });
  }

  handleReopen(vvn: VvnStatusResponse | VvnSummaryResponse): void {
    if (!this.vvnService.canReopen(vvn)) {
      this.modalService.showError(
        'Cannot Reopen',
        'This VVN cannot be reopened.'
      );
      return;
    }

    this.modalService.showConfirm(
      'Reopen VVN',
      'Are you sure you want to reopen this VVN for editing? It will return to "In Progress" status.',
      () => this.performReopen(vvn),
      () => console.log('Reopen cancelled')
    );
  }

  private performReopen(vvn: VvnStatusResponse | VvnSummaryResponse): void {
    this.saving = true;
    this.vvnService.reopenVvn(vvn.id).subscribe({
      next: () => {
        this.modalService.showSuccess(
          'VVN Reopened',
          'The VVN has been reopened and is now available for editing.'
        );
        this.loadVvns({});
        this.saving = false;
      },
      error: (error) => {
        console.error('Error reopening VVN:', error);
        const errorMsg = error.error?.error || 'Failed to reopen VVN. Please try again.';
        this.modalService.showError('Reopen Failed', errorMsg);
        this.saving = false;
      }
    });
  }

  // Form Modal Event Handlers
  handleModalClose(): void {
    this.showFormModal = false;
    this.selectedVvnForEdit = null;
  }

  handleCreate(request: CreateVvnRequest): void {
    this.saving = true;
    this.errorMessage = '';

    this.vvnService.createVvn(request).subscribe({
      next: () => {
        this.successMessage = 'VVN created successfully!';
        this.showFormModal = false;
        this.loadVvns({});
        this.saving = false;
        this.autoHideMessage();
      },
      error: (error) => {
        console.error('Error creating VVN:', error);
        this.errorMessage = error.error?.detail || error.error?.error || 'Failed to create VVN. Please try again.';
        this.saving = false;
        this.autoHideMessage();
      }
    });
  }

  handleUpdate(data: { id: string; request: UpdateVvnRequest }): void {
    this.saving = true;
    this.errorMessage = '';

    this.vvnService.updateVvn(data.id, data.request).subscribe({
      next: () => {
        this.successMessage = 'VVN updated successfully!';
        this.showFormModal = false;
        this.loadVvns({});
        this.saving = false;
        this.autoHideMessage();
      },
      error: (error) => {
        console.error('Error updating VVN:', error);
        this.errorMessage = error.error?.detail || error.error?.error || 'Failed to update VVN. Please try again.';
        this.saving = false;
        this.autoHideMessage();
      }
    });
  }

  // View Modal Event Handlers
  handleViewModalClose(): void {
    this.showViewModal = false;
    this.selectedVvn = null;
  }

  handleEditFromView(vvn: VvnStatusResponse | VvnSummaryResponse): void {
    this.showViewModal = false;
    this.handleEdit(vvn);
  }

  handleApprove(data: { id: string; request: VvnApprovalRequest }): void {
    this.saving = true;
    this.errorMessage = '';

    this.vvnService.approveVvn(data.id, data.request).subscribe({
      next: () => {
        this.successMessage = 'VVN approved successfully!';
        this.showViewModal = false;
        this.loadVvns({});
        this.saving = false;
        this.autoHideMessage();
      },
      error: (error) => {
        console.error('Error approving VVN:', error);
        this.errorMessage = error.error?.detail || error.error?.error || 'Failed to approve VVN. Please try again.';
        this.saving = false;
        this.autoHideMessage();
      }
    });
  }

  handleReject(data: { id: string; request: VvnRejectionRequest }): void {
    this.saving = true;
    this.errorMessage = '';

    this.vvnService.rejectVvn(data.id, data.request).subscribe({
      next: () => {
        this.successMessage = 'VVN rejected successfully!';
        this.showViewModal = false;
        this.loadVvns({});
        this.saving = false;
        this.autoHideMessage();
      },
      error: (error) => {
        console.error('Error rejecting VVN:', error);
        this.errorMessage = error.error?.detail || error.error?.error || 'Failed to reject VVN. Please try again.';
        this.saving = false;
        this.autoHideMessage();
      }
    });
  }

  autoHideMessage(): void {
    setTimeout(() => {
      this.successMessage = '';
      this.errorMessage = '';
    }, 5000);
  }

  goBack(): void {
    this.router.navigate(['/dashboard']);
  }
}
