import { Component, EventEmitter, Input, Output, OnChanges, SimpleChanges } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import {
  VvnStatusResponse,
  CreateVvnRequest,
  UpdateVvnRequest,
  VvnPurpose
} from '../../../../core/models/vvn.model';
import { TranslatePipe } from '../../../../core/pipes/translate.pipe';
import { LoadingComponent } from '../../../../shared/components/loading/loading.component';

@Component({
  selector: 'app-vvn-form-modal',
  standalone: true,
  imports: [CommonModule, FormsModule, TranslatePipe, LoadingComponent],
  templateUrl: './vvn-form-modal.component.html',
  styleUrls: ['./vvn-form-modal.component.scss']
})
export class VvnFormModalComponent implements OnChanges {
  @Input() show = false;
  @Input() isEditMode = false;
  @Input() vvn: VvnStatusResponse | null = null;
  @Input() vessels: any[] = [];
  @Input() saving = false;
  @Output() onClose = new EventEmitter<void>();
  @Output() onCreate = new EventEmitter<CreateVvnRequest>();
  @Output() onUpdate = new EventEmitter<{ id: string; request: UpdateVvnRequest }>();

  formData = {
    vesselImo: '',
    purpose: 'LOAD',
    eta: '',
    etd: '',
    captainName: '',
    captainCitizenId: '',
    captainNationality: '',
    crewCount: 0
  };

  purposeOptions = [
    { value: 'LOAD', label: 'Loading' },
    { value: 'UNLOAD', label: 'Unloading' },
    { value: 'BOTH', label: 'Both (Load & Unload)' },
    { value: 'MAINTENANCE', label: 'Maintenance' }
  ];

  validationErrors: { [key: string]: string } = {};

  ngOnChanges(changes: SimpleChanges): void {
    if (changes['show'] && this.show) {
      if (this.isEditMode && this.vvn) {
        this.loadVvnData();
      } else {
        this.resetForm();
      }
    }
  }

  loadVvnData(): void {
    if (this.vvn) {
      this.formData = {
        vesselImo: this.vvn.vesselImo,
        purpose: this.vvn.purpose,
        eta: this.formatDateForInput(this.vvn.eta),
        etd: this.formatDateForInput(this.vvn.etd),
        captainName: this.vvn.captainName,
        captainCitizenId: this.vvn.captainCitizenId,
        captainNationality: this.vvn.captainNationality,
        crewCount: this.vvn.crewCount
      };
    }
  }

  formatDateForInput(dateString: string): string {
    if (!dateString) return '';
    const date = new Date(dateString);
    // Format as YYYY-MM-DDTHH:MM for datetime-local input
    const year = date.getFullYear();
    const month = String(date.getMonth() + 1).padStart(2, '0');
    const day = String(date.getDate()).padStart(2, '0');
    const hours = String(date.getHours()).padStart(2, '0');
    const minutes = String(date.getMinutes()).padStart(2, '0');
    return `${year}-${month}-${day}T${hours}:${minutes}`;
  }

  resetForm(): void {
    this.formData = {
      vesselImo: '',
      purpose: 'LOAD',
      eta: '',
      etd: '',
      captainName: '',
      captainCitizenId: '',
      captainNationality: '',
      crewCount: 0
    };
    this.validationErrors = {};
  }

  validate(): boolean {
    this.validationErrors = {};

    if (!this.formData.vesselImo?.trim()) {
      this.validationErrors['vesselImo'] = 'Vessel IMO is required';
    } else if (!/^\d{7}$/.test(this.formData.vesselImo.trim())) {
      this.validationErrors['vesselImo'] = 'IMO must be exactly 7 digits';
    }

    if (!this.formData.purpose) {
      this.validationErrors['purpose'] = 'Purpose is required';
    }

    if (!this.formData.eta) {
      this.validationErrors['eta'] = 'ETA is required';
    }

    if (!this.formData.etd) {
      this.validationErrors['etd'] = 'ETD is required';
    }

    if (this.formData.eta && this.formData.etd) {
      const eta = new Date(this.formData.eta);
      const etd = new Date(this.formData.etd);
      if (etd <= eta) {
        this.validationErrors['etd'] = 'ETD must be after ETA';
      }
    }

    if (!this.formData.captainName?.trim()) {
      this.validationErrors['captainName'] = 'Captain name is required';
    }

    if (!this.formData.captainCitizenId?.trim()) {
      this.validationErrors['captainCitizenId'] = 'Captain citizen ID is required';
    }

    if (!this.formData.captainNationality?.trim()) {
      this.validationErrors['captainNationality'] = 'Captain nationality is required';
    } else if (!/^[A-Z]{2}$/.test(this.formData.captainNationality.trim().toUpperCase())) {
      this.validationErrors['captainNationality'] = 'Nationality must be 2-letter code (e.g., US, PT)';
    }

    if (this.formData.crewCount < 1) {
      this.validationErrors['crewCount'] = 'Crew count must be at least 1';
    }

    return Object.keys(this.validationErrors).length === 0;
  }

  handleSubmit(): void {
    if (!this.validate()) {
      return;
    }

    if (this.isEditMode && this.vvn) {
      const updateRequest: UpdateVvnRequest = {
        purpose: this.formData.purpose,
        eta: new Date(this.formData.eta).toISOString(),
        etd: new Date(this.formData.etd).toISOString(),
        captainName: this.formData.captainName.trim(),
        captainCitizenId: this.formData.captainCitizenId.trim(),
        captainNationality: this.formData.captainNationality.trim().toUpperCase(),
        crewCount: this.formData.crewCount
      };
      this.onUpdate.emit({ id: this.vvn.id, request: updateRequest });
    } else {
      const createRequest: CreateVvnRequest = {
        vesselImo: this.formData.vesselImo.trim(),
        purpose: this.formData.purpose,
        eta: new Date(this.formData.eta).toISOString(),
        etd: new Date(this.formData.etd).toISOString(),
        captainName: this.formData.captainName.trim(),
        captainCitizenId: this.formData.captainCitizenId.trim(),
        captainNationality: this.formData.captainNationality.trim().toUpperCase(),
        crewCount: this.formData.crewCount
      };
      this.onCreate.emit(createRequest);
    }
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
