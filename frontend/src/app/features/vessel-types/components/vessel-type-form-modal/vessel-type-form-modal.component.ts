import { Component, EventEmitter, Input, Output, OnInit, OnChanges } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import {
  VesselType,
  CreateVesselTypeRequest,
  UpdateVesselTypeRequest
} from '../../../../core/models/vessel-type.model';
import { TranslatePipe } from '../../../../core/pipes/translate.pipe';

@Component({
  selector: 'app-vessel-type-form-modal',
  standalone: true,
  imports: [CommonModule, FormsModule, TranslatePipe],
  templateUrl: './vessel-type-form-modal.component.html',
  styleUrls: ['./vessel-type-form-modal.component.scss']
})
export class VesselTypeFormModalComponent implements OnInit, OnChanges {
  @Input() show = false;
  @Input() isEditMode = false;
  @Input() vesselType: VesselType | null = null;
  @Input() saving = false;

  @Output() close = new EventEmitter<void>();
  @Output() create = new EventEmitter<CreateVesselTypeRequest>();
  @Output() update = new EventEmitter<UpdateVesselTypeRequest>();

  formData = {
    vesselTypeId: '',
    name: '',
    description: '',
    capacityTEU: null as number | null,
    maxRows: null as number | null,
    maxBays: null as number | null,
    maxTiers: null as number | null,
    operationalConstraints: ''
  };

  ngOnInit(): void {
    this.initializeForm();
  }

  ngOnChanges(): void {
    this.initializeForm();
  }

  initializeForm(): void {
    if (this.isEditMode && this.vesselType) {
      this.formData = {
        vesselTypeId: this.vesselType.vesselTypeId,
        name: this.vesselType.name,
        description: this.vesselType.description || '',
        capacityTEU: this.vesselType.capacityTEU || null,
        maxRows: this.vesselType.maxRows || null,
        maxBays: this.vesselType.maxBays || null,
        maxTiers: this.vesselType.maxTiers || null,
        operationalConstraints: this.vesselType.operationalConstraints || ''
      };
    } else {
      this.formData = {
        vesselTypeId: '',
        name: '',
        description: '',
        capacityTEU: null,
        maxRows: null,
        maxBays: null,
        maxTiers: null,
        operationalConstraints: ''
      };
    }
  }

  onSubmit(): void {
    if (!this.formData.name.trim()) {
      return;
    }

    if (this.isEditMode) {
      const request: UpdateVesselTypeRequest = {
        name: this.formData.name.trim(),
        description: this.formData.description.trim() || undefined,
        capacityTEU: this.formData.capacityTEU || undefined,
        maxRows: this.formData.maxRows || undefined,
        maxBays: this.formData.maxBays || undefined,
        maxTiers: this.formData.maxTiers || undefined,
        operationalConstraints: this.formData.operationalConstraints.trim() || undefined
      };
      this.update.emit(request);
    } else {
      const request: CreateVesselTypeRequest = {
        name: this.formData.name.trim(),
        description: this.formData.description.trim() || undefined,
        capacityTEU: this.formData.capacityTEU || undefined,
        maxRows: this.formData.maxRows || undefined,
        maxBays: this.formData.maxBays || undefined,
        maxTiers: this.formData.maxTiers || undefined,
        operationalConstraints: this.formData.operationalConstraints.trim() || undefined,
        vesselTypeId: this.formData.vesselTypeId.trim() || undefined
      };
      this.create.emit(request);
    }
  }

  onClose(): void {
    this.close.emit();
  }

  onBackdropClick(event: MouseEvent): void {
    if (event.target === event.currentTarget) {
      this.onClose();
    }
  }
}
