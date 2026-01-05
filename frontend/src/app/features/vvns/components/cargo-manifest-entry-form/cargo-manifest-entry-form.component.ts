import { Component, EventEmitter, Input, Output } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { AddManifestEntryRequest, ManifestType } from '../../../../core/models/vvn.model';

@Component({
  selector: 'app-cargo-manifest-entry-form',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './cargo-manifest-entry-form.component.html',
  styleUrls: ['./cargo-manifest-entry-form.component.scss']
})
export class CargoManifestEntryFormComponent {
  @Input() type: ManifestType = ManifestType.Load;
  @Input() disabled = false;
  @Output() onAdd = new EventEmitter<AddManifestEntryRequest>();
  @Output() onCancel = new EventEmitter<void>();

  formData = {
    containerCode: '',
    bay: 1,
    row: 1,
    tier: 1,
    goodsDescription: '',
    hazardous: false
  };

  validationErrors: { [key: string]: string } = {};

  resetForm(): void {
    this.formData = {
      containerCode: '',
      bay: 1,
      row: 1,
      tier: 1,
      goodsDescription: '',
      hazardous: false
    };
    this.validationErrors = {};
  }

  validate(): boolean {
    this.validationErrors = {};

    // Container Code validation (ISO 6346 format: 3 letters + U/J/Z + 7 digits)
    if (!this.formData.containerCode?.trim()) {
      this.validationErrors['containerCode'] = 'Container code is required';
    } else {
      const code = this.formData.containerCode.trim().toUpperCase();
      // ISO 6346: 3 letters (owner) + 1 category letter (U/J/Z) + 6 digits (serial) + 1 check digit
      if (!/^[A-Z]{3}[UJZ]\d{7}$/.test(code)) {
        this.validationErrors['containerCode'] = 'Invalid ISO 6346 format. Expected: 3 letters + category (U/J/Z) + 7 digits (e.g., MSCU1234560)';
      }
    }

    // Bay validation
    if (this.formData.bay < 1) {
      this.validationErrors['bay'] = 'Bay must be at least 1';
    }

    // Row validation
    if (this.formData.row < 1) {
      this.validationErrors['row'] = 'Row must be at least 1';
    }

    // Tier validation
    if (this.formData.tier < 1) {
      this.validationErrors['tier'] = 'Tier must be at least 1';
    }

    return Object.keys(this.validationErrors).length === 0;
  }

  handleSubmit(): void {
    if (!this.validate()) {
      return;
    }

    const request: AddManifestEntryRequest = {
      type: this.type,
      containerCode: this.formData.containerCode.trim().toUpperCase(),
      bay: this.formData.bay,
      row: this.formData.row,
      tier: this.formData.tier,
      goodsDescription: this.formData.goodsDescription?.trim() || undefined,
      hazardous: this.formData.hazardous
    };

    this.onAdd.emit(request);
    this.resetForm();
  }

  handleCancel(): void {
    this.resetForm();
    this.onCancel.emit();
  }
}
