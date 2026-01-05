import { Component, EventEmitter, Input, Output, OnChanges, SimpleChanges } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { Organization, RepresentativeInput, UpdateRepresentativeRequest } from '../../../../core/models/organization.model';
import { LoadingComponent } from '../../../../shared/components/loading/loading.component';

@Component({
  selector: 'app-representative-form-modal',
  standalone: true,
  imports: [CommonModule, FormsModule, LoadingComponent],
  templateUrl: './representative-form-modal.component.html',
  styleUrls: ['./representative-form-modal.component.scss']
})
export class RepresentativeFormModalComponent implements OnChanges {
  @Input() show: boolean = false;
  @Input() organization: Organization | null = null;
  @Input() representative: any = null;
  @Input() isEditMode: boolean = false;
  @Input() saving: boolean = false;
  @Output() onClose = new EventEmitter<void>();
  @Output() onAdd = new EventEmitter<{ organizationId: string; representatives: RepresentativeInput[] }>();
  @Output() onUpdate = new EventEmitter<{ organizationId: string; representativeId: string; request: UpdateRepresentativeRequest }>();

  formData: RepresentativeInput = {
    name: '',
    citizenId: '',
    nationality: '',
    email: '',
    phone: ''
  };

  ngOnChanges(changes: SimpleChanges): void {
    if (changes['show'] && this.show) {
      if (this.isEditMode && this.representative) {
        this.formData = {
          name: this.representative.name,
          citizenId: this.representative.citizenId,
          nationality: this.representative.nationality,
          email: this.representative.email,
          phone: this.representative.phone
        };
      } else {
        this.resetForm();
      }
    }
  }

  resetForm(): void {
    this.formData = {
      name: '',
      citizenId: '',
      nationality: '',
      email: '',
      phone: ''
    };
  }

  close(): void {
    this.onClose.emit();
  }

  isFormValid(): boolean {
    return !!(this.formData.name && 
              this.formData.citizenId && 
              this.formData.nationality && 
              this.formData.email && 
              this.formData.phone);
  }

  save(): void {
    if (!this.isFormValid() || !this.organization) {
      return;
    }

    if (this.isEditMode && this.representative) {
      const updateRequest: UpdateRepresentativeRequest = { ...this.formData };
      this.onUpdate.emit({
        organizationId: this.organization.organizationId,
        representativeId: this.representative.id,
        request: updateRequest
      });
    } else {
      this.onAdd.emit({
        organizationId: this.organization.organizationId,
        representatives: [this.formData]
      });
    }
  }
}
