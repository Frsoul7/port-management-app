import { Component, EventEmitter, Input, Output, OnChanges, SimpleChanges } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { Vessel, VesselType, Organization, CreateVesselRequest, UpdateVesselRequest } from '../../../../core/models/vessel.model';
import { TranslatePipe } from '../../../../core/pipes/translate.pipe';
import { LoadingComponent } from '../../../../shared/components/loading/loading.component';

@Component({
  selector: 'app-vessel-form-modal',
  standalone: true,
  imports: [CommonModule, FormsModule, TranslatePipe, LoadingComponent],
  templateUrl: './vessel-form-modal.component.html',
  styleUrls: ['./vessel-form-modal.component.scss']
})
export class VesselFormModalComponent implements OnChanges {
  @Input() show: boolean = false;
  @Input() isEditMode: boolean = false;
  @Input() vessel: Vessel | null = null;
  @Input() vesselTypes: VesselType[] = [];
  @Input() organizations: Organization[] = [];
  @Input() saving: boolean = false;
  @Output() onClose = new EventEmitter<void>();
  @Output() onCreate = new EventEmitter<CreateVesselRequest>();
  @Output() onUpdate = new EventEmitter<{ imo: string; request: UpdateVesselRequest }>();

  currentVessel: any = {
    imoNumber: '',
    name: '',
    vesselTypeId: '',
    organizationId: '',
    capacityTEU: null
  };

  imoValidationMessage = '';

  ngOnChanges(changes: SimpleChanges): void {
    if (changes['vessel'] && this.vessel) {
      this.currentVessel = {
        imoNumber: this.vessel.imoNumber,
        name: this.vessel.name,
        vesselTypeId: this.vessel.vesselTypeId,
        organizationId: this.vessel.organizationId,
        capacityTEU: this.vessel.capacityTEU
      };
    } else if (changes['show'] && this.show && !this.isEditMode) {
      this.resetForm();
    }
  }

  resetForm(): void {
    this.currentVessel = {
      imoNumber: '',
      name: '',
      vesselTypeId: '',
      organizationId: '',
      capacityTEU: null
    };
    this.imoValidationMessage = '';
  }

  get shippingAgentOrganizations(): Organization[] {
    return this.organizations.filter(org => org.type === 'SHIPPING_AGENT');
  }

  close(): void {
    this.onClose.emit();
    this.resetForm();
  }

  save(): void {
    if (this.isEditMode && this.vessel) {
      const updateRequest: UpdateVesselRequest = {
        name: this.currentVessel.name,
        vesselTypeId: this.currentVessel.vesselTypeId,
        organizationId: this.currentVessel.organizationId,
        capacityTEU: this.currentVessel.capacityTEU
      };
      this.onUpdate.emit({ imo: this.currentVessel.imoNumber, request: updateRequest });
    } else {
      const createRequest: CreateVesselRequest = {
        imoNumber: this.currentVessel.imoNumber,
        name: this.currentVessel.name,
        vesselTypeId: this.currentVessel.vesselTypeId,
        organizationId: this.currentVessel.organizationId,
        capacityTEU: this.currentVessel.capacityTEU
      };
      this.onCreate.emit(createRequest);
    }
  }

  onImoNumberChange(): void {
    if (!this.currentVessel.imoNumber || this.isEditMode) {
      this.imoValidationMessage = '';
      return;
    }

    const result = this.validateImoNumber(this.currentVessel.imoNumber);
    
    if (result.valid) {
      this.imoValidationMessage = 'Valid IMO number';
    } else {
      this.imoValidationMessage = result.message;
      if (result.suggestedImo) {
        this.imoValidationMessage += `. Did you mean: ${result.suggestedImo}?`;
      }
    }
  }

  validateImoNumber(imo: string): { valid: boolean; message: string; suggestedImo?: string } {
    if (!imo || imo.trim().length === 0) {
      return { valid: false, message: 'IMO number is required' };
    }

    const trimmed = imo.trim();
    
    if (trimmed.length !== 7) {
      return { valid: false, message: 'IMO must be exactly 7 digits' };
    }

    if (!/^\d{7}$/.test(trimmed)) {
      return { valid: false, message: 'IMO must contain only digits' };
    }

    // Calculate check digit
    const digits = trimmed.split('').map(d => parseInt(d, 10));
    const sum = digits[0] * 7 + digits[1] * 6 + digits[2] * 5 + 
                digits[3] * 4 + digits[4] * 3 + digits[5] * 2;
    const calculatedCheckDigit = sum % 10;
    const actualCheckDigit = digits[6];

    if (calculatedCheckDigit !== actualCheckDigit) {
      const correctImo = trimmed.substring(0, 6) + calculatedCheckDigit;
      return { 
        valid: false, 
        message: `Invalid check digit. Expected: ${calculatedCheckDigit}, but got: ${actualCheckDigit}`,
        suggestedImo: correctImo
      };
    }

    return { valid: true, message: 'Valid IMO number' };
  }

  showImoHelp(): void {
    const examples = [
      '9319466',
      '9176187', 
      '9074729',
      '9362255',
      '9321483'
    ];
    
    alert(
      'IMO Number Format:\n\n' +
      '• Exactly 7 digits (no letters, no "IMO" prefix)\n' +
      '• Last digit is a check digit calculated from first 6 digits\n' +
      '• Example valid IMO numbers:\n' +
      examples.map(imo => `  - ${imo}`).join('\n') +
      '\n\n' +
      'The check digit calculation:\n' +
      'digit1×7 + digit2×6 + digit3×5 + digit4×4 + digit5×3 + digit6×2\n' +
      'The last digit of this sum is the check digit.\n\n' +
      'Example for 9319466:\n' +
      '9×7 + 3×6 + 1×5 + 9×4 + 4×3 + 6×2 = 63 + 18 + 5 + 36 + 12 + 12 = 146\n' +
      'Check digit = 6 (last digit of 146)\n\n' +
      'Why 7123712 is invalid:\n' +
      '7×7 + 1×6 + 2×5 + 3×4 + 7×3 + 1×2 = 49 + 6 + 10 + 12 + 21 + 2 = 100\n' +
      'Check digit should be 0, but the number has 2.\n' +
      'Correct IMO: 7123710'
    );
  }
}
