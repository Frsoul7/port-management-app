import { Component, EventEmitter, Input, Output, OnChanges, SimpleChanges, inject } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { CreateOrganizationRequest, RepresentativeInput, OrganizationType } from '../../../../core/models/organization.model';
import { LoadingComponent } from '../../../../shared/components/loading/loading.component';
import { MessageModalService } from '../../../../core/services/message-modal.service';

@Component({
  selector: 'app-shipping-agent-form-modal',
  standalone: true,
  imports: [CommonModule, FormsModule, LoadingComponent],
  templateUrl: './shipping-agent-form-modal.component.html',
  styleUrls: ['./shipping-agent-form-modal.component.scss']
})
export class ShippingAgentFormModalComponent implements OnChanges {
  private messageModalService = inject(MessageModalService);

  @Input() show: boolean = false;
  @Input() saving: boolean = false;
  @Output() onClose = new EventEmitter<void>();
  @Output() onCreate = new EventEmitter<CreateOrganizationRequest>();

  organization = {
    identifier: '',
    legalName: '',
    alternativeName: '',
    address: '',
    taxNumber: '',
    type: OrganizationType.SHIPPING_AGENT
  };

  representatives: RepresentativeInput[] = [{
    name: '',
    citizenId: '',
    nationality: '',
    email: '',
    phone: ''
  }];

  ngOnChanges(changes: SimpleChanges): void {
    if (changes['show'] && this.show) {
      this.resetForm();
    }
  }

  resetForm(): void {
    this.organization = {
      identifier: '',
      legalName: '',
      alternativeName: '',
      address: '',
      taxNumber: '',
      type: OrganizationType.SHIPPING_AGENT
    };
    this.representatives = [{
      name: '',
      citizenId: '',
      nationality: '',
      email: '',
      phone: ''
    }];
  }

  close(): void {
    this.onClose.emit();
  }

  addRepresentative(): void {
    this.representatives.push({
      name: '',
      citizenId: '',
      nationality: '',
      email: '',
      phone: ''
    });
  }

  removeRepresentative(index: number): void {
    if (this.representatives.length > 1) {
      this.representatives.splice(index, 1);
    }
  }

  isFormValid(): boolean {
    const identifierValid = this.organization.identifier &&
      this.organization.identifier.length <= 10 &&
      /^[A-Za-z0-9]+$/.test(this.organization.identifier);

    const orgValid = identifierValid &&
      this.organization.legalName &&
      this.organization.alternativeName &&
      this.organization.address &&
      this.organization.taxNumber;

    const repsValid = this.representatives.length > 0 &&
      this.representatives.every(r =>
        r.name &&
        r.citizenId &&
        r.nationality && /^[A-Z]{2}$/.test(r.nationality.toUpperCase()) &&
        r.email && this.isGmailAddress(r.email) &&
        r.phone
      );

    return !!orgValid && repsValid;
  }

  isGmailAddress(email: string): boolean {
    return !!email && email.toLowerCase().endsWith('@gmail.com');
  }

  save(): void {
    // Check for duplicates within the form
    const emails = this.representatives.map(r => r.email.toLowerCase());
    const phones = this.representatives.map(r => r.phone);

    const duplicateEmails = emails.filter((e, i, a) => a.indexOf(e) !== i);
    const duplicatePhones = phones.filter((p, i, a) => a.indexOf(p) !== i);

    if (duplicateEmails.length > 0) {
      this.messageModalService.showError('Duplicate Emails', `Duplicate emails found: ${duplicateEmails.join(', ')}`);
      return;
    }

    if (duplicatePhones.length > 0) {
      this.messageModalService.showError('Duplicate Phones', `Duplicate phone numbers found: ${duplicatePhones.join(', ')}`);
      return;
    }

    if (!this.isFormValid()) {
      const invalidReps = this.representatives
        .map((r, idx) => {
          const errors = [];
          if (!this.isGmailAddress(r.email)) errors.push('Invalid Gmail');
          if (!/^[A-Z]{2}$/.test(r.nationality.toUpperCase())) errors.push('Invalid Nationality (2 letters)');
          return errors.length > 0 ? `Representative #${idx + 1}: ${errors.join(', ')}` : null;
        })
        .filter(r => r !== null);

      if (invalidReps.length > 0) {
        this.messageModalService.showError(
          'Validation Errors',
          'Please correct the following issues:\n' + invalidReps.join('\n')
        );
      } else if (!/^[A-Za-z0-9]+$/.test(this.organization.identifier)) {
        this.messageModalService.showError('Invalid Identifier', 'Identifier must be alphanumeric.');
      } else {
        this.messageModalService.showWarning('Incomplete Form', 'Please fill in all required fields.');
      }
      return;
    }

    const request: CreateOrganizationRequest = {
      ...this.organization,
      representatives: this.representatives.map(r => ({
        ...r,
        nationality: r.nationality.toUpperCase() // Ensure uppercase
      }))
    };

    this.onCreate.emit(request);
  }
}
