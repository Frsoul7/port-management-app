import { Component, Input, OnChanges, SimpleChanges } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { VvnService } from '../../../../core/services/vvn.service';
import { MessageModalService } from '../../../../core/services/message-modal.service';
import { VvnStatusResponse, CrewMemberDto, SetCrewRequest, VvnEntryResponse } from '../../../../core/models/vvn.model';

@Component({
  selector: 'app-crew-members-manager',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './crew-members-manager.component.html',
  styleUrls: ['./crew-members-manager.component.scss']
})
export class CrewMembersManagerComponent implements OnChanges {
  @Input() vvn: VvnStatusResponse | null = null;
  @Input() editable = false;

  crewMembers: CrewMemberDto[] = [];
  manifestEntries: VvnEntryResponse[] = [];
  showAddForm = false;
  saving = false;
  loadingEntries = false;

  formData: CrewMemberDto = {
    name: '',
    citizenId: '',
    nationality: ''
  };

  validationErrors: { [key: string]: string } = {};

  constructor(
    private vvnService: VvnService,
    private modalService: MessageModalService
  ) {}

  ngOnChanges(changes: SimpleChanges): void {
    if (changes['vvn'] && this.vvn) {
      this.crewMembers = [...(this.vvn.crewMembers || [])];
      this.loadManifestEntries();
    }
  }

  loadManifestEntries(): void {
    if (!this.vvn) return;

    this.loadingEntries = true;
    this.vvnService.getEntries(this.vvn.id).subscribe({
      next: (entries) => {
        this.manifestEntries = entries;
        this.loadingEntries = false;
      },
      error: (error) => {
        console.error('Error loading manifest entries:', error);
        this.loadingEntries = false;
      }
    });
  }

  get hasHazardousGoods(): boolean {
    return this.manifestEntries.some(entry => entry.hazardous);
  }

  get needsCrewMembers(): boolean {
    return this.hasHazardousGoods && this.crewMembers.length === 0;
  }

  toggleAddForm(): void {
    this.showAddForm = !this.showAddForm;
    if (this.showAddForm) {
      this.resetForm();
    }
  }

  resetForm(): void {
    this.formData = {
      name: '',
      citizenId: '',
      nationality: ''
    };
    this.validationErrors = {};
  }

  validate(): boolean {
    this.validationErrors = {};

    if (!this.formData.name?.trim()) {
      this.validationErrors['name'] = 'Name is required';
    }

    if (!this.formData.citizenId?.trim()) {
      this.validationErrors['citizenId'] = 'Citizen ID is required';
    }

    if (!this.formData.nationality?.trim()) {
      this.validationErrors['nationality'] = 'Nationality is required';
    } else if (!/^[A-Z]{2}$/.test(this.formData.nationality.trim().toUpperCase())) {
      this.validationErrors['nationality'] = 'Nationality must be 2-letter code (e.g., US, PT)';
    }

    return Object.keys(this.validationErrors).length === 0;
  }

  handleAddMember(): void {
    if (!this.validate() || !this.vvn) return;

    const newMember: CrewMemberDto = {
      name: this.formData.name.trim(),
      citizenId: this.formData.citizenId.trim(),
      nationality: this.formData.nationality.trim().toUpperCase()
    };

    const updatedMembers = [...this.crewMembers, newMember];
    this.saveCrewMembers(updatedMembers);
  }

  handleRemoveMember(index: number): void {
    if (!this.vvn) return;

    this.modalService.showConfirm(
      'Remove Crew Member',
      `Are you sure you want to remove ${this.crewMembers[index].name}?`,
      () => {
        const updatedMembers = this.crewMembers.filter((_, i) => i !== index);
        this.saveCrewMembers(updatedMembers);
      },
      () => console.log('Remove cancelled')
    );
  }

  private saveCrewMembers(members: CrewMemberDto[]): void {
    if (!this.vvn) return;

    this.saving = true;
    const request: SetCrewRequest = {
      members: members
    };

    this.vvnService.setCrew(this.vvn.id, request).subscribe({
      next: () => {
        this.crewMembers = members;
        this.modalService.showSuccess(
          'Crew Updated',
          'Hazardous cargo handlers updated successfully.'
        );
        this.showAddForm = false;
        this.saving = false;

        // Update the VVN in parent component
        if (this.vvn) {
          this.vvn.crewMembers = members;
        }
      },
      error: (error) => {
        console.error('Error updating crew members:', error);
        const errorMsg = error.error?.value?.detail || error.error?.detail || 'Failed to update crew members. Please try again.';
        this.modalService.showError('Update Failed', errorMsg);
        this.saving = false;
      }
    });
  }

  cancelAddForm(): void {
    this.showAddForm = false;
    this.resetForm();
  }
}
