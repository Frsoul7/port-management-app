import { Component, EventEmitter, Input, Output, inject } from '@angular/core';
import { CommonModule } from '@angular/common';
import { PhysicalResource, PhysicalResourceAvailability, MobileEquipmentType } from '../../../../core/models/physical-resource.model';
import { TranslatePipe } from '../../../../core/pipes/translate.pipe';

@Component({
  selector: 'app-physical-resource-view-modal',
  standalone: true,
  imports: [CommonModule, TranslatePipe],
  templateUrl: './physical-resource-view-modal.component.html',
  styleUrls: ['./physical-resource-view-modal.component.scss']
})
export class PhysicalResourceViewModalComponent {
  @Input() show: boolean = false;
  @Input() physicalResource: PhysicalResource | null = null;
  @Output() onClose = new EventEmitter<void>();
  @Output() onEdit = new EventEmitter<PhysicalResource>();

  close(): void {
    this.onClose.emit();
  }

  edit(): void {
    if (this.physicalResource) {
      this.onEdit.emit(this.physicalResource);
    }
  }

  getResourceTypeKey(type: string): string {
    return type === 'STS_CRANE' ? 'RESOURCES.STS_CRANE' : 'RESOURCES.MOBILE_EQUIPMENT';
  }

  getAvailabilityKey(availability: PhysicalResourceAvailability): string {
    switch (availability) {
      case PhysicalResourceAvailability.AVAILABLE:
        return 'RESOURCES.AVAILABLE';
      case PhysicalResourceAvailability.MAINTENANCE:
        return 'RESOURCES.MAINTENANCE';
      case PhysicalResourceAvailability.TEMP_OUT_OF_SERVICE:
        return 'RESOURCES.OUT_OF_SERVICE';
      default:
        return availability;
    }
  }

  getMobileTypeKey(type: string | null): string {
    if (!type) return '';
    return type === MobileEquipmentType.TRUCK ? 'RESOURCES.TRUCK' : 'RESOURCES.YARD_GANTRY_CRANE';
  }

  isActive(resource: PhysicalResource): boolean {
    return !resource.deactivatedAt;
  }

  formatDate(date: string | null): string {
    if (!date) return '-';
    return new Date(date).toLocaleDateString();
  }
}
