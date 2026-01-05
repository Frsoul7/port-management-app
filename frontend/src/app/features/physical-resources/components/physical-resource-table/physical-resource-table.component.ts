import { Component, EventEmitter, Input, Output } from '@angular/core';
import { CommonModule } from '@angular/common';
import { PhysicalResource, PhysicalResourceAvailability, MobileEquipmentType } from '../../../../core/models/physical-resource.model';
import { TranslatePipe } from '../../../../core/pipes/translate.pipe';

@Component({
  selector: 'app-physical-resource-table',
  standalone: true,
  imports: [CommonModule, TranslatePipe],
  templateUrl: './physical-resource-table.component.html',
  styleUrls: ['./physical-resource-table.component.scss']
})
export class PhysicalResourceTableComponent {
  @Input() physicalResources: PhysicalResource[] = [];
  @Input() loading: boolean = false;
  @Output() onView = new EventEmitter<PhysicalResource>();
  @Output() onEdit = new EventEmitter<PhysicalResource>();
  @Output() onDelete = new EventEmitter<PhysicalResource>();

  viewPhysicalResource(resource: PhysicalResource): void {
    this.onView.emit(resource);
  }

  editPhysicalResource(resource: PhysicalResource): void {
    this.onEdit.emit(resource);
  }

  deletePhysicalResource(resource: PhysicalResource): void {
    this.onDelete.emit(resource);
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

  getAvailabilityClass(availability: PhysicalResourceAvailability): string {
    switch (availability) {
      case PhysicalResourceAvailability.AVAILABLE:
        return 'available';
      case PhysicalResourceAvailability.MAINTENANCE:
        return 'maintenance';
      case PhysicalResourceAvailability.TEMP_OUT_OF_SERVICE:
        return 'out-of-service';
      default:
        return '';
    }
  }

  isActive(resource: PhysicalResource): boolean {
    return !resource.deactivatedAt;
  }
}
