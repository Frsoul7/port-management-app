import { Component, EventEmitter, Input, Output, OnChanges, SimpleChanges } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import {
  PhysicalResource,
  CreatePhysicalResourceRequest,
  UpdatePhysicalResourceRequest,
  MobileEquipmentType,
  DockInfo
} from '../../../../core/models/physical-resource.model';
import { LoadingComponent } from '../../../../shared/components/loading/loading.component';
import { TranslatePipe } from '../../../../core/pipes/translate.pipe';

@Component({
  selector: 'app-physical-resource-form-modal',
  standalone: true,
  imports: [CommonModule, FormsModule, LoadingComponent, TranslatePipe],
  templateUrl: './physical-resource-form-modal.component.html',
  styleUrls: ['./physical-resource-form-modal.component.scss']
})
export class PhysicalResourceFormModalComponent implements OnChanges {
  @Input() show: boolean = false;
  @Input() isEditMode: boolean = false;
  @Input() physicalResource: PhysicalResource | null = null;
  @Input() docks: DockInfo[] = [];
  @Input() saving: boolean = false;
  @Output() onClose = new EventEmitter<void>();
  @Output() onCreate = new EventEmitter<CreatePhysicalResourceRequest>();
  @Output() onUpdate = new EventEmitter<{ code: string; request: UpdatePhysicalResourceRequest }>();

  MobileEquipmentType = MobileEquipmentType;

  currentResource: any = {
    code: '',
    description: '',
    setupTimeSeconds: 0,
    resourceType: 'STS_CRANE',
    // STS Crane specific
    avgContainersPerHour: null,
    installedAtDockCode: '',
    // Mobile Equipment specific
    mobileType: null,
    maxSpeedKph: null,
    containersPerTrip: null,
    // Common
    requiredQualificationIds: []
  };

  resourceTypes = [
    { value: 'STS_CRANE', translationKey: 'STS_CRANE' },
    { value: 'MOBILE_EQUIPMENT', translationKey: 'MOBILE_EQUIPMENT' }
  ];

  mobileTypes = [
    { value: MobileEquipmentType.TRUCK, translationKey: 'TRUCK' },
    { value: MobileEquipmentType.YARD_GANTRY_CRANE, translationKey: 'YARD_GANTRY_CRANE' }
  ];

  ngOnChanges(changes: SimpleChanges): void {
    if (changes['physicalResource'] && this.physicalResource) {
      this.currentResource = {
        code: this.physicalResource.code,
        description: this.physicalResource.description || '',
        setupTimeSeconds: this.physicalResource.setupTimeSeconds,
        resourceType: this.physicalResource.resourceType,
        avgContainersPerHour: this.physicalResource.avgContainersPerHour,
        installedAtDockCode: this.physicalResource.installedAtDockCode || '',
        mobileType: this.physicalResource.mobileEquipmentType,
        maxSpeedKph: this.physicalResource.maxSpeedKph,
        containersPerTrip: this.physicalResource.containersPerTrip,
        requiredQualificationIds: this.physicalResource.requiredQualifications.map(q => q.qualificationId)
      };
    } else if (changes['show'] && this.show && !this.isEditMode) {
      this.resetForm();
    }
  }

  resetForm(): void {
    this.currentResource = {
      code: '',
      description: '',
      setupTimeSeconds: 0,
      resourceType: 'STS_CRANE',
      avgContainersPerHour: null,
      installedAtDockCode: '',
      mobileType: null,
      maxSpeedKph: null,
      containersPerTrip: null,
      requiredQualificationIds: []
    };
  }

  close(): void {
    this.onClose.emit();
    this.resetForm();
  }

  onResourceTypeChange(): void {
    // Reset type-specific fields when changing resource type
    if (this.currentResource.resourceType === 'STS_CRANE') {
      this.currentResource.mobileType = null;
      this.currentResource.maxSpeedKph = null;
      this.currentResource.containersPerTrip = null;
    } else {
      this.currentResource.avgContainersPerHour = null;
      this.currentResource.installedAtDockCode = '';
    }
  }

  save(): void {
    if (this.isEditMode && this.physicalResource) {
      const updateRequest: UpdatePhysicalResourceRequest = {
        description: this.currentResource.description || null,
        setupTimeSeconds: this.currentResource.setupTimeSeconds,
        avgContainersPerHour: this.currentResource.resourceType === 'STS_CRANE' ? this.currentResource.avgContainersPerHour : undefined,
        installedAtDockCode: this.currentResource.resourceType === 'STS_CRANE' ? this.currentResource.installedAtDockCode : undefined,
        mobileType: this.currentResource.resourceType === 'MOBILE_EQUIPMENT' ? this.currentResource.mobileType : undefined,
        maxSpeedKph: this.currentResource.resourceType === 'MOBILE_EQUIPMENT' ? this.currentResource.maxSpeedKph : undefined,
        containersPerTrip: this.currentResource.resourceType === 'MOBILE_EQUIPMENT' ? this.currentResource.containersPerTrip : undefined,
        requiredQualificationIds: this.currentResource.requiredQualificationIds
      };
      this.onUpdate.emit({ code: this.physicalResource.code, request: updateRequest });
    } else {
      const createRequest: CreatePhysicalResourceRequest = {
        code: this.currentResource.code,
        description: this.currentResource.description || null,
        setupTimeSeconds: this.currentResource.setupTimeSeconds,
        resourceType: this.currentResource.resourceType,
        avgContainersPerHour: this.currentResource.resourceType === 'STS_CRANE' ? this.currentResource.avgContainersPerHour : undefined,
        installedAtDockCode: this.currentResource.resourceType === 'STS_CRANE' ? this.currentResource.installedAtDockCode : undefined,
        mobileType: this.currentResource.resourceType === 'MOBILE_EQUIPMENT' ? this.currentResource.mobileType : undefined,
        maxSpeedKph: this.currentResource.resourceType === 'MOBILE_EQUIPMENT' ? this.currentResource.maxSpeedKph : undefined,
        containersPerTrip: this.currentResource.resourceType === 'MOBILE_EQUIPMENT' ? this.currentResource.containersPerTrip : undefined,
        requiredQualificationIds: this.currentResource.requiredQualificationIds
      };
      this.onCreate.emit(createRequest);
    }
  }
}
