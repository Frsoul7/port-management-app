import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment } from '../../../environments/environment';

// ---- DTOs para o layout 3D ----

export interface DockLayout {
  dockId: string;
  name: string;
  positionX: number;
  positionY: number;
  positionZ: number;
  length: number;
  width: number;
  height: number;
  rotationY?: number;
}

export interface StorageAreaLayout {
  storageAreaId: string;
  name: string;
  type: 'YARD' | 'WAREHOUSE';
  positionX: number;
  positionY: number;
  positionZ: number;
  length: number;
  width: number;
  height: number;
}

export interface PortLayout {
  docks: DockLayout[];
  storageAreas: StorageAreaLayout[];
}

// ---- DTOs para objetos vivos (vessels + resources) ----

export interface Vessel3D {
  id: string;
  name: string;
  dockId: string;
  length: number;
  width: number;
  height: number;
}

export interface Resource3D {
  id: string;
  type: string;          // e.g. "STS_CRANE", "YARD_GANTRY"
  assignedAreaId: string; // dockId ou storageAreaId
}

export interface LiveObjects {
  vessels: Vessel3D[];
  resources: Resource3D[];
}


// ---- DTOs para materiais/texturas ----

export interface MaterialConfig {
  category: string; // DOCK, YARD, WAREHOUSE, VESSEL, CRANE...
  colorMapUrl: string;
  normalMapUrl?: string;
  roughnessMapUrl?: string;
  bumpMapUrl?: string;
  metalness: number;
  roughness: number;
}

export interface MaterialsConfig {
  materials: MaterialConfig[];
}

@Injectable({
  providedIn: 'root'
})
export class VisualizationService {
  private apiUrl = `${environment.apiUrl}/Visualization`;

  constructor(private http: HttpClient) {}

  getPortLayout(): Observable<PortLayout> {
    return this.http.get<PortLayout>(`${this.apiUrl}/layout`);
  }

  getMaterials(): Observable<MaterialsConfig> {
    return this.http.get<MaterialsConfig>(`${this.apiUrl}/materials`);
  }

  getLiveObjects(): Observable<LiveObjects> {
  return this.http.get<LiveObjects>(`${this.apiUrl}/live-objects`);
}

}
