export enum StorageAreaType {
  ORDINARY = 'ORDINARY',
  YARD = 'YARD',
  WAREHOUSE = 'WAREHOUSE'
}

export interface StorageArea {
  storageAreaId: string;
  name: string;
  location: string;
  maxCapacityTEU: number;
  currentOccupancyTEU: number;
  type: StorageAreaType;
  servesAllDocks: boolean;
  servedDockCodes: string[];
  yardNotes?: string;
  warehouseNotes?: string;
}

export interface CreateStorageAreaRequest {
  name: string;
  location: string;
  maxCapacityTEU: number;
  type: StorageAreaType;
  servesAllDocks: boolean;
  servedDockCodes?: string[];
  yardNotes?: string;
  warehouseNotes?: string;
}

export interface UpdateStorageAreaRequest {
  name: string;
  location: string;
  maxCapacityTEU: number;
  servesAllDocks: boolean;
  servedDockCodes?: string[];
  yardNotes?: string;
  warehouseNotes?: string;
}

export interface UpdateOccupancyRequest {
  newOccupancyTEU: number;
}

export interface StorageAreaSearchParams {
  name?: string;
  location?: string;
  type?: StorageAreaType;
  servesAllDocks?: boolean;
}

export interface DockInfo {
  code: string;
  name: string;
}
