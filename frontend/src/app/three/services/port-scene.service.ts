// src/app/three/services/port-scene.service.ts
import { Injectable } from '@angular/core';
import * as THREE from 'three';
import { PortAssetsService } from './port-assets.service';

export interface PortData {
  docks: DockData[];
  vessels: VesselData[];
}

export interface DockData {
  code: string;
  name: string;
  location: string;
  lengthM: number;
}

export interface VesselData {
  imoNumber: string;
  name: string;
  dockCode?: string;
}

export interface PickableEntity {
  type: 'dock' | 'vessel' | 'yard' | 'warehouse' | 'storage';
  id: string;
  name: string;
  position: THREE.Vector3;
  object: THREE.Object3D;
}

@Injectable({
  providedIn: 'root'
})
export class PortSceneService {
  private pickableEntities: PickableEntity[] = [];

  constructor(private assets: PortAssetsService) {}

  getPickableEntities(): PickableEntity[] {
    return this.pickableEntities;
  }

  clearPickableEntities(): void {
    this.pickableEntities = [];
  }

  async buildPort(scene: THREE.Scene, portData?: PortData): Promise<void> {
    // Clear previous pickable entities
    this.clearPickableEntities();

    // 1) Mar (lado Z negativo) e chão (lado Z positivo)
    const sea = await this.assets.getSea();
    scene.add(sea);

    const ground = await this.assets.getGround();
    scene.add(ground);

    // 2) Docas – dynamic based on backend data
    const dockPositions: THREE.Vector3[] = [];
    const docksToRender = portData?.docks || [];
    
    // Use actual number of docks from database
    const numDocks = docksToRender.length;
    
    // Calculate spacing based on number of docks
    const totalWidth = 180; // total width for all docks
    const spacing = numDocks > 1 ? totalWidth / (numDocks - 1) : 0;
    
    for (let i = 0; i < numDocks; i++) {
      const dock = await this.assets.createDockInstance();
      const x = i * spacing - totalWidth / 2;
      dock.position.set(x, 0, 0);     // linha do cais é z = 0
      
      // Mark as pickable
      const dockData = docksToRender[i];
      dock.userData = {
        pickable: true,
        entityType: 'dock',
        entityId: dockData?.code || `dock-${i}`,
        entityName: dockData?.name || `Dock ${i + 1}`
      };

      scene.add(dock);
      
      const position = new THREE.Vector3(x, 0, 0);
      dockPositions.push(position);
      
      // Register as pickable entity
      this.pickableEntities.push({
        type: 'dock',
        id: dockData?.code || `dock-${i}`,
        name: dockData?.name || `Dock ${i + 1}`,
        position: position.clone(),
        object: dock
      });
    }

    // 3) Parâmetros de profundidade
    const yardZ = 85;        // gruas
    const middleYardZ = 5; 
    const warehouseZ = 250;  // armazéns
    const randomContainersZ = (yardZ + warehouseZ) / 2; // ContentoresRandom atrás

    // 4) Yards (gruas) – one per dock plus one in the middle (dynamic based on docks)

    const yardConfigs: Array<{ x: number; z: number }> = [];
    
    // Add a crane for each dock
    for (let i = 0; i < dockPositions.length; i++) {
      yardConfigs.push({ x: dockPositions[i].x, z: yardZ });
    }
    
    // Add a crane in the middle (closer to the sea)
    if (dockPositions.length > 0) {
      yardConfigs.push({ x: 0, z: middleYardZ });
    }

    for (let i = 0; i < yardConfigs.length; i++) {
      const cfg = yardConfigs[i];
      const yard = await this.assets.createYardInstance();
      yard.position.set(cfg.x, 0, cfg.z);
      yard.rotation.y = Math.PI / 2;
      
      // Mark as pickable
      yard.userData = {
        pickable: true,
        entityType: 'yard',
        entityId: `yard-${i}`,
        entityName: `Crane ${i + 1}`
      };
      
      scene.add(yard);
      
      // Register as pickable entity
      this.pickableEntities.push({
        type: 'yard',
        id: `yard-${i}`,
        name: `Crane ${i + 1}`,
        position: new THREE.Vector3(cfg.x, 0, cfg.z),
        object: yard
      });

      const craneLight = new THREE.PointLight(0xfff4cc, 0.45, 160);
      craneLight.position.set(cfg.x, 45, cfg.z + 5);
      craneLight.castShadow = true;
      scene.add(craneLight);
    }


    // 5) Armazéns lá atrás
    const warehouseConfigs = [
      { x: -125, z: warehouseZ },
      { x: 122,  z: warehouseZ }
    ];

    for (let i = 0; i < warehouseConfigs.length; i++) {
      const cfg = warehouseConfigs[i];
      const warehouse = await this.assets.createWarehouseInstance();
      warehouse.position.set(cfg.x, 0, cfg.z);
      
      // Mark as pickable
      warehouse.userData = {
        pickable: true,
        entityType: 'warehouse',
        entityId: `warehouse-${i}`,
        entityName: `Warehouse ${i + 1}`
      };
      
      scene.add(warehouse);
      
      // Register as pickable entity
      this.pickableEntities.push({
        type: 'warehouse',
        id: `warehouse-${i}`,
        name: `Warehouse ${i + 1}`,
        position: new THREE.Vector3(cfg.x, 0, cfg.z),
        object: warehouse
      });

      const warehouseLight = new THREE.PointLight(0xdde8ff, 0.35, 180);
      warehouseLight.position.set(cfg.x, 55, cfg.z + 20);
      warehouseLight.castShadow = false; // aqui podemos poupar sombras
      scene.add(warehouseLight);
    }

    // 6) ContentoresRandom.glb – ficam onde já tinhas: entre gruas e armazéns
    const randomColumnsX = dockPositions.map(p => p.x); // alinhados com docas

    for (const x of randomColumnsX) {
      const offsetsX = [-16, 0, 16];

      for (const offsetX of offsetsX) {
        const randomContainer = await this.assets.createRandomContainerInstance();
        randomContainer.position.set(x + offsetX, 0, randomContainersZ);
        randomContainer.rotation.y = (Math.random() - 0.5) * 0.5;
        scene.add(randomContainer);
      }
    }

    // 7) container.glb – encostados às gruas, do lado da terra
    const containersNearCranesZ = yardZ + 10; // logo a seguir às gruas

    for (const cfg of yardConfigs) {
      const offsetsX = [-10, 0, 10]; // 3 containers por grua

      for (const offsetX of offsetsX) {
        const container = await this.assets.createContainerInstance();
        container.position.set(cfg.x + offsetX, 0, containersNearCranesZ);
        container.rotation.y = (Math.random() - 0.5) * 0.2;
        scene.add(container);
      }
    }

// 7) Estradas pelo porto

// Estrada operacional: logo atrás das gruas/contentores
const roadZNearOps = yardZ + 35;

// Estrada principal: atrás dos armazéns
const roadZBack = warehouseZ + 50;

// número de segmentos e distância entre eles (ajusta se houver buracos/sobreposição)
const roadSegments = 10;
const roadSpacing = 40.5;

// Estrada contínua zona operacional
await this.addContinuousRoad(scene, roadZNearOps, roadSegments, roadSpacing);

// Estrada contínua atrás dos armazéns
await this.addContinuousRoad(scene, roadZBack, roadSegments, roadSpacing);



    // 8) Vessels – dynamic based on backend data
    const vesselZOffset = -40; // distância do cais; ajusta se quiseres mais perto/longe
    const vesselsToRender = portData?.vessels || [];
    
    // If we have vessel data, render based on that
    if (vesselsToRender.length > 0) {
      for (let i = 0; i < Math.min(vesselsToRender.length, dockPositions.length); i++) {
        const vessel = await this.assets.createVesselInstance();
        const dockPos = dockPositions[i];
        const vesselData = vesselsToRender[i];
        
        vessel.position.set(dockPos.x, 0, dockPos.z + vesselZOffset);
        vessel.rotation.y = Math.PI / 2; // paralelo à doca
        
        // Mark as pickable
        vessel.userData = {
          pickable: true,
          entityType: 'vessel',
          entityId: vesselData.imoNumber,
          entityName: vesselData.name
        };

        scene.add(vessel);
        
        // Register as pickable entity
        this.pickableEntities.push({
          type: 'vessel',
          id: vesselData.imoNumber,
          name: vesselData.name,
          position: new THREE.Vector3(dockPos.x, 0, dockPos.z + vesselZOffset),
          object: vessel
        });
      }
    } else {
      // Fallback: one vessel per dock
      for (let i = 0; i < dockPositions.length; i++) {
        const dockPos = dockPositions[i];
        const vessel = await this.assets.createVesselInstance();

        vessel.position.set(dockPos.x, 0, dockPos.z + vesselZOffset);
        vessel.rotation.y = Math.PI / 2; // paralelo à doca
        
        // Mark as pickable
        vessel.userData = {
          pickable: true,
          entityType: 'vessel',
          entityId: `vessel-${i}`,
          entityName: `Vessel ${i + 1}`
        };

        scene.add(vessel);
        
        // Register as pickable entity
        this.pickableEntities.push({
          type: 'vessel',
          id: `vessel-${i}`,
          name: `Vessel ${i + 1}`,
          position: new THREE.Vector3(dockPos.x, 0, dockPos.z + vesselZOffset),
          object: vessel
        });
      }
    }

    // 9) Luz no cais
    const pierLight = new THREE.SpotLight(0xfff0dd, 0.7, 400, Math.PI / 4, 0.4);
    pierLight.position.set(0, 80, 10);        // por cima da zona entre as duas docas
    pierLight.target.position.set(0, 0, 40);  // aponta para a área de trabalho
    pierLight.castShadow = true;
    scene.add(pierLight);
    scene.add(pierLight.target);

  }

  update(delta: number): void {
  // Por agora só animamos o mar – mas aqui no futuro
  // também podemos animar navios, gruas, etc.
  this.assets.animateSea(delta);
}


  private async addContinuousRoad(
  scene: THREE.Scene,
  z: number,
  segments: number,
  segmentSpacing: number
): Promise<void> {
  // centro da estrada em X = 0
  const half = ((segments - 1) * segmentSpacing) / 2;

  for (let i = 0; i < segments; i++) {
    const road = await this.assets.createRoadInstance();

    const x = i * segmentSpacing - half;
    road.position.set(x, 0.03, z);


    scene.add(road);
  }
}


}