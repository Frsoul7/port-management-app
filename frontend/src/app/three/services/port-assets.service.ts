// src/app/three/services/port-assets.service.ts
import { Injectable } from '@angular/core';
import * as THREE from 'three';
import { ModelLoaderService } from './model-loader.service';

@Injectable({
  providedIn: 'root'
})
export class PortAssetsService {
  private seaBase?: THREE.Object3D;
  private waterTexture?: THREE.Texture;
  private dockBase?: THREE.Object3D;
  private yardBase?: THREE.Object3D;
  private warehouseBase?: THREE.Object3D;
  private vesselBase?: THREE.Object3D;
  private containerBase?: THREE.Object3D;
  private randomContainerBase?: THREE.Object3D;
  private groundTexture?: THREE.Texture;
  private groundBase?: THREE.Object3D;
  private roadBase?: THREE.Object3D;




  constructor(private modelLoader: ModelLoaderService) {}

  // === MAR =================================================================

  async getSea(): Promise<THREE.Object3D> {
    if (!this.seaBase) {
      const seaWidth = 4000;   // largura em X
      const seaDepth = 2000;   // profundidade em Z (só lado do mar)

            // 🔹 Carregar textura de água
      const textureLoader = new THREE.TextureLoader();
      const waterTexture = textureLoader.load('textures/agua.jpg');

      // Repetir a textura para não ficar um quadrado gigante esticado
      waterTexture.wrapS = THREE.RepeatWrapping;
      waterTexture.wrapT = THREE.RepeatWrapping;
      waterTexture.repeat.set(16, 16); // aumenta/diminui para ajustar o "tile"

      const geometry = new THREE.PlaneGeometry(seaWidth, seaDepth, 1, 1);
      const material = new THREE.MeshPhongMaterial({
        map: waterTexture,
        shininess: 80,
        transparent: true,
        opacity: 0.95
      });

      const seaMesh = new THREE.Mesh(geometry, material);
      seaMesh.rotation.x = -Math.PI / 2;

      // 🔑 Fica todo do lado do mar (Z negativo): de -2000 até 0
      seaMesh.position.set(0, -0.02, -seaDepth / 2);

      seaMesh.receiveShadow = true;
      this.seaBase = seaMesh;
    }

    return this.seaBase.clone(true);

  }
      animateSea(delta: number): void {
      if (!this.waterTexture) return;

      // Velocidades de “corrente” em X e Y – ajusta ao gosto
      const speedX = 0.2;
      const speedY = 0.02;

      this.waterTexture.offset.x += speedX * delta;
      this.waterTexture.offset.y += speedY * delta;
    }


    async getGround(): Promise<THREE.Object3D> {
      if (!this.groundBase) {
        const groundWidth = 4000;
        const groundDepth = 4000;

        // Carregar textura do chão
        const textureLoader = new THREE.TextureLoader();
        this.groundTexture = textureLoader.load('textures/estrada.jpg');

        // repetição da textura para não ficar esticada
        this.groundTexture.wrapS = THREE.RepeatWrapping;
        this.groundTexture.wrapT = THREE.RepeatWrapping;
        this.groundTexture.repeat.set(30, 30); // ajusta para o tile que fica esteticamente melhor

        const geometry = new THREE.PlaneGeometry(groundWidth, groundDepth);
        const material = new THREE.MeshPhongMaterial({
          map: this.groundTexture,
          shininess: 12
        });

        const groundMesh = new THREE.Mesh(geometry, material);
        groundMesh.rotation.x = -Math.PI / 2;

        // chão só no lado positivo do porto (terra)
        groundMesh.position.set(0, 0.02, groundDepth / 2);

        groundMesh.receiveShadow = true;
        this.groundBase = groundMesh;
      }

      return this.groundBase.clone(true);
    }


  // === DOCA ================================================================

  async createDockInstance(): Promise<THREE.Object3D> {
    if (!this.dockBase) {
      this.dockBase = await this.modelLoader.loadScene('models/dock.glb');
      this.dockBase.scale.set(6, 6, 6);
      this.dockBase.traverse(obj => {
        if ((obj as any).isMesh) {
          const mesh = obj as THREE.Mesh;
          mesh.castShadow = true;
          mesh.receiveShadow = true;
        }
      });
    }

    return this.dockBase.clone(true);
  }

  // === YARD (pátio de contentores) ========================================

  async createYardInstance(): Promise<THREE.Object3D> {
    if (!this.yardBase) {
      this.yardBase = await this.modelLoader.loadScene('models/yard.glb');
      this.yardBase.scale.set(0.75, 0.75, 0.75);
      this.yardBase.traverse(obj => {
        if ((obj as any).isMesh) {
          const mesh = obj as THREE.Mesh;
          mesh.castShadow = true;
          mesh.receiveShadow = true;
        }
      });
    }

    return this.yardBase.clone(true);
  }

  // === WAREHOUSE (armazém) ================================================

  async createWarehouseInstance(): Promise<THREE.Object3D> {
    if (!this.warehouseBase) {
      this.warehouseBase = await this.modelLoader.loadScene('models/warehouse.glb');
      this.warehouseBase.scale.set(0.8, 0.8, 0.8);
      this.warehouseBase.traverse(obj => {
        if ((obj as any).isMesh) {
          const mesh = obj as THREE.Mesh;
          mesh.castShadow = true;
          mesh.receiveShadow = true;
        }
      });
    }

    return this.warehouseBase.clone(true);
  }

  // === VESSEL (navio porta-contentores) ====================================

  async createVesselInstance(): Promise<THREE.Object3D> {
    if (!this.vesselBase) {
      this.vesselBase = await this.modelLoader.loadScene('models/vessel_container.glb');
      this.vesselBase.scale.set(1, 1, 1);
      this.vesselBase.traverse(obj => {
        if ((obj as any).isMesh) {
          const mesh = obj as THREE.Mesh;
          mesh.castShadow = true;
          mesh.receiveShadow = true;
        }
      });
    }

    return this.vesselBase.clone(true);
  }

  // === CONTENTOR SIMPLES =====================================================

async createContainerInstance(): Promise<THREE.Object3D> {
  if (!this.containerBase) {
    this.containerBase = await this.modelLoader.loadScene('models/container.glb');
    this.containerBase.scale.set(1.8, 1.8, 1.8);
    this.containerBase.traverse(obj => {
      if ((obj as any).isMesh) {
        const mesh = obj as THREE.Mesh;
        mesh.castShadow = true;
        mesh.receiveShadow = true;
      }
    });
  }

  return this.containerBase.clone(true);
}

// === CONTENTORES RANDOM ====================================================

async createRandomContainerInstance(): Promise<THREE.Object3D> {
  if (!this.randomContainerBase) {
    this.randomContainerBase = await this.modelLoader.loadScene('models/ContentoresRandom.glb');
    this.randomContainerBase.scale.set(0.05, 0.05, 0.05);
    this.randomContainerBase.traverse(obj => {
      if ((obj as any).isMesh) {
        const mesh = obj as THREE.Mesh;
        mesh.castShadow = true;
        mesh.receiveShadow = true;
      }
    });
  }

  return this.randomContainerBase.clone(true);
}

// === ESTRADA ===============================================================

async createRoadInstance(): Promise<THREE.Object3D> {
  if (!this.roadBase) {
    // certifica-te que o ficheiro está em public/models/estrada.glb
    this.roadBase = await this.modelLoader.loadScene('models/estrada.glb');

    // Ajusta esta escala conforme o tamanho real do modelo
    this.roadBase.scale.set(0.3, 0.3, 0.3);
    this.roadBase.rotation.x = -Math.PI / 2;


    this.roadBase.traverse(obj => {
      if ((obj as any).isMesh) {
        const mesh = obj as THREE.Mesh;
        mesh.castShadow = true;
        mesh.receiveShadow = true;
      }
    });
  }

  return this.roadBase.clone(true);
}

}
