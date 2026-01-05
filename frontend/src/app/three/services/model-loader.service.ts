// src/app/three/services/model-loader.service.ts
import { Injectable } from '@angular/core';
import * as THREE from 'three';
import { GLTFLoader } from 'three/examples/jsm/loaders/GLTFLoader.js';

@Injectable({
  providedIn: 'root'
})
export class ModelLoaderService {
  private loader = new GLTFLoader();
  private cache = new Map<string, Promise<any>>();

  /**
   * Carrega um ficheiro GLTF/GLB e guarda em cache para não repetir downloads.
   * @param path Caminho relativo a partir da root pública (ex: 'models/dock.glb')
   */
  loadGLTF(path: string): Promise<any> {
    if (this.cache.has(path)) {
      return this.cache.get(path)!;
    }

    const promise = new Promise<any>((resolve, reject) => {
      this.loader.load(
        path,
        gltf => {
          resolve(gltf);
        },
        undefined,
        error => {
          console.error(`Erro ao carregar modelo GLTF '${path}'`, error);
          reject(error);
        }
      );
    });

    this.cache.set(path, promise);
    return promise;
  }

  /**
   * Atalho para obter diretamente a propriedade "scene" de um GLTF.
   */
  async loadScene(path: string): Promise<THREE.Object3D> {
    const gltf = await this.loadGLTF(path);
    return gltf.scene as THREE.Object3D;
  }
}
