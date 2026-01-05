import { Injectable } from '@angular/core';
import * as THREE from 'three';
import { OrbitControls } from 'three/examples/jsm/controls/OrbitControls.js';
import { PortSceneService, PickableEntity } from './port-scene.service';
import gsap from 'gsap';

@Injectable({
  providedIn: 'root'
})
export class InteractiveService {
  private raycaster = new THREE.Raycaster();
  private mouse = new THREE.Vector2();

  constructor(private portScene: PortSceneService) {}

  /**
   * Helper function to check if an object is a descendant of a specific parent.
   */
  private isChildOf(child: THREE.Object3D, parent: THREE.Object3D): boolean {
    let node: THREE.Object3D | null = child;
    while (node) {
      if (node === parent) return true;
      node = node.parent;
    }
    return false;
  }

  handlePicking(
    event: MouseEvent,
    camera: THREE.PerspectiveCamera,
    renderer: THREE.WebGLRenderer,
    controls: OrbitControls
  ): PickableEntity | null {
    const rect = renderer.domElement.getBoundingClientRect();
    this.mouse.x = ((event.clientX - rect.left) / rect.width) * 2 - 1;
    this.mouse.y = -((event.clientY - rect.top) / rect.height) * 2 + 1;

    this.raycaster.setFromCamera(this.mouse, camera);

    const pickableEntities = this.portScene.getPickableEntities();
    const objectsToTest = pickableEntities.map(e => e.object);
    const intersects = this.raycaster.intersectObjects(objectsToTest, true);

    if (intersects.length > 0) {
      const clickedObj = intersects[0].object;
      const entity = pickableEntities.find(e =>
        e.object === clickedObj || this.isChildOf(clickedObj, e.object)
      );

      if (entity) {
        this.focusCameraOnObject(entity.position, camera, controls);
        return entity;
      }
    }
    return null;
  }

  /**
   * Smoothly animates the camera target and position (US 4.2.2).
   * Zooms in closer to the selected object for better focus.
   */
  private focusCameraOnObject(targetPos: THREE.Vector3, camera: THREE.Camera, controls: OrbitControls) {
    const duration = 1.2;

    gsap.to(controls.target, {
      x: targetPos.x,
      y: 0,
      z: targetPos.z,
      duration: duration,
      ease: 'power2.inOut',
      onUpdate: () => {
        controls.update();
      }
    });

    // Calculate a closer camera position - zoom in to get a better view of the object
    const zoomDistance = 50; // Distance from the object
    const angle = Math.atan2(camera.position.x - targetPos.x, camera.position.z - targetPos.z);
    const elevationAngle = 0.4; // Angle from horizontal (radians)
    
    const newPos = new THREE.Vector3(
      targetPos.x + Math.sin(angle) * zoomDistance * Math.cos(elevationAngle),
      targetPos.y + zoomDistance * Math.sin(elevationAngle),
      targetPos.z + Math.cos(angle) * zoomDistance * Math.cos(elevationAngle)
    );

    gsap.to(camera.position, {
      x: newPos.x, y: newPos.y, z: newPos.z,
      duration: duration,
      ease: 'power2.inOut'
    });
  }

  /**
   * Resets camera to default viewpoint (US 4.2.7).
   */
  resetCamera(camera: THREE.PerspectiveCamera, controls: OrbitControls): void {
    const duration = 1.5;
    const defaultTarget = { x: 0, y: 40, z: 120 };
    const defaultCamPos = { x: 0, y: 140, z: -260 };

    gsap.to(controls.target, {
      x: defaultTarget.x, y: defaultTarget.y, z: defaultTarget.z,
      duration: duration,
      ease: 'power2.inOut',
      onUpdate: () => { controls.update(); }
    });

    gsap.to(camera.position, {
      x: defaultCamPos.x, y: defaultCamPos.y, z: defaultCamPos.z,
      duration: duration,
      ease: 'power2.inOut'
    });
  }
}
