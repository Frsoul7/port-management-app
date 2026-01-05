import {
  AfterViewInit,
  Component,
  ElementRef,
  OnDestroy,
  ViewChild,
  HostListener
} from '@angular/core';
import { CommonModule } from '@angular/common';
import { Router } from '@angular/router';
import * as THREE from 'three';
import { OrbitControls } from 'three/examples/jsm/controls/OrbitControls.js';
import { OutlinePass } from 'three/examples/jsm/postprocessing/OutlinePass.js';
import { EffectComposer } from 'three/examples/jsm/postprocessing/EffectComposer.js';
import { RenderPass } from 'three/examples/jsm/postprocessing/RenderPass.js';
import { TranslatePipe } from '../../core/pipes/translate.pipe';
import { PortSceneService, PortData } from '../../three/services/port-scene.service';
import { DockService } from '../../core/services/dock.service';
import { VesselService } from '../../core/services/vessel.service';
import { forkJoin } from 'rxjs';
import { Tween, Easing, Group } from '@tweenjs/tween.js';

@Component({
  selector: 'app-port-3d',
  standalone: true,
  imports: [CommonModule, TranslatePipe],
  templateUrl: './port-3d.component.html',
  styleUrls: ['./port-3d.component.scss']
})
export class Port3DComponent implements AfterViewInit, OnDestroy {
  @ViewChild('canvasContainer', { static: true })
  canvasContainer!: ElementRef<HTMLDivElement>;

  private scene!: THREE.Scene;
  private camera!: THREE.PerspectiveCamera;
  private renderer!: THREE.WebGLRenderer;
  private composer!: EffectComposer;
  private outlinePass!: OutlinePass;
  private controls!: OrbitControls;
  private clock = new THREE.Clock();
  private animationFrameId?: number;
  isLoading = true;
  errorMessage = '';
  
  // Object picking
  private raycaster = new THREE.Raycaster();
  private mouse = new THREE.Vector2();
  private selectedObject: THREE.Object3D | null = null;
  selectedEntityName = '';
  selectedEntityType = '';
  showOverlay = false;
  // Spotlight for focusing on selected objects
  private focusSpotlight: THREE.SpotLight | null = null;
  private spotlightTarget: THREE.Object3D = new THREE.Object3D();
  
  // Background darkening
  private originalBackgroundColor = new THREE.Color(0x87ceeb);
  private darkenedBackgroundColor = new THREE.Color(0x4a5a6a);
  
  // TWEEN.js group for managing animations
  private tweenGroup = new Group();
  
  // Animation settings (configurable for testing)
  transitionDuration = 1500; // milliseconds
  transitionEasing = Easing.Quadratic.InOut;
  
  // Camera constraints
  private readonly MIN_CAMERA_HEIGHT = 10; // Minimum height above ground
  private readonly GROUND_LEVEL = 0;
  
  // Current animations
  private currentCameraTween: Tween<any> | null = null;
  private currentLightTween: Tween<any> | null = null;
  private temporaryLight: THREE.PointLight | null = null;
  private temporaryBottomLight: THREE.PointLight | null = null;

  constructor(
    private router: Router,
    private portScene: PortSceneService,
    private dockService: DockService,
    private vesselService: VesselService
  ) {}

  // === Ciclo de vida Angular ===

  async ngAfterViewInit(): Promise<void> {
    try {
      this.isLoading = true;
      await this.initThree();
      this.startRenderingLoop();
      window.addEventListener('resize', this.onResize);
      
      // Force a resize after initialization to ensure proper canvas size
      // Use requestAnimationFrame for smooth sizing without visible flash
      requestAnimationFrame(() => {
        this.onResize();
        // Make canvas visible after proper sizing
        if (this.renderer?.domElement) {
          this.renderer.domElement.style.opacity = '1';
        }
      });
      
      // Add mouse event listeners for object picking
      this.renderer.domElement.addEventListener('click', this.onMouseClick);
      this.renderer.domElement.addEventListener('mousemove', this.onMouseMove);
    } catch (error) {
      console.error('Error initializing 3D port:', error);
      this.errorMessage = 'Failed to load 3D visualization. Please try again.';
    } finally {
      this.isLoading = false;
    }
  }

  ngOnDestroy(): void {
    window.removeEventListener('resize', this.onResize);

    if (this.renderer?.domElement) {
      this.renderer.domElement.removeEventListener('click', this.onMouseClick);
      this.renderer.domElement.removeEventListener('mousemove', this.onMouseMove);
    }
    
    // Stop ongoing animations
    if (this.currentCameraTween) {
      this.currentCameraTween.stop();
    }
    if (this.currentLightTween) {
      this.currentLightTween.stop();
    }
    
    // Remove controls event listener
    if (this.controls) {
      this.controls.removeEventListener('change', this.enforceHeightConstraint);
    }

    if (this.animationFrameId != null) {
      cancelAnimationFrame(this.animationFrameId);
    }

    if (this.controls) {
      this.controls.dispose();
    }

    if (this.renderer) {
      this.renderer.dispose();
      // remover o canvas do DOM
      const container = this.canvasContainer?.nativeElement;
      if (container && this.renderer.domElement.parentElement === container) {
        container.removeChild(this.renderer.domElement);
      }
    }
  }

  // === Navegação ===

  goBack(): void {
    this.router.navigate(['/dashboard']);
  }

  // === Inicialização THREE ===

  private async initThree(): Promise<void> {
    const container = this.canvasContainer.nativeElement;
    const width = container.clientWidth || window.innerWidth;
    const height = container.clientHeight || 600;

    // Cena
    this.scene = new THREE.Scene();
    this.scene.background = new THREE.Color(0x87ceeb); // céu
    this.scene.fog = new THREE.Fog(0x87ceeb, 500, 3000);

    // Câmara
    this.camera = new THREE.PerspectiveCamera(55, width / height, 0.1, 5000);
    this.camera.position.set(0, 140, -260);

    // Renderer
    this.renderer = new THREE.WebGLRenderer({
      antialias: true
    });
    this.renderer.setSize(width, height);
    this.renderer.shadowMap.enabled = true;
    this.renderer.outputColorSpace = THREE.SRGBColorSpace;
    
    // Hide canvas initially to prevent visible resize flash
    this.renderer.domElement.style.opacity = '0';
    this.renderer.domElement.style.transition = 'opacity 0.15s ease-in';

    container.appendChild(this.renderer.domElement);

    // Setup post-processing for outline effect
    this.composer = new EffectComposer(this.renderer);
    const renderPass = new RenderPass(this.scene, this.camera);
    this.composer.addPass(renderPass);

    this.outlinePass = new OutlinePass(
      new THREE.Vector2(width, height),
      this.scene,
      this.camera
    );
    this.outlinePass.edgeStrength = 3.0;
    this.outlinePass.edgeGlow = 0.5;
    this.outlinePass.edgeThickness = 2.0;
    this.outlinePass.pulsePeriod = 0;
    this.outlinePass.visibleEdgeColor.set('#ffffff');
    this.outlinePass.hiddenEdgeColor.set('#ffffff');
    this.composer.addPass(this.outlinePass);

    // Luzes e controlos
    this.initLights();
    this.initControls();

    // Fetch port data from backend
    const portData = await this.fetchPortData();

    // Construir a cena do porto com dados dinâmicos
    await this.portScene.buildPort(this.scene, portData);
  }

  private async fetchPortData(): Promise<PortData> {
    return new Promise((resolve) => {
      forkJoin({
        docks: this.dockService.searchDocks({}),
        vessels: this.vesselService.searchVessels({})
      }).subscribe({
        next: (data) => {
          const portData: PortData = {
            docks: data.docks.map(dock => ({
              code: dock.code,
              name: dock.name,
              location: dock.location,
              lengthM: dock.lengthM
            })),
            vessels: data.vessels.map(vessel => ({
              imoNumber: vessel.imoNumber,
              name: vessel.name
            }))
          };
          console.log('Port data loaded:', portData);
          resolve(portData);
        },
        error: (error) => {
          console.error('Error fetching port data:', error);
          // Return empty data on error
          resolve({ docks: [], vessels: [] });
        }
      });
    });
  }

  private initLights(): void {
    // "Luz de céu" – suave, azulada em cima, cinzenta em baixo
    const hemiLight = new THREE.HemisphereLight(0xb1e1ff, 0x404040, 0.6);
    hemiLight.position.set(0, 400, 0);
    this.scene.add(hemiLight);

    // Sol principal – quente, vem ligeiramente de lado e de trás do mar
    const sunLight = new THREE.DirectionalLight(0xfff2dd, 1.3);
    sunLight.position.set(-250, 320, -200); // lado do mar, alto
    sunLight.castShadow = true;
    sunLight.shadow.mapSize.width = 4096;
    sunLight.shadow.mapSize.height = 4096;
    sunLight.shadow.camera.near = 10;
    sunLight.shadow.camera.far = 2500;
    sunLight.shadow.camera.left = -800;
    sunLight.shadow.camera.right = 800;
    sunLight.shadow.camera.top = 800;
    sunLight.shadow.camera.bottom = -800;
    this.scene.add(sunLight);

    // Luz de preenchimento do lado da terra – mais neutra
    const fillLight = new THREE.DirectionalLight(0xffffff, 0.35);
    fillLight.position.set(300, 180, 200);
    this.scene.add(fillLight);
    
    // Focus spotlight for selected objects
    this.focusSpotlight = new THREE.SpotLight(0xffffff, 0); // Start with intensity 0
    this.focusSpotlight.angle = Math.PI / 5;
    this.focusSpotlight.penumbra = 0.5;
    this.focusSpotlight.decay = 2;
    this.focusSpotlight.distance = 800;
    this.focusSpotlight.castShadow = true;
    this.focusSpotlight.shadow.mapSize.width = 2048;
    this.focusSpotlight.shadow.mapSize.height = 2048;
    this.focusSpotlight.position.set(0, 150, 0);
    
    this.scene.add(this.focusSpotlight);
    this.scene.add(this.spotlightTarget);
    this.focusSpotlight.target = this.spotlightTarget;
  }

  private initControls(): void {
    this.controls = new OrbitControls(this.camera, this.renderer.domElement);

    // olhar para o "miolo" do porto: gruas + contentores
    this.controls.target.set(0, 40, 120);

    this.controls.enableDamping = true;
    this.controls.dampingFactor = 0.05;
    this.controls.enablePan = true;
    this.controls.enableZoom = true;
    
    // Prevent camera from going below ground
    this.controls.minPolarAngle = 0; // Allow looking down
    this.controls.maxPolarAngle = Math.PI / 2 - 0.1; // Prevent going below horizon
    this.controls.minDistance = 20; // Minimum zoom distance
    this.controls.maxDistance = 2000; // Maximum zoom distance
    
    // Add change event to enforce height constraint
    this.controls.addEventListener('change', this.enforceHeightConstraint);
    
    this.controls.update();
  }
  
  private enforceHeightConstraint = () => {
    // Prevent camera from going below minimum height
    if (this.camera.position.y < this.MIN_CAMERA_HEIGHT) {
      this.camera.position.y = this.MIN_CAMERA_HEIGHT;
      this.controls.update();
    }
  };

  // === Loop de renderização ===

  private startRenderingLoop(): void {
    const render = (time?: number) => {
      const delta = this.clock.getDelta();

      // Update TWEEN animations with current time (required for @tweenjs/tween.js v25+)
      if (time !== undefined) {
        this.tweenGroup.update(time);
      } else {
        this.tweenGroup.update();
      }

      // Atualizar animações da cena (ondas, etc.)
      this.portScene.update(delta);

      if (this.controls) {
        this.controls.update();
      }

      this.composer.render();

      this.animationFrameId = requestAnimationFrame(render);
    };

    render();
  }

  // === Resize ===

  private onResize = () => {
    if (!this.camera || !this.renderer) return;

    const container = this.canvasContainer.nativeElement;
    const width = container.clientWidth || window.innerWidth;
    const height = container.clientHeight || 600;

    this.camera.aspect = width / height;
    this.camera.updateProjectionMatrix();
    this.renderer.setSize(width, height);
    this.composer.setSize(width, height);
  };

  // === Object Picking ===

  private onMouseMove = (event: MouseEvent) => {
    const rect = this.renderer.domElement.getBoundingClientRect();
    this.mouse.x = ((event.clientX - rect.left) / rect.width) * 2 - 1;
    this.mouse.y = -((event.clientY - rect.top) / rect.height) * 2 + 1;

    // Update raycaster
    this.raycaster.setFromCamera(this.mouse, this.camera);

    // Check for intersections with pickable objects
    const intersects = this.raycaster.intersectObjects(this.scene.children, true);
    
    // Change cursor if hovering over pickable object
    let foundPickable = false;
    for (const intersect of intersects) {
      const obj = this.findPickableParent(intersect.object);
      if (obj && obj.userData['pickable']) {
        foundPickable = true;
        break;
      }
    }
    
    this.renderer.domElement.style.cursor = foundPickable ? 'pointer' : 'default';
  };

  private onMouseClick = (event: MouseEvent) => {
    const rect = this.renderer.domElement.getBoundingClientRect();
    this.mouse.x = ((event.clientX - rect.left) / rect.width) * 2 - 1;
    this.mouse.y = -((event.clientY - rect.top) / rect.height) * 2 + 1;

    // Update raycaster
    this.raycaster.setFromCamera(this.mouse, this.camera);

    // Find intersections
    const intersects = this.raycaster.intersectObjects(this.scene.children, true);

    if (intersects.length > 0) {
      // Find the first pickable object
      for (const intersect of intersects) {
        const obj = this.findPickableParent(intersect.object);
        if (obj && obj.userData['pickable']) {
          this.selectObject(obj);
          break;
        }
      }
    }
  };

  private findPickableParent(obj: THREE.Object3D): THREE.Object3D | null {
    let current: THREE.Object3D | null = obj;
    while (current) {
      if (current.userData['pickable']) {
        return current;
      }
      current = current.parent;
    }
    return null;
  }

  private selectObject(obj: THREE.Object3D): void {
    // Remove previous selection
    this.clearSelection();
  
    // Store selected object
    this.selectedObject = obj;
    this.selectedEntityName = obj.userData['entityName'] || 'Unknown';
    this.selectedEntityType = obj.userData['entityType'] || '';
  
    console.log('Selected:', this.selectedEntityType, this.selectedEntityName);
  
    // Add outline to selected object
    this.addOutline(obj);
  
    // Darken background
    this.darkenBackground();
    
    // ✨ ADD: Super bright point light at object center
    const targetPosition = new THREE.Vector3();
    obj.getWorldPosition(targetPosition);
    
    this.temporaryLight = new THREE.PointLight(0xffffff, 25.0, 800); // ← Intensidade 8.0!
    this.temporaryLight.position.copy(targetPosition);
    this.temporaryLight.position.y += 20; // 40 unidades acima
    this.scene.add(this.temporaryLight);

    this.temporaryBottomLight = new THREE.PointLight(0xffffff, 15.0, 600);
    this.temporaryBottomLight.position.copy(targetPosition);
    this.temporaryBottomLight.position.y = 5; // 5 unidades do chão
    this.scene.add(this.temporaryBottomLight);
    
    console.log('Added TWO lights at:', this.temporaryLight.position, this.temporaryBottomLight.position);
  
    // Focus camera and spotlight on object with smooth animation
    this.focusCameraAndSpotlightOnObject(obj);
  }

  private clearSelection(): void {
    // Stop ongoing animations
    if (this.currentCameraTween) {
      this.currentCameraTween.stop();
      this.currentCameraTween = null;
    }
    if (this.currentLightTween) {
      this.currentLightTween.stop();
      this.currentLightTween = null;
    }
    
    // ✨ REMOVE: Temporary light
    if (this.temporaryLight) {
      this.scene.remove(this.temporaryLight);
      this.temporaryLight = null;
      console.log('Removed temporary light');
    }
    if (this.temporaryBottomLight) {
      this.scene.remove(this.temporaryBottomLight);
      this.temporaryBottomLight = null;
    }
    
    // Re-enable controls if they were disabled
    if (this.controls && !this.controls.enabled) {
      this.controls.enabled = true;
    }
    
    // Clear outline pass
    this.outlinePass.selectedObjects = [];
    
    this.selectedObject = null;
    this.selectedEntityName = '';
    this.selectedEntityType = '';
    
    // Restore background
    this.restoreBackground();
    
    // Fade out spotlight
    if (this.focusSpotlight) {
      const lightStart = { intensity: this.focusSpotlight.intensity };
      const lightTween = new Tween(lightStart, this.tweenGroup)
        .to({ intensity: 0 }, 500)
        .easing(Easing.Quadratic.Out)
        .onUpdate(() => {
          if (this.focusSpotlight) {
            this.focusSpotlight.intensity = lightStart.intensity;
          }
        })
        .start();
    }
  }

  private darkenBackground(): void {
    if (!(this.scene.background instanceof THREE.Color)) return;
    
    const bgStart = { r: this.scene.background.r, g: this.scene.background.g, b: this.scene.background.b };
    const bgEnd = { 
      r: this.darkenedBackgroundColor.r, 
      g: this.darkenedBackgroundColor.g, 
      b: this.darkenedBackgroundColor.b 
    };
    
    new Tween(bgStart, this.tweenGroup)
      .to(bgEnd, 500)
      .easing(Easing.Quadratic.Out)
      .onUpdate(() => {
        if (this.scene.background instanceof THREE.Color) {
          this.scene.background.setRGB(bgStart.r, bgStart.g, bgStart.b);
        }
      })
      .start();
  }

  private restoreBackground(): void {
    if (!(this.scene.background instanceof THREE.Color)) return;
    
    const bgStart = { r: this.scene.background.r, g: this.scene.background.g, b: this.scene.background.b };
    const bgEnd = { 
      r: this.originalBackgroundColor.r, 
      g: this.originalBackgroundColor.g, 
      b: this.originalBackgroundColor.b 
    };
    
    new Tween(bgStart, this.tweenGroup)
      .to(bgEnd, 500)
      .easing(Easing.Quadratic.Out)
      .onUpdate(() => {
        if (this.scene.background instanceof THREE.Color) {
          this.scene.background.setRGB(bgStart.r, bgStart.g, bgStart.b);
        }
      })
      .start();
  }

  private addOutline(obj: THREE.Object3D): void {
    // Collect all meshes from the object and its children
    const meshes: THREE.Mesh[] = [];
    obj.traverse((child) => {
      if (child instanceof THREE.Mesh) {
        meshes.push(child);
      }
    });
    
    // Add meshes to outline pass
    this.outlinePass.selectedObjects = meshes;
  }

  private focusCameraAndSpotlightOnObject(obj: THREE.Object3D): void {
    if (!this.controls || !this.focusSpotlight) {
      console.warn('Controls or spotlight not available');
      return;
    }

    // Get object position
    const targetPosition = new THREE.Vector3();
    obj.getWorldPosition(targetPosition);
    
    console.log('Focusing on object at position:', targetPosition);

    // Current positions
    const currentTarget = this.controls.target.clone();
    const currentCameraPos = this.camera.position.clone();
    const currentLightPos = this.focusSpotlight.position.clone();
    const currentLightTarget = this.spotlightTarget.position.clone();
    
    console.log('Current camera position:', currentCameraPos);
    console.log('Current target:', currentTarget);

    // Calculate object size for better framing
    const box = new THREE.Box3().setFromObject(obj);
    const size = box.getSize(new THREE.Vector3());
    const maxDim = Math.max(size.x, size.y, size.z);
    
    // Calculate optimal distance based on object size
    const fov = this.camera.fov * (Math.PI / 180);
    const optimalDistance = Math.max(maxDim * 2, 50); // At least 50 units away
    
    // Calculate camera position - position it relative to current viewing angle
    const currentDirection = new THREE.Vector3()
      .subVectors(currentCameraPos, currentTarget)
      .normalize();
    
    // Use a nice angle if the current direction is too flat
    const heightRatio = 0.5; // Camera height relative to distance
    if (Math.abs(currentDirection.y) < 0.3) {
      currentDirection.y = heightRatio;
      currentDirection.normalize();
    }
    
    const newCameraPos = new THREE.Vector3()
      .copy(targetPosition)
      .add(currentDirection.multiplyScalar(optimalDistance));
    
    // Ensure minimum height
    newCameraPos.y = Math.max(newCameraPos.y, targetPosition.y + optimalDistance * 0.3);
    
    console.log('Moving camera to:', newCameraPos);
    console.log('Looking at:', targetPosition);

    // Calculate spotlight position (above and slightly offset from object)
    const spotlightPos = new THREE.Vector3(
      targetPosition.x,
      targetPosition.y + 60,
      targetPosition.z
    );
    console.log('🔦 Spotlight position:', spotlightPos);
    console.log('🎯 Target position:', targetPosition);
    console.log('📏 Optimal distance:', optimalDistance);
    // Animate camera position and target
    const cameraStart = { 
      px: currentCameraPos.x, 
      py: currentCameraPos.y, 
      pz: currentCameraPos.z,
      tx: currentTarget.x, 
      ty: currentTarget.y, 
      tz: currentTarget.z 
    };
    const cameraEnd = { 
      px: newCameraPos.x, 
      py: newCameraPos.y, 
      pz: newCameraPos.z,
      tx: targetPosition.x, 
      ty: targetPosition.y, 
      tz: targetPosition.z 
    };
    
    // Disable controls during animation to prevent interference
    if (this.controls) {
      this.controls.enabled = false;
    }
    
    console.log('Creating camera tween animation...');
    console.log('Start values:', cameraStart);
    console.log('End values:', cameraEnd);
    
    this.currentCameraTween = new Tween(cameraStart, this.tweenGroup)
      .to(cameraEnd, this.transitionDuration)
      .easing(this.transitionEasing)
      .onStart(() => {
        console.log('Camera tween animation started!');
      })
      .onUpdate(() => {
        if (this.controls) {
          // Update camera position
          this.camera.position.set(cameraStart.px, cameraStart.py, cameraStart.pz);
          // Update camera target (what it looks at)
          this.controls.target.set(cameraStart.tx, cameraStart.ty, cameraStart.tz);
          this.controls.update();
        }
      })
      .onComplete(() => {
        // Re-enable controls after animation completes
        if (this.controls) {
          this.controls.enabled = true;
        }
        console.log('Camera animation completed at:', this.camera.position);
      })
      .start();
    
    console.log('Tween created and started:', this.currentCameraTween);

    // Animate spotlight
    const lightStart = {
      px: currentLightPos.x,
      py: currentLightPos.y,
      pz: currentLightPos.z,
      tx: currentLightTarget.x,
      ty: currentLightTarget.y,
      tz: currentLightTarget.z,
      intensity: this.focusSpotlight.intensity
    };
    const lightEnd = {
      px: spotlightPos.x,
      py: spotlightPos.y,
      pz: spotlightPos.z,
      tx: targetPosition.x,
      ty: targetPosition.y,
      tz: targetPosition.z,
      intensity: 50 // Highlight intensity
    };
    
    this.currentLightTween = new Tween(lightStart, this.tweenGroup)
      .to(lightEnd, this.transitionDuration)
      .easing(this.transitionEasing)
      .onUpdate(() => {
        if (this.focusSpotlight) {
          this.focusSpotlight.position.set(lightStart.px, lightStart.py, lightStart.pz);
          this.spotlightTarget.position.set(lightStart.tx, lightStart.ty, lightStart.tz);
          this.focusSpotlight.intensity = lightStart.intensity;
        }
      })
      .start();
  }

  clearSelectionUI(): void {
    this.clearSelection();
  }

  /**
   * US 4.2.7: Reset camera to initial overview position
   */
  resetView(): void {
    console.log('Reset View button clicked');
    if (!this.camera || !this.controls) {
      console.warn('Reset View: camera or controls not initialized');
      return;
    }

    // Clear any selection (this stops any ongoing animations)
    this.clearSelection();

    // Get initial positions
    const initialPos = new THREE.Vector3(0, 140, -260);
    const initialTarget = new THREE.Vector3(0, 40, 120);

    console.log('Setting camera to:', initialPos);
    console.log('Setting target to:', initialTarget);

    // Directly set camera position and target (no animation for now to debug)
    this.camera.position.copy(initialPos);
    this.controls.target.copy(initialTarget);
    this.controls.update();
    
    console.log('Reset View complete - camera position:', this.camera.position);
    console.log('Reset View complete - camera target:', this.controls.target);
  }

  // Configuration methods for testing
  setTransitionDuration(duration: number): void {
    this.transitionDuration = duration;
  }

  setTransitionEasing(easing: typeof Easing.Quadratic.InOut): void {
    this.transitionEasing = easing;
  }
    /**
   * US 4.2.3: Handle keyboard events for toggling overlay
   */
    @HostListener('window:keydown', ['$event'])
    handleKeyboardEvent(event: KeyboardEvent): void {
      if (event.key === 'i' || event.key === 'I') {
        this.toggleOverlay();
      }
    }
  
    /**
     * US 4.2.3: Toggle the information overlay visibility
     */
    toggleOverlay(): void {
      this.showOverlay = !this.showOverlay;
      console.log('Overlay toggled:', this.showOverlay ? 'visible' : 'hidden');
    }
  
    /**
     * US 4.2.3: Check if overlay should be displayed
     */
    get shouldShowOverlay(): boolean {
      return this.showOverlay && this.selectedObject !== null;
    }
}
