import { Component, OnInit, Renderer2, effect } from '@angular/core';
import { Router, RouterOutlet, NavigationEnd } from '@angular/router';
import { CommonModule } from '@angular/common';
import { AuthService } from './core/services/auth.service';
import { NavbarComponent } from './shared/components/navbar/navbar.component';
import { MessageModalComponent } from './shared/components/message-modal/message-modal.component';
import { MessageModalService } from './core/services/message-modal.service';
import { PrivacyPolicyService } from './core/services/privacy-policy.service';
import { PrivacyAcknowledgmentModalComponent } from './features/privacy-policy/privacy-acknowledgment-modal.component';
import { FooterComponent } from './shared/components/footer/footer.component';
import { AcknowledgmentStatus } from './core/models/privacy-policy.model';
import { filter } from 'rxjs/operators';

@Component({
  selector: 'app-root',
  standalone: true,
  imports: [CommonModule, RouterOutlet, NavbarComponent, MessageModalComponent, PrivacyAcknowledgmentModalComponent, FooterComponent],
  template: `
    <app-navbar *ngIf="showNavbar"></app-navbar>
    <div class="main-content" [class.has-footer]="showFooter">
      <router-outlet></router-outlet>
    </div>
    <app-footer *ngIf="showFooter"></app-footer>
    
    <!-- Global Message Modal -->
    <app-message-modal
      [isOpen]="!!modalService.modal()"
      [type]="modalService.modal()?.type || 'info'"
      [title]="modalService.modal()?.title || ''"
      [message]="modalService.modal()?.message || ''"
      [confirmText]="modalService.modal()?.confirmText || 'OK'"
      [cancelText]="modalService.modal()?.cancelText || 'Cancel'"
      [showCancel]="modalService.modal()?.showCancel || false"
      (onConfirm)="handleModalConfirm()"
      (onCancel)="handleModalCancel()"
      (onClose)="handleModalClose()">
    </app-message-modal>

    <!-- Privacy Policy Acknowledgment Modal (GDPR - US 4.5.1) -->
    <app-privacy-acknowledgment-modal
      [isOpen]="showPrivacyModal"
      [acknowledgmentStatus]="privacyAcknowledgmentStatus"
      (acknowledged)="onPrivacyAcknowledged()">
    </app-privacy-acknowledgment-modal>
  `,
  styles: [`
    .main-content {
      min-height: calc(100vh - 60px);
    }
    .main-content.has-footer {
      padding-bottom: 60px;
    }
  `]
})
export class AppComponent implements OnInit {
  title = 'Port Management System';
  showNavbar = false;
  showFooter = false;
  
  // Privacy Policy Acknowledgment (GDPR)
  showPrivacyModal = false;
  privacyAcknowledgmentStatus: AcknowledgmentStatus | null = null;
  private hasCheckedPrivacy = false;

  constructor(
    private authService: AuthService,
    public modalService: MessageModalService,
    private privacyPolicyService: PrivacyPolicyService,
    private router: Router,
    private renderer: Renderer2
  ) {
    // React to user changes to update navbar visibility and check privacy acknowledgment
    effect(() => {
      const user = this.authService.currentUser();
      console.log('User changed:', user?.username || 'null');
      // Use setTimeout to avoid expression changed errors and improve responsiveness
      setTimeout(() => {
        this.updateNavbarVisibility();
        // Check privacy policy acknowledgment when user logs in
        if (user && !this.hasCheckedPrivacy) {
          this.checkPrivacyAcknowledgment();
        } else if (!user) {
          this.hasCheckedPrivacy = false;
          this.showPrivacyModal = false;
        }
      }, 0);
    });
  }

  ngOnInit(): void {
    console.log('AppComponent initialized');
    console.log('Current path:', window.location.pathname);
    
    // Update navbar visibility and log navigation events
    this.router.events.pipe(
      filter(event => event instanceof NavigationEnd)
    ).subscribe((event: any) => {
      console.log('Navigated to:', event.url);
      this.updateNavbarVisibility();
    });
  }

  handleModalConfirm(): void {
    const config = this.modalService.modal();
    config?.onConfirm?.();
    this.modalService.close();
  }

  handleModalCancel(): void {
    const config = this.modalService.modal();
    config?.onCancel?.();
    this.modalService.close();
  }

  handleModalClose(): void {
    this.modalService.close();
  }

  /**
   * Check if user needs to acknowledge the privacy policy (GDPR - US 4.5.1)
   */
  private checkPrivacyAcknowledgment(): void {
    this.hasCheckedPrivacy = true;
    
    this.privacyPolicyService.checkAcknowledgmentRequired().subscribe({
      next: (status) => {
        console.log('Privacy acknowledgment status:', status);
        if (status.acknowledgmentRequired) {
          this.privacyAcknowledgmentStatus = status;
          this.showPrivacyModal = true;
        }
      },
      error: (err) => {
        // Don't block user if privacy check fails - log and continue
        console.error('Failed to check privacy acknowledgment:', err);
      }
    });
  }

  /**
   * Handle when user acknowledges the privacy policy
   */
  onPrivacyAcknowledged(): void {
    console.log('Privacy policy acknowledged');
    this.showPrivacyModal = false;
    this.privacyAcknowledgmentStatus = null;
  }

  private updateNavbarVisibility(): void {
    const publicRoutes = ['/login', '/register', '/auth/callback', '/auth/activate', '/oauth-debug', '/access-denied', '/admin/login', '/redirect-uri-checker', '/privacy-policy'];
    const currentPath = this.router.url;
    
    // Show navbar only on authenticated pages
    this.showNavbar = this.authService.isAuthenticated() && 
                      !publicRoutes.some(route => currentPath.startsWith(route));
    
    // Show footer on all pages except some public routes (privacy-policy already has its own footer-like link)
    const noFooterRoutes = ['/auth/callback', '/oauth-debug', '/redirect-uri-checker'];
    this.showFooter = !noFooterRoutes.some(route => currentPath.startsWith(route));
    
    // Update body class to control padding
    if (this.showNavbar) {
      this.renderer.addClass(document.body, 'has-navbar');
    } else {
      this.renderer.removeClass(document.body, 'has-navbar');
    }
  }
}
