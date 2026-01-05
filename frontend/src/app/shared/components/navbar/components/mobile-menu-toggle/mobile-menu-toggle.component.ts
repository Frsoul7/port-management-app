import { Component, EventEmitter, Input, Output } from '@angular/core';
import { CommonModule } from '@angular/common';

/**
 * Mobile hamburger menu toggle button component
 * Displays animated hamburger icon that transforms to X when active
 */
@Component({
  selector: 'app-mobile-menu-toggle',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './mobile-menu-toggle.component.html',
  styleUrl: './mobile-menu-toggle.component.scss'
})
export class MobileMenuToggleComponent {
  @Input() isActive = false;
  @Output() toggle = new EventEmitter<void>();

  onToggle(): void {
    this.toggle.emit();
  }
}
