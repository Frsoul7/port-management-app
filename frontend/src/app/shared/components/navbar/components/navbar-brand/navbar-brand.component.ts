import { Component } from '@angular/core';
import { RouterLink } from '@angular/router';

/**
 * Navbar brand/logo component
 * Displays the application title with link to dashboard
 */
@Component({
  selector: 'app-navbar-brand',
  standalone: true,
  imports: [RouterLink],
  templateUrl: './navbar-brand.component.html',
  styleUrl: './navbar-brand.component.scss'
})
export class NavbarBrandComponent {}
