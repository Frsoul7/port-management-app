import { Routes } from '@angular/router';
import { authGuard } from './core/guards/auth.guard';
import { loginGuard } from './core/guards/login.guard';
import { adminGuard } from './core/guards/admin.guard';
import { storageAreaGuard } from './core/guards/storage-area.guard';
import { shippingAgentGuard } from './core/guards/shipping-agent.guard';

export const routes: Routes = [
  // Public authentication routes - only accessible when NOT logged in
  {
    path: 'login',
    loadComponent: () => import('./features/auth/login/login.component').then(m => m.LoginComponent),
    canActivate: [loginGuard]
  },
  {
    path: 'admin/login',
    loadComponent: () => import('./features/admin/admin-login.component').then(m => m.AdminLoginComponent),
    canActivate: [loginGuard]
  },
  {
    path: 'register',
    loadComponent: () => import('./features/auth/register/register.component').then(m => m.RegisterComponent),
    canActivate: [loginGuard]
  },

  // OAuth callback - no guard (needs to process auth response)
  {
    path: 'auth/callback',
    loadComponent: () => import('./features/auth/auth-callback/auth-callback.component').then(m => m.AuthCallbackComponent)
  },

  // Account activation - no guard (public link from email)
  {
    path: 'auth/activate',
    loadComponent: () => import('./features/auth/activate/activate.component').then(m => m.ActivateComponent)
  },

  // Privacy Policy - public page (GDPR compliance - US 4.5.2)
  {
    path: 'privacy-policy',
    loadComponent: () => import('./features/privacy-policy/privacy-policy.component').then(m => m.PrivacyPolicyComponent)
  },

  // Data Rights - authenticated users (GDPR compliance - US 4.5.3)
  {
    path: 'profile/data-rights',
    loadComponent: () => import('./features/profile/data-rights/data-rights.component').then(m => m.DataRightsComponent),
    canActivate: [authGuard]
  },

  // Debug/utility routes - no guard (for development/troubleshooting)
  {
    path: 'oauth-debug',
    loadComponent: () => import('./features/auth/oauth-debug/oauth-debug.component').then(m => m.OAuthDebugComponent)
  },
  {
    path: 'redirect-uri-checker',
    loadComponent: () => import('./features/auth/redirect-uri-checker/redirect-uri-checker.component').then(m => m.RedirectUriCheckerComponent)
  },

  // Public error page
  {
    path: 'access-denied',
    loadComponent: () => import('./features/auth/access-denied/access-denied.component').then(m => m.AccessDeniedComponent)
  },

  // Protected routes - require authentication
  {
    path: 'dashboard',
    loadComponent: () => import('./features/dashboard/dashboard.component').then(m => m.DashboardComponent),
    canActivate: [authGuard]
  },

  // Vessels - All authenticated users can view
  {
    path: 'vessels',
    loadComponent: () => import('./features/vessels/vessels.component').then(m => m.VesselsComponent),
    canActivate: [authGuard]
  },

  // Vessel Types - Port Authority can manage
  {
    path: 'vessel-types',
    loadComponent: () => import('./features/vessel-types/vessel-types.component').then(m => m.VesselTypesComponent),
    canActivate: [authGuard]
  },

  // Docks - All authenticated users can view
  {
    path: 'docks',
    loadComponent: () => import('./features/docks/docks.component').then(m => m.DocksComponent),
    canActivate: [authGuard]
  },

  // Storage Areas - Logistics Operators, Port Authority, and Administrators
  {
    path: 'storage-areas',
    loadComponent: () => import('./features/storage-areas/storage-areas.component').then(m => m.StorageAreasComponent),
    canActivate: [storageAreaGuard]
  },

  // Physical Resources - All authenticated users can view
  {
    path: 'physical-resources',
    loadComponent: () => import('./features/physical-resources/physical-resources.component').then(m => m.PhysicalResourcesComponent),
    canActivate: [authGuard]
  },

  // VVNs - All authenticated users (different views based on role)
  {
    path: 'vvns',
    loadComponent: () => import('./features/vvns/vvns.component').then(m => m.VvnsComponent),
    canActivate: [authGuard]
  },

  // Operation Plans - Logistics Operators and Port Authority (US 4.1.2, 4.1.3, 4.1.4, 4.1.5, 4.1.6)
  {
    path: 'operation-plans/generate',
    loadComponent: () => import('./features/operation-plans/generate-operation-plan.component').then(m => m.GenerateOperationPlanComponent),
    canActivate: [authGuard]
  },
  {
    path: 'operation-plans/missing',
    loadComponent: () => import('./features/operation-plans/missing-plans.component').then(m => m.MissingPlansComponent),
    canActivate: [authGuard]
  },
  {
    path: 'operation-plans/resource-allocation',
    loadComponent: () => import('./features/operation-plans/resource-allocation.component').then(m => m.ResourceAllocationComponent),
    canActivate: [authGuard]
  },
  {
    path: 'operation-plans/:id/edit',
    loadComponent: () => import('./features/operation-plans/edit-operation-plan.component').then(m => m.EditOperationPlanComponent),
    canActivate: [authGuard]
  },
  {
    path: 'operation-plans',
    loadComponent: () => import('./features/operation-plans/list-operation-plans.component').then(m => m.ListOperationPlansComponent),
    canActivate: [authGuard]
  },

  // Vessel Visit Executions - Logistics Operators and Port Authority (US 4.1.7, 4.1.8, 4.1.9, 4.1.10, 4.1.11)
  {
    path: 'vessel-visit-executions/:id/operations',
    loadComponent: () => import('./features/vessel-visit-executions/manage-vve-operations.component').then(m => m.ManageVveOperationsComponent),
    canActivate: [authGuard]
  },
  {
    path: 'vessel-visit-executions',
    loadComponent: () => import('./features/vessel-visit-executions/list-vves.component').then(m => m.ListVvesComponent),
    canActivate: [authGuard]
  },

  // Shipping Agents - Port Authority Officers and Administrators (US 2.2.5, 2.2.6)
  {
    path: 'shipping-agents',
    loadComponent: () => import('./features/shipping-agents/shipping-agents.component').then(m => m.ShippingAgentsComponent),
    canActivate: [shippingAgentGuard]
  },

  // Admin-only routes - require authentication AND administrator role
  {
    path: 'admin/users',
    loadComponent: () => import('./features/admin/user-management.component').then(m => m.UserManagementComponent),
    canActivate: [adminGuard]
  },

  // Privacy Policy Admin - Administrator only (US 4.5.1)
  {
    path: 'admin/privacy-policy',
    loadComponent: () => import('./features/privacy-policy/admin/privacy-policy-admin.component').then(m => m.PrivacyPolicyAdminComponent),
    canActivate: [adminGuard]
  },

  // Incident Types Management - Port Authority Officers and Administrators (US 4.1.12)
  {
    path: 'incident-types',
    loadComponent: () => import('./features/admin/list-incident-types.component').then(m => m.ListIncidentTypesComponent),
    canActivate: [authGuard]
  },
  {
    path: 'incident-types/create',
    loadComponent: () => import('./features/admin/incident-type-form.component').then(m => m.IncidentTypeFormComponent),
    canActivate: [authGuard]
  },
  {
    path: 'incident-types/edit/:id',
    loadComponent: () => import('./features/admin/incident-type-form.component').then(m => m.IncidentTypeFormComponent),
    canActivate: [authGuard]
  },

  // Incidents Management - Logistics Operators and Port Authority (US 4.1.13)
  {
    path: 'incidents-records',
    loadComponent: () => import('./features/incidents/list-incidents.component').then(m => m.ListIncidentsComponent),
    canActivate: [authGuard]
  },
  {
    path: 'incidents-records/create',
    loadComponent: () => import('./features/incidents/incident-form.component').then(m => m.IncidentFormComponent),
    canActivate: [authGuard]
  },
  {
    path: 'incidents-records/edit/:id',
    loadComponent: () => import('./features/incidents/incident-form.component').then(m => m.IncidentFormComponent),
    canActivate: [authGuard]
  },
  {
    path: 'incidents-records/:id',
    loadComponent: () => import('./features/incidents/list-incidents.component').then(m => m.ListIncidentsComponent),
    canActivate: [authGuard]
  },

  // Task Categories Management - Port Operations Supervisors and Port Authority (US 4.1.14)
  {
    path: 'task-categories',
    loadComponent: () => import('./features/task-categories/list-task-categories.component').then(m => m.ListTaskCategoriesComponent),
    canActivate: [authGuard]
  },

  // Complementary Tasks Management - Logistics Operators and Port Authority (US 4.1.15)
  {
    path: 'complementary-tasks',
    loadComponent: () => import('./features/complementary-tasks/pages/list-complementary-tasks/list-complementary-tasks.component').then(m => m.ListComplementaryTasksComponent),
    canActivate: [authGuard]
  },
  {
    path: 'complementary-tasks/create',
    loadComponent: () => import('./features/complementary-tasks/pages/create-complementary-task/create-complementary-task.component').then(m => m.CreateComplementaryTaskComponent),
    canActivate: [authGuard]
  },
  {
    path: 'complementary-tasks/:id/edit',
    loadComponent: () => import('./features/complementary-tasks/pages/edit-complementary-task/edit-complementary-task.component').then(m => m.EditComplementaryTaskComponent),
    canActivate: [authGuard]
  },

  // 3D Visualization - authenticated users
  {
    path: 'visualization',
    loadComponent: () =>
      import('./features/visualization/port-3d.component')
        .then(m => m.Port3DComponent),
    canActivate: [authGuard]
  },
  // Default redirects
  {
    path: '',
    redirectTo: '/dashboard',
    pathMatch: 'full'
  },
  {
    path: '**',
    redirectTo: '/dashboard'
  }
];
