import { Component, OnInit, signal, inject } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { Router } from '@angular/router';
import { IncidentTypeService, IncidentType } from '../../core/services/incident-type.service';

interface TreeNode {
  type: IncidentType;
  children: TreeNode[];
  expanded: boolean;
  level: number;
}

/**
 * US 4.1.12: Incident Types Catalog Component
 * Hierarchical tree view showing parent-child relationships
 */
@Component({
  selector: 'app-list-incident-types',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './list-incident-types.component.html',
  styleUrls: ['./list-incident-types.component.scss']
})
export class ListIncidentTypesComponent implements OnInit {
  private incidentTypeService = inject(IncidentTypeService);
  private router = inject(Router);

  // Data
  incidentTypes = signal<IncidentType[]>([]);
  allIncidentTypes = signal<IncidentType[]>([]); // Unfiltered list for filter dropdown
  rootTypes = signal<IncidentType[]>([]);
  treeNodes = signal<TreeNode[]>([]);

  // State
  loading = signal(false);
  error = signal<string | null>(null);

  // Filters
  filters = {
    typeNameFilter: '', // Filter by specific incident type name
    activeOnly: true,
    searchText: ''
  };

  ngOnInit(): void {
    this.loadIncidentTypes();
  }

  /**
   * Load all incident types and build tree
   */
  async loadIncidentTypes(): Promise<void> {
    this.loading.set(true);
    this.error.set(null);

    try {
      const response = await this.incidentTypeService.listIncidentTypes({
        activeOnly: this.filters.activeOnly
      }).toPromise();

      if (response?.success && response.data) {
        this.allIncidentTypes.set(response.data); // Store all for filter dropdown
        this.incidentTypes.set(response.data);
        this.rootTypes.set(response.data.filter(t => t.isRootType));
        this.buildTree(response.data);
      }
    } catch (err: any) {
      this.error.set(err.error?.message || 'Failed to load incident types');
      console.error('Error loading incident types:', err);
    } finally {
      this.loading.set(false);
    }
  }

  /**
   * Build hierarchical tree structure
   */
  buildTree(types: IncidentType[]): void {
    const buildNode = (type: IncidentType, level: number): TreeNode => {
      const children = types
        .filter(t => t.parentTypeId === type.incidentTypeId)
        .map(t => buildNode(t, level + 1));

      return {
        type,
        children,
        expanded: true, // Expand all by default
        level
      };
    };

    const tree = this.rootTypes()
      .map(t => buildNode(t, 0));

    this.treeNodes.set(tree);
  }

  /**
   * Toggle tree node expansion
   */
  toggleNode(node: TreeNode): void {
    node.expanded = !node.expanded;
    this.treeNodes.set([...this.treeNodes()]); // Trigger change detection
  }

  /**
   * Get flat list of visible nodes for rendering
   */
  getFlatList(): TreeNode[] {
    const result: TreeNode[] = [];

    const traverse = (node: TreeNode) => {
      result.push(node);
      if (node.expanded && node.children.length > 0) {
        node.children.forEach(child => traverse(child));
      }
    };

    this.treeNodes().forEach(node => traverse(node));
    return result;
  }

  /**
   * Apply filters - reload with filter criteria
   */
  applyFilters(): void {
    if (this.filters.typeNameFilter) {
      // Filter by selected incident type name
      const filtered = this.allIncidentTypes().filter(t => 
        t.typeName === this.filters.typeNameFilter &&
        (!this.filters.activeOnly || t.isActive)
      );
      this.incidentTypes.set(filtered);
      this.rootTypes.set(filtered.filter(t => t.isRootType));
      this.buildTree(filtered);
    } else {
      // No filter, reload all
      this.loadIncidentTypes();
    }
  }

  /**
   * Clear all filters
   */
  clearFilters(): void {
    this.filters.typeNameFilter = '';
    this.filters.activeOnly = true;
    this.filters.searchText = '';
    this.loadIncidentTypes();
  }

  /**
   * Navigate to create new incident type
   */
  createIncidentType(): void {
    this.router.navigate(['/incident-types/create']);
  }

  /**
   * Navigate to edit incident type
   */
  editIncidentType(id: string): void {
    this.router.navigate(['/incident-types/edit', id]);
  }

  /**
   * Deactivate incident type
   */
  async deactivateIncidentType(type: IncidentType): Promise<void> {
    if (!confirm(`Are you sure you want to deactivate "${type.typeName}"?`)) {
      return;
    }

    try {
      await this.incidentTypeService.deactivateIncidentType(type.incidentTypeId).toPromise();
      await this.loadIncidentTypes();
    } catch (err: any) {
      alert(err.error?.message || 'Failed to deactivate incident type');
    }
  }

  /**
   * Reactivate incident type
   */
  async reactivateIncidentType(type: IncidentType): Promise<void> {
    try {
      await this.incidentTypeService.reactivateIncidentType(type.incidentTypeId).toPromise();
      await this.loadIncidentTypes();
    } catch (err: any) {
      alert(err.error?.message || 'Failed to reactivate incident type');
    }
  }

  /**
   * Delete incident type
   */
  async deleteIncidentType(type: IncidentType): Promise<void> {
    if (!confirm(`Are you sure you want to permanently delete "${type.typeName}"? This cannot be undone.`)) {
      return;
    }

    try {
      await this.incidentTypeService.deleteIncidentType(type.incidentTypeId).toPromise();
      await this.loadIncidentTypes();
    } catch (err: any) {
      alert(err.error?.message || 'Failed to delete incident type');
    }
  }

  /**
   * Get severity badge class
   */
  getSeverityClass(severity: string): string {
    switch (severity) {
      case 'LOW': return 'severity-low';
      case 'MEDIUM': return 'severity-medium';
      case 'HIGH': return 'severity-high';
      case 'CRITICAL': return 'severity-critical';
      default: return 'severity-medium';
    }
  }

  /**
   * Get category badge class
   */
  getCategoryClass(category: string): string {
    switch (category) {
      case 'ENV': return 'category-env';
      case 'OPS': return 'category-ops';
      case 'SAF': return 'category-saf';
      default: return 'category-ops';
    }
  }
}
