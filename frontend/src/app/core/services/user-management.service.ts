import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable } from 'rxjs';
import { environment } from '../../../environments/environment';
import { User, UpdateUserRequest, Organization } from '../models/user-management.model';

@Injectable({
  providedIn: 'root'
})
export class UserManagementService {
  private apiUrl = `${environment.apiUrl}/users`;
  private orgApiUrl = `${environment.apiUrl}/organizations`;

  constructor(private http: HttpClient) {}

  /**
   * Get all users (Admin only)
   */
  getAllUsers(): Observable<User[]> {
    return this.http.get<User[]>(this.apiUrl);
  }

  /**
   * Get user by ID (Admin only)
   */
  getUserById(userId: string): Observable<User> {
    return this.http.get<User>(`${this.apiUrl}/${userId}`);
  }

  /**
   * Update user (Admin only)
   */
  updateUser(userId: string, user: UpdateUserRequest): Observable<User> {
    return this.http.put<User>(`${this.apiUrl}/${userId}`, user);
  }

  /**
   * Activate user and assign role (Admin only) - Sends email notification
   */
  activateUser(userId: string, role: string, activate: boolean = true): Observable<{ success: boolean; message: string; user?: User }> {
    return this.http.post<{ success: boolean; message: string; user?: User }>(`${this.apiUrl}/activate`, {
      userId,
      role,
      activate
    });
  }

  /**
   * Get all organizations for dropdown
   */
  getOrganizations(): Observable<Organization[]> {
    return this.http.get<Organization[]>(this.orgApiUrl);
  }
}
