import { Component, EventEmitter, Output } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';

export interface UserSearchParams {
  name: string;
  email: string;
  organizationName: string;
}

@Component({
  selector: 'app-user-search-bar',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './user-search-bar.component.html',
  styleUrls: ['./user-search-bar.component.scss']
})
export class UserSearchBarComponent {
  @Output() onSearch = new EventEmitter<UserSearchParams>();
  @Output() onClear = new EventEmitter<void>();

  searchParams: UserSearchParams = {
    name: '',
    email: '',
    organizationName: ''
  };

  search(): void {
    this.onSearch.emit(this.searchParams);
  }

  clear(): void {
    this.searchParams = {
      name: '',
      email: '',
      organizationName: ''
    };
    this.onClear.emit();
  }

  handleEnter(): void {
    this.search();
  }
}
