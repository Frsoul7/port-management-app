import { Component, Input } from '@angular/core';
import { CommonModule } from '@angular/common';
import { TranslatePipe } from '../../../core/pipes/translate.pipe';

@Component({
  selector: 'app-loading',
  standalone: true,
  imports: [CommonModule, TranslatePipe],
  templateUrl: './loading.component.html',
  styleUrls: ['./loading.component.scss']
})
export class LoadingComponent {
  @Input() message: string = 'COMMON.LOADING';
  @Input() overlay: boolean = true;
  @Input() size: 'small' | 'medium' | 'large' = 'medium';
}
