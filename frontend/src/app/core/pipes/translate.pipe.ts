import { Pipe, PipeTransform, inject, effect } from '@angular/core';
import { TranslationService } from '../services/translation.service';

/**
 * Translation pipe for use in templates
 * Usage: {{ 'NAV.HOME' | translate }}
 * Automatically updates when language changes using Angular signals
 */
@Pipe({
  name: 'translate',
  standalone: true,
  pure: false // Must be impure to react to signal changes
})
export class TranslatePipe implements PipeTransform {
  private translationService = inject(TranslationService);

  transform(key: string): string {
    if (!key) return '';
    
    // This accesses the translations signal which will trigger change detection
    return this.translationService.translate(key);
  }
}
